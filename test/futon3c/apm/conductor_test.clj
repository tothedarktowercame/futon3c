(ns futon3c.apm.conductor-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.conductor :as conductor]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.tools :as tools])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute FileTime]))

(def ^:private registration-path
  "holes/labs/M-apm-demonstration/round1-registration.edn")

(def ^:private registration (edn/read-string (slurp registration-path)))
(def ^:private environment-revision (:reg/environment-revision registration))
(def ^:private harness-revision (:reg/harness-revision registration))

;; The frozen round-1 EDN predates the seat-key gate (:unstaffed-carded-seat,
;; merged with feat/registration-seat-keys) and must not be edited, so the
;; fixture stages a staffed copy under a temp path for the machine to read.
(def ^:private staffed-registration
  (assoc registration
         :reg/guide-seat "conductor-test"
         :reg/proctor-seat "proctor-test"
         :reg/scribe-seat "scribe-test"
         :reg/student-seat "zai-1"))

(defn- fixture []
  (let [state-root (.toFile
                    (Files/createTempDirectory
                     "conductor-state-" (make-array FileAttribute 0)))
        scaffold (Files/createTempFile "conductor-scaffold-" ".lean"
                                       (make-array FileAttribute 0))
        closing (Files/createTempFile "conductor-closing-" ".lean"
                                      (make-array FileAttribute 0))
        witness (Files/createTempFile "conductor-witness-" ".edn"
                                      (make-array FileAttribute 0))
        authorization (Files/createTempFile "conductor-authorization-" ".edn"
                                            (make-array FileAttribute 0))
        staffed-reg (Files/createTempFile "conductor-registration-" ".edn"
                                          (make-array FileAttribute 0))
        deposit-seq (atom 0)
        dispatch-fn
        (fn [_ _]
          {:ok true :job-id "job-test"
           :evidence {:body {:job-id "job-test"
                             :eligible-memory-ids ["memory/a" "memory/b"]
                             :memory-use
                             {:memory-use/surfaced-ids ["memory/a"]}}}})
        provisioner
        (fn [{:keys [arm branch batch]}]
          {:checkout (str "/tmp/conductor/" arm)
           :base-revision environment-revision
           :branch branch :frame/id (str batch "-" arm) :batch batch})
        peripheral
        (problem/make-problem
         (tools/make-mock-backend) dispatch-fn (.getPath state-root) provisioner
         (fn [_] {:harness-revision harness-revision
                  :harness-tree-dirty? false})
         (constantly ["memory/a" "memory/b"])
         (constantly 0)
         (fn [_ _]
           {:ok true :id (str "memory/deposit-" (swap! deposit-seq inc))}))]
    (spit (.toFile scaffold) "scaffold\n")
    (spit (.toFile closing) "closing\n")
    (spit (.toFile witness) "{:contained? true}\n")
    (spit (.toFile staffed-reg) (pr-str staffed-registration))
    (Files/setLastModifiedTime scaffold (FileTime/fromMillis 1000))
    (Files/setLastModifiedTime closing (FileTime/fromMillis 2000))
    {:config
     {:session-id "conductor-test" :problem-id "t94J02" :mode :store-mode
      :registration-path (str staffed-reg)
      :frame {:scaffold-path scaffold :closing-path closing
              :witness-path witness}
      :checkout {:batch "conductor-test" :base-rev environment-revision
                 :solver-seat "codex-4" :student-seat "zai-1"
                 :recall-system "futon1b"}
      :evidence-store (atom {:entries {} :order []})
      :harness-repo "/harness" :lean-repo "/lean"
      :agency-endpoint "http://127.0.0.1:1/unreachable"
      :authorization-revision (apply str (repeat 40 "a"))
      :authorization-output (str authorization)
      :conductor "conductor-test" :peripheral peripheral}
     :paths [scaffold closing witness authorization staffed-reg]}))

(defn- solver-attempt []
  {:attempt/id "attempt/solver" :attempt/seq 0
   :cycle/regime "round-1" :cycle/store-revision "store-1"
   :cycle/runner-freshness :cold})

(defn- student-attempt []
  {:attempt/id "attempt/student" :attempt/seq 1
   :cycle/regime "round-1" :cycle/store-revision "store-2"
   :cycle/runner-freshness :cold})

(deftest conductor-runs-a-refused-cycle-and-keeps-its-rider-ledger
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            missing-mission (conductor/dispatch-solver! opened {} "packet")
            solver (conductor/dispatch-solver! opened {:mission "M-test"} "packet")
            intervening (conductor/record-solver-attempt!
                         solver (solver-attempt) {})
            deposited (conductor/deposit!
                       intervening
                       {:name "deposit" :kind :feedback :hook "test"
                        :body {:lesson "ledger"}
                        :subjects [{:ref/type :problem :ref/id "t94J02"}]})
            student (conductor/dispatch-student!
                     deposited {:mission "M-test"} "student packet")
            adjudicating (conductor/record-students!
                          student [(student-attempt)] [])
            closing (conductor/adjudicate!
                     adjudicating
                     {:outcome :tier-a :residual-sorries 1 :axiom-clean? false
                      :promotion-result []})
            closed (conductor/close! closing)]
        (is (:ok opened) (pr-str (:error opened)))
        (is (= :guided-solve (get-in opened [:state :current-phase])))
        (is (= :mission-absent (get-in missing-mission [:error :error/code])))
        (is (= ["memory/deposit-1"] (:deposits deposited)))
        (is (nil? (get-in closed [:state :current-phase]))
            "the final advance reaches the terminal sentinel")
        (is (false? (get-in closed [:envelope :launchable?])))
        (is (seq (get-in closed [:envelope :failures]))
            "round-one closes with an honest refusal envelope")
        (is (= [{:offer/id "offer/job-test/0"
                 :offer/memory-id "memory/a"}]
               (mapv #(select-keys % [:offer/id :offer/memory-id])
                     (get-in closed [:state :cycle/outputs :memory-offers])))
            "the conductor converts the dispatch receipt into offer entities")
        (is (not-any? #{:malformed-memory-offers}
                      (get-in closed [:envelope :failures]))
            "conductor-collected receipts validate as memory-offer entities"))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest resume-loads-a-checkpoint-and-can-continue
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            version (count (filter #(= :problem-save (:tool %)) (:log opened)))
            resumed (conductor/resume opened (:cycle-id opened) version)
            continued (conductor/dispatch-solver!
                       resumed {:mission "M-test"} "continued packet")]
        (is (:ok opened) (pr-str (:error opened)))
        (is (:ok resumed) (pr-str (:error resumed)))
        (is (= :guided-solve (get-in resumed [:state :current-phase])))
        (is (:ok continued) (pr-str (:error continued)))
        (is (= :dispatch-solver (:tool (last (remove #(= :problem-save (:tool %))
                                                     (:log continued)))))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest open-frame-refuses-invalid-mode-and-threads-conductor
  (let [{:keys [config]} (fixture)]
    (let [bad (conductor/open-frame! (assoc config :mode nil))]
      (is (false? (:ok bad)))
      (is (re-find #"store-mode" (or (:error/message bad) (str bad)))))
    (let [h (conductor/open-frame! (assoc config
                                          :mode :store-mode
                                          :deposit-state :with-deposit))]
      (is (not (false? (:ok h))))
      (is (= :store-mode (get-in h [:state :cycle/mode])))
      (is (= :with-deposit (get-in h [:state :cycle/deposit-state]))))))
