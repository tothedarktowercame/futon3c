(ns futon3c.smoke.traverse-test
  "Runner-only smoke traversal for the problem cycle."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute FileTime]))

(def ^:private registration
  ;; The frozen round-1 EDN predates the seat-key gate (:unstaffed-carded-seat)
  ;; and must not be edited; the traverse staffs the seats in-memory since the
  ;; payload carries the registration map directly.
  (assoc (edn/read-string
          (slurp "holes/labs/M-apm-demonstration/round1-registration.edn"))
         :reg/guide-seat "traverse-guide"
         :reg/proctor-seat "traverse-proctor"
         :reg/scribe-seat "traverse-scribe"
         :reg/student-seat "zai-1"))

;; READ FROM THE REGISTRATION, not hard-coded. Both pins are compared against the
;; frozen registration by environment-arms-match, so a literal here means every
;; legitimate pre-launch re-pin reddens the traverse -- which is exactly what
;; happened when the harness pin moved to 7787eb9a after the eligible-vector
;; recording. The traverse's job is to walk the phases; the pins have their own
;; dedicated tests (provisioned-revision-must-match-the-registration-pin and the
;; harness-revision one), and those still supply mismatching values on purpose.
(def ^:private env-revision (:reg/environment-revision registration))
(def ^:private harness-revision (:reg/harness-revision registration))

(defn- smoke-problem []
  (let [clock (atom 0)]
    (problem/make-problem
     (tools/make-mock-backend)
     (fn [& _]
       {:ok true :job-id "smoke"
        :evidence {:offer/id "offer/smoke"
                   :body {:eligible-memory-ids ["memory/smoke-a"
                                                "memory/smoke-b"]
                          :memory-use
                          {:memory-use/surfaced-ids ["memory/smoke-a"]}}}
        :environment-checkout
        {:checkout "/tmp/smoke/student" :base-revision env-revision}})
     "/tmp/smoke-state"
     (fn [{:keys [arm]}]
       {:checkout (str "/tmp/smoke/" (name (or arm :solver)))
        :base-revision env-revision})
     (fn [_]
       {:harness-revision harness-revision :harness-tree-dirty? false})
     (constantly ["memory/smoke-a" "memory/smoke-b"])
     #(swap! clock inc))))

(def ^:private phase-payloads
  {:register
   {:registration registration
    :environment-revision env-revision
    :harness-revision harness-revision
    :environment-checkouts {}}
   :frame
   {:frame {:frame/id "frame-1" :frame/scaffold-hash "s"
            :frame/closing-hash "c"}
    :containment-probe {:cprobe/id "cp-1" :cprobe/frame "frame-1"
                        :cprobe/claimed? true :cprobe/recorded? true}}
   :guided-solve
   {:solver-attempt {:attempt/id "a-solver" :attempt/seq 0
                     :cycle/regime "r" :cycle/store-revision "s"
                     :cycle/runner-freshness :cold}
    :ground-control-events [] :memory-offers []}
   :intervene {:intervention {:kind :store-write}}
   :student-attempts
   {:student-attempts [{:attempt/id "a-student" :attempt/seq 1
                        :cycle/regime "r" :cycle/store-revision "s"
                        :cycle/runner-freshness :cold}]
    :memory-uses []}
   :adjudicate {:disposition [{:disp/id "FORGED" :disp/cycle "FORGED"}]}
   :promote {:promotion-result [{:promo/id "FORGED"}]}
   :close {}})

(def ^:private phase-tools
  (assoc
   (into {}
         (for [[phase tool-set] problem/base-phase-tools]
           [phase (vec (remove #(or (= problem/advance %)
                                    (= :observe (get problem/tool-ops %)))
                               tool-set))]))
   :register
   [:snapshot-store :freeze-stratum :assign-checkouts]))

(defn- run-phase-tools [p state phase]
  (let [tool-args
        {:assign-checkouts
         [{:problem "t94J02" :batch "frame-1" :base-rev env-revision
           :solver-seat "codex-4" :student-seat "zai-1"
           :recall-system "futon1b"}]
         :write-disposition [{:outcome :closed}]
         :write-use [{:offer-id "offer/smoke"}]
         ;; guidance must declare a typed-bell performative (feat/typed-guidance)
         :guide-solver [{:bell-type :suggest} {:note "smoke continuation"}]
         :promote-artifact [{:artifact-id "artifact/smoke"
                             :importable? true
                             :need-tags ["smoke"]}]
         ;; author must equal the staffed :reg/scribe-seat (seat discipline)
         :record-scribe-lanes [{:lane :solve :ran? true
                                :yield ["memory/smoke-a"]
                                :author "traverse-scribe"}]}]
    (reduce
     (fn [{:keys [state] :as walk} tool]
       (if (:stop walk)
         (reduced walk)
         (let [result
               (if (= tool :emit-frame)
                 (let [scaffold (Files/createTempFile
                                 "smoke-scaffold-" ".lean"
                                 (make-array FileAttribute 0))
                       closing (Files/createTempFile
                                "smoke-closing-" ".lean"
                                (make-array FileAttribute 0))
                       witness (Files/createTempFile
                                "smoke-containment-" ".edn"
                                (make-array FileAttribute 0))]
                   (try
                     (spit (.toFile scaffold) "scaffold\n")
                     (spit (.toFile closing) "closing\n")
                     (spit (.toFile witness) "{:contained? true}\n")
                     (Files/setLastModifiedTime scaffold
                                                (FileTime/fromMillis 1000))
                     (Files/setLastModifiedTime closing
                                                (FileTime/fromMillis 2000))
                     (runner/step
                      p state
                      {:tool tool
                       :args [{:scaffold-path scaffold :closing-path closing
                               :containment-witness-path witness
                               :containment-claimed? true}]})
                     (finally
                       (Files/deleteIfExists scaffold)
                       (Files/deleteIfExists closing)
                       (Files/deleteIfExists witness))))
                 (runner/step p state
                              {:tool tool :args (get tool-args tool [])}))]
           (if (:ok result)
             {:state (:state result)}
             {:state state
              :stop {:phase phase :tool tool :code (:error/code result)
                     ;; carry the reason: a bare :tool-execution-failed cost a
                     ;; debugging round-trip when :close reddened (W.25)
                     :message (:error/message result)
                     :context (:error/context result)}}))))
     {:state state}
     (get phase-tools phase))))

(deftest smoke-traverse-through-runner-state
  (let [p (smoke-problem)
        context {:session-id "smoke" :problem-id "t94J02"
                 :cycle/mode :store-mode
                 ;; required since 58b2e1cb (EvidenceRequiredProblemPeripheral);
                 ;; without it start REFUSES, and stepping on the refusal's nil
                 ;; :state silently begins a context-less cycle that only dies
                 ;; at :close when :validate-trace needs :lean-repo (W.25)
                 :evidence-store (atom {:entries {} :order []})
                 :harness-repo "/home/joe/code/futon3c"
                 :lean-repo "/home/joe/code/mathlib4"
                 :agency-endpoint "http://localhost:7070/api/alpha/invoke/jobs?limit=200"
                 :authorization-revision (apply str (repeat 40 "a"))
                 :authorization-output "/tmp/smoke-auth.edn"}
        started (runner/start p context)
        begun (runner/step p (:state started)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        register-facts (atom nil)
        close-envelope (atom nil)
        outcome
        (loop [state (:state begun), advances 0]
          (if-let [phase (:current-phase state)]
            (if (> advances 12)
              {:stop {:phase phase :code :traversal-limit}}
              (let [{tool-state :state tool-stop :stop}
                    (run-phase-tools p state phase)]
                (if tool-stop
                  {:stop tool-stop}
                  (let [advanced
                        (runner/step
                         p tool-state
                         {:tool problem/advance
                          :args ["M" "C" (get phase-payloads phase {})]})]
                    (if (:ok advanced)
                      (do
                        (when (= phase :close)
                          (reset! close-envelope
                                  (some #(when (= :emit-trace (:tool %))
                                           (:result %))
                                        (reverse (:steps tool-state)))))
                        (when (= phase :register)
                          (reset! register-facts
                                  (select-keys
                                   (get-in advanced [:state :cycle/outputs])
                                   [:store-snapshot :stratum-frozen-at
                                    :assigned-at])))
                        (recur (:state advanced) (inc advances)))
                      {:stop {:phase phase :tool problem/advance
                              :code (:error/code advanced)}})))))
            {:completed? true :advances advances}))]
    (is (:ok started)
        "start must succeed; stepping past a start refusal runs context-less")
    (is (= :register (get-in begun [:state :current-phase])))
    (println "SMOKE register facts" (pr-str @register-facts))
    (println "SMOKE close envelope" (pr-str @close-envelope))
    (println "SMOKE traversal" (pr-str outcome))
    (is (= ["memory/smoke-a" "memory/smoke-b"]
           (get-in @register-facts [:store-snapshot :snap/memory-ids])))
    (is (< (:stratum-frozen-at @register-facts)
           (:assigned-at @register-facts)))
    (is (not-any? #(= :retrieval-probe (:entity-type %))
                  (:producer-failures @close-envelope)))
    (is (= {:completed? true :advances 8} outcome)
        "all eight real phases advance through :completed, which clears state")))
