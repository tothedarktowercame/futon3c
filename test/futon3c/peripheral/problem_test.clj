(ns futon3c.peripheral.problem-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.cycle-harness :as apm-harness]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.apm.preregistration :as prereg])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def dispatch-packet
  (apply str (repeat 220 "p")))

(defn dispatch-state [phase]
  (let [calls (atom [])
        dispatch-fn (fn [opts packet]
                      (swap! calls conj {:opts opts :packet packet})
                      {:job-id "job-1"
                       :assembled-packet packet
                       :evidence {:body {:memory-channel (:memory-channel opts)}}})
        p (problem/make-problem (tools/make-mock-backend) dispatch-fn)
        result (runner/start p {:session-id "s" :problem-id "p"
                                :cycle/mode :store-mode})]
    [p (assoc (:state result) :current-phase phase) calls]))

(deftest solver-dispatch-uses-push-plus-pull-and-returns-dispatcher-receipt
  (let [[p state calls] (dispatch-state :guided-solve)
        result (runner/step p state
                            {:tool :dispatch-solver
                             :args [{:problem "p" :to "codex-4"}
                                    dispatch-packet]})
        receipt (get-in result [:result :memory-offers 0])]
    (is (:ok result))
    (is (= :push+pull (get-in @calls [0 :opts :memory-channel])))
    (is (= {:body {:memory-channel :push+pull}} receipt))))

(deftest student-dispatch-is-pull-only-and-pushes-no-memories
  (let [recall-result {:status :ok
                       :memories [{:memory/id "must-not-reach-student"
                                   :memory/body {:content "a hint"}}]}
        calls (atom [])
        dispatch-fn
        (fn [opts packet]
          (swap! calls conj opts)
          {:assembled-packet
           (dispatch-with-recall/assemble-packet
            packet recall-result (:memory-channel opts))
           :evidence
           (dispatch-with-recall/offered-evidence
            opts (assoc recall-result :memories []) "job-1" "session-1")})
        provisioner (fn [{:keys [arm] :as options}]
                      {:checkout (str "/frames/" arm)
                       :base-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                       :branch (:branch options) :frame/id (str "b-p-" arm)
                       :batch "b"})
        p (problem/make-problem (tools/make-mock-backend) dispatch-fn
                                "/tmp/problem-test-state" provisioner)
        start (runner/start p {:session-id "s" :problem-id "p"
                               :cycle/mode :store-mode})
        assigned (runner/step
                  p (assoc (:state start) :current-phase :register)
                  {:tool :assign-checkouts
                   :args [{:problem "p" :batch "b" :base-rev "HEAD"
                           :solver-seat "codex-4" :student-seat "zai-1"
                           :recall-system "v1"}]})
        result (runner/step
                p (assoc (:state assigned) :current-phase :student-attempts)
                {:tool :dispatch-student-fresh
                 :args [{:problem "p" :to "zai-1"} dispatch-packet]})]
    (is (:ok result))
    (is (= :pull-only (:memory-channel (first @calls))))
    (is (not (.contains ^String (get-in result [:result :assembled-packet])
                        "must-not-reach-student")))
    (is (empty? (get-in result
                        [:result :memory-offers 0 :body :memory-use
                         :memory-use/surfaced-memory-ids])))))

(deftest role-channel-cannot-be-overridden-by-the-caller
  ;; The role fixes the channel. A caller-supplied :push+pull reaching the
  ;; student would be a containment breach of the same family as I.38's
  ;; environment pass-note, so the precedence is asserted, not just written.
  (let [calls (atom [])
        dispatch-fn (fn [opts _] (swap! calls conj (:memory-channel opts))
                      {:evidence {}})
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) dispatch-fn)]
    (tools/execute-tool backend :dispatch-student-fresh
                        [{:memory-channel :push+pull} "packet"])
    (tools/execute-tool backend :dispatch-solver
                        [{:memory-channel :none} "packet"])
    (is (= [:pull-only :push+pull] @calls))))

(deftest failed-bell-is-a-tool-failure-not-an-escaping-exception
  ;; ToolBackend promises {:ok true :result} | {:ok false :error}. run-dispatch!
  ;; throws when Agency returns no job-id, so an unguarded call breaks that
  ;; contract and the cycle cannot record its own failure.
  (let [boom (fn [_ _] (throw (ex-info "Agency bell returned no job-id" {})))
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) boom)
        result (tools/execute-tool backend :dispatch-solver
                                   [{:problem "p"} "packet"])]
    (is (false? (:ok result)))
    (is (re-find #"no job-id" (:error result)))))

(deftest failed-recall-still-records-an-empty-offer
  (let [backend (problem/make-ground-control-backend
                 (tools/make-mock-backend)
                 dispatch-with-recall/run-dispatch!)
        dispatch-result (atom nil)
        result
        (with-out-str
          (with-redefs [dispatch-with-recall/safe-recall
                        (fn [_ _]
                          {:status :recall-failed
                           :reason :probe-failure
                           :error "store unavailable"
                           :memories []})]
            (reset! dispatch-result
                    (tools/execute-tool
                     backend :dispatch-solver
                     [{:problem "p" :to "codex-4" :dry-run? true}
                      dispatch-packet]))))]
    (is (string? result))
    (is (:ok @dispatch-result))
    (is (= :recall-failed
           (get-in @dispatch-result
                   [:result :memory-offers 0 :body :recall-status])))
    (is (empty? (get-in @dispatch-result
                        [:result :memory-offers 0 :body :memory-use
                         :memory-use/surfaced-memory-ids])))))

(defn started [mode]
  (let [p (problem/make-problem (tools/make-mock-backend))
        result (runner/start p {:session-id "s" :problem-id "p"
                                :cycle/mode mode})]
    [p (:state result)]))

(deftest student-phase-has-no-substrate-write
  (let [[p state] (started :store-mode)
        result (runner/step p (assoc state :current-phase :student-attempts)
                            {:tool :write-substrate :args []})]
    (is (= :phase-tool-not-allowed (:error/code result)))))

(deftest intervention-tools-are-mutually-exclusive
  (let [[store-p store-state] (started :store-mode)
        [harness-p harness-state] (started :harness-mode)]
    (is (= :phase-tool-not-allowed
           (:error/code (runner/step store-p
                                     (assoc store-state :current-phase :intervene)
                                     {:tool :tune-harness :args []}))))
    (is (= :phase-tool-not-allowed
           (:error/code (runner/step harness-p
                                     (assoc harness-state :current-phase :intervene)
                                     {:tool :write-substrate :args []}))))))

(deftest later-phase-refuses-missing-earlier-output
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :frame
                           :cycle/outputs {:frame :f :containment-probe :c})
        result (runner/step p state
                            {:tool :advance-problem-phase
                             :args ["M" "C" {:frame :f
                                               :containment-probe :c}]})]
    (is (= :missing-required-outputs (:error/code result)))
    (is (some #{:registration} (get-in result [:error/context :missing])))))

(deftest register-refuses-missing-resource-pin
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :register :cycle/outputs {})
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C" {:registration :r :store-snapshot :s
                                    :stratum-frozen-at 1
                                    :environment-revision "env"}]})]
    (is (= :missing-required-outputs (:error/code result)))
    (is (some #{:harness-revision}
              (get-in result [:error/context :missing])))))

(deftest register-refuses-missing-environment-checkouts
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :register :cycle/outputs {})
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C" {:registration :r :store-snapshot :s
                                    :stratum-frozen-at 1
                                    :environment-revision "env"
                                    :harness-revision "harness"}]})]
    (is (= :missing-required-outputs (:error/code result)))
    (is (some #{:environment-checkouts}
              (get-in result [:error/context :missing])))))

(def checkout-options
  {:batch "round-1"
   :problem "t94J02"
   :base-rev "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   :solver-seat "codex-4"
   :student-seat "zai-1"
   :recall-system "apm-v1"})

(deftest assign-checkouts-provisions-the-solver-with-a-batch-qualified-branch
  (let [calls (atom [])
        revision "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        provisioner (fn [options]
                      (swap! calls conj options)
                      {:checkout (str "/frames/" (:arm options))
                       :base-revision revision
                       :branch (:branch options)
                       :frame/id (str (:batch options) "-"
                                      (:problem options) "-" (:arm options))})
        backend (problem/make-checkout-provisioning-backend
                 (tools/make-mock-backend) provisioner)
        result (tools/execute-tool backend :assign-checkouts [checkout-options])
        checkouts (get-in result [:result :environment-checkouts])]
    (is (:ok result))
    (is (= ["solver"] (mapv :arm @calls)))
    (is (= ["exp/round-1-t94J02-solver"]
           (mapv :branch @calls)))
    (is (= ["push"] (mapv :memory-channel @calls)))
    (is (= revision (get-in checkouts [:solver :base-revision])))
    (is (= [] (:student checkouts)))))

(deftest assign-checkouts-provisioner-failure-is-a-tool-failure
  (let [backend (problem/make-checkout-provisioning-backend
                 (tools/make-mock-backend)
                 (fn [_] (throw (ex-info "branch collision" {}))))
        result (tools/execute-tool backend :assign-checkouts [checkout-options])]
    (is (false? (:ok result)))
    (is (re-find #"branch collision" (:error result)))))

(deftest assign-checkouts-is-wired-through-the-register-phase
  (let [root (.toFile (Files/createTempDirectory
                       "checkout-register-" (make-array FileAttribute 0)))
        provisioner (fn [options]
                      {:checkout (str "/frames/" (:arm options))
                       :base-revision (:base-rev options)
                       :branch (:branch options)
                       :frame/id (str (:batch options) "-" (:problem options)
                                      "-" (:arm options))})
        p (problem/make-problem
           (tools/make-mock-backend)
           (fn [_ _] (throw (ex-info "not dispatched" {})))
           root provisioner)
        start (runner/start p {:session-id "checkout-register"
                               :problem-id "t94J02"
                               :cycle/mode :store-mode})
        state (assoc (:state start) :current-phase :register)
        result (runner/step p state
                            {:tool :assign-checkouts :args [checkout-options]})]
    (is (:ok result))
    (is (= #{:solver :student}
           (set (keys (get-in result [:result :environment-checkouts])))))))

(def outputs-through-student
  {:registration :r :store-snapshot :round-open :stratum-frozen-at 1
   :environment-revision "env-a" :harness-revision "harness-a"
   :environment-checkouts {:solver {:checkout "/solver"
                                    :base-revision "env-a"}
                           :student [{:checkout "/student"
                                      :base-revision "env-a"}]}
   :frame :f :containment-probe :c
   :solver-attempt {:cycle/environment-revision "env-a"
                    :cycle/environment-checkout "/solver"
                    :cycle/store-snapshot :solver-store}
   :ground-control-events [] :memory-offers [] :intervention :i})

(deftest attempt-environments-are-stamped-from-checkout-assignment
  (let [assigned-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        assignments {:solver {:checkout "/frames/solver"
                              :base-revision assigned-revision}
                     :student [{:checkout "/frames/student"
                                :base-revision assigned-revision}]}
        backend (tools/make-mock-backend
                 {:advance-problem-phase {:cycle/phase :frame}})
        p (problem/make-problem backend)
        start (runner/start p {:session-id "stamp-attempts"
                               :problem-id "t94J02"
                               :cycle/mode :store-mode})
        registered
        (runner/step
         p (assoc (:state start) :current-phase :register)
         {:tool :advance-problem-phase
          :args ["M" "C"
                 {:registration :r :store-snapshot :s :stratum-frozen-at 1
                  :environment-revision "caller-revision"
                  :harness-revision "h"
                  :environment-checkouts assignments}]})
        guided-state (-> (:state registered)
                         (assoc :current-phase :guided-solve)
                         (update :cycle/outputs merge
                                 {:frame :f :containment-probe :c}))
        guided
        (runner/step
         p guided-state
         {:tool :advance-problem-phase
          :args ["M" "C"
                 {:solver-attempt
                  {:cycle/environment-checkout "/caller/solver"
                   :cycle/environment-revision
                   "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}
                  :ground-control-events [] :memory-offers []}]})
        student-state (-> (:state guided)
                          (assoc :current-phase :student-attempts)
                          (update :cycle/outputs assoc :intervention :i))
        students
        (runner/step
         p student-state
         {:tool :advance-problem-phase
          :args ["M" "C"
                 {:student-attempts
                  [{:cycle/environment-checkout "/caller/student"
                    :cycle/environment-revision
                    "cccccccccccccccccccccccccccccccccccccccc"}]
                  :memory-uses []}]})
        outputs (get-in students [:state :cycle/outputs])]
    (is (= assigned-revision (:environment-revision outputs))
        "the registered pin is derived from the machine assignment")
    (is (= {:cycle/environment-checkout "/frames/solver"
            :cycle/environment-revision assigned-revision}
           (:solver-attempt outputs)))
    (is (= [{:cycle/environment-checkout "/frames/student"
             :cycle/environment-revision assigned-revision}]
           (:student-attempts outputs)))
    (is (not= (get-in outputs [:solver-attempt :cycle/environment-checkout])
              (get-in outputs [:student-attempts 0
                               :cycle/environment-checkout])))))

(deftest each-student-dispatch-provisions-and-stamps-its-own-tree
  (let [revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        provisioned (atom [])
        provisioner (fn [{:keys [arm] :as options}]
                      (let [frame {:checkout (str "/frames/" arm)
                                   :base-revision revision
                                   :branch (:branch options)
                                   :frame/id (str "round-p-" arm)
                                   :batch "round"}]
                        (swap! provisioned conj frame)
                        frame))
        backend (tools/make-mock-backend
                 {:advance-problem-phase {:cycle/phase :adjudicate}})
        p (problem/make-problem backend
                                (fn [_ _] {:job-id "job" :evidence {}})
                                "/tmp/student-tree-test" provisioner)
        start (runner/start p {:session-id "student-trees" :problem-id "p"
                               :cycle/mode :store-mode})
        assigned (runner/step
                  p (assoc (:state start) :current-phase :register)
                  {:tool :assign-checkouts :args [checkout-options]})
        first-dispatch (runner/step
                        p (assoc (:state assigned)
                                 :current-phase :student-attempts)
                        {:tool :dispatch-student-fresh
                         :args [{:environment-checkout "/caller"} "packet"]})
        second-dispatch (runner/step
                         p (:state first-dispatch)
                         {:tool :dispatch-student-fresh
                          :args [{:environment-checkout "/caller"} "packet"]})
        solver (get-in assigned [:result :environment-checkouts :solver])
        student-frames [(get-in first-dispatch
                                [:result :environment-checkout])
                        (get-in second-dispatch
                                [:result :environment-checkout])]
        ready-state (assoc (:state second-dispatch)
                           :current-phase :student-attempts
                           :cycle/outputs
                           (assoc outputs-through-student
                                  :environment-revision revision
                                  :environment-checkouts
                                  {:solver solver :student []}
                                  :solver-attempt
                                  {:cycle/environment-checkout (:checkout solver)
                                   :cycle/environment-revision revision}))
        advanced (runner/step
                  p ready-state
                  {:tool :advance-problem-phase
                   :args ["M" "C" {:student-attempts [{:attempt/id 1}
                                                       {:attempt/id 2}]
                                    :memory-uses []}]})
        attempts (get-in advanced [:state :cycle/outputs :student-attempts])]
    (is (= 3 (count @provisioned)) "one solver plus two student trees")
    (is (= 2 (count (set (map :checkout student-frames)))))
    (is (every? #(not= (:checkout solver) (:checkout %)) student-frames))
    (is (= (mapv :checkout student-frames)
           (mapv :cycle/environment-checkout attempts))
        "attempt i is paired with dispatch i's machine-recorded tree")
    (is (= [revision revision]
           (mapv :cycle/environment-revision attempts)))))

(deftest environment-mismatch-fails-at-first-complete-advance
  (let [[p state] (started :store-mode)
        outputs (assoc-in outputs-through-student
                          [:environment-checkouts :student 0 :base-revision]
                          "env-b")
        state (assoc state :current-phase :student-attempts
                           :cycle/outputs outputs)
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C"
                        {:student-attempts
                         [{:cycle/environment-revision "env-b"}]
                         :memory-uses []}]})]
    (is (= :environment-mismatch-between-arms (:error/code result)))
    (is (= :environment-arms-match
           (get-in result [:error/context :invariant])))))

(defn- advance-student-attempts [outputs attempts]
  (let [[p state] (started :store-mode)]
    (runner/step
     p (assoc state :current-phase :student-attempts
                    :cycle/outputs outputs)
     {:tool :advance-problem-phase
      :args ["M" "C" {:student-attempts attempts :memory-uses []}]})))

(deftest equal-revisions-and-distinct-arm-checkouts-are-accepted
  (let [result (advance-student-attempts
                outputs-through-student
                [{:cycle/environment-revision "caller"
                  :cycle/environment-checkout "/caller"}])]
    (is (:ok result))))

(deftest shared-solver-student-checkout-is-rejected
  (let [outputs (assoc-in outputs-through-student
                          [:environment-checkouts :student 0 :checkout]
                          "/solver")
        result (advance-student-attempts outputs [{}])]
    (is (= :environment-shared-checkout (:error/code result)))
    (is (= :environment-arms-match
           (get-in result [:error/context :invariant])))))

(deftest student-attempts-may-not-share-their-own-checkout
  (let [outputs (assoc outputs-through-student :environment-checkouts
                       {:solver {:checkout "/solver" :base-revision "env-a"}
                        :student [{:checkout "/student" :base-revision "env-a"}
                                  {:checkout "/student" :base-revision "env-a"}]})
        result (advance-student-attempts outputs [{} {}])]
    (is (= :environment-shared-checkout (:error/code result)))))

(deftest multiple-student-attempts-with-distinct-checkouts-are-accepted
  (let [outputs (assoc outputs-through-student :environment-checkouts
                       {:solver {:checkout "/solver" :base-revision "env-a"}
                        :student [{:checkout "/student-1" :base-revision "env-a"}
                                  {:checkout "/student-2" :base-revision "env-a"}
                                  {:checkout "/student-3" :base-revision "env-a"}]})
        result (advance-student-attempts outputs [{} {} {}])]
    (is (:ok result))))

(deftest absent-checkouts-do-not-pass-as-separate
  (let [outputs (-> outputs-through-student
                    (assoc-in [:solver-attempt :cycle/environment-checkout] nil)
                    (assoc-in [:environment-checkouts :solver :checkout] nil)
                    (assoc-in [:environment-checkouts :student 0 :checkout] nil))
        result (advance-student-attempts outputs [{}])]
    (is (= :environment-shared-checkout (:error/code result)))))

(deftest malformed-operand-is-a-failure-not-a-crash-and-not-a-pass
  ;; Invariant operands are supplied by TOOLS, so a malformed one must be
  ;; rejected as a structured error. Crashing loses the cycle; passing would let
  ;; a bad tool defeat the gate by emitting garbage instead of a mismatch.
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :student-attempts
                           :cycle/outputs outputs-through-student)
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C" {:student-attempts :not-a-sequence
                                 :memory-uses []}]})]
    (is (= :invariant-check-threw (:error/code result)))
    (is (= :environment-arms-match
           (get-in result [:error/context :invariant])))
    (is (not (:ok result)))))

(deftest differing-store-snapshots-are-explicitly-accepted
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :student-attempts
                           :cycle/outputs outputs-through-student)
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C"
                        {:student-attempts
                         [{:cycle/environment-revision "env-a"
                           :cycle/store-snapshot :student-store}]
                         :memory-uses []}]})]
    ;; Store snapshots are intentionally not invariant operands: the store is
    ;; the transfer channel and is permitted to differ between arms.
    (is (:ok result))))

(def synthetic-trace
  {:problem {:problem-id "p" :difficulty-stratum "s" :regime "r"
             :locked-lemma-exposure []}
   :frame {:scaffold-hash "a" :closing-hash "b"}
   :launch-gate-refused-without-witness? true :cycle-closed? true
   :disposition-ids ["d"] :memory-offers []
   :memory-disposition-offer-ids [] :stratum-frozen-at 1 :assigned-at 2
   :cycle/attempts [{:cycle/regime "r"
                     :cycle/store-revision "1111111111111111111111111111111111111111"
                     :cycle/harness-revision "2222222222222222222222222222222222222222"
                     :cycle/environment-checkout "/frames/solver"
                     :cycle/environment-revision "1111111111111111111111111111111111111111"
                     :cycle/runner-freshness true}]
   :cycle/mode :store-mode :cycle/deposit-state :n/a :cycle/paired-with nil
   :cycle/store-snapshot-id "snap/1" :cycle/store-snapshot-memory-ids []
   :cycle/window {:opened-at "2026-08-15T00:00:00Z"
                  :closed-at "2026-08-15T01:00:00Z"}
   :denominator-declared? true :denominator-inferred-from-corpus? false
   :available-artifact-ids [] :need-probe-retrieved-ids []
   :containment-claimed? true :containment-probe-recorded? true
   :containment-probe-passed? true :capability-probes []
   :required-measurement-fields ["x"]
   :measurement {:meas/values {"x" 1} :meas/unset {}}
   :promoted-artifact-ids [] :importable-promoted-artifact-ids []
   :need-tagged-promoted-artifact-ids []})

(deftest fruit-is-validator-trace-shape
  (let [[p state] (started :store-mode)
        stop (runner/stop p (assoc state :steps [{:tool :emit-trace
                                                   :result synthetic-trace}])
                          "synthetic")]
    (is (empty? (prereg/trace-shape-failures (:fruit stop))))))

(defn- temp-state-root []
  (.toFile (Files/createTempDirectory "problem-cycle-state-"
                                      (make-array FileAttribute 0))))

(defn- stateful-problem [root mode session-id]
  (let [p (problem/make-problem (tools/make-mock-backend)
                                (fn [_ _] (throw (ex-info "not dispatched" {})))
                                root)
        start (runner/start p {:session-id session-id
                               :problem-id "t94J02"
                               :cycle/mode mode})]
    [p (assoc (:state start)
              :current-cycle-id "cycle-1"
              :current-phase :frame)]))

(deftest problem-state-save-load-is-additive-and-mid-phase
  (let [root (temp-state-root)
        [p initial] (stateful-problem root :store-mode "state-roundtrip")
        saves (loop [version 1 state initial results []]
                (if (> version 5)
                  {:state state :results results}
                  (let [saved (runner/step
                               p (assoc state :checkpoint version)
                               {:tool :problem-save :args []})]
                    (recur (inc version) (:state saved) (conj results saved)))))
        cycle-dir (io/file root "cycle-1")
        vfiles (map #(io/file cycle-dir (str "v" % ".edn")) (range 1 6))
        loaded (runner/step p (:state saves)
                            {:tool :problem-load :args ["cycle-1" 3]})]
    (is (every? :ok (:results saves))
        "problem-save is always available while the frame phase is active")
    (is (= 3 (get-in loaded [:state :checkpoint])))
    (is (every? #(.isFile %) vfiles))
    (is (every? #(map? (edn/read-string (slurp %))) vfiles)
        "v4 and v5 remain present and parseable after rewinding to v3")
    (is (= (mapv #(str "v" % ".edn") (range 1 6))
           (mapv #(.getName %) vfiles)))
    (is (get-in (first (:results saves)) [:snapshot-evidence]))
    (is (= 1 (get-in (first (:results saves))
                     [:snapshot-evidence :evidence/body :version])))))

(deftest problem-state-versions-are-write-once-and-partials-are-not-versions
  (let [root (temp-state-root)
        [p state] (stateful-problem root :store-mode "state-write-once")
        first-save (runner/step p (assoc state :checkpoint :first)
                                {:tool :problem-save :args []})
        v1 (io/file root "cycle-1" "v1.edn")
        original (slurp v1)
        partial (io/file root "cycle-1" ".state-interrupted.tmp")
        _ (spit partial "{:truncated")
        second-save (runner/step p (assoc (:state first-save) :checkpoint :second)
                                 {:tool :problem-save :args []})
        v2 (io/file root "cycle-1" "v2.edn")]
    (is (:ok second-save))
    (is (= original (slurp v1)) "a later save cannot overwrite v1")
    (is (= :first (:checkpoint (edn/read-string (slurp v1)))))
    (is (= :second (:checkpoint (edn/read-string (slurp v2)))))
    (is (= ["v1.edn" "v2.edn"]
           (->> (.listFiles (io/file root "cycle-1"))
                (map #(.getName %))
                (filter #(re-matches #"v[0-9]+\.edn" %))
                sort
                vec))
        "an interrupted temp file never becomes a readable version")))

(deftest problem-state-written-form-round-trips-as-edn
  (let [root (temp-state-root)
        [p state] (stateful-problem root :store-mode "state-edn")
        saved (runner/step p state {:tool :problem-save :args []})
        stored (slurp (io/file root "cycle-1" "v1.edn"))]
    (is (:ok saved))
    (is (map? (edn/read-string stored)))
    (is (not (.contains stored "#object")))))

(deftest problem-state-load-refuses-cross-mode-runtime-pairing
  (let [root (temp-state-root)
        [store-p store-state] (stateful-problem root :store-mode "mode-check")
        saved (runner/step store-p store-state {:tool :problem-save :args []})
        [harness-p harness-state] (stateful-problem root :harness-mode "mode-check")
        loaded (runner/step harness-p harness-state
                            {:tool :problem-load :args ["cycle-1" 1]})]
    (is (:ok saved))
    (is (= :loaded-state-mode-mismatch (:error/code loaded)))
    (is (nil? (:state loaded)))))

(deftest cycle-id-cannot-escape-the-state-root
  ;; ".." matches [A-Za-z0-9._-]+ because both dots are in the class, so the
  ;; character check alone was not containment: a save with that cycle id wrote
  ;; to <root>/../v1.edn, outside the store.
  ;; The root is nested inside a dedicated parent so "above the root" is a
  ;; directory this test owns. Checking a shared parent like /tmp picks up other
  ;; runs' litter -- the first version of this test failed on a file left by the
  ;; probe that originally demonstrated the escape.
  (let [parent (str (java.nio.file.Files/createTempDirectory
                     "pstate" (make-array java.nio.file.attribute.FileAttribute 0)))
        root (str (java.io.File. parent "store"))
        ;; The root MUST already exist. With a missing root the traversal fails
        ;; incidentally -- the temp file cannot be created -- and the test then
        ;; passes without exercising the guard at all. That is how the first
        ;; version of this test survived having the guard deleted.
        _ (.mkdirs (java.io.File. root))
        b (problem/make-problem-state-backend (tools/make-mock-backend) root)
        saved (tools/execute-tool b :problem-save
                                  [{:current-cycle-id ".." :steps []}])
        loaded (tools/execute-tool b :problem-load ["../elsewhere" 1])]
    (is (false? (:ok saved)))
    (is (false? (:ok loaded)))
    (is (empty? (filter #(= "v1.edn" (.getName %))
                        (or (.listFiles (java.io.File. parent)) [])))
        "nothing was written above the root")))

(deftest missing-version-is-a-tool-failure-not-an-escaping-exception
  ;; ToolBackend promises {:ok true :result} | {:ok false :error}; slurp on a
  ;; missing version threw FileNotFoundException straight out of execute-tool.
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "pstate" (make-array java.nio.file.attribute.FileAttribute 0)))
        b (problem/make-problem-state-backend (tools/make-mock-backend) root)
        r (tools/execute-tool b :problem-load ["C1" 99])]
    (is (false? (:ok r)))
    (is (re-find #"problem-load failed" (:error r)))))

(deftest student-provisioning-failure-preserves-completed-frames
  ;; Was: "rolls back the whole assignment" -- a requirement I gave in the A3.5
  ;; packet and which is wrong mid-cycle. Verified before the fix: a transient
  ;; failure on student attempt 2 rolled back the SOLVER's tree, which by then
  ;; may hold the whole cycle's work. All-or-nothing is right at
  ;; :assign-checkouts, where nothing has been done; here it inverts its own
  ;; justification, since it existed so a retry would be possible and this made
  ;; retry mean redoing the solve.
  (let [rolled (atom [])
        student-count (atom 0)
        provisioner
        (fn [{:keys [arm] :as options}]
          (when (and (str/starts-with? arm "student-")
                     (= 2 (swap! student-count inc)))
            (throw (ex-info "student provisioning failed" {})))
          {:checkout (str "/tmp/" arm) :branch (:branch options)
           :base-revision "abc" :frame/id (str "b-p-" arm) :batch "b"})
        inner (tools/make-mock-backend
               {:dispatch-student-fresh {:job-id "job" :evidence {}}})
        b (problem/make-checkout-provisioning-backend
           inner provisioner (fn [frame] (swap! rolled conj frame)))
        assigned (tools/execute-tool b :assign-checkouts [checkout-options])
        first-attempt (tools/execute-tool
                       b :dispatch-student-fresh [{:to "zai-1"} "packet"])
        failed (tools/execute-tool
                b :dispatch-student-fresh [{:to "zai-1"} "packet"])]
    (is (:ok assigned))
    (is (:ok first-attempt))
    (is (false? (:ok failed)))
    (is (zero? (:rolled-back failed)))
    (is (empty? @rolled)
        "no completed frame may be destroyed by a later attempt's failure")))

(deftest recorded-assignment-beats-a-relayed-one
  ;; :environment-checkouts otherwise arrives through the caller-supplied advance
  ;; payload like every other output, so a caller could relay a fabricated
  ;; assignment and everything downstream would stamp consistently from it --
  ;; every check passing against paths that were never provisioned.
  (let [real {:solver  {:checkout "/real/solver"
                        :base-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
              :student []}
        forged {:solver  {:checkout "/FORGED"
                          :base-revision "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}
                :student {:checkout "/FORGED"
                          :base-revision "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}}
        backend (tools/make-mock-backend
                 {:advance-problem-phase {:cycle/phase :frame}})
        p (problem/make-problem backend (fn [_ _] {:evidence {}})
                                "/tmp/custody"
                                (fn [{:keys [arm]}] (get real (keyword arm))))
        start (runner/start p {:session-id "custody" :problem-id "t94J02"
                               :cycle/mode :store-mode})
        ;; the machine runs the tool -- its result lands in :steps
        assigned (runner/step p (assoc (:state start) :current-phase :register)
                              {:tool :assign-checkouts
                               :args [{:problem "t94J02" :batch "b" :base-rev "HEAD"
                                       :solver-seat "codex-4" :student-seat "zai-1"
                                       :recall-system "v1"}]})
        ;; the caller then relays a DIFFERENT assignment on the advance
        advanced (runner/step p (:state assigned)
                              {:tool :advance-problem-phase
                               :args ["M" "C"
                                      {:registration :r :store-snapshot :s
                                       :stratum-frozen-at 1 :harness-revision "h"
                                       :environment-revision "caller"
                                       :environment-checkouts forged}]})
        outputs (get-in advanced [:state :cycle/outputs])]
    (is (:ok assigned))
    (is (= real (:environment-checkouts outputs))
        "the recorded assignment must win over the relayed one")
    (is (= "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
           (:environment-revision outputs)))))

(deftest student-provisioning-failure-must-not-destroy-the-solver-tree
  ;; Verified before this fix: a transient failure on student attempt 2 rolled
  ;; back ["/tree/solver" "/tree/student-1"] -- deleting the worktree the solver
  ;; may have spent the whole cycle working in. All-or-nothing is right at
  ;; :assign-checkouts, where nothing has been done; mid-cycle it destroys work
  ;; and makes retry mean redoing the solve.
  (let [rolled (atom [])
        n (atom 0)
        provisioner (fn [{:keys [arm]}]
                      (swap! n inc)
                      (if (and (re-find #"^student-" (str arm)) (> @n 2))
                        (throw (ex-info "transient git failure" {}))
                        {:checkout (str "/tree/" arm)
                         :base-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                         :branch (str "exp/" arm) :frame/id (str "f-" arm)}))
        b (problem/make-checkout-provisioning-backend
           (tools/make-mock-backend {:dispatch-student-fresh {:ok true}})
           provisioner
           (fn [f] (swap! rolled conj (:checkout f))))]
    (tools/execute-tool b :assign-checkouts
                        [{:problem "p" :batch "bt" :base-rev "HEAD"
                          :solver-seat "codex-4" :student-seat "zai-1"
                          :recall-system "v1"}])
    (tools/execute-tool b :dispatch-student-fresh [{} "packet"])
    (let [r (tools/execute-tool b :dispatch-student-fresh [{} "packet"])]
      (is (false? (:ok r)))
      (is (zero? (:rolled-back r)))
      (is (empty? @rolled) "no completed frame may be rolled back mid-cycle")
      (is (not-any? #(re-find #"solver" %) @rolled)
          "the solver's tree must survive a student provisioning failure"))))

(deftest close-phase-is-inhabitable
  ;; The engine clears the cycle when an advance returns the LAST phase, so the
  ;; last phase is a transition and never a state. With :close last, its three
  ;; tools were unreachable -- declared in a phase the machine passed straight
  ;; through, which is why they were never implemented.
  (let [[p state] (started :store-mode)
        st (assoc state :current-phase :close :current-cycle-id "C1")
        r (runner/step p st {:tool :emit-trace :args []})]
    (is (not= :phase-tool-not-allowed (:error/code r))
        ":close tools must be reachable when the machine is in :close")
    (is (= :completed (last problem/phase-order)))
    (is (empty? (get problem/base-phase-tools :completed))
        "the terminal sentinel carries no tools")))

(defn- trace-emission-state [state]
  (assoc state
         :current-phase :close
         :current-cycle-id "cycle-1"
         :cycle/opened-at "2026-08-15T00:00:00Z"
         :cycle/deposit-state :n/a
         :cycle/paired-with nil
         :cycle/outputs
         {:registration {:required-measurement-fields ["x"]}
          :stratum-frozen-at 1
          :assigned-at 2
          :frame {:frame/id "frame-1"}
          :launch-gate-event {:gate/id "gate-1"}
          :store-snapshot {:snap/id "snap-1"}
          :containment-probe {:cprobe/id "cprobe-1"}
          :measurement {:meas/id "meas-1"}
          :solver-attempt {:attempt/id "attempt-1"}
          :student-attempts []
          :disposition []
          :memory-offers []
          :memory-uses []
          :retrieval-probes []
          :capability-probes []
          :promotion-result []}))

(deftest emit-trace-reuses-existing-projection-and-machine-cycle-facts
  (let [[p state] (started :store-mode)
        called (atom nil)
        closed-at "2026-08-15T01:02:03Z"
        result (with-redefs [apm-harness/derive-trace
                             (fn [registration cycle-id entities]
                               (reset! called {:registration registration
                                               :cycle-id cycle-id
                                               :entities entities})
                               {:projected true})
                             problem/now-string (constantly closed-at)]
                 (runner/step
                  p (trace-emission-state state)
                  {:tool :emit-trace
                   ;; A caller-supplied cycle entity is deliberately ignored.
                   :args [{:cycle/id "forged" :cycle/closed-at "forged"}]}))
        cycle-entity (some #(when (:cycle/id %) %) (:entities @called))]
    (is (:ok result))
    (is (= {:projected true} (:result result)))
    (is (= "cycle-1" (:cycle-id @called)))
    (is (= "cycle-1" (:cycle/id cycle-entity)))
    (is (= :store-mode (:cycle/mode cycle-entity)))
    (is (= "2026-08-15T00:00:00Z" (:cycle/opened-at cycle-entity)))
    (is (= closed-at (:cycle/closed-at cycle-entity))
        "the engine clock, not the caller, closes the trace")
    (is (= #{"cycle-1" "frame-1" "gate-1" "snap-1" "cprobe-1"
             "meas-1" "attempt-1"}
           (set (keep apm-harness/entity-id (:entities @called))))
        "typed entities are assembled from cycle outputs")))

(deftest emit-trace-names-missing-entity-producers
  (let [[p state] (started :store-mode)
        state (update (trace-emission-state state) :cycle/outputs
                      dissoc :retrieval-probes :capability-probes)
        result (runner/step p state {:tool :emit-trace :args []})]
    (is (= :tool-execution-failed (:error/code result)))
    (is (re-find #"retrieval-probe" (str result)))
    (is (re-find #"capability-probe" (str result)))))

(deftest empty-offer-output-is-not-a-missing-producer
  (let [[p state] (started :store-mode)
        called (atom false)
        result (with-redefs [apm-harness/derive-trace
                             (fn [_ _ entities]
                               (reset! called true)
                               (is (empty? (filter :offer/id entities)))
                               {:projected true})]
                 (runner/step p (trace-emission-state state)
                              {:tool :emit-trace :args []}))]
    (is (:ok result))
    (is @called)
    (let [missing (runner/step
                   p (update (trace-emission-state state) :cycle/outputs
                             dissoc :memory-offers)
                   {:tool :emit-trace :args []})]
      (is (= :tool-execution-failed (:error/code missing)))
      (is (re-find #"memory-offer" (str missing))))))

(deftest emit-trace-populates-through-the-REAL-projection
  ;; No with-redefs. The other emit-trace tests stub derive-trace, so they assert
  ;; it is CALLED but never run it -- and derive-trace locates entities by linking
  ;; keys (:frame/cycle, :attempt/cycle, :offer/cycle...). Unstamped outputs made
  ;; it throw "expected exactly one entity" on every real cycle while the stubbed
  ;; tests stayed green. A stubbed collaborator hides the integration it stands in
  ;; for.
  (let [p (problem/make-problem (tools/make-mock-backend))
        st (assoc (:state (runner/start p {:session-id "e2e" :problem-id "t94J02"
                                           :cycle/mode :store-mode}))
                  :current-phase :close :current-cycle-id "C1"
                  :cycle/outputs
                  {:registration {:problem {:problem-id "t94J02"
                                            :difficulty-stratum "s"
                                            :regime "r" :locked-lemma-exposure []}
                                  :required-measurement-fields ["a"]}
                   :frame {:frame/id "F1" :frame/scaffold-hash "aa"
                           :frame/closing-hash "bb"}
                   :launch-gate-event {:gate/refused-without-witness? true}
                   :store-snapshot {:snap/id "S1" :snap/memory-ids ["m1"]}
                   :containment-probe {:cprobe/claimed? true :cprobe/recorded? true
                                       :cprobe/passed? true}
                   :measurement {:meas/values {"a" 1} :meas/unset []}
                   :solver-attempt {:cycle/environment-checkout "/solver"}
                   :student-attempts [{:cycle/environment-checkout "/student"}]
                   :disposition {:disp/id "D1"}
                   :memory-offers [{:offer/id "O1" :offer/memory-id "M1"}]
                   :memory-uses [{:use/offer "O1"}]
                   :retrieval-probes [] :capability-probes []
                   :promotion-result {:promo/artifact-id "P1" :promo/importable? true
                                      :promo/need-tags ["t"]}
                   :stratum-frozen-at 1 :assigned-at "t"})
        r (runner/step p st {:tool :emit-trace :args []})
        trace (:result (last (get-in r [:state :steps])))]
    (is (:ok r) "the real projection must not throw")
    (is (= {:scaffold-hash "aa" :closing-hash "bb"} (:frame trace)))
    (is (= 2 (count (:cycle/attempts trace))) "solver + student attempts found")
    (is (= [{:offer/id "O1" :offer/memory-id "M1"}] (:memory-offers trace)))
    (is (= ["D1"] (:disposition-ids trace)))
    (is (true? (:launch-gate-refused-without-witness? trace)))
    (is (true? (:containment-probe-passed? trace)))
    (is (= "S1" (:cycle/store-snapshot-id trace)))
    (is (= ["P1"] (:promoted-artifact-ids trace)))
    (is (string? (get-in trace [:cycle/window :closed-at])))))
