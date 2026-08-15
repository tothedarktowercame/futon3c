(ns futon3c.peripheral.problem-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [mmca.apm-demonstration-preregistration :as prereg])
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
        p (problem/make-problem (tools/make-mock-backend) dispatch-fn)
        start (runner/start p {:session-id "s" :problem-id "p"
                               :cycle/mode :store-mode})
        result (runner/step
                p (assoc (:state start) :current-phase :student-attempts)
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

(deftest assign-checkouts-provisions-both-arms-with-batch-qualified-branches
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
    (is (= ["solver" "student"] (mapv :arm @calls))
        "the machine invokes the provisioner exactly once per fixed arm")
    (is (= ["exp/round-1-t94J02-solver"
            "exp/round-1-t94J02-student"]
           (mapv :branch @calls)))
    (is (= ["push" "none"] (mapv :memory-channel @calls)))
    (is (not= (get-in checkouts [:solver :checkout])
              (get-in checkouts [:student :checkout])))
    (is (= revision (get-in checkouts [:solver :base-revision])
           (get-in checkouts [:student :base-revision])))))

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
   :environment-checkouts {:solver {:checkout "/solver"}
                           :student {:checkout "/student"}}
   :frame :f :containment-probe :c
   :solver-attempt {:cycle/environment-revision "env-a"
                    :cycle/store-snapshot :solver-store}
   :ground-control-events [] :memory-offers [] :intervention :i})

(deftest environment-mismatch-fails-at-first-complete-advance
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :student-attempts
                           :cycle/outputs outputs-through-student)
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

(deftest assign-checkouts-is-all-or-nothing
  ;; Verified before the rollback existed: when the student arm failed, the
  ;; solver's worktree and branch survived on disk, and the retry then died on
  ;; "frame already exists". A sticky failure is worse than a loud one, because
  ;; the obvious response -- try again -- cannot work.
  (let [rolled (atom [])
        provisioner (fn [{:keys [arm]}]
                      (if (= "student" arm)
                        (throw (ex-info "student provisioning failed" {}))
                        {:checkout "/tmp/solver" :branch "exp/b-p-solver"
                         :base-revision "abc" :frame/id "b-p-solver"}))
        b (problem/make-checkout-provisioning-backend
           (tools/make-mock-backend) provisioner
           (fn [frame] (swap! rolled conj frame)))
        r (tools/execute-tool b :assign-checkouts
                              [{:problem "p" :batch "b" :base-rev "HEAD"
                                :solver-seat "codex-4" :student-seat "zai-1"
                                :recall-system "v1"}])]
    (is (false? (:ok r)))
    (is (= 1 (:rolled-back r)) "the solver frame must be rolled back")
    (is (= ["/tmp/solver"] (mapv :checkout @rolled)))))
