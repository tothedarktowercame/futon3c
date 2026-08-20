(ns futon3c.peripheral.problem-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.cycle-harness :as apm-harness]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.dispatch-with-recall :as dispatch-with-recall]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.memory-lifecycle :as memory-lifecycle]
            [futon3c.peripheral.memory-recall :as memory-recall]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.peripheral.pull-receipts :as pull-receipts]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.substrate.client :as substrate])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute FileTime]))

(defn- test-sha1-hex [s]
  (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-1")
                        (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(def dispatch-packet
  (apply str (repeat 220 "p")))

(defn- capturing-evidence-store []
  (atom {:entries {} :order []}))

(defn- start-problem [p context]
  (runner/start p (assoc context :evidence-store
                         (capturing-evidence-store))))

(defn dispatch-state [phase]
  (let [calls (atom [])
        dispatch-fn (fn [opts packet]
                      (swap! calls conj {:opts opts :packet packet})
                      {:job-id "job-1"
                       :assembled-packet packet
                       :evidence {:body {:memory-channel (:memory-channel opts)}}})
        p (problem/make-problem (tools/make-mock-backend) dispatch-fn)
        result (start-problem p {:session-id "s" :problem-id "p"
                                :cycle/mode :store-mode})]
    [p (assoc (:state result) :current-phase phase) calls]))

(deftest cycle-dispatch-joins-pull-use-receipts-and-filters-non-cycle-reads
  (let [store (capturing-evidence-store)
        cycle-id "cycle-pull-1"
        base-ctx {:evidence-store store :agent-id "zai-student"
                  :session-id "student-session" :turn-id "turn-1" :round 1}
        _ (pull-receipts/record-pull-uses!
           (assoc base-ctx :dispatch-id "job-cycle") "memory_read"
           {:ok true :result {:items [{:id "e-cycle-memory"}]}})
        _ (pull-receipts/record-pull-uses!
           (assoc base-ctx :dispatch-id "job-standalone") "memory_read"
           {:ok true :result {:items [{:id "e-standalone-memory"}]}})
        state {:evidence-store store :current-cycle-id cycle-id
               :current-phase :student-attempts
               :steps [{:tool :begin-problem-cycle
                        :result {:cycle/id cycle-id}}
                       {:tool :dispatch-student-fresh
                        :result {:job-id "job-cycle"}}]}
        outputs (#'problem/stamp-environment-outputs state {})]
    (is (= [{:pull/memory-id "e-cycle-memory"
             :pull/seat "zai-student"
             :pull/tool "memory_read"
             :pull/cycle cycle-id
             :pull/job-id "job-cycle"
             :pull/at (get-in outputs [:pull-uses 0 :pull/at])}]
           (:pull-uses outputs)))
    (is (not-any? #(= "e-standalone-memory" (:pull/memory-id %))
                  (:pull-uses outputs)))))

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

(deftest dispatch-produces-retrieval-probes-on-both-sides-of-cutoff
  (let [dispatch-fn
        (fn [opts _]
          (let [eligible (if (:untruncated? opts) ["m1" "m2"]
                             ["m1" "m2" "m3"])
                surfaced ["m1" "m2"]]
            {:evidence {:body {:eligible-memory-ids eligible
                               :memory-use
                               {:memory-use/surfaced-ids surfaced}}}}))
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) dispatch-fn)
        truncated (:result
                   (tools/execute-tool
                    backend :dispatch-solver
                    [{} "packet" {:cycle/id "cycle-1" :cycle/step-index 7}]))
        untruncated (:result
                     (tools/execute-tool
                      backend :dispatch-solver
                      [{:untruncated? true} "packet"
                       {:cycle/id "cycle-1" :cycle/step-index 8}]))
        truncated-probe (:retrieval-probe truncated)
        untruncated-probe (:retrieval-probe untruncated)]
    (is (= "rprobe/cycle-1/7" (:rprobe/id truncated-probe)))
    (is (> (count (:rprobe/available-ids truncated-probe))
           (count (:rprobe/retrieved-ids truncated-probe))))
    (is (= (:rprobe/retrieved-ids truncated-probe)
           (subvec (:rprobe/available-ids truncated-probe)
                   0 (count (:rprobe/retrieved-ids truncated-probe)))))
    (is (= (:rprobe/available-ids untruncated-probe)
           (:rprobe/retrieved-ids untruncated-probe)))))

(deftest recorded-retrieval-probe-overwrites-forged-advance-payload
  (let [recorded {:rprobe/id "rprobe/cycle-1/7"
                  :rprobe/cycle "cycle-1"
                  :rprobe/available-ids ["m1" "m2" "m3"]
                  :rprobe/retrieved-ids ["m1" "m2"]}
        state {:current-phase :guided-solve
               :steps [{:tool :begin-problem-cycle :result {:cycle/id "cycle-1"}}
                       {:tool :dispatch-solver
                        :result {:retrieval-probe recorded}}]}
        stamped (#'problem/stamp-environment-outputs
                 state
                 {:retrieval-probes [{:rprobe/id "FORGED"
                                      :rprobe/available-ids []
                                      :rprobe/retrieved-ids []}]})]
    (is (= [recorded] (:retrieval-probes stamped)))))

(defn- student-eligibility-dispatch [mode]
  (let [calls (atom [])
        dispatch-fn
        (fn [opts _]
          (swap! calls conj opts)
          {:job-id "student-job"
           :evidence
           {:body {:eligible-memory-ids (:eligible-memory-ids opts)
                   :eligible-memory-provenance
                   (:eligible-memory-provenance opts)
                   :memory-use {:memory-use/surfaced-ids []}}}})
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) dispatch-fn)
        p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec backend)
        start (:state (start-problem p {:session-id "eligibility"
                                       :problem-id "t94J02"
                                       :cycle/mode mode}))
        recorded-state
        (assoc start
               :current-phase :student-attempts
               :current-cycle-id "cycle-eligibility"
               :cycle/outputs
               {:store-snapshot {:snap/memory-ids ["memory/open"]}}
               :steps
               [{:tool :begin-problem-cycle
                 :result {:cycle/id "cycle-eligibility"}}
                {:tool :promote-artifact
                 :result {:promo/artifact-id "memory/promoted"}}])
        state (assoc recorded-state :cycle/outputs
                     (#'problem/stamp-environment-outputs
                      recorded-state
                      {:store-snapshot
                       {:snap/memory-ids ["memory/open"]}
                       :promotion-result
                       [{:promo/artifact-id "memory/FORGED"}]}))
        result (runner/step
                p state
                {:tool :dispatch-student-fresh
                 :args [{:mission "M"} dispatch-packet]})]
    {:result result :opts (first @calls)}))

(deftest store-mode-student-eligibility-includes-cycle-promotions
  (let [{:keys [result opts]} (student-eligibility-dispatch :store-mode)]
    (is (:ok result))
    (is (= ["memory/open" "memory/promoted"]
           (:eligible-memory-ids opts)))
    (is (= {:policy :snapshot-union-cycle-promoted
            :snapshot-memory-ids ["memory/open"]
            :cycle-promoted-memory-ids ["memory/promoted"]}
           (:eligible-memory-provenance opts)))
    (is (= (:eligible-memory-provenance opts)
           (get-in result
                   [:result :memory-offers 0 :body
                    :eligible-memory-provenance])))
    (is (= ["memory/open" "memory/promoted"]
           (get-in result [:result :retrieval-probe :rprobe/available-ids])))))

(deftest harness-mode-student-eligibility-remains-snapshot-only
  (let [{:keys [result opts]} (student-eligibility-dispatch :harness-mode)]
    (is (:ok result))
    (is (= ["memory/open"] (:eligible-memory-ids opts)))
    (is (= {:policy :snapshot-only
            :snapshot-memory-ids ["memory/open"]
            :cycle-promoted-memory-ids []}
           (:eligible-memory-provenance opts)))))

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
        start (start-problem p {:session-id "s" :problem-id "p"
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

(deftest registered-solver-config-is-sent-receipted-and-traced
  (let [calls (atom [])
        pin {:model "gpt-5.6-sol" :reasoning-effort "high"}
        dispatch-fn (fn [opts _]
                      (swap! calls conj opts)
                      {:job-id "solver-config-job" :evidence {:body {}}})
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) dispatch-fn)
        pinned (tools/execute-tool
                backend :dispatch-solver
                [{:model "caller-model" :reasoning-effort "low"}
                 "packet"
                 {:cycle/id "cycle-pin" :cycle/step-index 3
                  :solver-config pin}])
        legacy (tools/execute-tool
                backend :dispatch-solver
                [{} "packet" {:cycle/id "cycle-legacy" :cycle/step-index 4}])
        state {:current-phase :guided-solve
               :current-cycle-id "cycle-pin"
               :steps [{:tool :dispatch-solver :result (:result pinned)}]
               :cycle/outputs {:registration {:reg/solver-seat "codex-4"
                                              :reg/solver-config pin}}}
        outputs (#'problem/stamp-environment-outputs state {})]
    (is (= pin (select-keys (first @calls) [:model :reasoning-effort]))
        "the registration pin overrides caller-supplied solver options")
    (is (= :push+pull (:memory-channel (first @calls))))
    (is (= pin (get-in pinned [:result :ground-control/solver-config])))
    (is (= "cycle-pin" (get-in pinned [:result :ground-control/cycle])))
    (is (not (contains? (second @calls) :model))
        "an absent pin preserves the legacy dispatch shape")
    (is (not (contains? (second @calls) :reasoning-effort)))
    (is (not (contains? (:result legacy) :ground-control/solver-config)))
    (is (= pin (get-in outputs [:solver-dispatches 0
                                :ground-control/solver-config]))
        "the machine-recorded receipt reaches the cycle trace inputs")))

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
        result (start-problem p {:session-id "s" :problem-id "p"
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

(deftest register-refuses-an-unstaffed-carded-seat
  (let [[p state] (started :store-mode)
        state (assoc state :current-phase :register :cycle/outputs {})
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C"
                        {:registration
                         {:reg/role-cards
                          {:solver prereg/required-lean-revision
                           :proctor prereg/required-lean-revision}
                          :reg/solver-seat "codex-4"}
                         :store-snapshot :s
                         :stratum-frozen-at 1
                         :environment-revision "env"
                         :harness-revision "harness"
                         :environment-checkouts {}}]})]
    (is (= :registration-shape-invalid (:error/code result)))
    (is (= :seat-registration-valid
           (get-in result [:error/context :invariant])))
    (is (some #(= {:finding :unstaffed-carded-seat
                   :role :proctor
                   :seat-key :reg/proctor-seat}
                  %)
              (get-in result [:error/context :details :findings])))))

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
                       :runner-freshness true
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
    (is (true? (get-in checkouts [:solver :runner-freshness])))
    (is (= [] (:student checkouts)))))

(deftest assign-checkouts-provisioner-failure-is-a-tool-failure
  (let [backend (problem/make-checkout-provisioning-backend
                 (tools/make-mock-backend)
                 (fn [_] (throw (ex-info "branch collision" {}))))
        result (tools/execute-tool backend :assign-checkouts [checkout-options])]
    (is (false? (:ok result)))
    (is (re-find #"branch collision" (:error result)))))

(deftest frame-provisioning-preserves-command-stderr-in-tool-failure
  (let [git-error "fatal: a branch named 'exp/round-1-t94J02-solver' already exists"
        options (assoc checkout-options
                       :arm "solver"
                       :seat "codex-4"
                       :memory-channel "push"
                       :branch "exp/round-1-t94J02-solver")
        backend (problem/make-checkout-provisioning-backend
                 (tools/make-mock-backend)
                 (fn [opts]
                   (with-redefs [clojure.java.shell/sh
                                 (fn [& _]
                                   {:exit 128 :out "" :err git-error})]
                     (#'problem/provision-frame! opts))))
        result (tools/execute-tool backend :assign-checkouts [options])]
    (is (false? (:ok result)))
    (is (str/includes? (:error result) git-error))
    (is (str/includes? (:error result) "exit 128"))))

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
        start (start-problem p {:session-id "checkout-register"
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
   :ground-control-events [] :memory-offers [] :intervention :i
   :promotion-result []})

(deftest attempt-environments-are-stamped-from-checkout-assignment
  (let [assigned-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        assignments {:solver {:checkout "/frames/solver"
                              :base-revision assigned-revision}
                     :student [{:checkout "/frames/student"
                                :base-revision assigned-revision}]}
        backend (tools/make-mock-backend
                 {:advance-problem-phase {:cycle/phase :frame}})
        p (problem/make-problem backend)
        start (start-problem p {:session-id "stamp-attempts"
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
                          (update :cycle/outputs assoc
                                  :intervention :i
                                  :promotion-result []))
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

(defn- stamp-machine-attempt-run [snapshot? registration? runner-freshness?]
  (let [revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        regime "e7b9ec02599b36845c68ca053416787a23ac29a8"
        memory-ids ["memory/z" "memory/a" "memory/m"]
        p (problem/make-problem
           (tools/make-mock-backend)
           (fn [_ _] {:evidence {}})
           "/tmp/store-revision-stamp-state"
           (fn [_] (cond-> {:checkout "/frames/solver" :base-revision revision}
                     runner-freshness? (assoc :runner-freshness true)))
           (fn [_] {:harness-revision revision :harness-tree-dirty? false})
           (constantly memory-ids)
           (constantly 1))
        start (start-problem p {:session-id "store-revision-stamp"
                                :problem-id "p"
                                :cycle/mode :store-mode
                                :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        snap-state (if snapshot?
                     (:state (runner/step p (:state begun)
                                         {:tool :snapshot-store :args []}))
                     (:state begun))
        snap-state (cond-> snap-state
                     registration?
                     (update :steps conj
                             {:tool :read-registration
                              :result {:problem {:regime regime}}}))
        assignment (cond-> {:checkout "/frames/solver"
                            :base-revision revision}
                     runner-freshness? (assoc :runner-freshness true))
        state (assoc snap-state :current-phase :guided-solve
                     :cycle/outputs
                     {:registration :r
                      :store-snapshot :s
                      :stratum-frozen-at 1
                      :environment-revision revision
                      :harness-revision revision
                      :environment-checkouts
                      {:solver assignment
                       :student []}
                      :frame :f :containment-probe :c})
        advanced (runner/step
                  p state
                  {:tool :advance-problem-phase
                   :args ["M" "C" {:solver-attempt {}
                                     :ground-control-events []
                                     :memory-offers []}]})]
    {:attempt (get-in advanced [:state :cycle/outputs :solver-attempt])
     :memory-ids memory-ids
     :regime regime}))

(deftest attempts-carry-the-recorded-store-snapshot-revision
  (let [{:keys [attempt memory-ids]} (stamp-machine-attempt-run true true true)
        shuffled [(nth memory-ids 1) (nth memory-ids 2) (nth memory-ids 0)]
        expected (test-sha1-hex (str/join "\n" (sort shuffled)))]
    (is (= expected (:cycle/store-revision attempt)))
    (is (prereg/attempt? attempt))))

(deftest attempts-omit-store-revision-without-a-recorded-snapshot
  (let [{:keys [attempt]} (stamp-machine-attempt-run false true true)]
    (is (not (contains? attempt :cycle/store-revision)))))

(deftest attempts-carry-machine-recorded-regime
  (let [{:keys [attempt regime]} (stamp-machine-attempt-run true true true)]
    (is (= regime (:cycle/regime attempt)))))

(deftest attempts-omit-regime-without-recorded-registration
  (let [{:keys [attempt]} (stamp-machine-attempt-run true false true)]
    (is (not (contains? attempt :cycle/regime)))))

(deftest attempts-carry-provisioner-runner-freshness
  (let [{:keys [attempt]} (stamp-machine-attempt-run true true true)]
    (is (true? (:cycle/runner-freshness attempt)))))

(deftest attempts-omit-runner-freshness-without-provisioner-measurement
  (let [{:keys [attempt]} (stamp-machine-attempt-run true true false)]
    (is (not (contains? attempt :cycle/runner-freshness)))))

(deftest machine-stamped-attempt-satisfies-preregistration
  (let [{:keys [attempt]} (stamp-machine-attempt-run true true true)]
    (is (prereg/attempt? attempt))))

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
        start (start-problem p {:session-id "student-trees" :problem-id "p"
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
        start (start-problem p {:session-id session-id
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
        start (start-problem p {:session-id "custody" :problem-id "t94J02"
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

(defn- harness-measured-problem [measurement]
  (problem/make-problem
   (tools/make-mock-backend
    {:begin-problem-cycle {:cycle/id "C-harness"}
     :advance-problem-phase {:cycle/phase :intervene}})
   (fn [_ _] {:evidence {}})
   "/tmp/harness-measurement-state"
   (fn [_] (throw (ex-info "provisioner not used" {})))
   (fn [repo]
     (is (= "/measured/harness" repo))
     measurement)))

(deftest problem-cycle-begin-is-deterministic-and-opens-the-register-phase
  (let [context {:session-id "deterministic-cycle" :problem-id "t94J02"
                 :cycle/mode :store-mode
                 :harness-repo "/measured/harness"}
        begin (fn []
                (let [p (harness-measured-problem
                         {:harness-revision
                          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                          :harness-tree-dirty? false})
                      start (start-problem p context)]
                  (runner/step p (:state start)
                               {:tool :begin-problem-cycle
                                :args ["M-demo" "round-1"]})))
        first-begin (begin)
        second-begin (begin)]
    (is (= :register (get-in first-begin [:state :current-phase])))
    (is (= (get-in first-begin [:result :cycle/id])
           (get-in first-begin [:state :current-cycle-id])))
    (is (= (get-in first-begin [:result :cycle/id])
           (get-in second-begin [:result :cycle/id]))
        "the same session/problem/begin inputs produce the same cycle id")
    (is (string? (get-in first-begin [:result :cycle/opened-at])))))

(deftest second-problem-cycle-begin-refuses-while-cycle-is-open
  (let [p (harness-measured-problem
           {:harness-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            :harness-tree-dirty? false})
        start (start-problem p {:session-id "double-begin" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        repeated (runner/step p (:state begun)
                              {:tool :begin-problem-cycle :args ["M" "C"]})]
    (is (= :phase-tool-not-allowed (:error/code repeated)))
    (is (some? (get-in begun [:state :current-cycle-id])))))

(deftest problem-phase-advance-moves-and-retains-its-payload
  (let [revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        p (harness-measured-problem {:harness-revision revision
                                     :harness-tree-dirty? false})
        start (start-problem p {:session-id "advance" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        payload {:registration :r
                 :store-snapshot :s
                 :stratum-frozen-at 1
                 :environment-revision revision
                 :harness-revision "FORGED"
                 :environment-checkouts
                 {:solver {:checkout "/solver" :base-revision revision}
                  :student []}
                 :payload-marker :retained}
        advanced (runner/step p (:state begun)
                              {:tool :advance-problem-phase
                               :args ["M" "C" payload]})]
    (is (:ok advanced) (pr-str advanced))
    (is (= :frame (get-in advanced [:state :current-phase])))
    (is (= :retained
           (get-in advanced [:state :cycle/outputs :payload-marker])))
    (is (= revision
           (get-in advanced [:state :cycle/outputs :harness-revision])))))

(deftest problem-phase-advance-refuses-with-no-open-cycle
  (let [p (harness-measured-problem
           {:harness-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            :harness-tree-dirty? false})
        start (start-problem p {:session-id "no-cycle" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        advanced (runner/step p (:state start)
                              {:tool :advance-problem-phase
                               :args ["M" "C" {}]})]
    (is (= :phase-tool-not-allowed (:error/code advanced)))
    (is (= :setup (get-in advanced [:error/context :phase])))))

(defn- machine-frame-result [scaffold closing witness claimed?]
  (let [p (harness-measured-problem
           {:harness-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            :harness-tree-dirty? false})
        start (start-problem p {:session-id "frame-emission" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        frame-state (assoc (:state begun)
                           :current-phase :frame
                           :cycle/outputs
                           {:registration :r
                            :store-snapshot :s
                            :stratum-frozen-at 1
                            :environment-revision "env"
                            :harness-revision
                            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                            :environment-checkouts {}})
        emitted (runner/step
                 p frame-state
                 {:tool :emit-frame
                  :args [{:scaffold-path scaffold :closing-path closing
                          :containment-witness-path witness
                          :containment-claimed? claimed?}]})
        advanced (runner/step
                  p (:state emitted)
                  {:tool :advance-problem-phase
                   :args ["M" "C"
                          {:frame {:frame/id "FORGED"
                                   :frame/scaffold-hash "FORGED"
                                   :frame/closing-hash "FORGED"}
                           :containment-probe
                           {:cprobe/id "FORGED" :cprobe/recorded? true}}]})]
    {:emitted emitted :advanced advanced}))

(defn- timestamp-frame-snapshots! [scaffold closing]
  (Files/setLastModifiedTime scaffold (FileTime/fromMillis 1000))
  (Files/setLastModifiedTime closing (FileTime/fromMillis 2000)))

(defn- frame-derived-trace [frame-output]
  (let [cycle-id "cycle/frame-test"
        registration {:problem (:problem synthetic-trace)
                      :required-capabilities []
                      :required-measurement-fields []
                      :reg/solver-seat "solver"}
        frame (assoc (:frame frame-output) :frame/cycle cycle-id)
        probe (assoc (:containment-probe frame-output)
                     :cprobe/frame (:frame/id frame))
        entities [{:cycle/id cycle-id :cycle/closed-at "closed"
                   :cycle/stratum-frozen-at 1 :cycle/assigned-at 2}
                  frame
                  {:snap/id "snap/frame-test" :snap/cycle cycle-id
                   :snap/memory-ids []}
                  probe
                  {:meas/id "meas/frame-test" :meas/cycle cycle-id
                   :meas/values {} :meas/unset {}}]
        trace (apm-harness/derive-trace registration cycle-id entities)]
    {:registration registration :trace trace}))

(defn- frame-validation-failures [frame-output]
  (let [{:keys [registration trace]} (frame-derived-trace frame-output)]
    (prereg/trace-content-failures registration trace
                                   {:status :ok :jobs []} "solver")))

(deftest machine-produced-scaffold-identical-frame-fires-f1
  (let [scaffold (Files/createTempFile
                  "problem-frame-scaffold-identical-" ".lean"
                  (make-array FileAttribute 0))
        closing (Files/createTempFile
                 "problem-frame-closing-identical-" ".lean"
                 (make-array FileAttribute 0))]
    (try
      (spit (.toFile scaffold) "theorem scaffold : True := by trivial\n")
      (spit (.toFile closing) "theorem scaffold : True := by trivial\n")
      (timestamp-frame-snapshots! scaffold closing)
      (let [{:keys [emitted]}
            (machine-frame-result scaffold closing nil false)
            output (:result emitted)]
        (is (:ok emitted))
        (is (some #{:f1-scaffold-identical-frame}
                  (frame-validation-failures output))
            "F1 must fire on equal hashes produced from the actual file bytes"))
      (finally
        (Files/deleteIfExists scaffold)
        (Files/deleteIfExists closing)))))

(deftest machine-produced-unrecorded-containment-claim-fires-f8
  (let [scaffold (Files/createTempFile
                  "problem-frame-scaffold-" ".lean"
                  (make-array FileAttribute 0))
        closing (Files/createTempFile
                 "problem-frame-closing-" ".lean"
                 (make-array FileAttribute 0))]
    (try
      (spit (.toFile scaffold) "scaffold\n")
      (spit (.toFile closing) "closing\n")
      (timestamp-frame-snapshots! scaffold closing)
      (let [{:keys [emitted]} (machine-frame-result scaffold closing nil true)
            output (:result emitted)]
        (is (false? (get-in output [:containment-probe :cprobe/recorded?])))
        (is (some #{:f8-unwitnessed-containment}
                  (frame-validation-failures output))
            "F8 must fire when the arm claims containment without a witness"))
      (finally
        (Files/deleteIfExists scaffold)
        (Files/deleteIfExists closing)))))

(deftest frame-advance-retains-tool-result-not-caller-forgeries
  (let [scaffold (Files/createTempFile
                  "problem-frame-scaffold-" ".lean"
                  (make-array FileAttribute 0))
        closing (Files/createTempFile
                 "problem-frame-closing-" ".lean"
                 (make-array FileAttribute 0))
        witness (Files/createTempFile
                 "problem-frame-witness-" ".edn"
                 (make-array FileAttribute 0))]
    (try
      (spit (.toFile scaffold) "scaffold\n")
      (spit (.toFile closing) "closing\n")
      (spit (.toFile witness) "{:contained? true}\n")
      (timestamp-frame-snapshots! scaffold closing)
      (let [{:keys [emitted advanced]}
            (machine-frame-result scaffold closing witness true)
            tool-output (:result emitted)
            outputs (get-in advanced [:state :cycle/outputs])
            trace (:trace (frame-derived-trace
                           (select-keys outputs [:frame :containment-probe])))]
        (is (:ok advanced) (pr-str advanced))
        (is (= (:frame tool-output) (:frame outputs)))
        (is (= (:containment-probe tool-output)
               (:containment-probe outputs)))
        (is (not= "FORGED" (get-in outputs [:frame :frame/id])))
        (is (= (select-keys (:frame trace) [:scaffold-hash :closing-hash])
               {:scaffold-hash
                (get-in tool-output [:frame :frame/scaffold-hash])
                :closing-hash
                (get-in tool-output [:frame :frame/closing-hash])}))
        (is (true? (:containment-probe-recorded? trace))))
      (finally
        (Files/deleteIfExists scaffold)
        (Files/deleteIfExists closing)
        (Files/deleteIfExists witness)))))

(defn- register-tool-run [clock snapshot]
  (let [revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        p (problem/make-problem
           (tools/make-mock-backend)
           (fn [_ _] {:evidence {}})
           "/tmp/register-tool-state"
           (fn [{:keys [arm]}]
             {:checkout (str "/frames/" arm) :base-revision revision})
           (fn [_] {:harness-revision revision :harness-tree-dirty? false})
           (constantly snapshot) clock)
        start (start-problem p {:session-id "register-tools" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        snapped (runner/step p (:state begun) {:tool :snapshot-store :args []})
        frozen (runner/step p (:state snapped) {:tool :freeze-stratum :args []})
        assigned (runner/step
                  p (:state frozen)
                  {:tool :assign-checkouts
                   :args [{:problem "p" :batch "b" :base-rev revision
                           :solver-seat "codex-4" :student-seat "zai-1"
                           :recall-system "v1"}]})
        advanced (runner/step
                  p (:state assigned)
                  {:tool :advance-problem-phase
                   :args ["M" "C" {:registration :r
                                      :store-snapshot {:snap/id "FORGED"}
                                      :stratum-frozen-at -1
                                      :environment-revision revision
                                      :harness-revision revision
                                      :environment-checkouts {}}]})]
    {:p p :advanced advanced}))

(deftest snapshot-tool-result-reaches-the-trace-projection
  (let [ticks (atom 0)
        {:keys [p advanced]} (register-tool-run #(swap! ticks inc)
                                                ["memory/a" "memory/b"])
        outputs (get-in advanced [:state :cycle/outputs])
        snapshot (:store-snapshot outputs)
        captured (atom nil)
        state (assoc (:state advanced)
                     :current-phase :close
                     :cycle/outputs {:registration :r
                                     :store-snapshot snapshot})
        emitted (with-redefs [apm-harness/derive-trace
                              (fn [_ _ entities]
                                (reset! captured entities)
                                {:projected true})]
                  (runner/step p state {:tool :emit-trace :args []}))
        snap-entity (some #(when (:snap/id %) %) @captured)]
    (is (:ok advanced))
    (is (:ok emitted))
    (is (not= "FORGED" (:snap/id snapshot)))
    (is (= (:current-cycle-id (:state advanced)) (:snap/cycle snapshot)))
    (is (= ["memory/a" "memory/b"] (:snap/memory-ids snapshot)))
    (is (= (select-keys snapshot [:snap/id :snap/cycle :snap/memory-ids])
           (select-keys snap-entity [:snap/id :snap/cycle :snap/memory-ids])))))

(defn- f4-failures-for-order [tool-order]
  ;; Drive the two real tools in the given order and ask F4 about the result.
  ;; The clock is the engine's STEP INDEX, so the ordering cannot be forced by
  ;; injecting a degenerate clock -- it can only be produced by actually doing
  ;; the operations in the wrong order, which is the failure F4 exists to catch.
  (let [revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        p (problem/make-problem
           (tools/make-mock-backend) (fn [_ _] {:evidence {}})
           "/tmp/f4-order-state"
           (fn [{:keys [arm]}] {:checkout (str "/frames/" arm)
                                :base-revision revision})
           (fn [_] {:harness-revision revision :harness-tree-dirty? false})
           (constantly []) (constantly 0))
        start (start-problem p {:session-id "f4-order" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (:state (runner/step p (:state start)
                                   {:tool :begin-problem-cycle :args ["M" "C"]}))
        step (fn [st tool]
               (:state (runner/step
                        p st {:tool tool
                              :args (if (= tool :assign-checkouts)
                                      [{:problem "p" :batch "b" :base-rev revision
                                        :solver-seat "codex-4" :student-seat "zai-1"
                                        :recall-system "v1"}]
                                      [])})))
        done (reduce step begun tool-order)
        advanced (runner/step
                  p done {:tool :advance-problem-phase
                          :args ["M" "C" {:registration :r
                                             :store-snapshot {:snap/id "s"}
                                             :environment-revision revision
                                             :harness-revision revision
                                             :environment-checkouts {}}]})
        outputs (get-in advanced [:state :cycle/outputs])]
    {:outputs outputs
     :failures (prereg/trace-content-failures
                {} (assoc synthetic-trace
                          :stratum-frozen-at (:stratum-frozen-at outputs)
                          :assigned-at (:assigned-at outputs))
                [] nil)}))

(deftest f4-fires-when-the-stratum-is-frozen-after-assignment
  (let [wrong (f4-failures-for-order [:assign-checkouts :freeze-stratum])
        right (f4-failures-for-order [:freeze-stratum :assign-checkouts])]
    (is (some #{:f4-stratum-not-frozen-before-assignment} (:failures wrong))
        "freezing the stratum AFTER assignment must trip F4")
    (is (not (some #{:f4-stratum-not-frozen-before-assignment} (:failures right)))
        "the correct order must NOT trip F4 -- a gate that always fires is useless")
    (is (< (:stratum-frozen-at (:outputs right))
           (:assigned-at (:outputs right))))))

(deftest runner-driven-problem-traverse-stops-at-the-first-real-gate
  (let [p (harness-measured-problem
           {:harness-revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            :harness-tree-dirty? false})
        start (start-problem p {:session-id "dry-traverse" :problem-id "t94J02"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        advanced (runner/step p (:state begun)
                              {:tool :advance-problem-phase
                               :args ["M" "C" {}]})]
    (is (= :register (get-in begun [:state :current-phase])))
    (is (= :missing-required-outputs (:error/code advanced)))
    (is (some #{:registration} (get-in advanced [:error/context :missing])))
    (is (= :register (get-in begun [:state :current-phase]))
        "the traversal uses runner state and never hand-assocs a phase")))

(deftest forged-harness-revision-is-overwritten-by-cycle-open-measurement
  (let [measured "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        p (harness-measured-problem {:harness-revision measured
                                     :harness-tree-dirty? false})
        start (start-problem p {:session-id "harness-stamp" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        state (assoc (:state begun) :current-phase :guided-solve
                     :cycle/outputs
                     {:registration {:reg/harness-revision measured}
                      :store-snapshot :s :stratum-frozen-at 1
                      :environment-revision "env"
                      :harness-revision measured
                      :environment-checkouts
                      {:solver {:checkout "/solver" :base-revision "env"}
                       :student []}
                      :frame :f :containment-probe :c})
        advanced (runner/step
                  p state
                  {:tool :advance-problem-phase
                   :args ["M" "C" {:solver-attempt
                                      {:cycle/harness-revision "FORGED"}
                                      :ground-control-events []
                                      :memory-offers []}]})]
    (is (:ok begun))
    (is (:ok advanced) (pr-str advanced))
    (is (= measured (get-in begun [:result :harness-revision])))
    (is (= measured
           (get-in advanced
                   [:state :cycle/outputs :solver-attempt
                    :cycle/harness-revision])))))

(deftest dirty-harness-at-cycle-open-fails-the-register-advance
  (let [revision "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        p (harness-measured-problem {:harness-revision revision
                                     :harness-tree-dirty? true})
        start (start-problem p {:session-id "dirty-harness" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        result (runner/step
                p (:state begun)
                {:tool :advance-problem-phase
                 :args ["M" "C" {:registration :r :store-snapshot :s
                                    :stratum-frozen-at 1
                                    :environment-revision "env"
                                    :harness-revision "FORGED"
                                    :environment-checkouts {}}]})]
    (is (= :harness-tree-dirty (:error/code result)))
    (is (= :harness-tree-clean
           (get-in result [:error/context :invariant])))))

(deftest measured-harness-revision-must-match-registration
  (let [measured "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        registered "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        p (harness-measured-problem {:harness-revision measured
                                     :harness-tree-dirty? false})
        start (start-problem p {:session-id "harness-pin" :problem-id "p"
                               :cycle/mode :store-mode
                               :harness-repo "/measured/harness"})
        begun (runner/step p (:state start)
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        state (assoc (:state begun) :current-phase :student-attempts
                     :cycle/outputs
                     (-> outputs-through-student
                         (assoc :registration {:reg/harness-revision registered}
                                :harness-revision measured
                                :harness-tree-dirty? false)
                         (assoc-in [:solver-attempt :cycle/harness-revision]
                                   measured)))
        result (runner/step
                p state
                {:tool :advance-problem-phase
                 :args ["M" "C" {:student-attempts [{}] :memory-uses []}]})]
    (is (= :harness-revision-not-registered (:error/code result)))
    (is (= [measured measured]
           (get-in result [:error/context :details :actual])))))

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
         :steps [{:tool :record-measurement
                  :result {:meas/id "meas-cycle-1"
                           :meas/values {"x" 1} :meas/unset {}}
                  :evidence/id "e-measurement"}
                 {:tool :emit-capability-probes :result []
                  :evidence/id "e-capability-probes"}]
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
          :ground-control-events
          [{:job-id "guide-job" :ground-control/recipient "codex-4"
            :ground-control/cycle "cycle-1" :ground-control/type :challenge}]
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
    (is (= {:projected true
            :guidance-events
            [{:job-id "guide-job" :ground-control/recipient "codex-4"
              :ground-control/cycle "cycle-1" :ground-control/type :challenge}]
            ;; :action-refusals was added to the emitted trace by D6 (585a980e,
            ;; durable refusal receipts). This assertion is exact map equality,
            ;; so the new key must be declared. Empty is correct here: the
            ;; fixture refuses nothing.
            :action-refusals []
            :measurement-summary {:measured 1 :unset 0}}
           (get-in result [:result :trace])))
    ;; NOT empty: retrieval-probe has no producer, and this fixture previously
    ;; supplied :retrieval-probes in outputs to make the list empty -- i.e. it was
    ;; relying on exactly the caller-injection route Fable's finding 3 closed. The
    ;; honest expectation is the one known gap and nothing else.
    (is (= [{:failure :missing-trace-entity-producer :entity-type :retrieval-probe}]
           (get-in result [:result :producer-failures])))
    (is (= "cycle-1" (:cycle-id @called)))
    (is (= "cycle-1" (:cycle/id cycle-entity)))
    (is (= :store-mode (:cycle/mode cycle-entity)))
    (is (= "2026-08-15T00:00:00Z" (:cycle/opened-at cycle-entity)))
    (is (= closed-at (:cycle/closed-at cycle-entity))
        "the engine clock, not the caller, closes the trace")
    (is (= #{"cycle-1" "frame-1" "snap-1" "cprobe-1"
             "meas-cycle-1" "attempt-1"}
           (set (keep apm-harness/entity-id (:entities @called))))
        "typed entities are assembled from cycle outputs")))

(deftest emit-trace-carries-the-solver-config-actually-dispatched
  (let [[p state] (started :store-mode)
        pin {:model "gpt-5.6-sol" :reasoning-effort "high"}
        state (assoc-in (trace-emission-state state)
                        [:cycle/outputs :solver-dispatches]
                        [{:job-id "solver-job"
                          :ground-control/recipient "codex-4"
                          :ground-control/cycle "cycle-1"
                          :ground-control/solver-config pin}])
        result (with-redefs [apm-harness/derive-trace
                             (fn [_registration _cycle-id _entities]
                               {:projected true})]
                 (runner/step p state {:tool :emit-trace :args []}))]
    (is (:ok result))
    (is (= pin (get-in result [:result :trace :solver-dispatches 0
                               :ground-control/solver-config])))))

(deftest emit-trace-names-missing-entity-producers
  (let [[p state] (started :store-mode)
        state (-> (trace-emission-state state)
                  (update :cycle/outputs dissoc :retrieval-probes))
        result (runner/step p state {:tool :emit-trace :args []})
        missing (set (map :entity-type
                          (get-in result [:result :producer-failures])))]
    (is (:ok result))
    (is (= #{:retrieval-probe} missing))))

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
                   {:tool :emit-trace :args []})
          types (set (map :entity-type
                          (get-in missing [:result :producer-failures])))]
      (is (:ok missing))
      (is (contains? types :memory-offer)))))

(deftest emit-trace-populates-through-the-REAL-projection
  ;; No with-redefs. The other emit-trace tests stub derive-trace, so they assert
  ;; it is CALLED but never run it -- and derive-trace locates entities by linking
  ;; keys (:frame/cycle, :attempt/cycle, :offer/cycle...). Unstamped outputs made
  ;; it throw "expected exactly one entity" on every real cycle while the stubbed
  ;; tests stayed green. A stubbed collaborator hides the integration it stands in
  ;; for.
  (let [p (problem/make-problem (tools/make-mock-backend))
        st (assoc (:state (start-problem p {:session-id "e2e" :problem-id "t94J02"
                                           :cycle/mode :store-mode}))
                  :current-phase :close :current-cycle-id "C1"
                  :steps [{:tool :record-measurement
                           :result {:meas/values {"a" 1} :meas/unset {}}
                           :evidence/id "e-measurement"}
                          {:tool :emit-capability-probes :result []
                           :evidence/id "e-capability-probes"}]
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
        trace (get-in (last (get-in r [:state :steps])) [:result :trace])]
    (is (:ok r) "the real projection must not throw")
    (is (= {:scaffold-hash "aa" :closing-hash "bb"} (:frame trace)))
    (is (= 2 (count (:cycle/attempts trace))) "solver + student attempts found")
    (is (= [{:offer/id "O1" :offer/memory-id "M1"}] (:memory-offers trace)))
    (is (= ["D1"] (:disposition-ids trace)))
    (is (true? (:containment-probe-passed? trace)))
    (is (= "S1" (:cycle/store-snapshot-id trace)))
    (is (= ["P1"] (:promoted-artifact-ids trace)))
    (is (string? (get-in trace [:cycle/window :closed-at])))))

(defn- capability-probe-state [state steps]
  (assoc state :current-phase :close :current-cycle-id "C-probes" :steps steps))

(deftest capability-probes-cite-the-latest-attesting-steps
  (let [backend (tools/make-mock-backend)
        p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec backend)
        state (:state (start-problem p {:session-id "cap-probes"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        steps [{:tool :emit-frame :evidence/id "e-frame"}
               {:tool :write-disposition :evidence/id "e-disposition"}
               {:tool :write-use :evidence/id "e-use-old"}
               {:tool :write-use :evidence/id "e-use-latest"}
               {:tool :promote-artifact :evidence/id "e-promotion"}]
        result (runner/step
                p (capability-probe-state state steps)
                {:tool :emit-capability-probes :args [{:forged true}]})
        probes (:result result)
        by-capability (into {} (map (juxt :probe/capability identity)) probes)]
    (is (:ok result))
    (is (= 6 (count probes)))
    (is (= "e-frame"
           (get-in by-capability [:created-frame-worked :probe/evidence-id])))
    (is (= "e-use-latest"
           (get-in by-capability [:offer-use-disposition :probe/evidence-id]))
        "when several steps attest a capability, the latest is cited")
    (is (= "e-promotion"
           (get-in by-capability [:promotion-importable :probe/evidence-id])))
    (is (every? #(and (:probe/recorded? %)
                      (not (str/blank? (:probe/evidence-id %))))
                probes))
    (is (empty? (tools/recorded-calls backend))
        "the engine derives probes without consulting the backend")))

(deftest absent-attesting-step-yields-no-probe-and-real-f9-failure
  (let [p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec
                                       (tools/make-mock-backend))
        state (:state (start-problem p {:session-id "missing-probe"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        result (runner/step
                p (capability-probe-state
                   state [{:tool :emit-frame :evidence/id "e-frame"}])
                {:tool :emit-capability-probes :args []})
        probes (:result result)
        trace (assoc synthetic-trace
                     :capability-probes
                     (mapv (fn [probe]
                             {:capability (:probe/capability probe)
                              :evidence-id (:probe/evidence-id probe)
                              :recorded? (:probe/recorded? probe)})
                           probes))
        registration {:required-capabilities prereg/required-capabilities
                      :reg/solver-seat "codex-4"}
        content-failures (prereg/trace-content-failures
                          registration trace {:status :ok :jobs []} "codex-4")]
    (is (= #{:created-frame-worked :frame-containment-witnessed}
           (set (map :probe/capability probes)))
        "capabilities with no attesting step get no probe")
    (is (some #{:f9-capability-probe-missing} content-failures))))

(defn- adjudication-machine-results []
  (let [p (problem/make-problem
           (tools/make-mock-backend) (fn [& _] {:ok true}) "/tmp/adjudication-state"
           (fn [_] {})
           (fn [_] {:harness-revision (apply str (repeat 40 "a"))
                    :harness-tree-dirty? false})
           (constantly []) (constantly 0))
        start (:state (start-problem p {:session-id "adjudication-tools"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode
                                       :harness-repo "/harness"
                                       :lean-repo "/lean"}))
        begun (runner/step p start {:tool :begin-problem-cycle :args ["M" "C"]})
        adjudicate (assoc (:state begun)
                          :current-phase :adjudicate
                          :cycle/outputs
                          {:registration {:reg/environment-revision "env"
                                          :reg/harness-revision
                                          (apply str (repeat 40 "a"))}
                           :store-snapshot {}
                           :stratum-frozen-at 1
                           :environment-revision "env"
                           :harness-revision
                           (apply str (repeat 40 "a"))
                           :environment-checkouts {}
                           :frame {}
                           :containment-probe {}
                           :solver-attempt
                           {:cycle/environment-revision "env"
                            :cycle/harness-revision
                            (apply str (repeat 40 "a"))
                            :cycle/environment-checkout "/solver"}
                           :ground-control-events []
                           :memory-offers []
                           :intervention {}
                           :promotion-result []
                           :student-attempts
                           [{:cycle/environment-revision "env"
                             :cycle/harness-revision
                             (apply str (repeat 40 "a"))
                             :cycle/environment-checkout "/student"}]
                           :memory-uses []})
        d1 (runner/step p adjudicate
                        {:tool :write-disposition :args [{:outcome :closed}]})
        d2 (runner/step p (:state d1)
                        {:tool :write-disposition :args [{:outcome :tier-a}]})
        u1 (runner/step p (:state d2)
                        {:tool :write-use :args [{:offer-id "offer/one"}]})
        u2 (runner/step p (:state u1)
                        {:tool :write-use :args [{:offer-id "offer/two"}]})
        advanced (runner/step
                  p (:state u2)
                  {:tool problem/advance
                   :args ["M" "C"
                          {:disposition [{:disp/id "FORGED"}]
                           :memory-uses [{:use/id "FORGED"
                                          :use/offer "offer/FORGED"}]}]})
        promoted (runner/step
                  p (:state advanced)
                  {:tool :promote-artifact
                   :args [{:artifact-id "artifact/one"
                           :importable? true :need-tags ["needed"]}]})
        promoted-without-tags (runner/step
                               p (:state promoted)
                               {:tool :promote-artifact
                                :args [{:artifact-id "artifact/two"
                                        :importable? true}]})
        promotion-advanced
        (runner/step p (:state promoted-without-tags)
                     {:tool problem/advance
                      :args ["M" "C" {:promotion-result [{:promo/id "FORGED"}]}]})]
    {:cycle-id (get-in begun [:state :current-cycle-id])
     :dispositions [(:result d1) (:result d2)]
     :uses [(:result u1) (:result u2)]
     :promotions [(:result promoted) (:result promoted-without-tags)]
     :adjudicate-outputs (get-in advanced [:state :cycle/outputs])
     :promotion-outputs (get-in promotion-advanced [:state :cycle/outputs])}))

(defn- adjudication-trace [cycle-id dispositions uses promotions]
  (let [registration {:problem (:problem synthetic-trace)
                      :required-capabilities []
                      :required-measurement-fields []
                      :reg/solver-seat "solver"}
        entities (concat
                  [{:cycle/id cycle-id :cycle/closed-at "closed"
                    :cycle/stratum-frozen-at 1 :cycle/assigned-at 2}
                   {:frame/id "frame/adjudication" :frame/cycle cycle-id
                    :frame/scaffold-hash "before" :frame/closing-hash "after"}
                   {:snap/id "snap/adjudication" :snap/cycle cycle-id
                    :snap/memory-ids []}
                   {:cprobe/id "probe/adjudication"
                    :cprobe/frame "frame/adjudication"
                    :cprobe/claimed? false :cprobe/recorded? true
                    :cprobe/passed? true}
                   {:meas/id "meas/adjudication" :meas/cycle cycle-id
                    :meas/values {} :meas/unset {}}
                   {:offer/id "offer/one" :offer/cycle cycle-id
                    :offer/memory-id "memory/one"}
                   {:offer/id "offer/two" :offer/cycle cycle-id
                    :offer/memory-id "memory/two"}]
                  dispositions uses promotions)]
    {:registration registration
     :trace (apm-harness/derive-trace registration cycle-id entities)}))

(defn- adjudication-failures [{:keys [registration trace]}]
  (prereg/trace-content-failures registration trace
                                  {:status :ok :jobs []} "solver"))

(deftest machine-dispositions-and-uses-drive-both-directions-of-f2-and-f3
  (let [{:keys [cycle-id dispositions uses adjudicate-outputs]}
        (adjudication-machine-results)
        zero-dispositions (adjudication-trace cycle-id [] uses [])
        one-disposition (adjudication-trace cycle-id [(first dispositions)] uses [])
        two-dispositions (adjudication-trace cycle-id dispositions uses [])
        missing-use (adjudication-trace cycle-id [(first dispositions)]
                                          [(first uses)] [])]
    (is (some #{:f2-non-unique-disposition}
              (adjudication-failures zero-dispositions)))
    (is (some #{:f2-non-unique-disposition}
              (adjudication-failures two-dispositions)))
    (is (not (some #{:f2-non-unique-disposition}
                   (adjudication-failures one-disposition))))
    (is (some #{:f3-undispositioned-offer}
              (adjudication-failures missing-use)))
    (is (not (some #{:f3-undispositioned-offer}
                   (adjudication-failures one-disposition))))
    (is (= (mapv :disp/id dispositions)
           (mapv :disp/id (:disposition adjudicate-outputs)))
        "recorded tool dispositions overwrite the forged advance payload")
    (is (= (mapv :use/id uses) (mapv :use/id (:memory-uses adjudicate-outputs)))
        "recorded tool uses overwrite the forged advance payload")))

(deftest machine-promotions-drive-both-capabilities-and-overwrite-forgeries
  (let [{:keys [cycle-id dispositions uses promotions promotion-outputs]}
        (adjudication-machine-results)
        complete (adjudication-trace cycle-id [(first dispositions)] uses
                                      [(first promotions)])
        missing-tags (adjudication-trace cycle-id [(first dispositions)] uses
                                          [(second promotions)])]
    (is (prereg/capability-holds? :promotion-importable (:trace complete)))
    (is (prereg/capability-holds? :promotion-need-taggable (:trace complete)))
    (is (not (prereg/capability-holds? :promotion-need-taggable
                                       (:trace missing-tags))))
    (is (= (mapv :promo/id promotions)
           (mapv :promo/id (:promotion-result promotion-outputs)))
        "recorded promotions overwrite the forged advance payload")))

(defn- measurement-state [state]
  (assoc state
         :current-phase :close
         :current-cycle-id "C-measurement"
         :cycle/outputs
         {:registration
          {:required-measurement-fields prereg/required-measurement-fields
           :problem {:locked-lemma-exposure ["lemma/a" "lemma/b"]}}
          :disposition {:disp/outcome :closed
                        :disp/residual-sorries 0
                        :disp/axiom-clean? true}}))

(defn- scribe-report-state [state]
  (assoc state
         :current-phase :promote
         :current-cycle-id "C-scribe"
         :cycle/outputs
         {:registration
          {:reg/scribe-seat "ams-scribe-1"
           :required-measurement-fields
           ["scribe lane coverage" "arc-lane yield"]}}))

(deftest staffed-scribe-lane-reports-populate-measurement
  (let [p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec
                                       (tools/make-mock-backend))
        start (:state (start-problem p {:session-id "scribe-report"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        solve (runner/step
               p (scribe-report-state start)
               {:tool :record-scribe-lanes
                :args [{:lane :solve :ran? true :yield ["memory/solve"]
                        :author "ams-scribe-1"}]})
        arc (runner/step
             p (:state solve)
             {:tool :record-scribe-lanes
              :args [{:lane :arc :ran? true
                      :yield ["memory/arc-1" "memory/arc-2"]
                      :author "ams-scribe-1"}]})
        outputs (#'problem/stamp-environment-outputs
                 (:state arc) (:cycle/outputs (:state arc)))
        close-state (assoc (:state arc)
                           :current-phase :close
                           :cycle/outputs outputs)
        measured (runner/step p close-state
                              {:tool :record-measurement :args []})
        values (get-in measured [:result :meas/values])]
    (is (= [{:lane :solve :ran? true :yield ["memory/solve"]
             :author "ams-scribe-1"}
            {:lane :arc :ran? true
             :yield ["memory/arc-1" "memory/arc-2"]
             :author "ams-scribe-1"}]
           (:scribe-lane-reports outputs)))
    (is (= {:lanes-ran [:arc :solve] :ran 2 :total 4}
           (get values "scribe lane coverage")))
    (is (= 2 (get values "arc-lane yield")))))

(deftest scribe-lane-report-refuses-unknown-lane-and-wrong-seat
  (let [p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec
                                       (tools/make-mock-backend))
        start (:state (start-problem p {:session-id "scribe-refusal"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        state (scribe-report-state start)
        unknown (runner/step
                 p state
                 {:tool :record-scribe-lanes
                  :args [{:lane :unknown :ran? true :yield []
                          :author "ams-scribe-1"}]})
        mismatch (runner/step
                  p state
                  {:tool :record-scribe-lanes
                   :args [{:lane :arc :ran? true :yield []
                           :author "other-seat"}]})]
    (is (= :tool-execution-failed (:error/code unknown)))
    (is (re-find #":unknown-scribe-lane" (pr-str unknown)))
    (is (= :tool-execution-failed (:error/code mismatch)))
    (is (re-find #":scribe-seat-mismatch" (pr-str mismatch)))))

(deftest absent-scribe-reports-retain-explicit-unset-reasons
  (let [p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec
                                       (tools/make-mock-backend))
        start (:state (start-problem p {:session-id "scribe-unset"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        state (-> (scribe-report-state start)
                  (assoc :current-phase :close))
        measured (runner/step p state
                              {:tool :record-measurement :args []})]
    (is (= "unset: no scribe-lane event is present in cycle outputs"
           (get-in measured [:result :meas/unset "scribe lane coverage"])))
    (is (= "unset: no arc-lane event is present in cycle outputs"
           (get-in measured [:result :meas/unset "arc-lane yield"])))))

(deftest promotions-and-scribe-reports-aggregate-across-both-promote-phases
  (let [promotion {:promo/id "promo/recorded"
                   :promo/artifact-id "recorded/artifact"
                   :promo/importable? true
                   :promo/need-tags ["recorded"]}
        p (cycle/make-cycle-peripheral
           problem/problem-domain-config problem/problem-spec
           (tools/make-mock-backend {:promote-artifact promotion}))
        start (:state (start-problem p {:session-id "two-promotions"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        state (-> (scribe-report-state start)
                  (assoc :current-phase :promote-solver)
                  (assoc-in [:cycle/outputs :memory-offers]
                            [{:offer/id "offer/solver"}]))
        p1 (runner/step p state
                        {:tool :promote-artifact
                         :args [{:artifact-id "solver/artifact"
                                 :importable? true :need-tags ["solver"]}]})
        s1 (runner/step p (:state p1)
                        {:tool :record-scribe-lanes
                         :args [{:lane :solve :ran? true
                                 :yield ["solver/artifact"]
                                 :author "ams-scribe-1"}]})
        post-student (assoc (:state s1) :current-phase :promote)
        p2 (runner/step p post-student
                        {:tool :promote-artifact
                         :args [{:artifact-id "student/artifact"
                                 :importable? true :need-tags ["student"]}]})
        s2 (runner/step p (:state p2)
                        {:tool :record-scribe-lanes
                         :args [{:lane :trajectory :ran? true
                                 :yield ["student/artifact"]
                                 :author "ams-scribe-1"}]})
        outputs (#'problem/stamp-environment-outputs
                 (:state s2) (:cycle/outputs (:state s2)))]
    (is (:ok p1))
    (is (:ok s1))
    (is (:ok p2))
    (is (:ok s2))
    (is (= ["recorded/artifact" "recorded/artifact"]
           (mapv :promo/artifact-id (:promotion-result outputs))))
    (is (= [:solve :trajectory]
           (mapv :lane (:scribe-lane-reports outputs))))
    (is (= [{:offer/id "offer/solver"}] (:memory-offers outputs))
        "F3's offer input survives both promotion advances")))

(deftest record-measurement-covers-every-required-field-with-values-or-reasons
  (let [backend (tools/make-mock-backend)
        p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec backend)
        state (:state (start-problem p {:session-id "measurement"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        result (runner/step p (measurement-state state)
                            {:tool :record-measurement
                             :args [{:meas/values {"forged" 1}}]})
        measurement (:result result)
        values (:meas/values measurement)
        unset (:meas/unset measurement)
        covered (into (set (keys values)) (keys unset))]
    (is (:ok result))
    (is (every? covered prereg/required-measurement-fields)
        "every registered field is measured or explicitly unset")
    (is (= {:outcome :closed :residual 0 :axiom-clean? true
            :locked-exposure ["lemma/a" "lemma/b"]}
           {:outcome (get values "terminal disposition")
            :residual (get values "residual executable sorries")
            :axiom-clean? (get values "axiom cleanliness")
            :locked-exposure (get values "locked-lemma exposure")}))
    (is (every? (fn [[_ reason]]
                  (and (string? reason) (not (str/blank? reason))))
                unset)
        "unset is evidence-bearing, never a blank label")
    (is (not (contains? values "forged"))
        "caller-supplied measurements are ignored")
    (is (empty? (tools/recorded-calls backend))
        "measurement is computed by the engine, not the backend")))

(deftest record-measurement-refuses-a-mutated-silent-omission
  (let [p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec
                                       (tools/make-mock-backend))
        state (:state (start-problem p {:session-id "measurement-mutation"
                                       :problem-id "t94J02"
                                       :cycle/mode :store-mode}))
        field "statement defects at review"
        original @#'problem/measurement-unset-reasons
        result (with-redefs [problem/measurement-unset-reasons
                             (fn [missing]
                               (dissoc (original missing) field))]
                 (runner/step p (measurement-state state)
                              {:tool :record-measurement :args []}))]
    (is (= :tool-execution-failed (:error/code result)))
    (is (re-find #"silently omitted" (str result)))
    (is (re-find #"statement defects at review" (str result)))))

(defn- close-tool-state [p context emit-envelope]
  (let [state (:state (start-problem p context))]
    (assoc state
           :current-phase :close
           :current-cycle-id "C-close"
           :cycle/outputs {:registration {:lean-revision "lean-pin"
                                          :problem {:problem-id "p"}
                                          :reg/solver-seat "codex-4"}}
           :steps [{:tool :emit-trace :result emit-envelope
                    :evidence/id "e-emit-trace"}])))

(deftest validate-trace-accumulates-producer-and-validator-failures
  (let [backend (tools/make-mock-backend)
        p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec backend)
        context {:session-id "validate-close" :problem-id "p"
                 :cycle/mode :store-mode
                 :lean-repo "/machine/lean"
                 :agency-endpoint "http://machine/agency"}
        envelope {:trace {:trace/id "T"}
                  :producer-failures
                  [{:failure :missing-trace-entity-producer
                    :entity-type :retrieval-probe}]}
        called (atom nil)
        result (with-redefs [prereg/report
                             (fn [registration trace lean-repo agency solver]
                               (reset! called [registration trace lean-repo
                                               agency solver])
                               {:failures [:f9-capability-probe-missing
                                           :guidance-evidence-unavailable]
                                :launchable? false})]
                 (runner/step
                  p (close-tool-state p context envelope)
                  {:tool :validate-trace
                   :args [{:lean-repo "/forged" :agency-endpoint "forged"}]}))
        validation (:result result)]
    (is (:ok result))
    (is (= ["/machine/lean" "http://machine/agency" "codex-4"]
           (subvec (vec @called) 2)))
    (is (= #{:f9-capability-probe-missing
             :guidance-evidence-unavailable
             {:failure :missing-trace-entity-producer
              :entity-type :retrieval-probe}}
           (set (:failures validation)))
        "the envelope accumulates every producer and validator failure")
    (is (false? (:launchable? validation)))
    (is (empty? (tools/recorded-calls backend)))))

(deftest write-authorization-retains-refusal-and-writes-no-file
  (let [output (str (io/file (temp-state-root) "must-not-exist.edn"))
        backend (tools/make-mock-backend)
        p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec backend)
        context {:session-id "refuse-auth" :problem-id "p"
                 :cycle/mode :store-mode :authorization-output output
                 :authorization-revision
                 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
        validation {:trace {:trace/id "T"}
                    :validation-report {:launchable? false}
                    :failures [:f9-capability-probe-missing]
                    :launchable? false}
        state (update (close-tool-state p context {:trace {:trace/id "T"}
                                                   :producer-failures []})
                      :steps conj
                      {:tool :validate-trace :result validation
                       :evidence/id "e-validate"})
        result (runner/step p state {:tool :write-authorization :args []})]
    (is (:ok result) "a retained refusal is a successful audit operation")
    (is (= {:written? false :refused? true}
           (select-keys (:result result) [:written? :refused?])))
    (is (= [:f9-capability-probe-missing]
           (get-in result [:result :failures])))
    (is (false? (.exists (io/file output)))
        "non-launchable validation can never create an authorization")
    (is (= :write-authorization
           (get-in result [:state :steps (dec (count (get-in result
                                                               [:state :steps])))
                           :tool]))
        "the refusal remains in the auditable step record")
    (is (empty? (tools/recorded-calls backend)))))

(deftest write-authorization-writes-only-a-launchable-report
  (let [output (str (io/file (temp-state-root) "authorization.edn"))
        backend (tools/make-mock-backend)
        p (cycle/make-cycle-peripheral problem/problem-domain-config
                                       problem/problem-spec backend)
        context {:session-id "write-auth" :problem-id "p"
                 :cycle/mode :store-mode :authorization-output output
                 :authorization-revision
                 "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}
        report {:launchable? true :failures []}
        validation {:trace {:trace/id "T"}
                    :validation-report report :failures [] :launchable? true}
        state (update (close-tool-state p context {:trace {:trace/id "T"}
                                                   :producer-failures []})
                      :steps conj
                      {:tool :validate-trace :result validation
                       :evidence/id "e-validate"})
        result (runner/step p state {:tool :write-authorization :args []})
        written (edn/read-string (slurp output))]
    (is (:ok result))
    (is (true? (get-in result [:result :written?])))
    (is (= :apm-demonstration-round1-launch-authorization (:kind written)))
    (is (= report (:validation written)))
    (is (empty? (tools/recorded-calls backend)))))

(deftest injected-retrieval-probes-cannot-silence-the-missing-producer
  ;; Fable's finding 3. Verified before the fix: injecting :retrieval-probes []
  ;; removed :missing-trace-entity-producer for :retrieval-probe from the close
  ;; envelope. The one entity type with no producer was the one a caller could
  ;; supply -- and "an empty collection proves its producer ran" was exactly the
  ;; rule it exploited.
  (let [p (problem/make-problem (tools/make-mock-backend))
        base (:state (start-problem p {:session-id "inj" :problem-id "t94J02"
                                      :cycle/mode :store-mode}))
        outs {:registration {:problem {:problem-id "t94J02" :difficulty-stratum "s"
                                       :regime "r" :locked-lemma-exposure []}
                             :required-measurement-fields ["a"]}
              :frame {:frame/id "F1" :frame/scaffold-hash "aa" :frame/closing-hash "bb"}
              :launch-gate-event {} :store-snapshot {:snap/id "S1" :snap/memory-ids []}
              :containment-probe {} :solver-attempt {} :student-attempts []
              :disposition {:disp/id "D1"} :memory-offers [] :memory-uses []
              :capability-probes [] :promotion-result {}
              :retrieval-probes []
              :stratum-frozen-at 1 :assigned-at "t"}
        st (assoc base :current-phase :close :current-cycle-id "C1"
                  :cycle/outputs outs)
        r (runner/step p st {:tool :emit-trace :args []})
        failures (:producer-failures (:result (last (get-in r [:state :steps]))))]
    (is (some #(= :retrieval-probe (:entity-type %)) failures)
        "an injected empty probe set must not clear the missing-producer failure")))

(deftest provisioned-revision-must-match-the-registration-pin
  ;; Fable's finding 1. The pins were read by nothing, and :assign-checkouts takes
  ;; a caller-supplied :base-rev -- so a cycle provisioned at the WRONG revision
  ;; validated cleanly. Every other check compares the arms against each other.
  (let [pin "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        wrong "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        outputs (-> outputs-through-student
                    (assoc :registration {:reg/environment-revision pin}
                           :environment-revision wrong))
        result (advance-student-attempts outputs [{}])]
    (is (= :environment-revision-not-registered (:error/code result)))))

;; The measurement is PATH-SCOPED, not HEAD-scoped. This is the difference
;; between a pin that means "this harness code ran" and one that goes stale the
;; moment anything else in the repo is committed -- including the very
;; amendment that records the pin, since a commit cannot contain its own sha.
;; Exercises the real git-backed measurer against a throwaway repository, so it
;; does not depend on futon3c's own working-tree state.
(defn- git! [dir & args]
  (let [{:keys [exit err]} (apply clojure.java.shell/sh
                                  (concat ["git" "-C" (str dir)] args))]
    (assert (zero? exit) (str "git " (pr-str args) " failed: " err))))

(defn- throwaway-harness-repo []
  (let [dir (.toFile (Files/createTempDirectory
                      "harness-scope" (make-array FileAttribute 0)))]
    (git! dir "init" "-q")
    (git! dir "config" "user.email" "t@t")
    (git! dir "config" "user.name" "t")
    (io/make-parents (io/file dir "src" "code.clj"))
    (spit (io/file dir "src" "code.clj") "(ns code)\n")
    (git! dir "add" "-A") (git! dir "commit" "-q" "-m" "harness code")
    dir))

(deftest harness-revision-is-the-harness-paths-commit-not-repository-head
  (let [dir (throwaway-harness-repo)
        harness-commit (str/trim (:out (clojure.java.shell/sh
                                        "git" "-C" (str dir) "rev-parse" "HEAD")))
        _ (do (spit (io/file dir "NOTES.md") "a note outside the harness\n")
              (git! dir "add" "-A") (git! dir "commit" "-q" "-m" "a note"))
        head (str/trim (:out (clojure.java.shell/sh
                              "git" "-C" (str dir) "rev-parse" "HEAD")))
        measured (#'problem/measure-harness-repository (str dir))]
    (is (not= harness-commit head) "the note must have moved HEAD")
    (is (= harness-commit (:harness-revision measured))
        "a commit outside src/scripts/deps.edn must NOT move the harness pin")
    (is (false? (:harness-tree-dirty? measured)))))

(deftest harness-dirtiness-ignores-changes-outside-the-harness-paths
  (let [dir (throwaway-harness-repo)]
    (spit (io/file dir "NOTES.md") "an uncommitted note\n")
    (is (false? (:harness-tree-dirty? (#'problem/measure-harness-repository (str dir))))
        "an uncommitted note must not read as an unfrozen harness")
    (spit (io/file dir "src" "code.clj") "(ns code) ;; edited\n")
    (is (true? (:harness-tree-dirty? (#'problem/measure-harness-repository (str dir))))
        "an uncommitted edit to harness code MUST read as dirty")))

;; DETERMINISTIC IS NOT THE SAME AS CONSTANT. The cycle id keys the save/load
;; state directory (data/problem-state/<cycle-id>/, write-once), so it must be
;; reproducible -- but a second cycle of the same session on the same problem
;; with the same begin args must not reuse the first cycle's id. It did: the
;; digest covered only session, problem and args. Two consequences, the second
;; worse than the first: cycle 2 writes into cycle 1's version directory, and the
;; :problem-load cross-cycle guard compares :current-cycle-id, so with identical
;; ids it cannot fire -- cycle 2 could restore cycle 1's state as its own.
(defn- begin-two-cycles-in-one-session []
  (let [p (problem/make-problem
           (tools/make-mock-backend) (fn [& _] {:ok true}) "/tmp/problem-id-test"
           (fn [_] {:checkout "/tmp/problem-id-test/co" :base-revision "rev"})
           (fn [_] {:harness-revision (apply str (repeat 40 "a"))
                    :harness-tree-dirty? false}))
        state (:state (start-problem p {:session-id "S" :problem-id "t94J02"
                                       :cycle/mode :store-mode
                                       :harness-repo "/tmp/harness"}))
        first-begin (runner/step p state
                                 {:tool :begin-problem-cycle :args ["M" "C"]})
        ;; the engine clears these on the final advance; this is the state a
        ;; second cycle legitimately begins from
        cleared (dissoc (:state first-begin) :current-phase :current-cycle-id)
        second-begin (runner/step p cleared
                                  {:tool :begin-problem-cycle :args ["M" "C"]})]
    [(get-in first-begin [:state :current-cycle-id])
     (get-in second-begin [:state :current-cycle-id])]))

(deftest two-cycles-in-one-session-get-distinct-ids
  (let [[first-id second-id] (begin-two-cycles-in-one-session)]
    (is (some? first-id))
    (is (some? second-id))
    (is (not= first-id second-id)
        "a second cycle must not reuse the first cycle's state directory")))

(deftest cycle-ids-are-reproducible-across-fresh-peripherals
  (is (= (begin-two-cycles-in-one-session) (begin-two-cycles-in-one-session))
      "replaying the same sequence of begins must reproduce the same ids"))

;; THE OTHER DIRECTION. B's F1 test proves the gate FIRES on a deliberately
;; scaffold-identical frame. Nothing proved it stays SILENT on an ordinary one --
;; so a defect making every frame scaffold-identical shipped green. Verified by
;; mutation: hashing the scaffold file for BOTH hashes reddened nothing across 65
;; tests. A gate that always fires is as useless as one that never does, and only
;; the pair of tests pins the tool to the gate.
(deftest a-distinct-frame-does-not-fire-f1
  (let [scaffold (Files/createTempFile "problem-frame-scaffold-distinct-" ".lean"
                                       (make-array FileAttribute 0))
        closing (Files/createTempFile "problem-frame-closing-distinct-" ".lean"
                                      (make-array FileAttribute 0))]
    (try
      (spit (.toFile scaffold) "theorem t : True := by sorry\n")
      (spit (.toFile closing) "theorem t : True := by trivial\n")
      (timestamp-frame-snapshots! scaffold closing)
      (let [{:keys [emitted]} (machine-frame-result scaffold closing nil false)
            output (:result emitted)]
        (is (:ok emitted))
        (is (not= (get-in output [:frame :frame/scaffold-hash])
                  (get-in output [:frame :frame/closing-hash]))
            "different file bytes must produce different hashes")
        (is (not (some #{:f1-scaffold-identical-frame}
                       (frame-validation-failures output)))
            "F1 must NOT fire on a frame whose closing differs from its scaffold"))
      (finally
        (Files/deleteIfExists scaffold)
        (Files/deleteIfExists closing)))))

;; The store snapshot IS the round's measured transfer channel, and the
;; hyperedges endpoint has no cursor -- so a full page is indistinguishable from
;; a complete read. It must refuse rather than silently under-report. (The
;; previous limit, 10000, was above the substrate's maximum of 5000 and returned
;; a hard 400, so :snapshot-store failed on every production call; the tests did
;; not catch it because they inject a snapshotter.)
(deftest a-full-page-store-snapshot-refuses-rather-than-truncating
  (let [full-page (repeat 5000 {:hx/props {:roles {:entry "memory/x"}}})]
    (with-redefs [substrate/hyperedges-by-type
                  (fn [_ _] full-page)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"truncated"
                            (#'problem/snapshot-reviewed-memories))))
    (with-redefs [substrate/hyperedges-by-type
                  (fn [_ _] (repeat 4999 {:hx/props {:roles {:entry "memory/x"}}}))]
      (is (= ["memory/x"] (#'problem/snapshot-reviewed-memories))
          "a short page is a complete read and must succeed"))))

;; F8's OTHER DIRECTION -- the test that was missing, and whose absence hid a
;; one-character bug for the whole life of :emit-frame. The tool wrote
;; :cprobe/passed; derive-trace reads :cprobe/passed? (cycle_harness.clj:126).
;; So :containment-probe-passed? was ALWAYS nil and F8 fired on EVERY claimed
;; containment, however well witnessed. The firing test could not see it, because
;; a gate that always fires still fires.
(deftest a-witnessed-containment-does-not-fire-f8
  (let [scaffold (Files/createTempFile "problem-frame-scaffold-witnessed-" ".lean"
                                       (make-array FileAttribute 0))
        closing (Files/createTempFile "problem-frame-closing-witnessed-" ".lean"
                                      (make-array FileAttribute 0))
        witness (Files/createTempFile "problem-frame-witness-ok-" ".edn"
                                      (make-array FileAttribute 0))]
    (try
      (spit (.toFile scaffold) "theorem t : True := by sorry\n")
      (spit (.toFile closing) "theorem t : True := by trivial\n")
      (spit (.toFile witness) "{:containment :observed}\n")
      (timestamp-frame-snapshots! scaffold closing)
      (let [{:keys [emitted]} (machine-frame-result scaffold closing witness true)
            output (:result emitted)]
        (is (:ok emitted))
        (is (true? (get-in output [:containment-probe :cprobe/recorded?])))
        (is (true? (get-in output [:containment-probe :cprobe/passed?]))
            "the key must be :cprobe/passed? -- what derive-trace actually reads")
        (is (not (some #{:f8-unwitnessed-containment}
                       (frame-validation-failures output)))
            "F8 must NOT fire when the containment claim IS witnessed"))
      (finally
        (Files/deleteIfExists scaffold)
        (Files/deleteIfExists closing)
        (Files/deleteIfExists witness)))))

;; One spelling per fact. :promote-artifact emitted both :promo/importable and
;; :promo/importable?; derive-trace reads the `?` spelling (cycle_harness.clj:136).
;; A dual emission is a hedge, and a hedge is a second route to a field the
;; machine owns.
(deftest promotion-emits-one-spelling-of-importable
  (let [{:keys [promotions]} (adjudication-machine-results)
        entity (first promotions)]
    (is (true? (:promo/importable? entity)))
    (is (not (contains? entity :promo/importable))
        "the non-predicate spelling must not be emitted at all")))

(deftest ground-control-dispatch-defaults-agency-base-without-changing-role-channel
  (let [calls (atom [])
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend)
                 (fn [opts _]
                   (swap! calls conj opts)
                   {:evidence {}}))]
    (tools/execute-tool backend :dispatch-solver
                        [{:memory-channel :none} "packet"])
    (tools/execute-tool backend :dispatch-solver
                        [{:base "http://elsewhere:1"
                          :memory-channel :pull-only}
                         "packet"])
    (tools/execute-tool backend :dispatch-student-fresh
                        [{:memory-channel :push+pull} "packet"])
    (is (= dispatch-with-recall/default-agency-base
           (:base (first @calls))))
    (is (= "http://elsewhere:1" (:base (second @calls))))
    (is (= [:push+pull :push+pull :pull-only]
           (mapv :memory-channel @calls)))))

(deftest typed-guidance-obeys-the-optional-registration-regime
  (let [calls (atom [])
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend)
                 (fn [opts _]
                   (swap! calls conj opts)
                   {:job-id "guide-job"})
                 (fn [_ _] {:id "park"})
                 (atom nil))
        measured {:solver-seat "codex-4" :cycle/id "cycle-1"}
        untyped (tools/execute-tool backend :guide-solver
                                    [{} "packet" measured])
        permitted (tools/execute-tool
                   backend :guide-solver
                   [{:bell-type :suggest} "packet"
                    (assoc measured :guidance-regime #{:suggest :challenge})])
        refused (tools/execute-tool
                 backend :guide-solver
                 [{:bell-type :assert} "packet"
                  (assoc measured :guidance-regime #{:suggest :challenge})])
        legacy (tools/execute-tool backend :guide-solver
                                   [{:bell-type :define} "packet" measured])]
    (is (false? (:ok untyped)))
    (is (= :suggest (get-in permitted [:result :ground-control/type])))
    (is (= :guidance-type-off-regime (get-in refused [:error :failure])))
    (is (= :define (get-in legacy [:result :ground-control/type])))
    (is (= [:suggest :define] (mapv :bell-type @calls)))
    (is (= ["codex-4" "codex-4"] (mapv :to @calls)))))

(deftest student-dispatch-packet-names-its-provisioned-checkout
  (let [dispatches (atom [])
        frame {:checkout "/frames/batch/student-1"
               :branch "exp/batch-student-1"
               :base-revision "0123456789abcdef"
               :frame/id "batch-student-1"}
        inner (reify tools/ToolBackend
                (execute-tool [_ tool-id args]
                  (when (= :dispatch-student-fresh tool-id)
                    (swap! dispatches conj args))
                  {:ok true :result {:job-id "job-1"}}))
        backend (problem/make-checkout-provisioning-backend
                 inner (constantly frame) (constantly nil))
        _ (tools/execute-tool backend :assign-checkouts [checkout-options])
        original "ORIGINAL STUDENT PACKET\nDo the proof."
        result (tools/execute-tool backend :dispatch-student-fresh
                                   [{:to "zai-1"} original])
        [opts packet] (take 2 (first @dispatches))
        failed-dispatches (atom [])
        provision-count (atom 0)
        failing (problem/make-checkout-provisioning-backend
                 (reify tools/ToolBackend
                   (execute-tool [_ _ args]
                     (swap! failed-dispatches conj args)
                     {:ok true :result {}}))
                 (fn [_]
                   (if (= 1 (swap! provision-count inc))
                     frame
                     (throw (ex-info "provision failed" {}))))
                 (constantly nil))
        _ (tools/execute-tool failing :assign-checkouts [checkout-options])
        failed (tools/execute-tool failing :dispatch-student-fresh
                                   [{} original])]
    (is (:ok result))
    (is (str/starts-with? packet original))
    (is (str/includes? packet (:checkout frame)))
    (is (str/includes? packet (:branch frame)))
    (is (str/includes? packet (:base-revision frame)))
    (is (= (:checkout frame) (:environment-checkout opts)))
    (is (= (:base-revision frame) (:environment-revision opts)))
    (is (false? (:ok failed)))
    (is (empty? @failed-dispatches)
        "a failed provision must not reach the inner dispatch")))

(deftest successful-dispatches-atomically-register-conductor-parks
  (let [parks (atom [])
        now (System/currentTimeMillis)
        context (atom {:conductor {:agent "claude-7"
                                   :session "session-7"
                                   :surface "emacs-repl"
                                   :park-deadline-s 2700
                                   :park-base "http://agency:7070"}})
        dispatch-count (atom 0)
        dispatch-fn (fn [_ _]
                      (let [n (swap! dispatch-count inc)]
                        {:job-id (str "job-" n)
                         :evidence {:body {}}}))
        park-post (fn [base payload]
                    (swap! parks conj [base payload])
                    {:id (str "park-" (count @parks))})
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) dispatch-fn park-post context)
        continuation {:conductor/cycle-id "cycle-1" :conductor/version 12}
        solver (tools/execute-tool
                backend :dispatch-solver [continuation "solver" {}])
        student (tools/execute-tool
                 backend :dispatch-student-fresh [{} "student" {}])
        guide (tools/execute-tool
               backend :guide-solver
               [{:park-payload "continue guide" :bell-type :suggest} "guide"
                {:solver-seat "codex-4" :cycle/id "cycle-1"}])]
    (is (= 3 (count @parks)) "exactly one park per successful dispatch")
    (is (= ["job-1"] (get-in @parks [0 1 :awaiting])))
    (is (= "claude-7" (get-in @parks [0 1 :agent])))
    (is (= "session-7" (get-in @parks [0 1 :session])))
    (is (= "emacs-repl" (get-in @parks [0 1 :surface])))
    (is (= "cycle-1" (get-in @parks [0 1 :cycle-id])))
    (is (= 12 (get-in @parks [0 1 :version])))
    (is (= "http://agency:7070" (get-in @parks [0 0])))
    (is (> (get-in @parks [0 1 :deadline-ms]) now)
        "the engine sends an absolute epoch deadline")
    (is (= "continue guide" (get-in @parks [2 1 :payload])))
    (is (= "park-1" (get-in solver [:result :park/id])))
    (is (= "park-2" (get-in student [:result :park/id])))
    (is (= "park-3" (get-in guide [:result :park/id])))))

(deftest park-failure-is-loud-without-rewriting-dispatch-success
  (let [context (atom {:conductor {:agent "claude-7"
                                   :session "session-7"
                                   :surface "emacs-repl"
                                   :park-deadline-s 10
                                   :park-base "http://agency:7070"}})
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend)
                 (fn [_ _] {:job-id "job-ok" :evidence {:body {}}})
                 (fn [_ _] (throw (ex-info "park unavailable" {})))
                 context)
        result (tools/execute-tool backend :dispatch-solver [{} "packet" {}])]
    (is (:ok result))
    (is (= "park unavailable" (get-in result [:result :park/error :message])))))

(deftest absent-conductor-does-not-attempt-parking
  (let [park-calls (atom 0)
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend)
                 (fn [_ _] {:job-id "job-ok" :evidence {:body {}}})
                 (fn [_ _] (swap! park-calls inc) {:id "must-not-exist"})
                 (atom {:session-id "s"}))
        result (tools/execute-tool backend :dispatch-solver [{} "packet" {}])]
    (is (:ok result))
    (is (zero? @park-calls))
    (is (not (contains? (:result result) :park/id)))))

(deftest write-substrate-uses-the-production-writer-contract
  (let [payload {:name "cycle-memory"
                 :kind :feedback
                 :hook "when rehearsing"
                 :body {:finding "the write path is live"}
                 :author "spoofed-caller"
                 :subjects [{:ref/type :problem :ref/id "p"}]}
        calls (atom [])
        context (atom {:session-id "session-1"
                       :problem-id "p"
                       :author "f9-guide"
                       :harness-repo "/harness"})
        writer (fn [ctx actual]
                 (swap! calls conj [ctx actual])
                 {:ok true :id "e-memory-1"})
        inner (tools/make-mock-backend {:sentinel {:unchanged true}})
        backend (problem/make-problem-cycle-backend
                 inner (constantly {}) context (constantly [])
                 (constantly 0) writer)
        unopened (tools/execute-tool backend :write-substrate [payload])
        _ (tools/execute-tool backend :begin-problem-cycle [])
        written (tools/execute-tool backend :write-substrate [payload])
        [ctx actual] (first @calls)
        delegated (tools/execute-tool backend :sentinel [])
        failed-backend
        (problem/make-problem-cycle-backend
         inner (constantly {}) context (constantly []) (constantly 0)
         (fn [_ _] {:ok false
                    :error {:error/code :rejected
                            :error/message "store refused"}}))
        _ (tools/execute-tool failed-backend :begin-problem-cycle [])
        failed (tools/execute-tool failed-backend :write-substrate [payload])]
    (is (false? (:ok unopened)))
    (is (= 1 (count @calls)) "no-open-cycle refusal must not invoke the writer")
    (is (identical? payload actual) "the payload object passes through unchanged")
    (is (= "session-1" (:session-id ctx)))
    (is (= "f9-guide" (:agent-id ctx)))
    (is (= "problem-peripheral" (:via ctx)))
    (is (not= (:author payload) (:agent-id ctx))
        "caller payload cannot spoof the machine-owned cycle author")
    (is (= :mathematics (:domain ctx)))
    (is (some? (:evidence-store ctx)))
    (is (= "e-memory-1" (get-in written [:result :memory-id])))
    (is (string? (get-in written [:result :cycle])))
    (is (false? (:ok failed)))
    (is (re-find #"store refused" (:error failed)))
    (is (= {:unchanged true} (:result delegated))
        "all other tools still delegate to the existing backend")))

(defn- begun-problem-cycle-backend []
  (let [backend (problem/make-problem-cycle-backend
                 (tools/make-mock-backend)
                 (constantly {})
                 (atom {:session-id "session-disposition"
                        :problem-id "p" :harness-repo "/harness"})
                 (constantly []) (constantly 7))]
    (tools/execute-tool backend :begin-problem-cycle [])
    backend))

(deftest write-disposition-passes-measurement-fields-through
  (let [backend (begun-problem-cycle-backend)
        result (tools/execute-tool
                backend :write-disposition
                [{:outcome :closed
                  :disp/residual-sorries 0
                  :disp/axiom-clean? true}
                 {:cycle/step-index 9}])
        entity (:result result)
        values (#'problem/measurement-values {:disposition entity})]
    (is (:ok result))
    (is (= 0 (:disp/residual-sorries entity)))
    (is (true? (:disp/axiom-clean? entity)))
    (is (= 0 (get values "residual executable sorries")))
    (is (true? (get values "axiom cleanliness")))))

(deftest promote-attaches-reviews-and-makes-only-that-memory-recallable
  (let [pattern-id "math-formalization/transport-across-an-instance-diamond"
        problem-id "t00A05"
        memory-entry
        (fn [id]
          {:evidence/id id
           :evidence/subject {:ref/type :problem :ref/id problem-id}
           :evidence/type :memory
           :evidence/claim-type :assert
           :evidence/author "depositor"
           :evidence/session-id "session-disposition"
           :evidence/at "2026-08-16T00:00:00Z"
           :evidence/body {:kind :content :name id :hook "hook"
                           :body "transport an instance diamond"}})
        edge
        (fn [id]
          {:hx/id (str "hx-" id)
           :hx/type :memory/assert
           :hx/endpoints [id problem-id]
           :hx/props {:domain :mathematics :state :current
                      :roles {:entry id :subjects [problem-id]
                              :distills [] :session "session-disposition"}}})
        promoted-id "e-promoted"
        sibling-id "e-unpromoted"
        entries (atom {promoted-id (memory-entry promoted-id)
                       sibling-id (memory-entry sibling-id)})
        edges (atom {promoted-id (edge promoted-id)
                     sibling-id (edge sibling-id)})
        fetch-hyperedges
        (fn [endpoint _]
          (->> (vals @edges)
               (filter #(some #{endpoint} (:hx/endpoints %)))
               vec))
        post-hyperedge
        (fn [_ updated]
          (swap! edges assoc (get-in updated [:hx/props :roles :entry]) updated)
          {:ok true :hyperedge updated})
        append-evidence
        (fn [_ entry]
          (swap! entries assoc (:evidence/id entry) entry)
          {:ok true :entry entry})
        promote
        (fn [backend request]
          (tools/execute-tool backend :promote-artifact
                              [request {:cycle/step-index 12}]))]
    (with-redefs [substrate/hyperedges-by-end fetch-hyperedges
                  estore/get-entry* (fn [_ id] (get @entries id))
                  memory-write/post-hyperedge! post-hyperedge
                  boundary/append! append-evidence]
      (let [result (promote (begun-problem-cycle-backend)
                            {:memory-id promoted-id
                             :pattern-id pattern-id
                             :reviewer "reviewer"})
            projection
            (memory-recall/recall-by-endpoints
             {:domain :mathematics} [problem-id]
             {:limit 10
              :fetch-components
              (fn [endpoints _]
                {:groups
                 (mapv (fn [endpoint]
                         {:endpoint endpoint
                          :components
                          (mapv (fn [edge]
                                  {:edge edge
                                   :entry (get @entries
                                               (get-in edge
                                                       [:hx/props :roles :entry]))})
                                (fetch-hyperedges endpoint {}))})
                       endpoints)})})
            recalled (mapv :memory/id (get-in projection [:recalls 0 :memories]))
            self-review (promote (begun-problem-cycle-backend)
                                 {:memory-id sibling-id
                                  :pattern-id pattern-id
                                  :reviewer "depositor"})
            blank-pattern (promote (begun-problem-cycle-backend)
                                   {:memory-id sibling-id
                                    :pattern-id " "
                                    :reviewer "reviewer"})
            missing-pattern (promote (begun-problem-cycle-backend)
                                     {:memory-id sibling-id
                                      :reviewer "reviewer"})
            unshaped-pattern (promote (begun-problem-cycle-backend)
                                      {:memory-id sibling-id
                                       :pattern-id "no-library-segment"
                                       :reviewer "reviewer"})
            review-id (get-in result [:result :promo/review-evidence-id])]
        (is (:ok result) result)
        (is (= :reviewed
               (get-in @edges [promoted-id :hx/props :attachment-status])))
        (is (= "reviewer" (get-in @entries [review-id :evidence/author])))
        (is (not= (get-in @entries [promoted-id :evidence/author])
                  (get-in @entries [review-id :evidence/author])))
        (is (= [promoted-id] recalled))
        (is (not (some #{sibling-id} recalled)))
        (is (= :promotion-reviewer-is-depositor
               (get-in self-review [:error :failure])))
        (is (= :promotion-pattern-id-missing
               (get-in blank-pattern [:error :failure])))
        (is (= :promotion-pattern-id-missing
               (get-in missing-pattern [:error :failure])))
        (is (= :promotion-pattern-id-not-library-shaped
               (get-in unshaped-pattern [:error :failure]))
            "a future library like math-informal-AT/... must be accepted, so
             the check is shape-only and taxonomy stays with the adjudicator")
        (is (nil? (get-in @edges [sibling-id :hx/props :attachment-status])))))))

(deftest write-disposition-accepts-measurement-field-aliases
  (let [result (tools/execute-tool
                (begun-problem-cycle-backend) :write-disposition
                [{:outcome :closed :residual-sorries 2 :axiom-clean? false}
                 {:cycle/step-index 10}])]
    (is (:ok result))
    (is (= 2 (get-in result [:result :disp/residual-sorries])))
    (is (false? (get-in result [:result :disp/axiom-clean?])))))

(deftest promotion-threads-existing-independent-review
  (let [captured (atom nil)
        request {:memory-id "e-guide-memory"
                 :pattern-ids ["math-strategy/exact-pattern"]
                 :verdict :approve
                 :review-evidence-id "e-scribe-review"}]
    (with-redefs [memory-lifecycle/promote-memory-attachment!
                  (fn [_ctx promotion]
                    (reset! captured promotion)
                    {:ok true
                     :review-evidence-id (:review-evidence-id promotion)})]
      (let [result (tools/execute-tool
                    (begun-problem-cycle-backend) :promote-artifact
                    [request {:cycle/step-index 13}])]
        (is (:ok result) result)
        (is (= request @captured))
        (is (= "e-scribe-review"
               (get-in result [:result :promo/review-evidence-id])))))))

(deftest write-disposition-omits-absent-measurement-fields
  (let [result (tools/execute-tool
                (begun-problem-cycle-backend) :write-disposition
                [{:outcome :tier-a} {:cycle/step-index 3}])
        entity (:result result)]
    (is (:ok result))
    (is (false? (contains? entity :disp/residual-sorries)))
    (is (false? (contains? entity :disp/axiom-clean?)))))

(deftest write-disposition-rejects-malformed-measurement-fields
  (let [backend (begun-problem-cycle-backend)
        residual (tools/execute-tool
                  backend :write-disposition
                  [{:outcome :closed :disp/residual-sorries -1}
                   {:cycle/step-index 4}])
        axiom (tools/execute-tool
               backend :write-disposition
               [{:outcome :closed :disp/axiom-clean? "yes"}
                {:cycle/step-index 5}])]
    (is (false? (:ok residual)))
    (is (= :disp/residual-sorries (get-in residual [:error :field])))
    (is (false? (:ok axiom)))
    (is (= :disp/axiom-clean? (get-in axiom [:error :field])))))

(deftest write-disposition-keeps-outcome-validation
  (let [result (tools/execute-tool
                (begun-problem-cycle-backend) :write-disposition
                [{:outcome :not-allowed
                  :disp/residual-sorries 0
                  :disp/axiom-clean? true}
                 {:cycle/step-index 6}])]
    (is (false? (:ok result)))
    (is (re-find #"one of" (:error result)))))

(deftest problem-start-requires-evidence-store
  (let [p (problem/make-problem)
        result (runner/start p {:session-id "missing-evidence-store"
                                :problem-id "p"
                                :cycle/mode :store-mode})]
    (is (not (:ok result)))
    (is (= :missing-context (:error/code result)))
    (is (= [:evidence-store] (get-in result [:error/context :missing])))
    (is (re-find #":evidence-store" (:error/message result)))))

(deftest problem-start-and-step-append-to-required-evidence-store
  (let [store (capturing-evidence-store)
        p (problem/make-problem)
        start (runner/start p {:session-id "capturing-evidence"
                               :problem-id "p"
                               :cycle/mode :store-mode
                               :evidence-store store})
        step (runner/step p (:state start)
                          {:tool :list-problems :args []})]
    (is (:ok start))
    (is (:ok step))
    (is (= 2 (count (:order @store))))
    (is (= #{:start :step}
           (set (map #(get-in @store [:entries % :evidence/body :event])
                     (:order @store)))))))

(deftest make-problem-construction-does-not-require-evidence-store
  (is (satisfies? runner/PeripheralRunner (problem/make-problem))))

(deftest student-runner-budget-is-defaulted-pinned-and-receipted
  (let [calls (atom [])
        dispatch-fn (fn [opts _packet]
                      (swap! calls conj opts)
                      {:job-id "student-budget-job" :evidence {:body {}}})
        backend (problem/make-ground-control-backend
                 (tools/make-mock-backend) dispatch-fn)
        pinned-budget {:wall-clock-minutes 45}
        pinned (tools/execute-tool
                backend :dispatch-student-fresh
                [{} "packet" {:cycle/id "cycle-pinned"
                               :cycle/step-index 2
                               :student-runner-budget pinned-budget}])
        defaulted (tools/execute-tool
                   backend :dispatch-student-fresh
                   [{} "packet" {:cycle/id "cycle-default"
                                  :cycle/step-index 3}])]
    (is (= pinned-budget (:student-runner-budget (first @calls))))
    (is (= (* 45 60 1000) (:timeout-ms (first @calls))))
    (is (= {:wall-clock-minutes 60}
           (:student-runner-budget (second @calls))))
    (is (= (* 60 60 1000) (:timeout-ms (second @calls))))
    (is (= pinned-budget
           (get-in pinned [:result :ground-control/student-runner-budget])))
    (is (= {:wall-clock-minutes 60}
           (get-in defaulted
                   [:result :ground-control/student-runner-budget])))))
