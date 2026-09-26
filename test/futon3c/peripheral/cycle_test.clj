(ns futon3c.peripheral.cycle-test
  "Tests for the generic cycle machine.

   Tests the cycle machine in isolation using a minimal test domain config,
   proving that the extraction from proof.clj preserved all behavior."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

;; =============================================================================
;; Minimal test domain config
;; =============================================================================

(def test-phase-order
  [:alpha :beta :gamma :done])

(def test-tool-ops
  {:tool-a :observe
   :tool-b :action
   :cycle-begin :action
   :cycle-advance :action
   :cycle-get :observe
   :read :observe})

(def test-config
  {:domain-id :test-domain
   :phase-order test-phase-order
   :phase-tools {:alpha #{:tool-a :read :cycle-advance :cycle-get}
                 :beta #{:tool-b :read :cycle-advance :cycle-get}
                 :gamma #{:tool-a :read :cycle-advance :cycle-get}
                 :done #{}}
   :setup-tools #{:cycle-begin :cycle-get :read :tool-a}
   :tool-ops test-tool-ops
   :required-outputs {:alpha #{:observed}
                      :beta #{:acted}
                      :gamma #{:reviewed}}
   :cycle-begin-tool :cycle-begin
   :cycle-advance-tool :cycle-advance
   :state-init-fn (fn [ctx] {:test-field (:test-field ctx)})
   :fruit-fn (fn [state] {:cycles (:cycles-completed state)
                           :steps (count (:steps state))})
   :exit-context-fn (fn [state] {:session-id (:session-id state)})
   :phase-tags-fn (fn [phase _tool] (when (= phase :alpha) [:tag/alpha]))})

(def test-spec
  "A minimal peripheral spec for the test domain."
  {:peripheral/id :test-domain
   :peripheral/tools #{:tool-a :tool-b :cycle-begin :cycle-advance :cycle-get :read}
   :peripheral/scope :full-codebase
   :peripheral/entry #{:user-request}
   :peripheral/exit #{:user-request}
   :peripheral/context {}})

(defn- make-test-peripheral
  "Create a test cycle peripheral with the test domain config and spec."
  [backend]
  (cycle/make-cycle-peripheral test-config test-spec backend))

(defn- make-test-mock
  ([]
   (make-test-mock {}))
  ([extra]
   (tools/make-mock-backend
    (merge
     {:cycle-begin {:cycle/id "C1"
                    :cycle/blocker-id "B1"
                    :cycle/phase :alpha
                    :cycle/phases-completed []
                    :cycle/phase-data {}
                    :cycle/started-at "t0"
                    :cycle/updated-at "t0"}
      :cycle-advance {:cycle/id "C1"
                      :cycle/blocker-id "B1"
                      :cycle/phase :beta
                      :cycle/phases-completed [:alpha]
                      :cycle/phase-data {:alpha {:observed true}}
                      :cycle/started-at "t0"
                      :cycle/updated-at "t1"}
      :tool-a "observed"
      :tool-b "acted"
      :read "file contents"}
     extra))))

;; =============================================================================
;; Config validation
;; =============================================================================

(deftest valid-domain-config-accepts-well-formed
  (is (cycle/valid-domain-config? test-config)))

(deftest valid-domain-config-rejects-missing-keys
  (is (not (cycle/valid-domain-config? (dissoc test-config :domain-id))))
  (is (not (cycle/valid-domain-config? (dissoc test-config :phase-order))))
  (is (not (cycle/valid-domain-config? (dissoc test-config :fruit-fn))))
  (is (not (cycle/valid-domain-config?
            (assoc test-config :derived-tools {:bad :not-a-function}))))
  (is (cycle/valid-domain-config?
       (assoc test-config :derived-tools {:derived (fn [_ _] :ok)}))))

;; =============================================================================
;; Lifecycle — start/stop
;; =============================================================================

(deftest cycle-start-produces-goal-evidence
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-1" :agent-id "test-agent"})]
    (is (:ok start))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))))

(deftest cycle-stop-returns-fruit
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-2" :test-field "hello"})
        stop (runner/stop p (:state start) "done")]
    (is (:ok stop))
    (is (= {:cycles 0 :steps 0} (:fruit stop)))
    (is (= "sess-2" (get-in stop [:context :session-id])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))))

(deftest cycle-start-includes-domain-state
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-3" :test-field "custom"})]
    (is (= "custom" (get-in start [:state :test-field])))))

(deftest cycle-step-refuses-absent-state-before-tool-execution
  (let [backend (make-test-mock)
        p (make-test-peripheral backend)
        result (runner/step p nil {:tool :cycle-begin :args ["M" "B"]})]
    (fix/assert-valid! shapes/SocialError result)
    (is (= :absent-state (:error/code result)))
    (is (nil? (:state result)) "no context-less cycle state is synthesized")
    (is (empty? (tools/recorded-calls backend))
        "the begin tool never reaches its backend")
    (is (nil? (get-in result [:state :current-cycle-id])))
    (is (empty? (or (get-in result [:state :steps]) [])))))

;; =============================================================================
;; Phase gating
;; =============================================================================

(deftest setup-allows-configured-tools
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-4"})
        step (runner/step p (:state start) {:tool :read :args ["f.txt"]})]
    (is (:ok step))))

(deftest setup-rejects-non-setup-tools
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-5"})
        step (runner/step p (:state start) {:tool :tool-b :args []})]
    (fix/assert-valid! shapes/SocialError step)
    (is (= :phase-tool-not-allowed (:error/code step)))))

(deftest phase-rejects-wrong-tools
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-6"})
        ;; Begin cycle — enters :alpha
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "B1"]})
        ;; Try tool-b in :alpha — should be rejected
        step (runner/step p (:state cycle-step) {:tool :tool-b :args []})]
    (is (:ok cycle-step))
    (is (= :alpha (get-in cycle-step [:state :current-phase])))
    (fix/assert-valid! shapes/SocialError step)
    (is (= :phase-tool-not-allowed (:error/code step)))))

(deftest phase-allows-correct-tools
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-7"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "B1"]})
        step (runner/step p (:state cycle-step) {:tool :tool-a :args []})]
    (is (:ok step))))

(deftest step-records-retain-their-own-evidence-chain-ids
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "step-evidence-chain"})
        first-step (runner/step p (:state start) {:tool :read :args ["a"]})
        second-step (runner/step p (:state first-step)
                                 {:tool :tool-a :args []})
        first-id (get-in first-step [:evidence :evidence/id])
        second-id (get-in second-step [:evidence :evidence/id])
        records (get-in second-step [:state :steps])]
    (is (= [first-id second-id] (mapv :evidence/id records))
        "each record retains its own id, not the preceding evidence id")
    (is (not= first-id second-id))
    (is (= second-id (get-in second-step [:state :last-evidence-id]))
        ":last-evidence-id remains the newest emitted evidence")
    (is (= (get-in start [:evidence :evidence/id])
           (get-in first-step [:evidence :evidence/in-reply-to])))
    (is (= first-id
           (get-in second-step [:evidence :evidence/in-reply-to]))
        "step n+1 links to step n's retained evidence id")))

;; =============================================================================
;; Phase transitions
;; =============================================================================

(deftest cycle-advance-updates-phase
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-8"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "B1"]})
        advance (runner/step p (:state cycle-step) {:tool :cycle-advance :args ["M-test" "C1" {:observed true}]})]
    (is (:ok advance))
    (is (= :beta (get-in advance [:state :current-phase])))))

(deftest domain-without-output-stamp-preserves-the-advance-payload
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "no-output-stamp"})
        begun (runner/step p (:state start)
                           {:tool :cycle-begin :args ["M" "B"]})
        payload {:observed :caller-value}
        advanced (runner/step p (:state begun)
                              {:tool :cycle-advance :args ["M" "C1" payload]})]
    (is (:ok advanced))
    (is (= payload (get-in advanced [:state :cycle/outputs])))))

(deftest required-output-enforcement-defaults-off
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "default-off"})
        begun (runner/step p (:state start)
                           {:tool :cycle-begin :args ["M" "B"]})
        advanced (runner/step p (:state begun)
                              {:tool :cycle-advance :args ["M" "C1" {}]})]
    (is (:ok advanced))))

(deftest output-invariant-waits-for-all-required-operands
  (let [called (atom false)
        config (assoc test-config :output-invariants
                      [{:id :needs-never-produced
                        :requires #{:left :right}
                        :check (fn [_] (reset! called true) {:failure :boom})}])
        p (cycle/make-cycle-peripheral config test-spec (make-test-mock))
        start (runner/start p {:session-id "invariant-not-ready"})
        begun (runner/step p (:state start)
                           {:tool :cycle-begin :args ["M" "B"]})
        advanced (runner/step p (:state begun)
                              {:tool :cycle-advance
                               :args ["M" "C1" {:observed true}]})]
    (is (:ok advanced))
    (is (false? @called))))

(deftest cycle-completion-clears-phase
  (let [backend (tools/make-mock-backend
                 {:cycle-begin {:cycle/id "C1"
                                :cycle/phase :alpha
                                :cycle/phases-completed []
                                :cycle/phase-data {}
                                :cycle/started-at "t" :cycle/updated-at "t"}
                  :cycle-advance (fn [_tool _args]
                                   {:ok true
                                    :result {:cycle/id "C1"
                                             :cycle/phase :done
                                             :cycle/phases-completed [:alpha :beta :gamma]
                                             :cycle/phase-data {}
                                             :cycle/started-at "t"
                                             :cycle/updated-at "t"}})
                  :tool-a "ok" :read "ok"})
        p (cycle/make-cycle-peripheral test-config test-spec backend)
        start (runner/start p {:session-id "sess-9"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "B1"]})
        advance (runner/step p (:state cycle-step) {:tool :cycle-advance :args ["M-test" "C1" {:reviewed true}]})]
    (is (:ok advance))
    (is (nil? (get-in advance [:state :current-phase])))
    (is (= 1 (get-in advance [:state :cycles-completed])))))

;; =============================================================================
;; Evidence enrichment
;; =============================================================================

(deftest step-evidence-includes-operation-kind
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-10"})
        step (runner/step p (:state start) {:tool :tool-a :args []})]
    (is (:ok step))
    (is (= :observe (get-in step [:evidence :evidence/body :test-domain/operation-kind])))))

(deftest step-evidence-includes-phase-tags
  (let [p (make-test-peripheral (make-test-mock))
        start (runner/start p {:session-id "sess-11"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "B1"]})
        ;; In :alpha phase, phase-tags-fn returns [:tag/alpha]
        step (runner/step p (:state cycle-step) {:tool :tool-a :args []})]
    (is (:ok step))
    (is (some #{:tag/alpha} (get-in step [:evidence :evidence/tags])))))

;; =============================================================================
;; Autoconf
;; =============================================================================

(deftest autoconf-fn-is-called-on-start
  (let [autoconf-called (atom false)
        config (assoc test-config
                      :autoconf-fn (fn [_ctx cfg]
                                     (reset! autoconf-called true)
                                     cfg))
        p (cycle/make-cycle-peripheral config test-spec (make-test-mock))
        start (runner/start p {:session-id "sess-12"})]
    (is (:ok start))
    (is @autoconf-called)))

(deftest autoconf-changes-persist-into-step
  (testing "autoconf removes a setup tool → that tool is rejected in step"
    (let [config (assoc test-config
                        :autoconf-fn (fn [_ctx cfg]
                                       ;; Remove :tool-a from setup-tools
                                       (update cfg :setup-tools disj :tool-a)))
          backend (tools/make-mock-backend {:tool-a "should-not-run"})
          p (cycle/make-cycle-peripheral config test-spec backend)
          start (runner/start p {:session-id "sess-autoconf"})
          step (runner/step p (:state start) {:tool :tool-a :args []})]
      ;; tool-a was removed by autoconf, so it should be rejected
      (fix/assert-valid! shapes/SocialError step)
      (is (= :phase-tool-not-allowed (:error/code step))))))

;; =============================================================================
;; Unclassified tool rejection
;; =============================================================================

(deftest unclassified-tool-is-rejected
  (testing "tool without observe/action classification is rejected"
    (let [config (assoc test-config
                        :setup-tools #{:cycle-begin :mystery-tool}
                        :tool-ops (dissoc test-tool-ops :tool-a))
          ;; mock must return something for mystery-tool
          spec (assoc test-spec :peripheral/tools #{:cycle-begin :mystery-tool})
          backend (tools/make-mock-backend {:mystery-tool "x" :cycle-begin {:cycle/id "C1"}})
          p (cycle/make-cycle-peripheral config spec backend)
          start (runner/start p {:session-id "sess-13"})
          step (runner/step p (:state start) {:tool :mystery-tool :args []})]
      (fix/assert-valid! shapes/SocialError step)
      (is (= :unclassified-tool (:error/code step))))))

;; =============================================================================
;; Opt-in engine-owned state I/O
;; =============================================================================

(def state-io-config
  (-> test-config
      (assoc :state-io-tools {:save :state-save :load :state-load}
             :always-available-tools #{:state-save :state-load})
      (update :tool-ops assoc :state-save :action :state-load :action)))

(def state-io-spec
  (update test-spec :peripheral/tools into #{:state-save :state-load}))

(deftest domain-without-state-io-keeps-backend-argument-contract
  (let [backend (make-test-mock)
        p (make-test-peripheral backend)
        start (runner/start p {:session-id "no-state-io"})
        step (runner/step p (:state start) {:tool :tool-a :args [:caller-arg]})]
    (is (:ok step))
    (is (= [:caller-arg]
           (:args (last (tools/recorded-calls backend)))))))

;; =============================================================================
;; Engine-derived tools
;; =============================================================================

(def derived-spec
  (update test-spec :peripheral/tools conj :derive-state))

(defn- derived-config [derive]
  (-> test-config
      (update :setup-tools conj :derive-state)
      (update :tool-ops assoc :derive-state :observe)
      (assoc :derived-tools {:derive-state derive})))

(deftest derived-tool-uses-engine-state-and-never-calls-backend
  (let [backend (make-test-mock {:derive-state :backend-must-not-run})
        config (derived-config
                (fn [state args]
                  {:from-state (:test-field state) :args args}))
        p (cycle/make-cycle-peripheral config derived-spec backend)
        start (runner/start p {:session-id "derived" :test-field :authoritative})
        result (runner/step p (:state start)
                            {:tool :derive-state :args [:caller]})]
    (is (:ok result))
    (is (= {:from-state :authoritative :args [:caller]} (:result result)))
    (is (empty? (tools/recorded-calls backend)))))

(deftest derived-tool-still-obeys-phase-gating
  (let [called (atom false)
        backend (make-test-mock)
        config (derived-config (fn [_ _] (reset! called true)))
        p (cycle/make-cycle-peripheral config derived-spec backend)
        start (runner/start p {:session-id "derived-gate"})
        state (assoc (:state start) :current-phase :alpha)
        result (runner/step p state {:tool :derive-state :args []})]
    (is (= :phase-tool-not-allowed (:error/code result)))
    (is (false? @called))
    (is (empty? (tools/recorded-calls backend)))))

(deftest derived-tool-still-obeys-the-peripheral-spec
  (let [called (atom false)
        backend (make-test-mock)
        config (derived-config (fn [_ _] (reset! called true)))
        ;; test-spec deliberately does not list :derive-state.
        p (cycle/make-cycle-peripheral config test-spec backend)
        start (runner/start p {:session-id "derived-spec"})
        result (runner/step p (:state start)
                            {:tool :derive-state :args []})]
    (is (= :tool-not-allowed (:error/code result)))
    (is (false? @called))
    (is (empty? (tools/recorded-calls backend)))))

(deftest throwing-derived-tool-is-a-structured-failure
  (let [backend (make-test-mock)
        config (derived-config
                (fn [_ _] (throw (ex-info "cannot derive" {}))))
        p (cycle/make-cycle-peripheral config derived-spec backend)
        start (runner/start p {:session-id "derived-throw"})
        result (runner/step p (:state start)
                            {:tool :derive-state :args []})]
    (is (= :tool-execution-failed (:error/code result)))
    (is (re-find #"cannot derive"
                 (str (get-in result [:error/context :result :error]))))
    (is (empty? (tools/recorded-calls backend)))))

(deftest state-save-receives-authoritative-engine-state
  (let [seen (atom nil)
        backend (make-test-mock
                 {:state-save
                  (fn [_ args]
                    (reset! seen args)
                    {:ok true :result {:saved? true}})})
        p (cycle/make-cycle-peripheral state-io-config state-io-spec backend)
        start (runner/start p {:session-id "save-state" :test-field :engine})
        fake {:session-id "save-state" :test-field :caller-fake}
        saved (runner/step p (:state start)
                           {:tool :state-save :args [fake :v1]})]
    (is (:ok saved))
    (is (= :engine (:test-field (first @seen))))
    (is (not= fake (first @seen)))
    (is (= [fake :v1] (vec (rest @seen))))))

(deftest state-save-excludes-runtime-values-and-round-trips-as-edn
  (let [seen (atom nil)
        nested-runtime {:callback (fn [] :not-edn)}
        config (assoc state-io-config :state-runtime-keys
                      #{:runtime :cycle-config :evidence-store})
        backend (make-test-mock
                 {:state-save
                  (fn [_ args]
                    (reset! seen (first args))
                    {:ok true :result {:saved? true}})})
        p (cycle/make-cycle-peripheral config state-io-spec backend)
        start (runner/start p {:session-id "save-edn"})
        state (assoc (:state start) :runtime nested-runtime :persisted :yes)
        saved (runner/step p state {:tool :state-save :args []})
        encoded (pr-str @seen)]
    (is (:ok saved))
    (is (not (contains? @seen :runtime)))
    (is (= @seen (edn/read-string encoded)))
    (is (= :yes (:persisted @seen)))))

(deftest state-load-reattaches-current-runtime-before-validation
  (let [validated (atom nil)
        current-runtime {:sink :current}
        stale-runtime {:sink :loaded}
        loaded (atom nil)
        config (assoc state-io-config
                      :state-runtime-keys
                      #{:runtime :cycle-config :evidence-store}
                      :state-validate-fn
                      (fn [_ candidate]
                        (reset! validated (:runtime candidate))
                        nil))
        backend (make-test-mock
                 {:state-load (fn [_ _] {:ok true :result @loaded})})
        p (cycle/make-cycle-peripheral config state-io-spec backend)
        start (runner/start p {:session-id "runtime-load"})
        current (assoc (:state start) :runtime current-runtime)
        candidate (assoc (:state start) :runtime stale-runtime :loaded? true)
        _ (reset! loaded candidate)
        result (runner/step p current {:tool :state-load :args [1]})]
    (is (:ok result))
    (is (= current-runtime @validated)
        "domain validation sees the state that will actually be installed")
    (is (= current-runtime (get-in result [:state :runtime])))
    (is (true? (get-in result [:state :loaded?])))))

(deftest valid-state-load-replaces-state-and-records-branch-marker
  (let [loaded (atom nil)
        backend (make-test-mock
                 {:state-load (fn [_ _] {:ok true :result @loaded})})
        p (cycle/make-cycle-peripheral state-io-config state-io-spec backend)
        start (runner/start p {:session-id "load-state" :test-field :current})
        begun (runner/step p (:state start)
                           {:tool :cycle-begin :args ["M" "B"]})
        candidate (-> (:state start)
                      (assoc :current-phase :beta
                             :test-field :loaded
                             :loaded-only true
                             :steps []))
        _ (reset! loaded candidate)
        result (runner/step p (:state begun)
                            {:tool :state-load :args [3]})
        marker (last (get-in result [:state :branch-markers]))]
    (is (:ok result))
    (is (= :loaded (get-in result [:state :test-field])))
    (is (true? (get-in result [:state :loaded-only])))
    (is (= :beta (get-in result [:state :current-phase])))
    (is (string? (:branch/id marker)))
    (is (= [3] (:branch/load-args marker)))
    (is (= marker (get-in result [:state :steps 0 :branch-marker])))))

(deftest invalid-state-load-leaves-authoritative-state-untouched
  (let [backend (make-test-mock
                 {:state-load {:session-id "different-session"
                               :current-phase :alpha}})
        p (cycle/make-cycle-peripheral state-io-config state-io-spec backend)
        start (runner/start p {:session-id "keep-state" :test-field :original})
        before (:state start)
        bytes-before (pr-str before)
        result (runner/step p before {:tool :state-load :args [99]})]
    (is (= :loaded-state-session-mismatch (:error/code result)))
    (is (nil? (:state result)))
    (is (= bytes-before (pr-str before)))
    (is (= :original (:test-field before)))
    (is (empty? (:steps before)))))

(deftest always-available-tools-do-not-weaken-ordinary-phase-gating
  (let [backend (make-test-mock
                 {:state-save {:saved? true}
                  :state-load {:session-id "always" :current-phase :alpha}})
        p (cycle/make-cycle-peripheral state-io-config state-io-spec backend)
        start (runner/start p {:session-id "always"})
        begun (runner/step p (:state start)
                           {:tool :cycle-begin :args ["M" "B"]})
        saved (runner/step p (:state begun) {:tool :state-save :args []})
        forbidden (runner/step p (:state begun) {:tool :tool-b :args []})]
    (is (:ok saved))
    (is (= :phase-tool-not-allowed (:error/code forbidden)))))

(deftest load-refuses-a-foreign-cycle
  ;; The cycle id is set BY THE ENGINE at cycle-begin, so this test must let the
  ;; engine set it rather than assoc it in. The first version of this test built
  ;; :cycle/id on the state by hand -- a key the engine never writes -- so it
  ;; passed against a guard that could not fire in reality.
  (let [foreign {:session-id "xcycle" :current-cycle-id "CYCLE-B"
                 :steps [] :cycle/outputs {}}
        backend (make-test-mock
                 {:cycle-begin (fn [_ _] {:ok true :result {:cycle/id "CYCLE-A"}})
                  :state-load  (fn [_ _] {:ok true :result foreign})})
        p (cycle/make-cycle-peripheral state-io-config state-io-spec backend)
        start (runner/start p {:session-id "xcycle"})
        begun (runner/step p (:state start) {:tool :cycle-begin :args ["M" "C"]})
        _ (is (= "CYCLE-A" (:current-cycle-id (:state begun)))
              "the engine, not the test, must establish the cycle id")
        r (runner/step p (:state begun) {:tool :state-load :args []})]
    (is (= :loaded-state-cycle-mismatch (:error/code r)))
    (is (nil? (:state r)))))

(deftest identity-keys-cannot-be-declared-runtime-keys
  ;; Validation runs after reattachment, so declaring :session-id or
  ;; :current-cycle-id as runtime would make the engine compare current state
  ;; against itself. Verified before this guard: with :session-id declared, a
  ;; load of a FOREIGN session succeeded and installed its steps and
  ;; :cycle/outputs under our own session id.
  (is (cycle/valid-domain-config?
       (assoc state-io-config :state-runtime-keys #{:cycle-config})))
  (is (not (cycle/valid-domain-config?
            (assoc state-io-config :state-runtime-keys #{:session-id}))))
  (is (not (cycle/valid-domain-config?
            (assoc state-io-config :state-runtime-keys
                   #{:cycle-config :current-cycle-id})))))
