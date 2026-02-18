(ns futon3c.peripheral.cycle-test
  "Tests for the generic cycle machine.

   Tests the cycle machine in isolation using a minimal test domain config,
   proving that the extraction from proof.clj preserved all behavior."
  (:require [clojure.test :refer [deftest is testing]]
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
  (is (not (cycle/valid-domain-config? (dissoc test-config :fruit-fn)))))

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
