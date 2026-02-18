(ns futon3c.social.dispatch-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.peripheral.registry :as preg]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.persist :as persist]
            [futon3c.social.test-fixtures :as fix]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (f)))

(defn- register-test-agent!
  [typed-id]
  (reg/register-agent!
   {:agent-id typed-id
    :type :mock
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil :exit-code 0})
    :capabilities [:edit]}))

(deftest happy-path-dispatch-returns-receipt
  (testing "known target agent + successful invoke yields a valid DispatchReceipt"
    (let [registry (fix/mock-registry)
          to (fix/make-agent-id "claude-1" :continuity)]
      (register-test-agent! to)
      (let [msg (assoc (fix/make-classified-message {:msg/payload "ping"}) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (fix/assert-valid! shapes/DispatchReceipt result)
        (is (= (:msg/id msg) (:receipt/msg-id result)))
        (is (= to (:receipt/to result)))
        (is (true? (:receipt/delivered? result)))))))

(deftest missing-target-returns-social-error
  (testing "missing :msg/to yields SocialError :no-target"
    (let [registry (fix/mock-registry)
          msg (fix/make-classified-message)
          result (dispatch/dispatch msg registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-dispatch (:error/component result)))
      (is (= :no-target (:error/code result))))))

(deftest unknown-agent-in-snapshot-returns-social-error
  (testing "unknown agent in registry snapshot yields :agent-not-found"
    (let [registry (fix/mock-registry)
          to (fix/make-agent-id "ghost" :continuity)
          msg (assoc (fix/make-classified-message) :msg/to to)
          result (dispatch/dispatch msg registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-dispatch (:error/component result)))
      (is (= :agent-not-found (:error/code result))))))

(deftest agent-in-snapshot-but-not-live-returns-invoke-failed
  (testing "agent present in snapshot but not registered live yields :invoke-failed (I3: snapshot authorizes, invocation handles runtime)"
    (let [registry (fix/mock-registry)
          to (fix/make-agent-id "claude-1" :continuity)
          msg (assoc (fix/make-classified-message) :msg/to to)
          result (dispatch/dispatch msg registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-dispatch (:error/component result)))
      (is (= :invoke-failed (:error/code result))))))

(deftest invalid-message-input-returns-social-error
  (testing "invalid ClassifiedMessage yields :invalid-message SocialError"
    (let [registry (fix/mock-registry)
          result (dispatch/dispatch {:msg/id "x"} registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-dispatch (:error/component result)))
      (is (= :invalid-message (:error/code result))))))

(deftest invoke-failure-returns-social-error
  (testing "registry invocation failure yields :invoke-failed SocialError"
    (let [registry (fix/mock-registry)
          to (fix/make-agent-id "claude-1" :continuity)]
      (reg/register-agent!
       {:agent-id to
        :type :mock
        :invoke-fn (fn [_prompt _session-id] (throw (ex-info "boom" {:reason :test})))
        :capabilities []})
      (let [msg (assoc (fix/make-classified-message {:msg/payload {:type "standup"}}) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (fix/assert-valid! shapes/SocialError result)
        (is (= :S-dispatch (:error/component result)))
        (is (= :invoke-failed (:error/code result)))))))

(deftest outputs-are-shape-validated
  (testing "every dispatch return is DispatchReceipt or SocialError"
    (let [registry (fix/mock-registry)
          to (fix/make-agent-id "claude-1" :continuity)]
      (register-test-agent! to)
      (doseq [m [(assoc (fix/make-classified-message {:msg/payload "ping"}) :msg/to to)
                 (assoc (fix/make-classified-message) :msg/to (fix/make-agent-id "ghost" :continuity))
                 (fix/make-classified-message)
                 {:msg/id "x"}]]
        (let [r (dispatch/dispatch m registry)]
          (is (or (shapes/valid? shapes/DispatchReceipt r)
                  (shapes/valid? shapes/SocialError r))
              (str "Unexpected result shape: " r)))))))

;; =============================================================================
;; Route selection tests (Part I: peripheral bridge)
;; =============================================================================

(deftest select-peripheral-claude-defaults-to-explore
  (testing ":claude agent defaults to :explore peripheral"
    (is (= :explore (dispatch/select-peripheral :claude "some prompt")))))

(deftest select-peripheral-codex-defaults-to-edit
  (testing ":codex agent defaults to :edit peripheral"
    (is (= :edit (dispatch/select-peripheral :codex "some prompt")))))

(deftest select-peripheral-tickle-defaults-to-mission-control
  (testing ":tickle agent defaults to :mission-control peripheral"
    (is (= :mission-control (dispatch/select-peripheral :tickle "keep work moving")))))

(deftest select-peripheral-mock-defaults-to-explore
  (testing ":mock agent defaults to :explore peripheral"
    (is (= :explore (dispatch/select-peripheral :mock "some prompt")))))

(deftest select-peripheral-payload-override
  (testing "payload {:peripheral :deploy} overrides default"
    (is (= :deploy (dispatch/select-peripheral :claude {:peripheral :deploy})))))

(deftest select-peripheral-payload-string-override
  (testing "payload {:peripheral \"discipline\"} (JSON-style) overrides default"
    (is (= :discipline (dispatch/select-peripheral :codex {:peripheral "discipline"})))))

(deftest select-peripheral-payload-colon-prefixed-string-override
  (testing "payload {:peripheral \":reflect\"} normalizes and overrides default"
    (is (= :reflect (dispatch/select-peripheral :codex {:peripheral ":reflect"})))))

(deftest select-peripheral-invalid-override-uses-default
  (testing "payload {:peripheral :bogus} falls back to agent-type default"
    (is (= :explore (dispatch/select-peripheral :claude {:peripheral :bogus})))))

(deftest select-route-coordination-is-direct
  (testing ":coordination mode → {:route :direct}"
    (let [msg (fix/make-classified-message)
          agent {:agent/type :claude}]
      (is (= {:route :direct} (dispatch/select-route msg agent))))))

(deftest select-route-action-is-peripheral
  (testing ":action mode → {:route :peripheral ...}"
    (let [msg (fix/make-action-message)
          agent {:agent/type :claude}
          result (dispatch/select-route msg agent)]
      (is (= :peripheral (:route result)))
      (is (contains? result :peripheral-id)))))

(deftest select-route-action-uses-agent-type
  (testing "Claude action → :explore, Codex action → :edit, Tickle action → :mission-control"
    (let [msg (fix/make-action-message)]
      (is (= :explore (:peripheral-id (dispatch/select-route msg {:agent/type :claude}))))
      (is (= :edit (:peripheral-id (dispatch/select-route msg {:agent/type :codex}))))
      (is (= :mission-control (:peripheral-id (dispatch/select-route msg {:agent/type :tickle})))))))

(deftest select-route-action-with-payload-override
  (testing "payload :peripheral key overrides agent-type default"
    (let [msg (fix/make-action-message {:msg/payload {:peripheral :test}})
          agent {:agent/type :claude}
          result (dispatch/select-route msg agent)]
      (is (= :peripheral (:route result)))
      (is (= :test (:peripheral-id result))))))

(deftest existing-dispatch-still-works
  (testing "regression: existing happy-path dispatch unchanged after Part I additions"
    (let [registry (fix/mock-registry)
          to (fix/make-agent-id "claude-1" :continuity)]
      (register-test-agent! to)
      (let [msg (assoc (fix/make-classified-message {:msg/payload "ping"}) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (fix/assert-valid! shapes/DispatchReceipt result)
        (is (true? (:receipt/delivered? result)))))))

;; =============================================================================
;; Peripheral dispatch tests (Part II)
;; =============================================================================

(deftest peripheral-dispatch-happy-path
  (testing "action message with peripheral-config → enriched DispatchReceipt"
    (let [config (fix/make-peripheral-config)
          registry (fix/mock-registry {:peripheral-config config})
          to (fix/make-agent-id "claude-1" :continuity)
          msg (assoc (fix/make-action-message {:msg/payload "implement feature X"})
                     :msg/to to)
          result (dispatch/dispatch msg registry)]
      (is (true? (:receipt/delivered? result))
          (str "Expected delivered receipt, got: " (pr-str result)))
      (is (string? (:receipt/session-id result)))
      (is (some? (:receipt/peripheral-id result)))
      (is (= "peripheral/run-chain" (:receipt/route result))))))

(deftest peripheral-dispatch-receipt-is-shape-valid
  (testing "peripheral dispatch result validates against DispatchReceipt shape"
    (let [config (fix/make-peripheral-config)
          registry (fix/mock-registry {:peripheral-config config})
          to (fix/make-agent-id "claude-1" :continuity)
          msg (assoc (fix/make-action-message) :msg/to to)
          result (dispatch/dispatch msg registry)]
      (fix/assert-valid! shapes/DispatchReceipt result))))

(deftest peripheral-dispatch-emits-root-evidence
  (testing "evidence store contains dispatch root entry with [:dispatch :session-start] tags"
    (let [evidence-store (atom {:entries {} :order []})
          config (fix/make-peripheral-config {:evidence-store evidence-store})
          registry (fix/mock-registry {:peripheral-config config})
          to (fix/make-agent-id "claude-1" :continuity)
          msg (assoc (fix/make-action-message) :msg/to to)
          _result (dispatch/dispatch msg registry)
          entries (vals (:entries @evidence-store))
          root-entries (filter #(= [:dispatch :session-start] (:evidence/tags %)) entries)]
      (is (= 1 (count root-entries))
          (str "Expected exactly one root evidence entry, got " (count root-entries)))
      (when (seq root-entries)
        (let [root (first root-entries)]
          (is (= :coordination (:evidence/type root)))
          (is (= :goal (:evidence/claim-type root)))
          (is (= :dispatch (get-in root [:evidence/body :event]))))))))

(deftest peripheral-dispatch-coordination-unchanged
  (testing "coordination message still goes through direct invoke (no session-id in receipt)"
    (let [config (fix/make-peripheral-config)
          registry (fix/mock-registry {:peripheral-config config})
          to (fix/make-agent-id "claude-1" :continuity)]
      (register-test-agent! to)
      (let [msg (assoc (fix/make-classified-message {:msg/payload "standup"}) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (fix/assert-valid! shapes/DispatchReceipt result)
        (is (nil? (:receipt/session-id result)))
        (is (nil? (:receipt/peripheral-id result)))
        (is (= "registry/invoke" (:receipt/route result)))))))

(deftest peripheral-dispatch-no-config-falls-back
  (testing "action message WITHOUT :peripheral-config in registry → falls back to direct invoke"
    (let [registry (fix/mock-registry) ;; no :peripheral-config
          to (fix/make-agent-id "claude-1" :continuity)]
      (register-test-agent! to)
      (let [msg (assoc (fix/make-action-message {:msg/payload "do something"}) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (fix/assert-valid! shapes/DispatchReceipt result)
        (is (true? (:receipt/delivered? result)))
        (is (= "registry/invoke" (:receipt/route result)))
        (is (nil? (:receipt/session-id result)))))))

(deftest peripheral-dispatch-uses-select-peripheral
  (testing "Claude agent action → :explore, Codex agent action → :edit"
    (let [config (fix/make-peripheral-config)
          registry (fix/mock-registry {:peripheral-config config})]
      ;; Claude agent → :explore
      (let [to (fix/make-agent-id "claude-1" :continuity)
            msg (assoc (fix/make-action-message) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (is (= :explore (:receipt/peripheral-id result))
            (str "Expected :explore for claude, got: " (pr-str result))))
      ;; Codex agent → :edit
      (let [to (fix/make-agent-id "codex-1" :continuity)
            msg (assoc (fix/make-action-message) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (is (= :edit (:receipt/peripheral-id result))
            (str "Expected :edit for codex, got: " (pr-str result)))))))

(deftest peripheral-dispatch-error-returns-social-error
  (testing "if run-chain throws, dispatch returns SocialError with :peripheral-failed"
    (with-redefs [futon3c.peripheral.registry/run-chain
                  (fn [_ _ _] (throw (ex-info "chain exploded" {:reason :test})))]
      (let [config (fix/make-peripheral-config)
            registry (fix/mock-registry {:peripheral-config config})
            to (fix/make-agent-id "claude-1" :continuity)
            msg (assoc (fix/make-action-message {:msg/payload "do X"}) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (fix/assert-valid! shapes/SocialError result)
        (is (= :peripheral-failed (:error/code result)))))))

(deftest peripheral-dispatch-existing-direct-path-regression
  (testing "explicit regression: direct path still works with peripheral-config present"
    (let [config (fix/make-peripheral-config)
          registry (fix/mock-registry {:peripheral-config config})
          to (fix/make-agent-id "claude-1" :continuity)]
      (register-test-agent! to)
      ;; Coordination message should still use direct invoke
      (let [msg (assoc (fix/make-classified-message {:msg/payload "status"}) :msg/to to)
            result (dispatch/dispatch msg registry)]
        (fix/assert-valid! shapes/DispatchReceipt result)
        (is (= "registry/invoke" (:receipt/route result)))))))
