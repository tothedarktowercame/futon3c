(ns futon3c.social.dispatch-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
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
