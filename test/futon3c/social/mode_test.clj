(ns futon3c.social.mode-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.social.mode :as mode]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest classify-coordination-message
  (testing "payload {:type \"standup\"} -> :coordination"
    (let [patterns (fix/mock-patterns)
          message (dissoc (fix/make-classified-message {:msg/payload {:type "standup"}}) :msg/mode)
          result (mode/classify message patterns)]
      (fix/assert-valid! shapes/ClassifiedMessage result)
      (is (= :coordination (:msg/mode result))))))

(deftest classify-action-message
  (testing "string payload -> :action"
    (let [patterns (fix/mock-patterns)
          message (dissoc (fix/make-classified-message {:msg/payload "run tests"}) :msg/mode)
          result (mode/classify message patterns)]
      (fix/assert-valid! shapes/ClassifiedMessage result)
      (is (= :action (:msg/mode result))))))

(deftest validate-transition-happy-path
  (testing "DISCUSS -> DIAGNOSE -> EXECUTE -> DISCUSS is valid (with approval token)"
    (let [t1 (mode/validate-transition :discuss :diagnose "claude-1")
          t2 (mode/validate-transition :diagnose :execute "claude-1" :approval-token "ok-token")
          t3 (mode/validate-transition :execute :discuss "claude-1" :summary "done")]
      (fix/assert-valid! shapes/ModeTransition t1)
      (fix/assert-valid! shapes/ModeTransition t2)
      (fix/assert-valid! shapes/ModeTransition t3)
      (is (= :discuss (:mode/from t1)))
      (is (= :diagnose (:mode/to t1)))
      (is (= "ok-token" (:mode/approval-token t2)))
      (is (= "done" (:mode/summary t3))))))

(deftest invalid-transition-discuss-to-execute
  (testing "DISCUSS -> EXECUTE is invalid"
    (let [result (mode/validate-transition :discuss :execute "claude-1")]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-mode (:error/component result)))
      (is (= :invalid-transition (:error/code result))))))

(deftest diagnose-to-execute-requires-approval-token
  (testing "DIAGNOSE -> EXECUTE without approval token is rejected"
    (let [result (mode/validate-transition :diagnose :execute "claude-1")]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-mode (:error/component result)))
      (is (= :approval-required (:error/code result))))))

(deftest invalid-inputs-return-social-error
  (testing "invalid message / patterns / modes return SocialError (never nil)"
    (let [bad-classify (mode/classify "nope" (fix/mock-patterns))
          bad-patterns (mode/classify (dissoc (fix/make-classified-message) :msg/mode) {:patterns/ids "not-a-vector"})
          bad-transition (mode/validate-transition :unknown :diagnose "claude-1")]
      (fix/assert-valid! shapes/SocialError bad-classify)
      (fix/assert-valid! shapes/SocialError bad-patterns)
      (fix/assert-valid! shapes/SocialError bad-transition)
      (is (= :S-mode (:error/component bad-classify)))
      (is (= :S-mode (:error/component bad-patterns)))
      (is (= :S-mode (:error/component bad-transition))))))

(deftest outputs-are-shape-validated
  (testing "all outputs conform to ClassifiedMessage|ModeTransition|SocialError"
    (let [patterns (fix/mock-patterns)
          msg (dissoc (fix/make-classified-message {:msg/payload {:type "handoff"}}) :msg/mode)
          results [(mode/classify msg patterns)
                   (mode/classify {:msg/id "x"} patterns)
                   (mode/validate-transition :discuss :diagnose "claude-1")
                   (mode/validate-transition :discuss :execute "claude-1")]]
      (doseq [r results]
        (is (or (shapes/valid? shapes/ClassifiedMessage r)
                (shapes/valid? shapes/ModeTransition r)
                (shapes/valid? shapes/SocialError r))
            (str "Unexpected result shape: " r))))))
