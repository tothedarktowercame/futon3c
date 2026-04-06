(ns futon3c.dev.apm-dispatch-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.dev.apm-dispatch :as dispatch]))

(use-fixtures :each
  (fn [f]
    ;; Clean state before each test
    (reset! dispatch/!conductors {})
    (reg/set-on-idle! nil)
    (f)
    ;; Clean up after
    (reset! dispatch/!conductors {})
    (reg/set-on-idle! nil)))

(deftest register-and-route-single-conductor
  (let [received (atom [])]
    (dispatch/register-conductor! :test-v1 "claude-1"
      (fn [agent-id outcome]
        (swap! received conj {:agent-id agent-id :outcome outcome})))

    (testing "callback is wired"
      (is (some? @reg/!on-idle)))

    (testing "fires for matching agent"
      (@reg/!on-idle "claude-1" {:ok true :result "hello"})
      (is (= 1 (count @received)))
      (is (= "claude-1" (:agent-id (first @received)))))

    (testing "ignores non-matching agent"
      (@reg/!on-idle "codex-1" {:ok true :result "ignored"})
      (is (= 1 (count @received))))))

(deftest parallel-conductors-route-independently
  (let [v1-received (atom [])
        v2-received (atom [])]
    (dispatch/register-conductor! :apm-v1 "codex-1"
      (fn [_ outcome] (swap! v1-received conj outcome)))
    (dispatch/register-conductor! :apm-v2 "claude-1"
      (fn [_ outcome] (swap! v2-received conj outcome)))

    (testing "both registered"
      (is (= 2 (count (dispatch/active-conductors)))))

    (testing "codex-1 event goes to v1 only"
      (@reg/!on-idle "codex-1" {:ok true :result "codex-work"})
      (is (= 1 (count @v1-received)))
      (is (= 0 (count @v2-received))))

    (testing "claude-1 event goes to v2 only"
      (@reg/!on-idle "claude-1" {:ok true :result "claude-work"})
      (is (= 1 (count @v1-received)))
      (is (= 1 (count @v2-received))))))

(deftest deregister-removes-conductor
  (let [v1-received (atom [])
        v2-received (atom [])]
    (dispatch/register-conductor! :apm-v1 "codex-1"
      (fn [_ outcome] (swap! v1-received conj outcome)))
    (dispatch/register-conductor! :apm-v2 "claude-1"
      (fn [_ outcome] (swap! v2-received conj outcome)))

    (dispatch/deregister-conductor! :apm-v1)

    (testing "v1 no longer receives events"
      (@reg/!on-idle "codex-1" {:ok true})
      (is (= 0 (count @v1-received))))

    (testing "v2 still works"
      (@reg/!on-idle "claude-1" {:ok true})
      (is (= 1 (count @v2-received))))

    (testing "one conductor remains"
      (is (= 1 (count (dispatch/active-conductors)))))))

(deftest deregister-last-clears-global-callback
  (dispatch/register-conductor! :solo "claude-1" (fn [_ _]))
  (is (some? @reg/!on-idle))

  (dispatch/deregister-conductor! :solo)
  (is (nil? @reg/!on-idle)))

(deftest duplicate-agent-id-rejected
  (dispatch/register-conductor! :first "claude-1" (fn [_ _]))

  (testing "second conductor on same agent throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
          #"already owned by conductor"
          (dispatch/register-conductor! :second "claude-1" (fn [_ _])))))

  (testing "same conductor-id can re-register (update)"
    (dispatch/register-conductor! :first "claude-1" (fn [_ _]))
    (is (= 1 (count (dispatch/active-conductors))))))
