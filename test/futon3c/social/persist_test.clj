(ns futon3c.social.persist-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.social.persist :as persist]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(use-fixtures
  :each
  (fn [f]
    (persist/reset-sessions!)
    (f)))

(deftest persist-session-happy-path
  (testing "persist-session! with valid receipt -> {:ok true :session SessionRecord}"
    (let [receipt (fix/make-dispatch-receipt {:receipt/to (fix/make-agent-id "claude-1" :continuity)})
          result (persist/persist-session! receipt {:session/id "sess-1" :thread-id "t-1"})]
      (is (= true (:ok result)))
      (fix/assert-valid! shapes/SessionRecord (:session result))
      (is (= "sess-1" (:session/id (:session result))))
      (is (= (:receipt/to receipt) (:session/agent-id (:session result)))))))

(deftest get-session-returns-persisted-record
  (testing "get-session returns persisted record"
    (let [receipt (fix/make-dispatch-receipt {:receipt/to (fix/make-agent-id "codex-1" :continuity)})]
      (persist/persist-session! receipt {:session/id "sess-2" :foo :bar})
      (let [session (persist/get-session "sess-2")]
        (fix/assert-valid! shapes/SessionRecord session)
        (is (= "sess-2" (:session/id session)))
        (is (= :bar (get-in session [:session/state :data :foo])))))))

(deftest update-session-appends-and-reflects-update
  (testing "update-session! appends event and derived state reflects update"
    (let [receipt (fix/make-dispatch-receipt {:receipt/to (fix/make-agent-id "claude-1" :continuity)})]
      (persist/persist-session! receipt {:session/id "sess-3" :count 0})
      (let [before (persist/get-session "sess-3")
            r (persist/update-session! "sess-3" {:count 1 :status :running})
            after (:session r)]
        (is (= true (:ok r)))
        (fix/assert-valid! shapes/SessionRecord after)
        (is (= 2 (count (get-in after [:session/state :events]))))
        (is (= 1 (count (get-in before [:session/state :events]))))
        (is (= 1 (get-in after [:session/state :data :count])))
        (is (= :running (get-in after [:session/state :data :status])))))))

(deftest missing-session-returns-social-error
  (testing "get-session unknown -> SocialError :session-not-found"
    (let [result (persist/get-session "nope")]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-persist (:error/component result)))
      (is (= :session-not-found (:error/code result))))))

(deftest invalid-input-returns-social-error
  (testing "malformed receipt -> SocialError :invalid-input"
    (let [result (persist/persist-session! {:receipt/msg-id "x"} {:session/id "sess-x"})]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-persist (:error/component result)))
      (is (= :invalid-input (:error/code result))))))

(deftest list-sessions-and-filter
  (testing "list-sessions returns all; filtered by agent-id returns subset"
    (let [a1 (fix/make-agent-id "claude-1" :continuity)
          a2 (fix/make-agent-id "codex-1" :continuity)
          r1 (fix/make-dispatch-receipt {:receipt/to a1 :receipt/msg-id "m1"})
          r2 (fix/make-dispatch-receipt {:receipt/to a2 :receipt/msg-id "m2"})]
      (persist/persist-session! r1 {:session/id "sess-a"})
      (persist/persist-session! r2 {:session/id "sess-b"})
      (is (= 2 (count (persist/list-sessions {}))))
      (let [only-a1 (persist/list-sessions {:agent-id a1})]
        (is (= 1 (count only-a1)))
        (is (= a1 (:session/agent-id (first only-a1))))))))

(deftest output-shapes-are-valid
  (testing "returns are SessionRecord|SocialError (or {:ok true :session SessionRecord})"
    (let [receipt (fix/make-dispatch-receipt {:receipt/to (fix/make-agent-id "claude-1" :continuity)})
          ok (persist/persist-session! receipt {:session/id "sess-4"})
          sess (persist/get-session "sess-4")
          upd (persist/update-session! "sess-4" {:k :v})
          missing (persist/get-session "none")]
      (fix/assert-valid! shapes/SessionRecord (:session ok))
      (fix/assert-valid! shapes/SessionRecord sess)
      (fix/assert-valid! shapes/SessionRecord (:session upd))
      (fix/assert-valid! shapes/SocialError missing)
      (is (or (shapes/valid? shapes/SessionRecord sess)
              (shapes/valid? shapes/SocialError sess))))))

