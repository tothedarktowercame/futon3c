(ns futon3c.agency.turn-notice-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.turn-notice :as notice]))

(use-fixtures :each (fn [f] (notice/reset-state!) (f)))

(defn row [id]
  {:agent "a" :session "s" :notice-id id :kind "unresolved"})

(deftest duplicate-publish-is-a-no-op-and-take-is-exact-seat
  (is (= :queued (notice/publish! (row "n1"))))
  (is (= :duplicate (notice/publish! (row "n1"))))
  (is (nil? (notice/take! "a" "other")))
  (is (= "n1" (:notice/id (notice/take! "a" "s"))))
  (is (nil? (notice/take! "a" "s"))))

(deftest queue-is-bounded-and-drops-oldest
  (with-redefs [notice/queue-limit 2 notice/seen-limit 3]
    (doseq [id ["n1" "n2" "n3" "n4"]] (notice/publish! (row id)))
    (is (= {:queued 2 :seen 3 :drops 2} (notice/stats "a" "s")))
    (is (= ["n3" "n4"]
           (mapv :notice/id [(notice/take! "a" "s")
                             (notice/take! "a" "s")])))))

(deftest concurrent-takes-cannot-return-one-notice-twice
  (notice/publish! (row "n1"))
  (let [takes (doall (map deref [(future (notice/take! "a" "s"))
                                 (future (notice/take! "a" "s"))]))]
    (is (= 1 (count (remove nil? takes))))
    (is (= "n1" (:notice/id (first (remove nil? takes)))))))

(deftest a-header-before-the-first-notice-does-not-block-it
  ;; Every exact-seat header calls take!; the first notice for that seat
  ;; usually arrives afterwards.
  (notice/reset-state!)
  (is (nil? (notice/take! "agent-z" "session-z")))
  (is (= :queued (notice/publish! {:agent "agent-z" :session "session-z"
                                   :notice-id "n-1" :kind "no-grant"})))
  (is (= "withdraw inferred: off (no grant)"
         (:notice/text (notice/take! "agent-z" "session-z")))))

(deftest agreement-outcomes-are-rendered-from-closed-fields
  (notice/publish! {:agent "a" :session "s" :notice-id "accepted"
                    :kind "agreement-accepted"
                    :agreement-id "act:agreement" :offer-id "act:offer"
                    :option-id "1" :grant-id "act:grant"
                    :grant-until "2026-09-29T00:00:00Z"})
  (is (= (str "agreement act:agreement: you offered act:offer, Joe accepted option 1; "
              "grant act:grant until 2026-09-29T00:00:00Z")
         (:notice/text (notice/take! "a" "s"))))
  (notice/publish! {:agent "a" :session "s" :notice-id "only"
                    :kind "agreement-accepted"
                    :agreement-id "act:agreement" :offer-id "act:offer"
                    :option-id "2" :grant-reason "agreement-only"})
  (is (str/ends-with? (:notice/text (notice/take! "a" "s"))
                      "; agreement only, no grant"))
  (is (= :incomplete-grant
         (try
           (notice/publish! {:agent "a" :session "s" :notice-id "bad"
                             :kind "agreement-accepted"
                             :agreement-id "act:agreement" :offer-id "act:offer"
                             :option-id "1" :grant-id "act:grant"})
           nil
           (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
