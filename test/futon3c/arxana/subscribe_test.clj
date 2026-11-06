(ns futon3c.arxana.subscribe-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.journal :as journal]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.subscribe :as subscribe]))

(defn- mk []
  (let [j (journal/journal (store/mock-store) 3)]
    {:j j :subs (subscribe/subscriptions j)}))

(defn- publish [j topic mid]
  (journal/append! j {:topic topic :cause/message-id mid
                      :affected/ids [mid] :payload {:mid mid}}))

(deftest open-snapshot-then-poll-test
  (let [{:keys [j subs]} (mk)
        {:keys [ok]} (subscribe/open subs {:topics [:assertion/promoted]
                                           :principal/id "p1"})
        sid (:subscription/id ok)]
    (is (= 0 (:snapshot/cursor ok)))
    (publish j :assertion/promoted "m1")
    (publish j :revision/ingested "m2")     ; filtered out by topic
    (let [{p :ok} (subscribe/poll subs {:subscription/id sid})]
      (is (= 1 (count (:events p))))
      (is (= "m1" (:cause/message-id (first (:events p)))))
      ;; at-least-once: unacked event redelivered
      (let [{p2 :ok} (subscribe/poll subs {:subscription/id sid})]
        (is (= 1 (count (:events p2))))))))

(deftest ack-advances-and-dedupes-test
  (let [{:keys [j subs]} (mk)
        sid (get-in (subscribe/open subs {:topics [] :principal/id "p1"})
                    [:ok :subscription/id])]
    (publish j :assertion/promoted "m1")
    (publish j :assertion/promoted "m2")
    (is (= 2 (count (get-in (subscribe/poll subs {:subscription/id sid})
                            [:ok :events]))))
    (subscribe/ack subs {:subscription/id sid :cursor 1})
    (let [{p :ok} (subscribe/poll subs {:subscription/id sid})]
      (is (= ["m2"] (mapv :cause/message-id (:events p)))))
    ;; ack beyond head is invalid
    (is (= :arxana.subscribe/ack-beyond-head
           (:error/code (:error (subscribe/ack subs {:subscription/id sid
                                                     :cursor 99})))))))

(deftest resume-within-retention-test
  (let [{:keys [j subs]} (mk)]
    (publish j :assertion/promoted "m1")
    (publish j :assertion/promoted "m2")
    (let [{:keys [ok]} (subscribe/open subs {:topics [] :resume/cursor 1
                                             :principal/id "p1"})]
      (is (true? (:resumed? ok)))
      (is (= 1 (:pending ok))))))

(deftest expired-cursor-resync-required-test
  (let [{:keys [j subs]} (mk)]
    (doseq [i (range 5)] (publish j :assertion/promoted (str "m" i)))
    (let [{:keys [error]} (subscribe/open subs {:topics [] :resume/cursor 0
                                                :principal/id "p1"})]
      (is (= :stale (:error/category error)))
      (is (true? (:resync-required error)))
      ;; recovery: fresh snapshot open succeeds
      (let [{:keys [ok]} (subscribe/open subs {:topics [] :principal/id "p1"})]
        (is (= 5 (:snapshot/cursor ok)))))))

(deftest unknown-subscription-and-topic-test
  (let [{:keys [subs]} (mk)]
    (is (= :arxana.subscribe/unknown-subscription
           (:error/code (:error (subscribe/poll subs {:subscription/id "nope"})))))
    (is (= :arxana.subscribe/unknown-topic
           (:error/code (:error (subscribe/open subs {:topics [:bogus]
                                                      :principal/id "p"})))))))
