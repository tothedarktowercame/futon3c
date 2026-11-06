(ns futon3c.arxana.journal-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.journal :as journal]
            [futon3c.arxana.store :as store]))

(defn- mk [] (journal/journal (store/mock-store) 3))

(deftest append-orders-and-persists-test
  (let [j (mk)
        {:keys [ok]} (journal/append! j {:topic :revision/ingested
                                         :cause/message-id "m1"
                                         :affected/ids ["a"]
                                         :payload {:x 1}})]
    (is (= 1 (:cursor ok)))
    (is (string? (:event/id ok)))
    (is (= 1 (journal/head-cursor j)))
    ;; durable in the graph store as an immutable journal-event entity
    (is (:ok (store/get-entity (:graph-store j) (:event/id ok))))))

(deftest events-after-ordering-test
  (let [j (mk)]
    (doseq [i (range 3)]
      (journal/append! j {:topic :assertion/promoted :cause/message-id (str "m" i)
                          :affected/ids [] :payload {:i i}}))
    (let [{:keys [ok]} (journal/events-after j 0)]
      (is (= [1 2 3] (mapv :cursor (:events ok)))))
    (let [{:keys [ok]} (journal/events-after j 2)]
      (is (= [3] (mapv :cursor (:events ok)))))))

(deftest cursor-expiry-requires-resync-test
  (let [j (mk)]
    (doseq [i (range 5)]
      (journal/append! j {:topic :assertion/promoted :cause/message-id (str "m" i)
                          :affected/ids [] :payload {:i i}}))
    ;; retention 3: cursors <= 1 are expired
    (let [{:keys [error]} (journal/events-after j 0)]
      (is (= :stale (:error/category error)))
      (is (= :arxana.journal/cursor-expired (:error/code error)))
      (is (true? (:resync-required error))))
    (let [{:keys [ok]} (journal/events-after j 2)]
      (is (= [3 4 5] (mapv :cursor (:events ok)))))))

(deftest unknown-topic-and-malformed-cursor-test
  (let [j (mk)]
    (is (= :arxana.journal/unknown-topic
           (:error/code (:error (journal/append! j {:topic :nope})))))
    (is (= :arxana.journal/malformed-cursor
           (:error/code (:error (journal/events-after j "x")))))
    (is (= :arxana.journal/cursor-beyond-head
           (:error/code (:error (journal/events-after j 99)))))))

(deftest crash-republish-dedupes-by-event-id-test
  ;; Same applied command republished after a crash yields the same event ID
  ;; at the same cursor only if not yet appended; here we assert the ID is a
  ;; pure function of (cursor, topic, cause, payload-digest).
  (is (= (journal/event-id 7 :assertion/promoted "m1" "d")
         (journal/event-id 7 :assertion/promoted "m1" "d")))
  (is (not= (journal/event-id 7 :assertion/promoted "m1" "d")
            (journal/event-id 8 :assertion/promoted "m1" "d"))))
