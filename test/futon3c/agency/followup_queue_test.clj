(ns futon3c.agency.followup-queue-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.followup-queue :as queue]))

(use-fixtures :each
  (fn [f]
    (let [path (str (System/getProperty "java.io.tmpdir")
                    "/futon3c-followup-test-" (random-uuid) ".edn")]
      (binding [queue/*path-override* path]
        (queue/clear!)
        (f)))))

(def request {:agent "codex-11" :session "session-1" :type :inbox-zero
              :dedupe-key ["seat" "repo" "hash"] :prompt "Review five files"})

(deftest typed-dedupe-lease-and-ack
  (let [first-result (queue/enqueue! request)
        duplicate (queue/enqueue! request)
        item (queue/lease-one! "codex-11" "session-1" (constantly true))]
    (is (= :queued (:status first-result)))
    (is (= (:id first-result) (:id duplicate)))
    (is (= :inbox-zero (:type item)))
    (is (nil? (queue/lease-one! "codex-11" "other-session" (constantly true))))
    (is (true? (queue/ack! (:followup-id item))))
    (is (= :acked (get-in (queue/snapshot) [:terminal (:followup-id item) :state])))))

(deftest failed-revalidation-cancels-without-redirection
  (let [id (:id (queue/enqueue! request))]
    (is (nil? (queue/lease-one! "codex-11" "session-1" (constantly false))))
    (is (= :cancelled (get-in (queue/snapshot) [:terminal id :state])))
    (is (nil? (queue/lease-one! "codex-11" "session-2" (constantly true))))))
