(ns futon3c.agency.followup-queue-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
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

(deftest dedupe-covers-only-outstanding-items
  (let [first-id (:id (queue/enqueue! request))]
    (is (= :deduplicated (:status (queue/enqueue! request))))
    (let [item (queue/lease-one! "codex-11" "session-1" (constantly true))]
      (is (= :deduplicated (:status (queue/enqueue! request))))
      (queue/ack! (:followup-id item))
      (is (= :queued (:status (queue/enqueue! request)))))
    (queue/cancel! (:id (queue/enqueue! (assoc request :dedupe-key ["other"]))) :test)
    (is (= :queued (:status (queue/enqueue! (assoc request :dedupe-key ["other"])))))
    (is (string? first-id))))

(deftest validation-reasons-are-terminal-and-skip-to-next-item
  (let [expired-id (:id (queue/enqueue! request))
        valid-request (assoc request :dedupe-key ["next"] :prompt "next")
        valid-id (:id (queue/enqueue! valid-request))
        leased (queue/lease-one! "codex-11" "session-1"
                                 #(if (= expired-id (:followup-id %)) :expired true))]
    (is (= valid-id (:followup-id leased)))
    (is (= :expired (get-in (queue/snapshot) [:terminal expired-id :reason])))
    (queue/ack! valid-id)
    (doseq [[reason verdict] [[:stale :stale]
                              [:revalidation-failed false]]]
      (let [id (:id (queue/enqueue! (assoc request :dedupe-key [reason])))]
        (queue/lease-one! "codex-11" "session-1" (constantly verdict))
        (is (= reason (get-in (queue/snapshot) [:terminal id :reason])))))))

(deftest loaded-terminal-dedupe-is-migrated
  (let [path queue/*path-override*
        item (assoc request :followup-id "done")]
    (spit path (pr-str {:queued {} :leased {} :terminal {"done" item}
                        :dedupe {(:dedupe-key request) "done"}}))
    (reset! @#'queue/!state nil)
    (is (empty? (:dedupe (queue/snapshot))))
    (is (empty? (:dedupe (edn/read-string (slurp path)))))
    (is (= :queued (:status (queue/enqueue! request))))))
