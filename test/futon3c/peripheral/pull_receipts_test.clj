(ns futon3c.peripheral.pull-receipts-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.pull-receipts :as pull]))

(defn- empty-store []
  (atom {:entries {} :order []}))

(deftest pull-offers-union-per-dispatch-with-call-timing
  (let [store (empty-store)
        ctx {:evidence-store store
             :agent-id "zai-1"
             :session-id "shared-seat-session"
             :dispatch-id "job-s3"
             :turn-id "turn-s3"}]
    (pull/record-pull-offer!
     (assoc ctx :round 1) "memory_search" {:tags ["memory"]}
     {:ok true :result {:items [{:id "e-one"} {:id "e-two"}]}})
    (pull/record-pull-offer!
     (assoc ctx :round 3) "memory_search" {:author "codex-5"}
     {:ok true :result {:items [{:id "e-two"} {:id "e-three"}]}})
    (is (= ["e-one" "e-three" "e-two"]
           (pull/pull-surfaced-ids store "job-s3")))
    (is (= [1 3]
           (mapv #(get-in % [:evidence/body :round])
                 (pull/pull-offer-receipts store "job-s3"))))))

(deftest same-seat-dispatches-do-not-commingle-pull-offers
  (testing "one seat session can contain distinct S3/S6 dispatches"
    (let [store (empty-store)
          base {:evidence-store store
                :agent-id "zai-1"
                :session-id "same-seat-session"
                :round 1}]
      (pull/record-pull-offer!
       (assoc base :dispatch-id "job-s3" :turn-id "turn-s3")
       "memory_search" {} {:ok true :result {:items [{:id "e-s3"}]}})
      (pull/record-pull-offer!
       (assoc base :dispatch-id "job-s6" :turn-id "turn-s6")
       "memory_search" {} {:ok true :result {:items [{:id "e-s6"}]}})
      (is (= ["e-s3"] (pull/pull-surfaced-ids store "job-s3")))
      (is (= ["e-s6"] (pull/pull-surfaced-ids store "job-s6"))))))
