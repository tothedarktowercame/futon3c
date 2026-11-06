(ns futon3c.arxana.store-http-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1-arxana]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn]))

(defn- command [message-id expected bytes]
  {:protocol/version "arxana.v1"
   :message/id message-id
   :operation :revision/ingest
   :principal/id f1-arxana/service-principal
   :body {:target/id "artifact:cross-boundary"
          :expected/revision-id expected
          :command {:revision/canonical-bytes bytes
                    :revision/media-type "text/plain"}}})

(deftest real-http-conditional-store-test
  (gates/seed-mission-contract!)
  (with-open [node (xtn/start-node)]
    (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"})]
      (try
        (let [base (str "http://127.0.0.1:" (.getPort (.getAddress server)))
              adapter (store-http/http-store base f1-arxana/service-principal)
              first-result (store/conditional-write!
                            adapter (command "cross-1" :absent "revision-one"))
              revision (get-in first-result [:ok :revision/id])
              duplicate (store/conditional-write!
                         adapter (command "cross-1" :absent "changed-retry-body"))
              stale (store/conditional-write!
                     adapter (command "cross-2" :absent "revision-two"))]
          (testing "capabilities describe the narrow live seam"
            (is (true? (:entity-compare-and-set (store/capabilities adapter))))
            (is (false? (:entity-write (store/capabilities adapter)))))
          (testing "futon3c adapter commits through the real futon1b route"
            (is (string? revision))
            (is (= revision (f1-arxana/current-head node "artifact:cross-boundary"))))
          (testing "message id is idempotent across the HTTP boundary"
            (is (true? (:duplicate? duplicate)))
            (is (= revision (get-in duplicate [:ok :revision/id])))
            (is (= 1 (count (f1-arxana/journal-entries
                             node "artifact:cross-boundary")))))
          (testing "stale refusal stays typed across the adapter"
            (is (= :stale (get-in stale [:error :error/category])))
            (is (= :arxana/stale-head (get-in stale [:error :error/code])))
            (is (= 409 (get-in stale [:error :error/detail :http/status])))))
        (finally
          (f1-server/stop-server! server))))))
