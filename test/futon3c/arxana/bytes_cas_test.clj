(ns futon3c.arxana.bytes-cas-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as formal-f1]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn- request [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "bytes-cas-test" :body body})

(defn- dispatch [cfg mid op body]
  (service/dispatch cfg (request mid op body)))

(defn- head [cfg artifact-id]
  (get-in (dispatch cfg (str "head:" artifact-id ":" (random-uuid))
                    :revision/head {:artifact/id artifact-id})
          [:ok :head/revision-id]))

(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "bytes-cas-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server!
                      {:node node :port 0 :bind-host "127.0.0.1"
                       :arxana-index-path (.getPath db)})
              cfg {:graph-store
                   (store-http/http-store
                    (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                    f1/service-principal)
                   :artifact-provider (artifact/in-memory-provider)}]
          (try (f cfg) (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(deftest byte-arrays-use-the-real-head-cas
  (with-stack
    (fn [cfg]
      (let [artifact-id "bytes-cas:direct"
            body {:artifact/id artifact-id
                  :bytes (.getBytes "first 🈳 revision" "UTF-8")
                  :media-type "text/plain; charset=utf-8"
                  :expected/revision-id :absent}
            first-result (dispatch cfg "bytes:first" :revision/ingest body)
            revision (get-in first-result [:ok :artifact/revision-id])
            replay (dispatch cfg "bytes:first" :revision/ingest body)
            stale (dispatch cfg "bytes:stale" :revision/ingest
                            (assoc body :bytes (.getBytes "stale" "UTF-8")))
            invalid (dispatch cfg "bytes:invalid" :revision/ingest
                              (assoc body :expected/revision-id revision
                                     :bytes (byte-array [(unchecked-byte 0xc3)
                                                        (unchecked-byte 0x28)])))]
        (testing "byte arrays commit through futon1b and message replay is a no-op"
          (is (string? revision) (pr-str first-result))
          (is (= revision (head cfg artifact-id)))
          (is (true? (get-in replay [:ok :duplicate?])))
          (is (= revision (head cfg artifact-id))))
        (testing "a stale absent precondition is typed and does not move the head"
          (is (= :stale (get-in stale [:error :error/category])))
          (is (= revision (get-in stale [:error :error/detail :futon1b/error
                                         :context :current-head]))
              (pr-str stale))
          (is (= revision (head cfg artifact-id))))
        (testing "malformed UTF-8 is refused before the route and leaves the head"
          (is (= :invalid (get-in invalid [:error :error/category])))
          (is (= :arxana.service/ingest-bytes-not-utf8
                 (get-in invalid [:error :error/code])))
          (is (= revision (head cfg artifact-id))))))))

(deftest f1-byte-revisions-are-public-heads-and-replay-is-idempotent
  (with-stack
    (fn [cfg]
      (let [plans (formal-f1/add-operator-judgement (formal-f1/plan-fixtures))
            first-run (formal-f1/materialize! cfg plans)
            heads-before (mapv #(head cfg (:artifact/id %)) plans)
            second-run (formal-f1/materialize!
                        cfg (formal-f1/add-operator-judgement
                             (formal-f1/plan-fixtures)))
            heads-after (mapv #(head cfg (:artifact/id %)) plans)]
        (is (every? :ok (map :ingest first-run)))
        (is (= (mapv #(artifact/revision-id-for-bytes (:bytes %)) plans)
               heads-before))
        (is (every? #(true? (get-in % [:ingest :ok :duplicate?])) second-run))
        (is (= heads-before heads-after))))))
