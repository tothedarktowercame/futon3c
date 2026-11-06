(ns futon3c.arxana.http-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.http :as http]
            [futon3c.arxana.journal :as journal]
            [futon3c.arxana.json :as ajson]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.subscribe :as subscribe])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]))

(def test-port 17084)

(defn- test-cfg []
  (let [s (store/mock-store)
        j (journal/journal s)
        subs (subscribe/subscriptions j)]
    {:graph-store s
     :artifact-provider (artifact/in-memory-provider)
     :journal j
     :subscriptions subs}))

(defn- post [^String body]
  (let [client (HttpClient/newHttpClient)
        req (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" test-port "/arxana/v1")))
                (.header "Content-Type" "application/json")
                (.POST (HttpRequest$BodyPublishers/ofString body))
                .build)]
    (.body (.send client req (HttpResponse$BodyHandlers/ofString)))))

(defn- get-capabilities []
  (let [client (HttpClient/newHttpClient)
        req (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" test-port "/arxana/v1/capabilities")))
                .GET .build)]
    (.body (.send client req (HttpResponse$BodyHandlers/ofString)))))

(defn- envelope [op body]
  (ajson/encode {:protocol/version "arxana.v1"
                 :message/id (str "t-" (random-uuid))
                 :operation op
                 :principal/id "http-test"
                 :sent-at "2026-11-07T00:00:00Z"
                 :body body}))

(deftest json-codec-round-trip-test
  (let [v {:protocol/version "arxana.v1"
           :operation :scope/resolve
           :nested [{:a/b 1 :c :d/e}]
           :bytes (.getBytes "hi" "UTF-8")}
        rt (ajson/decode (ajson/encode v))]
    (is (= (dissoc v :bytes) (dissoc rt :bytes)))
    (is (and (bytes? (:bytes rt))
             (= "hi" (String. ^bytes (:bytes rt) "UTF-8")))))
  (is (thrown? clojure.lang.ExceptionInfo
               (ajson/encode {:bad #{1 2}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (ajson/encode {:s ":looks-like-keyword"}))))

(deftest http-surface-test
  (let [server (http/start! {:port test-port :service-cfg (test-cfg)})]
    (try
      (testing "preflight refuses an occupied port"
        (is (thrown? clojure.lang.ExceptionInfo
                     (http/preflight-ports! "127.0.0.1" [["arxana" test-port]]))))
      (testing "capabilities handshake over HTTP"
        (let [resp (ajson/decode (get-capabilities))]
          (is (= "arxana.v1" (:protocol/version resp)))
          (is (true? (:subscription resp)))
          (is (contains? (:operations resp) :scope/resolve))))
      (testing "strict envelope: unknown fields refused"
        (let [resp (ajson/decode
                    (post (ajson/encode {:protocol/version "arxana.v1"
                                         :message/id "x1"
                                         :operation :capabilities/handshake
                                         :principal/id "p"
                                         :sent-at "t"
                                         :body {}
                                         :bogus/field 1})))]
          (is (= :arxana.envelope/unknown-field
                 (get-in resp [:error :error/code])))))
      (testing "malformed JSON is a typed invalid, not a 500"
        (let [resp (ajson/decode (post "{not json"))]
          (is (= :arxana.http/malformed-json
                 (get-in resp [:error :error/code])))))
      (testing "subscription open/poll/ack round-trip"
        (let [opened (ajson/decode (post (envelope :subscription/open
                                                   {:topics [:assertion/promoted]})))
              sid (get-in opened [:ok :subscription/id])]
          (is (some? sid) (pr-str opened))
          (let [polled (ajson/decode (post (envelope :subscription/poll
                                                     {:subscription/id sid})))]
            (is (= [] (get-in polled [:ok :events]))))
          (let [acked (ajson/decode (post (envelope :subscription/ack
                                                    {:subscription/id sid
                                                     :cursor 0})))]
            (is (= 0 (get-in acked [:ok :acked]))))))
      (testing "compile is public and its schema refuses incomplete input"
        (let [resp (ajson/decode (post (envelope :action/compile {:intent "x"})))]
          (is (= :missing (get-in resp [:error :error/category])))))
      (finally (server)))))

(deftest ingest-promote-publish-flow-test
  (let [cfg (test-cfg)
        server (http/start! {:port test-port :service-cfg cfg})]
    (try
      (let [opened (ajson/decode (post (envelope :subscription/open {:topics []})))
            sid (get-in opened [:ok :subscription/id])
            ing (ajson/decode (post (envelope :revision/ingest
                                              {:artifact/id "demo.tex"
                                               :bytes "hello arxana"
                                               :media-type "text/plain"
                                               :expected/revision-id :absent})))]
        (is (string? (get-in ing [:ok :artifact/revision-id])) (pr-str ing))
        ;; stale precondition refused with current head + recovery
        (let [stale (ajson/decode (post (envelope :revision/ingest
                                                  {:artifact/id "demo.tex"
                                                   :bytes "v2"
                                                   :media-type "text/plain"
                                                   :expected/revision-id :absent})))]
          (is (= :stale (get-in stale [:error :error/category])) (pr-str stale))
          (is (= [:refresh :rebase-and-resubmit :export-patch]
                 (get-in stale [:error :error/recovery]))))
        ;; the ingest published a journal event, visible to the subscriber
        (let [polled (ajson/decode (post (envelope :subscription/poll
                                                   {:subscription/id sid})))]
          (is (= [:revision/ingested]
                 (mapv :topic (get-in polled [:ok :events]))))
          (is (every? #(string? (:event/id %)) (get-in polled [:ok :events])))))
      (finally (server)))))
