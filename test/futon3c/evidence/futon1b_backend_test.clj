(ns futon3c.evidence.futon1b-backend-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as sut]
            [org.httpkit.client :as http]))

(deftest post-filters-retain-the-bounded-server-window
  (testing "the server fills an exact bounded window and the client rechecks membership"
    (let [seen-url (atom nil)
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [http/get (fn [url _]
                               (reset! seen-url url)
                               (delay {:status 200
                                       :body "{:entries []}"}))]
        (is (= []
               (backend/-query store
                               {:query/tags [:open :pipeline-tracer]
                                :query/subject {:ref/type :thread :ref/id "t 1"}
                                :query/pattern-id :agent/pause
                                :query/limit 5})))
        (is (str/includes? @seen-url "tags=open%2Cpipeline-tracer"))
        (is (str/includes? @seen-url "subject-type=thread"))
        (is (str/includes? @seen-url "subject-id=t+1"))
        (is (str/includes? @seen-url "pattern-id=pause"))
        (is (str/includes? @seen-url "limit=5"))
        (is (str/includes? @seen-url "include-ephemeral=false"))))))

(deftest post-filtered-count-uses-projected-count-route
  (testing "counting tags never hydrates an unbounded evidence response"
    (let [seen-url (atom nil)
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [http/get (fn [url _]
                               (reset! seen-url url)
                               (delay {:status 200 :body "{:count 7}"}))]
        (is (= 7 (backend/-count store {:query/tags [:open]})))
        (is (str/includes? @seen-url "/api/alpha/evidence/count?"))
        (is (str/includes? @seen-url "tags=open"))))))

(deftest unbounded-protocol-query-pages-through-server-cursors
  (testing "-all never asks the store JVM for an unbounded response"
    (let [seen-urls (atom [])
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [http/get
                    (fn [url _]
                      (swap! seen-urls conj url)
                      (delay
                        {:status 200
                         :body (if (str/includes? url "cursor-at=")
                                 "{:entries [{:evidence/id \"e1\", :evidence/at \"2026-01-01T00:00:00Z\"}]}"
                                 "{:entries [{:evidence/id \"e2\", :evidence/at \"2026-01-02T00:00:00Z\"}], :next-cursor {:at \"2026-01-02T00:00:00Z\", :id \"e2\"}}" )}))]
        (is (= ["e2" "e1"]
               (mapv :evidence/id (backend/-all store))))
        (is (= 2 (count @seen-urls)))
        (is (every? #(str/includes? % "limit=1000") @seen-urls))
        (is (str/includes? (second @seen-urls) "cursor-id=e2"))))))

(deftest append-classifies-timeout-separately-from-unreachable
  (let [entry {:evidence/id "e-timeout"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-07-22T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}
        store (sut/make-futon1b-backend "http://store.test")]
    (with-redefs [http/get (fn [_ _] (delay {:status 404 :body "{}"}))
                  http/post (fn [_ _]
                              (delay {:error (java.net.SocketTimeoutException.
                                               "read timed out")}))]
      (is (= :store-timeout
             (:error/code (backend/-append store entry)))))
    (with-redefs [http/get (fn [_ _] (delay {:status 404 :body "{}"}))
                  http/post (fn [_ _]
                              (delay {:error (java.net.ConnectException.
                                               "connection refused")}))]
      (is (= :store-unreachable
             (:error/code (backend/-append store entry)))))))
