(ns futon3c.test-registry-read-chain-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.evidence.http-backend :as http-backend]
            [futon3c.evidence.store :as store]
            [futon3c.test-registry :as registry])
  (:import [com.sun.net.httpserver HttpServer HttpHandler]
           [java.net InetSocketAddress]
           [java.util.concurrent Executors TimeUnit]))

(defn records []
  (let [backend (atom {:entries {} :order []})
        p (registry/append-record! backend {:author "read-test" :run/id "read" :kind :parent} nil)
        c (registry/append-record! backend {:author "read-test" :run/id "read" :kind :child} (:evidence/id p))]
    {:parent (:evidence/id p) :child (:evidence/id c)
     :entries (into {} (for [r [p c] :let [id (:evidence/id r)]] [id (store/get-entry* backend id)]))}))

(defn with-server [reply f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newFixedThreadPool 4) calls (atom {})]
    (.setExecutor server executor)
    (.createContext server "/api/alpha/evidence/"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (try
                          (let [id (last (.split (.getPath (.getRequestURI exchange)) "/"))
                                n (get (swap! calls update id (fnil inc 0)) id)
                                {:keys [delay status body]} (reply id n)]
                            (when delay (Thread/sleep (long delay)))
                            (let [bytes (.getBytes ^String body "UTF-8")]
                              (.set (.getResponseHeaders exchange) "Content-Type" "application/json")
                              (.sendResponseHeaders exchange status (alength bytes))
                              (.write (.getResponseBody exchange) bytes)))
                          (catch java.io.IOException _ nil) ; client has timed out
                          (catch InterruptedException _ nil) ; teardown cancels delayed handlers
                          (finally (.close exchange))))))
    (.start server)
    (try
      (binding [http-backend/*entry-read-timeout-ms* 75
                ;; Exercise real HTTP timeouts without sleeping 1.75s per case.
                registry/*registry-read-retry-delays-ms* [1 1 1]]
        (f (http-backend/->HttpBackend (str "http://127.0.0.1:" (.getPort (.getAddress server)))) calls))
      (finally (.stop server 0) (.shutdownNow executor) (.awaitTermination executor 2 TimeUnit/SECONDS)))))

(defn response [entry] {:status 200 :body (json/generate-string {:entry entry})})
(defn outcome [backend id]
  (try (registry/read-chain! backend id) (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest timeouts-twice-then-success
  (let [{:keys [parent entries]} (records)]
    (is (= [250 500 1000] registry/*registry-read-retry-delays-ms*))
    (with-server (fn [id n] (cond-> (response (get entries id)) (<= n 2) (assoc :delay 300)))
      (fn [b calls]
        (let [chain (registry/read-chain! b parent)]
          (is (= [parent] (mapv :evidence/id chain)))
          (is (= 3 (get @calls parent)))
          (is (= {parent 3} (:registry-read-attempts (meta chain)))))))))

(deftest timeout-is-not-absence
  (with-server (fn [_ _] (assoc (response nil) :delay 300))
    (fn [b calls]
      (let [r (outcome b "unresponsive")]
        (is (= :registry-read-failed (:reason r)))
        (is (= {:evidence/id "unresponsive" :kind :timeout :attempts 4}
               (select-keys (:details r) [:evidence/id :kind :attempts])))
        (is (= 4 (get @calls "unresponsive")))))))

(deftest clean-absence-is-not-retried
  (doseq [reply [{:status 404 :body "not found"} (response nil)]]
    (with-server (fn [_ _] reply)
      (fn [b calls]
        (is (= :missing-entry (:reason (outcome b "missing"))))
        (is (= 1 (get @calls "missing")))))))

(deftest parent-timeout-is-not-a-broken-link
  (let [{:keys [parent child entries]} (records)]
    (with-server (fn [id _] (cond-> (response (get entries id)) (= parent id) (assoc :delay 300)))
      (fn [b calls]
        (let [r (outcome b child)]
          (is (= :registry-read-failed (:reason r)))
          (is (= {:evidence/id parent :kind :timeout :attempts 4}
                 (select-keys (:details r) [:evidence/id :kind :attempts])))
          (is (= {child 1 parent 4} @calls)))))))

(deftest answered-http-and-parse-failures-retain-their-kind
  (doseq [[reply kind] [[{:status 503 :body "busy"} :http]
                       [{:status 200 :body "not json"} :parse]]]
    (with-server (fn [_ _] reply)
      (fn [b _]
        (let [r (http-backend/get-entry-or-failure b "entry")]
          (is (= :read-failed (:error/code r)))
          (is (= kind (:error/kind r))))))))

(deftest a-clean-parent-is-read-only-once-and-absence-keeps-link-refusal
  (let [{:keys [parent child entries]} (records)]
    (with-server (fn [id _] (response (get entries id)))
      (fn [b calls]
        (is (= [parent child] (mapv :evidence/id (registry/read-chain! b child))))
        (is (= {parent 1 child 1} @calls))))
    (with-server (fn [id _] (if (= id parent) {:status 404 :body "missing"} (response (get entries id))))
      (fn [b calls]
        (is (= :chain-link-mismatch (:reason (outcome b child))))
        (is (= {parent 1 child 1} @calls))))))
