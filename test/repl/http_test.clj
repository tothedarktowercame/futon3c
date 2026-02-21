(ns repl.http-test
  "Tests for the Drawbridge /eval endpoint."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [repl.http :as repl-http])
  (:import [java.io BufferedReader InputStreamReader]
           [java.net HttpURLConnection URL]))

;; =============================================================================
;; Test server lifecycle
;; =============================================================================

(def ^:dynamic *port* nil)
(def ^:dynamic *token* "test-token-123")

(defn- start-test-server! []
  ;; Use a random high port
  (let [port (+ 10000 (rand-int 50000))]
    (repl-http/start! {:port port
                       :bind "127.0.0.1"
                       :token *token*
                       :allow ["127.0.0.1"]})
    port))

(defn with-server [f]
  (let [port (start-test-server!)]
    (try
      (binding [*port* port]
        (f))
      (finally
        (repl-http/stop!)))))

(use-fixtures :each with-server)

;; =============================================================================
;; HTTP helpers
;; =============================================================================

(defn- http-post
  "POST to the eval endpoint. Returns {:status int :body str}."
  ([path body] (http-post path body {}))
  ([path body {:keys [token] :or {token *token*}}]
   (let [url (URL. (str "http://127.0.0.1:" *port* path))
         conn (doto ^HttpURLConnection (.openConnection url)
                (.setRequestMethod "POST")
                (.setDoOutput true)
                (.setConnectTimeout 5000)
                (.setReadTimeout 30000)
                (.setRequestProperty "Content-Type" "text/plain")
                (.setRequestProperty "x-admin-token" token))]
     (with-open [os (.getOutputStream conn)]
       (.write os (.getBytes (str body) "UTF-8")))
     (let [status (.getResponseCode conn)
           body-str (try
                      (slurp (.getInputStream conn))
                      (catch Exception _
                        (try (slurp (.getErrorStream conn))
                             (catch Exception _ ""))))]
       {:status status :body body-str}))))

(defn- http-get
  [path & {:keys [token] :or {token *token*}}]
  (let [url (URL. (str "http://127.0.0.1:" *port* path "?token=" token))
        conn (doto ^HttpURLConnection (.openConnection url)
               (.setRequestMethod "GET")
               (.setConnectTimeout 5000)
               (.setReadTimeout 10000))]
    {:status (.getResponseCode conn)
     :body (try (slurp (.getInputStream conn))
                (catch Exception _
                  (try (slurp (.getErrorStream conn))
                       (catch Exception _ ""))))}))

;; =============================================================================
;; Tests
;; =============================================================================

(deftest eval-simple-expression
  (testing "basic arithmetic"
    (let [{:keys [status body]} (http-post "/eval" "(+ 1 2 3)")]
      (is (= 200 status))
      (let [result (edn/read-string body)]
        (is (:ok result))
        (is (= 6 (:value result)))))))

(deftest eval-returns-edn
  (testing "maps and vectors"
    (let [{:keys [status body]} (http-post "/eval" "{:a 1 :b [2 3]}")]
      (is (= 200 status))
      (let [result (edn/read-string body)]
        (is (:ok result))
        (is (= {:a 1 :b [2 3]} (:value result)))))))

(deftest eval-error-returns-500
  (testing "exception in eval"
    (let [{:keys [status body]} (http-post "/eval" "(throw (ex-info \"boom\" {:x 1}))")]
      (is (= 500 status))
      (let [result (edn/read-string body)]
        (is (false? (:ok result)))
        (is (string? (:error result)))
        (is (string? (:type result)))))))

(deftest eval-empty-code-returns-400
  (testing "empty body"
    (let [{:keys [status body]} (http-post "/eval" "")]
      (is (= 400 status))
      (is (= "empty code" body)))))

(deftest eval-requires-post
  (testing "GET to /eval returns 405"
    (let [{:keys [status]} (http-get "/eval")]
      (is (= 405 status)))))

(deftest eval-requires-auth
  (testing "wrong token returns 403"
    (let [{:keys [status body]} (http-post "/eval" "(+ 1 1)" {:token "wrong"})]
      (is (= 403 status))
      (is (= "forbidden" body)))))

(deftest eval-can-require-namespaces
  (testing "require and call"
    (let [{:keys [status body]} (http-post "/eval"
                                  "(do (require '[clojure.string :as str]) (str/upper-case \"hello\"))")]
      (is (= 200 status))
      (let [result (edn/read-string body)]
        (is (:ok result))
        (is (= "HELLO" (:value result)))))))

(deftest eval-nil-result
  (testing "nil value is ok"
    (let [{:keys [status body]} (http-post "/eval" "nil")]
      (is (= 200 status))
      (let [result (edn/read-string body)]
        (is (:ok result))
        (is (nil? (:value result)))))))
