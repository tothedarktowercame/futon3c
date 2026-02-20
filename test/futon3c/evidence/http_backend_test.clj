(ns futon3c.evidence.http-backend-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.http-backend :as http-be]
            [futon3c.evidence.backend :as backend]
            [futon3c.social.test-fixtures :as fix]
            [cheshire.core :as json]
            [org.httpkit.server :as hk]))

(defn- with-mock-server
  [handler f]
  (let [port (+ 9100 (rand-int 900))
        stop (hk/run-server handler {:port port})]
    (try
      (f (str "http://localhost:" port))
      (finally
        (stop)))))

(defn- json-response
  [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string body)})

(defn- json-normalize
  [x]
  (json/parse-string (json/generate-string x) true))

(deftest append-posts-json-to-evidence-endpoint
  (testing "append posts JSON to /api/alpha/evidence and returns parsed success payload"
    (let [captured (atom nil)
          entry (fix/make-evidence-entry {:evidence/id "e-http-append"})
          normalized-entry (json-normalize entry)
          result (with-mock-server
                   (fn [req]
                     (let [body-str (slurp (:body req))
                           parsed (json/parse-string body-str true)]
                       (reset! captured {:method (:request-method req)
                                         :uri (:uri req)
                                         :query-string (:query-string req)
                                         :headers (:headers req)
                                         :body body-str
                                         :parsed parsed})
                       (json-response 200 {:ok true :entry parsed})))
                   (fn [base-url]
                     (backend/-append (http-be/->HttpBackend base-url) entry)))]
      (is (= :post (:method @captured)))
      (is (= "/api/alpha/evidence" (:uri @captured)))
      (is (= "application/json"
             (or (get-in @captured [:headers "content-type"])
                 (get-in @captured [:headers "Content-Type"]))))
      (is (= normalized-entry (:parsed @captured)))
      (is (= {:ok true :entry normalized-entry} result)))))

(deftest append-returns-error-on-http-failure
  (testing "append returns SocialError with :http-error when the endpoint fails"
    (let [entry (fix/make-evidence-entry {:evidence/id "e-http-fail"})
          result (with-mock-server
                   (fn [_]
                     (json-response 500 {:ok false :error "boom"}))
                   (fn [base-url]
                     (backend/-append (http-be/->HttpBackend base-url) entry)))]
      (is (= :E-store (:error/component result)))
      (is (= :http-error (:error/code result))))))

(deftest get-retrieves-entry-by-id
  (testing "get fetches /api/alpha/evidence/:id and returns :entry payload"
    (let [captured (atom nil)
          entry (fix/make-evidence-entry {:evidence/id "e-http-get"})
          normalized-entry (json-normalize entry)
          result (with-mock-server
                   (fn [req]
                     (reset! captured {:method (:request-method req)
                                       :uri (:uri req)})
                     (json-response 200 {:entry entry}))
                   (fn [base-url]
                     (backend/-get (http-be/->HttpBackend base-url) "e-http-get")))]
      (is (= :get (:method @captured)))
      (is (= "/api/alpha/evidence/e-http-get" (:uri @captured)))
      (is (= normalized-entry result)))))

(deftest exists-returns-true-when-entry-present
  (testing "exists? returns true when GET returns an entry"
    (let [entry (fix/make-evidence-entry {:evidence/id "e-http-exists"})
          normalized-entry (json-normalize entry)
          result (with-mock-server
                   (fn [_]
                     (json-response 200 {:entry normalized-entry}))
                   (fn [base-url]
                     (backend/-exists? (http-be/->HttpBackend base-url) "e-http-exists")))]
      (is (true? result)))))

(deftest exists-returns-false-when-missing
  (testing "exists? returns false when GET has no entry payload"
    (let [result (with-mock-server
                   (fn [_]
                     {:status 404
                      :headers {"Content-Type" "application/json"}
                      :body nil})
                   (fn [base-url]
                     (backend/-exists? (http-be/->HttpBackend base-url) "e-http-missing")))]
      (is (false? result)))))

(deftest query-builds-query-string-from-params
  (testing "query sends type/limit query params and returns entries"
    (let [captured (atom nil)
          entries [(fix/make-evidence-entry {:evidence/id "e-http-q1"})]
          normalized-entries (json-normalize entries)
          result (with-mock-server
                   (fn [req]
                     (reset! captured {:method (:request-method req)
                                       :uri (:uri req)
                                       :query-string (:query-string req)})
                     (json-response 200 {:entries normalized-entries}))
                   (fn [base-url]
                     (backend/-query (http-be/->HttpBackend base-url)
                                     {:query/type :coordination
                                      :query/limit 10})))]
      (is (= :get (:method @captured)))
      (is (= "/api/alpha/evidence" (:uri @captured)))
      (is (= "type=coordination&limit=10" (:query-string @captured)))
      (is (= normalized-entries result)))))

(deftest query-returns-empty-on-error
  (testing "query returns [] when the endpoint responds with an error payload"
    (let [result (with-mock-server
                   (fn [_]
                     (json-response 500 {:error "server-error"}))
                   (fn [base-url]
                     (backend/-query (http-be/->HttpBackend base-url)
                                     {:query/type :coordination})))]
      (is (= [] result)))))

(deftest all-returns-all-entries
  (testing "all reads /api/alpha/evidence and returns :entries"
    (let [entries [(fix/make-evidence-entry {:evidence/id "e-http-all-1"})
                   (fix/make-evidence-entry {:evidence/id "e-http-all-2"})]
          normalized-entries (json-normalize entries)
          result (with-mock-server
                   (fn [_]
                     (json-response 200 {:entries normalized-entries}))
                   (fn [base-url]
                     (backend/-all (http-be/->HttpBackend base-url))))]
      (is (= normalized-entries result)))))
