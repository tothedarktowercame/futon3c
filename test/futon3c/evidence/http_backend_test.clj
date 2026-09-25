(ns futon3c.evidence.http-backend-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.http-backend :as http-be]
            [futon3c.evidence.backend :as backend]
            [futon3c.social.test-fixtures :as fix]
            [cheshire.core :as json]
            [org.httpkit.client]
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
  (testing "append posts the PUBLIC unqualified shape (:evidence-id, NOT raw :evidence/id — the shape the server's POST /api/alpha/evidence handler actually reads at http.clj:1078) and returns the server's persisted entry"
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
                       ;; a realistic server returns the PERSISTED (qualified) EvidenceEntry,
                       ;; not an echo of the posted body.
                       (json-response 200 {:ok true :entry normalized-entry})))
                   (fn [base-url]
                     (backend/-append (http-be/->HttpBackend base-url) entry)))
          posted (:parsed @captured)]
      (is (= :post (:method @captured)))
      (is (= "/api/alpha/evidence" (:uri @captured)))
      (is (= "application/json"
             (or (get-in @captured [:headers "content-type"])
                 (get-in @captured [:headers "Content-Type"]))))
      ;; the body carries the entry's values under the PUBLIC unqualified keys the
      ;; append handler reads — the previous raw :evidence/... body was rejected.
      (is (= "e-http-append" (:evidence-id posted)))
      (is (= "claude-1" (:author posted)))
      (is (= "observation" (:claim-type posted)))
      (is (= {:text "hello"} (:body posted)))
      (is (= ["test"] (:tags posted)))
      (is (not (contains? posted :evidence/id)))
      ;; and the backend returns the server's persisted entry verbatim.
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

(deftest query-returns-typed-failure-on-error
  (testing "query returns the typed read-failed map when the endpoint responds 500 — never a substituted []"
    (let [result (with-mock-server
                   (fn [_]
                     (json-response 500 {:error "server-error"}))
                   (fn [base-url]
                     (backend/-query (http-be/->HttpBackend base-url)
                                     {:query/type :coordination})))]
      (is (= :E-store (:error/component result)))
      (is (= :read-failed (:error/code result)))
      (is (= :http (:error/kind result)))
      (is (= 500 (:status result))))))

(deftest count-builds-count-query-string-from-params
  (testing "count sends supported filters to /api/alpha/evidence/count"
    (let [captured (atom nil)
          result (with-mock-server
                   (fn [req]
                     (reset! captured {:method (:request-method req)
                                       :uri (:uri req)
                                       :query-string (:query-string req)})
                     (json-response 200 {:count 7}))
                   (fn [base-url]
                     (backend/-count (http-be/->HttpBackend base-url)
                                     {:query/type :coordination
                                      :query/author "codex"})))]
      (is (= :get (:method @captured)))
      (is (= "/api/alpha/evidence/count" (:uri @captured)))
      (is (= "type=coordination&author=codex" (:query-string @captured)))
      (is (= 7 result)))))

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

;; ---------------------------------------------------------------------------
;; AR-43: a failed or silently windowed read never reads as a complete empty
;; result. Every failure mode returns the typed read-failed map (the same
;; social-error shape -append returns); a defaulted server window travels on
;; the entries' metadata.

(deftest query-returns-typed-failure-on-unparseable-body
  (testing "a 200 whose body is not JSON is a :parse failure, not an empty page"
    (let [result (with-mock-server
                   (fn [_]
                     {:status 200
                      :headers {"Content-Type" "application/json"}
                      :body "this is not json"})
                   (fn [base-url]
                     (backend/-query (http-be/->HttpBackend base-url)
                                     {:query/type :coordination})))]
      (is (= :read-failed (:error/code result)))
      (is (= :parse (:error/kind result)))
      (is (= 200 (:status result))))))

(deftest query-returns-typed-failure-on-timeout
  (testing "a transport timeout is a :timeout failure, not an empty page"
    (with-redefs [org.httpkit.client/get
                  (fn [& _]
                    (future {:error (java.util.concurrent.TimeoutException. "timed out")
                             :status nil}))]
      (let [result (backend/-query (http-be/->HttpBackend "http://localhost:1")
                                   {:query/type :coordination})]
        (is (= :read-failed (:error/code result)))
        (is (= :timeout (:error/kind result)))))))

(deftest count-returns-typed-failure-on-error
  (testing "count returns the typed failure on HTTP error — never a substituted 0"
    (let [result (with-mock-server
                   (fn [_]
                     (json-response 500 {:error "server-error"}))
                   (fn [base-url]
                     (backend/-count (http-be/->HttpBackend base-url)
                                     {:query/type :coordination})))]
      (is (= :read-failed (:error/code result)))
      (is (= :http (:error/kind result))))))

(deftest all-returns-typed-failure-on-error
  (testing "all returns the typed failure on HTTP error — never a substituted []"
    (let [result (with-mock-server
                   (fn [_]
                     (json-response 500 {:error "server-error"}))
                   (fn [base-url]
                     (backend/-all (http-be/->HttpBackend base-url))))]
      (is (= :read-failed (:error/code result))))))

(deftest query-carries-the-servers-window-stamp-as-metadata
  (testing "a defaulted window rides on the entries' metadata so a consumer concluding absence can see it"
    (let [result (with-mock-server
                   (fn [_]
                     (json-response 200 {:entries []
                                         :window {:since "2026-09-23T00:00:00Z"
                                                  :before nil
                                                  :defaulted? true}}))
                   (fn [base-url]
                     (backend/-query (http-be/->HttpBackend base-url)
                                     {:query/tags [:test-registry]})))]
      (is (= [] result))
      (is (true? (:defaulted? (:window (meta result))))))))

(deftest query-genuine-empty-200-still-reads-as-empty
  (testing "a clean 200 with no entries and no defaulted window is genuinely empty"
    (let [result (with-mock-server
                   (fn [_]
                     (json-response 200 {:entries []
                                         :window {:since "1970-01-01T00:00:00Z"
                                                  :before nil
                                                  :defaulted? false}}))
                   (fn [base-url]
                     (backend/-query (http-be/->HttpBackend base-url)
                                     {:query/since "1970-01-01T00:00:00Z"})))]
      (is (= [] result))
      (is (false? (:defaulted? (:window (meta result))))))))
