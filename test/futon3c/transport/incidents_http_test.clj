(ns futon3c.transport.incidents-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.transport.http :as http]))

(def ^:private incidents-uri "/api/alpha/jvm/incidents")
(def ^:private health-uri "/api/alpha/jvm/health")

(defn- response-body [response]
  (json/parse-string (:body response) true))

(defn- get-route
  ([uri] (get-route uri nil))
  ([uri query-string]
   ((http/make-handler {})
    {:request-method :get :uri uri :query-string query-string})))

(defn- resolver [implementations]
  (fn [sym] (get implementations sym)))

(deftest incidents-route-returns-newest-records-and-count
  (let [called-with (atom nil)
        records [{:jvm-incident/id "jvi-new" :at "2026-07-18T12:00:00Z"}]]
    (with-redefs [clojure.core/requiring-resolve
                  (resolver
                   {'futon3c.runtime.incidents/incidents
                    (fn [limit] (reset! called-with limit) records)})]
      (let [response (get-route incidents-uri "limit=7")
            body (response-body response)]
        (is (= 200 (:status response)))
        (is (= 7 @called-with))
        (is (true? (:ok body)))
        (is (= 1 (:count body)))
        (is (= "jvi-new"
               (get-in body [:incidents 0 :jvm-incident/id])))))))

(deftest health-route-returns-required-shape
  (with-redefs [clojure.core/requiring-resolve
                (resolver
                 {'futon3c.runtime.incidents/health
                  (fn [] {:heap {:used-mb 12 :max-mb 256}
                          :threads 8
                          :uptime-ms 9000
                          :incident-count 3})})]
    (let [response (get-route health-uri)
          body (response-body response)]
      (is (= 200 (:status response)))
      (is (= {:used-mb 12 :max-mb 256} (:heap body)))
      (is (= 8 (:threads body)))
      (is (= 9000 (:uptime-ms body)))
      (is (= 3 (:incident-count body)))
      (is (true? (:ok body))))))

(deftest jvm-routes-report-an-unavailable-incident-layer
  (with-redefs [clojure.core/requiring-resolve (constantly nil)]
    (doseq [uri [incidents-uri health-uri]]
      (testing uri
        (let [response (get-route uri)]
          (is (= 501 (:status response)))
          (is (= "jvm-incidents-unavailable"
                 (:err (response-body response)))))))))
