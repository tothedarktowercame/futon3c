(ns futon3c.transport.provenance-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.runtime.provenance :as provenance]
            [futon3c.transport.http :as http]))

(def ^:private uri "/api/alpha/provenance")

(defn- get-route [config]
  ((http/make-handler config) {:request-method :get :uri uri}))

(deftest provenance-route-returns-ticket-shape
  (with-redefs
    [provenance/snapshot
     (fn [config]
       (provenance/summarize
        {:checkout "/home/joe/code/futon3c"
         :branch "master"
         :head-sha "c7ffdff8a2bf68c389a2e115564667d229aa3ae9"
         :dirty? true
         :reloaded-from ["/tmp/live.clj"]
         :port (:port config)
         :repo "futon3c"
         :started-at "2026-11-03T09:00:00Z"}))]
    (let [response (get-route {:port 7070})
          body (json/parse-string (:body response) true)]
      (is (= 200 (:status response)))
      (is (true? (:ok body)))
      (is (= "master" (:branch body)))
      (is (= "c7ffdff8a2bf68c389a2e115564667d229aa3ae9" (:head-sha body)))
      (is (true? (:dirty? body)))
      (is (= ["/tmp/live.clj"] (:reloaded-from body)))
      (is (= 7070 (:port body)))
      (is (= "futon3c" (:repo body)))
      (is (= "2026-11-03T09:00:00Z" (:started-at body))))))

(deftest provenance-route-answers-without-config
  ;; The route must answer even when the handler was built with no config:
  ;; a JVM with no known port still states what code it is.
  (let [response (get-route {})
        body (json/parse-string (:body response) true)]
    (is (= 200 (:status response)))
    (is (string? (:checkout body)))
    (is (string? (:started-at body)))))
