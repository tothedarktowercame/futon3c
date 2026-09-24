(ns futon3c.wm.flight-click-http-test
  "A flight click through POST /api/alpha/wm/click: the flight arrives as
  an EDN string, so keyword want tokens survive the JSON boundary, and
  reaches the runner options as :flight (futon2.aif.flight-runner)."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as service]))

(defn- request [payload]
  {:request-method :post :uri "/api/alpha/wm/click" :headers {}
   :body (java.io.ByteArrayInputStream. (.getBytes (json/generate-string payload) "UTF-8"))})

(def flight {:flight/id "flight-x" :target "M-futon-seams" :click 1
             :wants [:exit/hac75428b9c97 :exit/h54d16050a9dc]
             :locators {:exit/h54d16050a9dc {:class :C4 :repo "futon3c"}}})

(deftest a-flight-reaches-the-runner-with-keywords-intact
  (let [handler (http/make-handler {}) calls (atom [])]
    (with-redefs [service/click! (fn [opts] (swap! calls conj opts) {:started true})
                  ;; the cast-seat preflight asks the live roster; the boundary
                  ;; under test is the payload, so it is stubbed
                  service/cast-preflight-refusal (constantly nil)]
      (let [r (handler (request {:flight-edn (pr-str flight) :run-id "flight-x-click-1"}))]
        (is (= 200 (:status r)) (str (:body r))))
      (is (= flight (:flight (first @calls))))
      (is (= "flight-x-click-1" (:run-id (first @calls)))))))

(deftest a-malformed-flight-is-refused-before-any-click
  ;; bad case: a flight with no :target would assemble an empty universe
  (let [handler (http/make-handler {}) calls (atom [])]
    (with-redefs [service/click! (fn [opts] (swap! calls conj opts) {:started true})
                  ;; the cast-seat preflight asks the live roster; the boundary
                  ;; under test is the payload, so it is stubbed
                  service/cast-preflight-refusal (constantly nil)]
      (is (= 400 (:status (handler (request {:flight-edn (pr-str {:wants []})})))))
      (is (= 400 (:status (handler (request {:flight-edn "[1 2 3]"})))))
      (is (empty? @calls)))))

(deftest no-flight-no-key
  (let [handler (http/make-handler {}) calls (atom [])]
    (with-redefs [service/click! (fn [opts] (swap! calls conj opts) {:started true})
                  ;; the cast-seat preflight asks the live roster; the boundary
                  ;; under test is the payload, so it is stubbed
                  service/cast-preflight-refusal (constantly nil)]
      (handler (request {:author "legacy"}))
      (is (not (contains? (first @calls) :flight))))))
