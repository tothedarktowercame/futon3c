(ns repl.http-nrepl-guard-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [repl.http :as repl-http]))

(deftest eval-refresh-is-refused-and-ordinary-code-still-runs
  (testing "the /eval handler remains guarded"
    (let [handler #'repl-http/eval-handler
          response (handler {:request-method :post
                             :remote-addr "127.0.0.1"
                             :body "(clojure.tools.namespace.repl/refresh)"})]
      (is (= 403 (:status response)))
      (is (re-find #"REFUSED.*tools.namespace" (:body response)))))
  (testing "ordinary /eval code remains executable"
    (let [handler #'repl-http/eval-handler
          response (handler {:request-method :post
                             :remote-addr "127.0.0.1"
                             :body "(+ 20 22)"})]
      (is (= 200 (:status response)))
      (is (= 42 (:value (edn/read-string (:body response))))))))

(deftest nrepl-refresh-is-refused-before-dispatch
  (let [dispatched? (atom false)
        handler (#'repl-http/route-handler
                 (fn [_]
                   (reset! dispatched? true)
                   {:status 200}))
        request-body
        "d4:code39:(clojure.tools.namespace.repl/refresh)2:op4:evale"
        response (handler {:uri "/repl"
                           :request-method :post
                           :body request-body})]
    (is (= 403 (:status response)))
    (is (re-find #"REFUSED.*tools.namespace" (:body response)))
    (is (false? @dispatched?))))

(deftest ordinary-nrepl-request-is-dispatched-with-body-intact
  (let [seen-body (atom nil)
        handler (#'repl-http/route-handler
                 (fn [request]
                   (reset! seen-body (slurp (:body request)))
                   {:status 200 :body "downstream"}))
        request-body "d4:code7:(+ 1 2)2:op4:evale"
        response (handler {:uri "/repl"
                           :request-method :post
                           :body (java.io.ByteArrayInputStream.
                                  (.getBytes request-body "UTF-8"))})]
    (is (= 200 (:status response)))
    (is (= "downstream" (:body response)))
    (is (= request-body @seen-body))))
