(ns futon3c.wm.canonical-g-click-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.registry :as registry]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as service]))

(deftest g-law-reaches-click-with-default-unchanged
  (let [handler (http/make-handler {}) calls (atom [])
        request (fn [extra]
                  {:request-method :post :uri "/api/alpha/wm/click" :headers {}
                   :body (java.io.ByteArrayInputStream.
                          (.getBytes (json/generate-string
                                      (merge {:run-id "2026-10-22-aab6ddc5-924e-46e5-b28a-c12feccf4ddf"
                                              :trigger "duree-click-on-demand"} extra)) "UTF-8"))})]
    (with-redefs [service/click! (fn [opts] (swap! calls conj opts) {:started true})
                  service/prepare-ordinary-click-opts identity
                  service/cast-preflight-refusal (constantly nil)
                  registry/reset-session! (constantly {:ok true})]
      (is (= 200 (:status (handler (request {:g-law "canonical-coapply"})))) )
      (is (= :canonical-coapply (:g-law (first @calls))))
      (is (= "canonical-coapply" (get-in (first @calls) [:acceptance-payload :g-law])))
      (is (= 200 (:status (handler (request {})))))
      (is (not (contains? (second @calls) :g-law)))
      (is (= 400 (:status (handler (request {:g-law "unknown"})))) )
      (is (= 2 (count @calls))))))

(deftest supplied-task-reaches-runner-and-acceptance-record
  (let [handler (http/make-handler {})
        calls (atom [])
        request (fn [payload]
                  {:request-method :post
                   :uri "/api/alpha/wm/click"
                   :headers {}
                   :body (java.io.ByteArrayInputStream.
                          (.getBytes (json/generate-string payload) "UTF-8"))})]
    (with-redefs [service/click! (fn [opts]
                                   (swap! calls conj opts)
                                   {:started true})
                  service/prepare-ordinary-click-opts identity
                  service/cast-preflight-refusal (constantly nil)
                  registry/reset-session! (constantly {:ok true})]
      (is (= 200 (:status (handler (request {:task "M-x"})))))
      (is (= {:id "M-x"} (:task (first @calls))))
      (is (= "M-x" (get-in (first @calls) [:acceptance-payload :task])))
      (is (= 200 (:status (handler (request {})))))
      (is (not (contains? (second @calls) :task)))
      (is (not (contains? (:acceptance-payload (second @calls)) :task))))))
