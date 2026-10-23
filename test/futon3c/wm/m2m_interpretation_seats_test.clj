(ns futon3c.wm.m2m-interpretation-seats-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.runner-service :as service]))
(def seats ["zai-4" "zai-5" "zai-6"])
(deftest request-preserves-ask-seats
  (let [calls (atom []) handler (http/make-handler {})]
    (with-redefs [reg/reset-session! (constantly {:ok true})
                  service/click! (fn [opts] (swap! calls conj opts) {:started true})
                  service/prepare-ordinary-click-opts identity
                  service/cast-preflight-refusal (constantly nil)]
      (is (= 200 (:status (handler {:request-method :post :uri "/api/alpha/wm/click" :headers {}
                                   :body (java.io.ByteArrayInputStream.
                                          (.getBytes (json/generate-string
                                                       {:run-id "2026-10-23-aab6ddc5-924e-46e5-b28a-c12feccf4ddf" :trigger "duree-click-on-demand"
                                                        :interpretation-seats seats}) "UTF-8"))}))))
      (is (= seats (:interpretation-seats (first @calls))))
      (is (= seats (get-in (first @calls) [:acceptance-payload :interpretation-seats]))))))
(deftest child-options-keep-parent-seats
  (let [build (ns-resolve 'futon3c.wm.runner-service 'retry-child-opts)]
    (is (some? build))
    (when build
      (let [child (build {:interpretation-seats seats :idempotency-key "parent"
                          :ordinary-click/issue! :parent-only}
                         {:retry-click "child" :injury-id "injury" :repair-id "repair"})]
        (is (= seats (:interpretation-seats child)))
        (is (= "child" (:accepted-click-id child)))
        (is (not (contains? child :ordinary-click/issue!)))
        (is (= "meta-retry/child" (:idempotency-key child)))))))
