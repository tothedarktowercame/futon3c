(ns futon3c.transport.handler-reconfiguration-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.transport.http :as http]))

(defn- installed-request [request]
  ((var-get (ns-resolve 'futon3c.transport.http 'installed-handler)) request))

(defn- health []
  (-> (installed-request {:request-method :get :uri "/health"})
      :body
      (json/parse-string true)))

(deftest structural-reconfiguration-is-atomic-and-rebuildable
  (let [original {:patterns {:patterns/ids [:retained]}
                  :irc-send-fn (fn [_ _ _] :ok)
                  :irc-send-base "before"}]
    (http/rebuild-handler! (http/make-handler original))
    (is (= "before" (:irc-send-base (health))))
    (is (true? (:irc-relay-configured (health))))
    (is (= {:ok true :status :handler-reconfigured :run4-configured? true}
           (http/reconfigure-handler!
            #(-> %
                 (assoc :irc-send-base "after")
                 (assoc :run4 {:enabled? false})))))
    (is (= "after" (:irc-send-base (health))))
    (testing "failed transform leaves the active handler unchanged"
      (is (thrown? clojure.lang.ExceptionInfo
                   (http/reconfigure-handler!
                    (fn [_] (throw (ex-info "refuse" {}))))))
      (is (= "after" (:irc-send-base (health)))))
    (testing "ordinary canonical rebuild retains transformed configuration"
      (http/rebuild-handler!)
      (is (= "after" (:irc-send-base (health))))
      (is (true? (:irc-relay-configured (health)))))))

(deftest invalid-transform-result-does-not-replace-handler
  (http/rebuild-handler! (http/make-handler {:irc-send-base "stable"}))
  (is (thrown? clojure.lang.ExceptionInfo
               (http/reconfigure-handler! (constantly nil))))
  (is (= "stable" (:irc-send-base (health)))))
