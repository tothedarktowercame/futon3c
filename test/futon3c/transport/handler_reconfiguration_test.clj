(ns futon3c.transport.handler-reconfiguration-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.transport.http :as http]
            [futon3c.runtime.agents :as runtime]))

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

(deftest failed-handler-construction-preserves-rebuild-source
  (http/rebuild-handler! (http/make-handler {:irc-send-base "retained"}))
  (with-redefs [http/make-handler
                (fn [_] (throw (ex-info "injected construction failure" {})))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (http/reconfigure-handler! #(assoc % :irc-send-base "rejected"))))
    (is (= "retained" (:irc-send-base (health)))))
  (http/rebuild-handler!)
  (is (= "retained" (:irc-send-base (health)))))

(deftest composed-handler-preserves-websocket-routing
  (let [calls (atom [])
        ws (fn [request] (swap! calls conj request) {:status 101})
        app (http/compose-http-websocket-handler
             (http/make-handler {:irc-send-base "original"}) ws)]
    (http/rebuild-handler! app)
    (is (= "original" (:irc-send-base (health))))
    (is (= 101 (:status (installed-request {:websocket? true :id 1}))))
    (http/reconfigure-handler! #(assoc % :irc-send-base "updated"))
    (is (= "updated" (:irc-send-base (health))))
    (is (= 101 (:status (installed-request {:websocket? true :id 2}))))
    (with-redefs [http/make-handler
                  (fn [_] (throw (ex-info "construction refused" {})))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (http/reconfigure-handler! #(assoc % :irc-send-base "bad")))))
    (is (= "updated" (:irc-send-base (health))))
    (http/rebuild-handler!)
    (is (= "updated" (:irc-send-base (health))))
    (is (= 101 (:status (installed-request {:websocket? true :id 3}))))
    (is (= [1 2 3] (mapv :id @calls)))))

(deftest runtime-forwards-explicit-run4-configuration
  (let [captured (atom nil)]
    (with-redefs [runtime/runtime-config (constantly {:patterns {:patterns/ids []}})
                  http/make-handler (fn [config]
                                      (reset! captured config)
                                      (fn [_] {:status 200}))]
      (runtime/make-http-handler {:run4 {:enabled? false}})
      (is (= {:enabled? false} (:run4 @captured)))
      (runtime/make-http-handler {})
      (is (not (contains? @captured :run4))))))
