(ns futon3c.transport.bootstrap-handler-migration-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [futon3c.transport.http :as http]
            [futon3c.transport.bootstrap-handler-migration :as migration]))

;; Compile the exact old bootstrap composition in its original namespace.
;; This exercises the compiler-produced fields used by the live migration.
(defn- legacy-app [http-handler ws-handler]
  (let [n (or (find-ns 'futon3c.dev.bootstrap)
              (create-ns 'futon3c.dev.bootstrap))
        factory (binding [*ns* n]
                  (clojure.core/refer 'clojure.core)
                  (eval '(defn start-futon3c! [http-handler handler]
                           (let [app (fn [request]
                                       (if (:websocket? request)
                                         (handler request)
                                         (http-handler request)))]
                             app))))]
    (factory http-handler ws-handler)))

(defn- active []
  @(var-get (ns-resolve 'futon3c.transport.http '!installed-handler)))

(deftest migrates-retained-handlers-without-invoking-them
  (let [calls (atom [])
        config {:irc-send-base "retained" :patterns {:patterns/ids []}}
        http-handler (http/make-handler config)
        old-http (with-meta http-handler
                   {:futon3c.transport.http/rebuild-fn #(http/make-handler config)})
        ws (fn [r] (swap! calls conj r) {:status 101})]
    (http/rebuild-handler! (legacy-app old-http ws))
    (is (= :bootstrap-composition-migrated (:status (migration/migrate!))))
    (is (empty? @calls))
    (is (= "retained" (:irc-send-base (json/parse-string
                                       (:body ((active) {:request-method :get :uri "/health"})) true))))
    (http/reconfigure-handler! #(assoc % :irc-send-base "updated"))
    (http/rebuild-handler!)
    (is (= "updated" (:irc-send-base (json/parse-string
                                      (:body ((active) {:request-method :get :uri "/health"})) true))))
    (is (= 101 (:status ((active) {:websocket? true :id :retained}))))
    (is (= [{:websocket? true :id :retained}] @calls))))

(deftest invalid-migration-preserves-installed-handler
  (doseq [app [(fn [_] {:status 200})
               (legacy-app (fn [_] {:status 200}) (fn [_] {:status 101}))
               (legacy-app (with-meta (fn [_] {:status 200})
                             {:futon3c.transport.http/rebuild-fn
                              #(throw (ex-info "build failed" {}))})
                           (fn [_] {:status 101}))]]
    (http/rebuild-handler! app)
    (is (thrown? clojure.lang.ExceptionInfo (migration/migrate!)))
    (is (identical? app (active)))))
