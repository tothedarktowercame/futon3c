(ns futon3c.dev.bootstrap-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.dev.bootstrap :as bootstrap]
            [futon3c.dev.config :as config]))

(deftest start-webarxana-wraps-resolved-vars-as-functions
  (testing "start-webarxana! exposes real functions for CYDER registration"
    (let [started-with (atom nil)
          stopped? (atom false)
          status-called? (atom false)
          start-var (intern *ns* (gensym "fake-start-")
                            (fn [opts]
                              (reset! started-with opts)
                              {:server :ok}))
          status-var (intern *ns* (gensym "fake-status-")
                             (fn []
                               (reset! status-called? true)
                               {:running? true}))
          stop-var (intern *ns* (gensym "fake-stop-")
                           (fn []
                             (reset! stopped? true)
                             :stopped))]
      (with-redefs [config/env-bool (fn
                                      ([_] false)
                                      ([_ default] default))
                    config/env-int (fn
                                     ([_] nil)
                                     ([k default]
                                      (case k
                                        "FUTON3C_WEBARXANA_PORT" 3100
                                        "FUTON1A_PORT" 7071
                                        default)))
                    config/env (fn
                                 ([_] nil)
                                 ([k default]
                                  (case k
                                    "FUTON4_BASE_URL" default
                                    "FUTON3C_EMACS_SOCKET" "server"
                                    default)))
                    clojure.core/requiring-resolve
                    (fn [sym]
                      (case sym
                        webarxana.server.core/start! start-var
                        webarxana.server.core/status status-var
                        webarxana.server.core/stop! stop-var
                        (throw (ex-info "unexpected requiring-resolve" {:sym sym}))))]
        (let [system (bootstrap/start-webarxana!)]
          (is (= {:port 3100
                  :futon1a-url "http://127.0.0.1:7071"
                  :emacs-socket "server"}
                 @started-with))
          (is (fn? (:stop-fn system)))
          (is (fn? (:state-fn system)))
          (is (= {:running? true} ((:state-fn system))))
          (is @status-called?)
          (is (= :stopped ((:stop-fn system))))
          (is @stopped?))))))
