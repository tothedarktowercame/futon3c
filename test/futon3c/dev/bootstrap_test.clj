(ns futon3c.dev.bootstrap-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.dev.bootstrap :as bootstrap]
            [futon3c.dev.config :as config]))

(deftest artifact-live-copy-is-registered-but-not-run-at-startup
  (testing "boot preserves the explicit probe without paying for its repo scan"
    (let [source (slurp (io/resource "futon3c/dev/bootstrap.clj"))]
      (is (str/includes? source "(locus/register-locus-taps!)"))
      (is (not (str/includes?
                source
                "(locus/check-artifact-live-copy-locus-on-load!"))))))

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

;; --- futon1b embed preflight (I-0) ----------------------------------------
;; Regression cover for the self-defeating startup of 2026-07-16: a taken :7074
;; died on a bare BindException, and dev-linode-env's embed-blind health check
;; told the operator to start the very unit that would cause it.

(deftest port-in-use-detects-a-live-listener
  (testing "an occupied port reads as in-use, and a free one does not"
    (with-open [held (java.net.ServerSocket.)]
      (.bind held (java.net.InetSocketAddress. 0))
      (let [taken (.getLocalPort held)]
        (is (true? (#'bootstrap/port-in-use? taken))
            "a bound port is detected before we pay for a store open")
        (let [free (with-open [probe (java.net.ServerSocket.)]
                     (.bind probe (java.net.InetSocketAddress. 0))
                     (.getLocalPort probe))]
          (is (false? (#'bootstrap/port-in-use? free))
              "a released port is bindable"))))))

(deftest futon1b-health-check-is-embed-aware
  (testing "every launcher gates its futon1b-server check on FUTON1B_EMBED"
    (doseq [path ["scripts/dev-linode-env" "scripts/dev-linode2-env"]]
      (let [source (slurp path)
            export (.indexOf source "export FUTON1B_EMBED=")
            embed-guard (.indexOf source "if [[ \"${FUTON1B_EMBED}\" != \"0\" ]]; then")
            check (.indexOf source "systemctl --user is-active futon1b-server.service")]
        (is (<= 0 embed-guard)
            (str path " gates the check on FUTON1B_EMBED, not unconditionally"))
        (is (< embed-guard check)
            (str path " gates before the first futon1b-server probe"))
        ;; These launchers run under `set -u`, so an unexported FUTON1B_EMBED
        ;; would abort the script at the guard rather than fall through.
        (is (< export embed-guard)
            (str path " exports FUTON1B_EMBED before the guard reads it"))
        (is (<= 0 (.indexOf source "stop it with: systemctl --user stop futon1b-server"))
            (str path " reports a running unit as the fault under embed"))))))

(deftest launchers-pin-their-own-futon1b-store
  (testing "a launcher that can embed names its store rather than inheriting
            bootstrap.clj's switchover-store default, which belongs to lucy"
    (doseq [[path store] {"scripts/dev-linode-env" "switchover-store"
                          "scripts/dev-linode2-env" "chicago-store"}]
      (let [source (slurp path)]
        (is (<= 0 (.indexOf source (str "export FUTON1B_STORE_DIR=")))
            (str path " pins FUTON1B_STORE_DIR"))
        (is (<= 0 (.indexOf source store))
            (str path " pins it to " store))))))
