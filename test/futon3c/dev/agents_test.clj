(ns futon3c.dev.agents-test
  "Regression tests for codex registration's reachability gate.

   Guards the 2026-06-06 fix: a configured-but-down peer (e.g. a linode whose
   Agency isn't running) must make codex register LOCALLY, not as a remote
   ws-bridge agent with decoupled reporting. Before the fix the decision was
   purely structural (a peer URL is set) and ignored whether the peer answered."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.dev.agents :as agents]
            [futon3c.dev.config :as config]))

(def ^:private reg-config #'agents/codex-registration-config)

(defn- with-remote-peer
  "Run (f) with a remote linode peer URL configured, agency-reachable? forced
   to REACHABLE?. Returns the codex-registration-config decision map."
  [reachable?]
  (with-redefs [config/env (fn
                             ([k] (when (= k "FUTON3C_LINODE_URL")
                                    "http://172.236.28.208:7070"))
                             ([k default] (or (when (= k "FUTON3C_LINODE_URL")
                                                "http://172.236.28.208:7070")
                                              default)))
                config/first-peer-url (constantly "http://172.236.28.208:7070")
                config/agency-reachable? (fn [& _] reachable?)]
    (reg-config {:f3c-sys {:port 7070} :role :laptop :codex-ws-bridge? true})))

(deftest unreachable-peer-falls-back-to-local
  (testing "down peer ⇒ ws-bridge disabled, target not remote (local inline invoke)"
    (let [cfg (with-remote-peer false)]
      (is (false? (:ws-bridge-enabled? cfg))
          "ws-bridge must be disabled when the peer Agency is unreachable")
      (is (false? (:remote-ws-target? cfg))
          "must not register as remote when the peer is unreachable"))))

(deftest reachable-peer-enables-remote-bridge
  (testing "live peer ⇒ ws-bridge enabled and target remote"
    (let [cfg (with-remote-peer true)]
      (is (true? (:ws-bridge-enabled? cfg))
          "ws-bridge must be enabled when the peer Agency answers")
      (is (true? (:remote-ws-target? cfg))
          "must register as remote when the peer is reachable"))))

(deftest agency-reachable-false-on-closed-port
  (testing "a closed port resolves to not-reachable (fast, no false positive)"
    ;; Port 1 on loopback: connection refused ⇒ false. Bounds the call so a
    ;; regression that hangs the probe also fails the assertion.
    (is (false? (config/agency-reachable? "http://127.0.0.1:1" 1000)))
    (is (false? (config/agency-reachable? nil 1000))
        "garbage/blank base ⇒ false, never throws")))
