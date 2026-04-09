(ns futon3c.dev-irc-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.dev :as dev]
            [futon3c.dev.irc :as dev-irc]))

(deftest configured-bridge-http-port-honors-env
  (testing "no-arg bridge sender defaults to BRIDGE_HTTP_PORT when configured"
    (with-redefs [futon3c.dev.config/env-int (fn [k default]
                                               (if (= k "BRIDGE_HTTP_PORT")
                                                 7769
                                                 default))]
      (is (= 7769 (#'futon3c.dev.irc/configured-bridge-http-port)))
      (is (= "http://127.0.0.1:7769/say"
             (#'futon3c.dev.irc/bridge-say-url
              (#'futon3c.dev.irc/configured-bridge-http-port)))))))

(deftest configured-bridge-max-lines-defaults-to-four
  (testing "bridge /say keeps the historical cap by default"
    (with-redefs [futon3c.dev.config/env-bool (fn [_k default] default)]
      (is (= 4 (#'futon3c.dev.irc/configured-bridge-max-lines))))))

(deftest configured-bridge-max-lines-allows-no-limit-opt-in
  (testing "bridge /say can opt into unlimited delivery for Matrix-backed rooms"
    (with-redefs [futon3c.dev.config/env-bool (fn [k default]
                                                (if (= k "FUTON3C_MATRIX_REPLY_NO_LIMITS")
                                                  true
                                                  default))]
      (is (= 0 (#'futon3c.dev.irc/configured-bridge-max-lines))))))

(deftest start-dispatch-relay-defaults-to-futon-room
  (testing "dispatch relay keeps the generic room binding by default"
    (let [joined-agent (atom nil)
          joined-virtual (atom nil)
          relay-bridge {:join-agent! (fn [agent-id nick channel ws-send-fn]
                                       (reset! joined-agent {:agent-id agent-id
                                                             :nick nick
                                                             :channel channel
                                                             :ws-send-fn ws-send-fn}))}
          irc-server {:join-virtual-nick! (fn [channel nick]
                                            (reset! joined-virtual {:channel channel
                                                                    :nick nick}))}]
      (is (= {:agent-id "codex-1" :nick "codex"}
             (dev/start-dispatch-relay!
              {:relay-bridge relay-bridge
               :irc-server irc-server
               :agent-id "codex-1"
               :nick "codex"})))
      (is (= "#futon" (:channel @joined-agent)))
      (is (= "#futon" (:channel @joined-virtual))))))

(deftest start-dispatch-relay-honors-explicit-channel
  (testing "dispatch relay can be rebound to #math for the dedicated FrontierMath lane"
    (let [joined-agent (atom nil)
          joined-virtual (atom nil)
          relay-bridge {:join-agent! (fn [agent-id nick channel ws-send-fn]
                                       (reset! joined-agent {:agent-id agent-id
                                                             :nick nick
                                                             :channel channel
                                                             :ws-send-fn ws-send-fn}))}
          irc-server {:join-virtual-nick! (fn [channel nick]
                                            (reset! joined-virtual {:channel channel
                                                                    :nick nick}))}]
      (is (= {:agent-id "codex-1" :nick "codex"}
             (dev/start-dispatch-relay!
              {:relay-bridge relay-bridge
               :irc-server irc-server
               :agent-id "codex-1"
               :nick "codex"
               :channel "#math"})))
      (is (= "#math" (:channel @joined-agent)))
      (is (= "#math" (:channel @joined-virtual))))))
