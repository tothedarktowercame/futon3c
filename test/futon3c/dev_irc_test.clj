(ns futon3c.dev-irc-test
  (:require [clojure.test :refer [deftest is testing]]
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
