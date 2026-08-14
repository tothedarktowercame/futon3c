(ns futon3c.agency.fed-uplink-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.fed-uplink :as fed-uplink]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.transport.protocol :as proto]
            [futon3c.transport.ws :as ws]))

(use-fixtures
  :each
  (fn [f]
    (let [old-token (System/getProperty "FUTON3C_FED_TOKEN")
          old-site (System/getProperty "FUTON3C_SITE")]
      (try
        (System/setProperty "FUTON3C_FED_TOKEN" "test-token")
        (System/setProperty "FUTON3C_SITE" "lon")
        (reg/reset-registry!)
        (f)
        (finally
          (if old-token
            (System/setProperty "FUTON3C_FED_TOKEN" old-token)
            (System/clearProperty "FUTON3C_FED_TOKEN"))
          (if old-site
            (System/setProperty "FUTON3C_SITE" old-site)
            (System/clearProperty "FUTON3C_SITE"))
          (reg/reset-registry!))))))

(defn- make-test-ws []
  (let [sent (atom [])
        closed (atom [])
        callbacks (ws/make-ws-callbacks
                   {:registry {:agents {}}
                    :patterns []
                    :send-fn (fn [ch data] (swap! sent conj {:ch ch :data data}))
                    :close-fn (fn [ch] (swap! closed conj ch))})]
    (assoc callbacks :sent sent :closed closed)))

(defn- parsed-sent
  [sent]
  (mapv #(json/parse-string (:data %) true) @sent))

(defn- announce-frame
  [token roster]
  (proto/render-fed-announce "oxf" token roster))

(deftest uplink-backoff-caps-without-overflow
  (testing "a long outage reaches the cap without overflowing long arithmetic"
    (is (= 60000 (#'fed-uplink/backoff-delay-ms Long/MAX_VALUE)))))

(deftest stopping-uplink-resolves-in-flight-invokes
  (testing "cycling a connection cannot orphan promises owned by the old socket"
    (let [result (promise)
          pending (atom {"invoke-1" result})
          state (var-get #'fed-uplink/!state)
          original @state]
      (try
        (reset! state (assoc original
                             :running? true
                             :connected? true
                             :pending pending))
        (fed-uplink/stop-uplink!)
        (is (= {} @pending))
        (is (= {:ok false :error "Federation uplink stopped"}
               @result))
        (finally
          (reset! state original))))))

(deftest federation-launchers-load-token-before-exec
  (testing "the final JVM inherits the token instead of exec bypassing its loader"
    (doseq [path ["scripts/dev-laptop-env" "scripts/dev-linode-env"]]
      (let [source (slurp path)
            token-index (.indexOf source "export FUTON3C_FED_TOKEN=")
            exec-index (.indexOf source "exec make dev")]
        (is (<= 0 token-index) (str path " loads FUTON3C_FED_TOKEN"))
        (is (< token-index exec-index) (str path " loads the token before exec"))))))

(deftest fed-announce-registers-qualified-uplink-proxies
  (testing "valid announce imports remote agents through the CP-D proxy seam"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :uplink-a]
      (on-open ch {:request-method :get :request-uri "/ws"})
      (on-receive ch
                  (announce-frame
                   "test-token"
                   [{:agent-id "codex-1"
                     :type "codex"
                     :capabilities ["edit"]}
                    {:agent-id "claude-1"
                     :type "claude"
                     :capabilities ["explore"]
                     :home-site "lon"}]))
      (let [proxy (reg/get-agent "oxf-codex-1")
            frames (parsed-sent sent)]
        (is (= :codex (:agent/type proxy)))
        (is (= true (get-in proxy [:agent/metadata :proxy?])))
        (is (= :ws-uplink (get-in proxy [:agent/metadata :federation/transport])))
        (is (= "codex-1" (get-in proxy [:agent/metadata :remote-agent-id])))
        (is (nil? (reg/get-agent "lon-claude-1"))
            "own-site reflection is skipped")
        (is (= "fed_roster" (:type (last frames))))))))

(deftest uplink-proxy-invoke-sends-bare-remote-id-and-resolves
  (testing "invoke on uplink proxy emits fed_invoke with the bare remote id"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :uplink-invoke]
      (on-open ch {:request-method :get :request-uri "/ws"})
      (on-receive ch
                  (announce-frame
                   "test-token"
                   [{:agent-id "codex-1"
                     :type "codex"
                     :capabilities ["edit"]}]))
      (reset! sent [])
      (let [invoke-fn (:agent/invoke-fn (reg/get-agent "oxf-codex-1"))
            result-f (future (invoke-fn "hello" "sess-1"))]
        (Thread/sleep 50)
        (let [invoke-frame (first (parsed-sent sent))
              invoke-id (:invoke_id invoke-frame)]
          (is (= "fed_invoke" (:type invoke-frame)))
          (is (= "codex-1" (:agent_id invoke-frame)))
          (is (= "hello" (:prompt invoke-frame)))
          (on-receive ch
                      (proto/render-fed-invoke-result
                       {:invoke-id invoke-id
                        :ok true
                        :result "done"
                        :session-id "sess-2"}))
          (is (= {:result "done" :session-id "sess-2"} @result-f)))))))

(deftest hub-dispatches-inbound-federated-invoke
  (testing "an authenticated uplink can invoke a hub-visible qualified agent"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :uplink-inbound
          invoked (promise)]
      (on-open ch {:request-method :get :request-uri "/ws"})
      (on-receive ch (announce-frame "test-token" []))
      (reset! sent [])
      (with-redefs [reg/invoke-agent!
                    (fn [agent-id prompt timeout-ms]
                      (deliver invoked [agent-id prompt timeout-ms])
                      {:result "MESH_FIXED_OK" :session-id "sess-zone"})]
        (on-receive ch
                    (proto/render-fed-invoke
                     {:invoke-id "invoke-upstream"
                      :agent-id "ams-claude-3"
                      :prompt "ping"
                      :timeout-ms 1234}))
        (is (= ["ams-claude-3" "ping" 1234]
               (deref invoked 1000 ::timeout)))
        (let [deadline (+ (System/currentTimeMillis) 1000)
              frame (loop []
                      (if-let [frame (first (parsed-sent sent))]
                        frame
                        (when (< (System/currentTimeMillis) deadline)
                          (Thread/sleep 10)
                          (recur))))]
          (is (= {:type "fed_invoke_result"
                  :invoke_id "invoke-upstream"
                  :ok true
                  :result "MESH_FIXED_OK"
                  :session_id "sess-zone"}
                 frame)))))))

(deftest uplink-close-marks-stale-and-prunes-after-missed-announces
  (testing "close marks stale immediately; prune requires three missed announces"
    (let [{:keys [on-open on-receive on-close]} (make-test-ws)
          ch :uplink-close]
      (on-open ch {:request-method :get :request-uri "/ws"})
      (on-receive ch
                  (announce-frame
                   "test-token"
                   [{:agent-id "codex-1"
                     :type "codex"
                     :capabilities ["edit"]}]))
      (on-close ch 1006)
      (is (true? (get-in (reg/get-agent "oxf-codex-1")
                         [:agent/metadata :federation/stale?])))
      (is (some? (reg/get-agent "oxf-codex-1")))
      (federation/mark-uplink-site-stale! "oxf" "missed-announce")
      (federation/mark-uplink-site-stale! "oxf" "missed-announce")
      (is (= [{:agent-id "oxf-codex-1" :action :pruned}]
             (federation/prune-stale-uplink-site! "oxf")))
      (is (nil? (reg/get-agent "oxf-codex-1"))))))

(deftest fed-announce-rejects-bad-token
  (testing "bad token closes the socket and registers nothing"
    (let [{:keys [on-open on-receive closed]} (make-test-ws)
          ch :uplink-bad-token]
      (on-open ch {:request-method :get :request-uri "/ws"})
      (on-receive ch
                  (announce-frame
                   "wrong"
                   [{:agent-id "codex-1"
                     :type "codex"
                     :capabilities ["edit"]}]))
      (is (= [ch] @closed))
      (is (nil? (reg/get-agent "oxf-codex-1"))))))
