(ns scripts.joint-par-server
  "Persistent server for live joint PAR sessions.

   Boots HTTP+WS + IRC + relay bridge and blocks. Agents connect via WS,
   humans observe via IRC. Evidence accumulates in memory (printed on SIGINT).

   Run:
     clojure -Sdeps '{:paths [\"src\" \"resources\" \"library\" \".\"]}' -M -m scripts.joint-par-server

   Connect agents:
     WS: ws://127.0.0.1:5056/ws?agent_id=claude-1&session_id=...
     WS: ws://127.0.0.1:5056/ws?agent_id=codex-1&session_id=...

   Observe via IRC:
     irssi -c localhost -p 6667 -n observer
     /join #futon-par

   Query evidence:
     curl http://localhost:5056/api/alpha/evidence | jq"
  (:require [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.runtime.agents :as runtime]
            [futon3c.transport.http :as http]
            [futon3c.transport.irc :as irc]
            [futon3c.transport.ws :as ws]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]))

(defn- now-str [] (str (Instant/now)))

(defn- find-channel-for-agent [connections-atom agent-id]
  (some (fn [[ch state]]
          (when (and (= agent-id (:agent-id state))
                     (:connected? state))
            ch))
        @connections-atom))

(let [ws-port (Integer/parseInt (or (System/getenv "FUTON3C_PORT") "5056"))
      irc-port (Integer/parseInt (or (System/getenv "FUTON3C_IRC_PORT") "6667"))
      evidence-store (atom {:entries {} :order []})
      mission-id (str "live-joint-par-" (.toString (java.util.UUID/randomUUID)))]

  ;; Reset and register agents
  (reg/reset-registry!)
  (runtime/register-claude!
   {:agent-id "claude-1"
    :invoke-fn (fn [_ _] {:result "live-session" :session-id nil})})
  (runtime/register-codex!
   {:agent-id "codex-1"
    :invoke-fn (fn [_ _] {:result "live-session" :session-id nil})})

  (let [periph-config (runtime/make-default-peripheral-config
                       {:cwd (System/getProperty "user.dir")
                        :evidence-store evidence-store
                        :timeout-ms 30000})
        irc-server (irc/start-irc-server! {:port irc-port
                                           :evidence-store evidence-store})
        relay-bridge (irc/make-relay-bridge {:evidence-store evidence-store})
        _ ((:set-irc-send-fn! relay-bridge) (:send-to-channel! irc-server))
        !connections-ref (atom nil)
        on-connect (fn [agent-id]
                     (println (str "[" (now-str) "] Agent connected: " agent-id))
                     (when-let [conns @!connections-ref]
                       (when-let [ch (find-channel-for-agent conns agent-id)]
                         ((:join-agent! relay-bridge)
                          agent-id agent-id "#futon-par"
                          (fn [msg] (hk/send! ch msg)))
                         ((:join-virtual-nick! irc-server) "#futon-par" agent-id))))
        on-disconnect (fn [agent-id]
                        (println (str "[" (now-str) "] Agent disconnected: " agent-id))
                        ((:part-agent! relay-bridge) agent-id))
        opts {:patterns {:patterns/ids []}
              :peripheral-config periph-config
              :irc-interceptor (:irc-interceptor relay-bridge)
              :on-connect on-connect
              :on-disconnect on-disconnect}
        http-handler (runtime/make-http-handler opts)
        {:keys [handler connections]} (runtime/make-ws-handler opts)
        _ (reset! !connections-ref connections)
        app (fn [req]
              (if (= "/ws" (:uri req))
                (handler req)
                (http-handler req)))
        result (http/start-server! app ws-port)]

    ;; Emit mission-start evidence
    (estore/append* evidence-store
                    {:evidence/id (str "mission-" mission-id)
                     :evidence/subject {:ref/type :mission :ref/id mission-id}
                     :evidence/type :coordination
                     :evidence/claim-type :goal
                     :evidence/author "mission-orchestrator"
                     :evidence/at (now-str)
                     :evidence/body {:event :mission-start
                                     :mission-id mission-id
                                     :channel "#futon-par"}
                     :evidence/tags [:mission :joint-par]
                     :evidence/session-id (str "mission-" mission-id)})

    (println)
    (println "╔══════════════════════════════════════════════════════════╗")
    (println "║  futon3c joint PAR server — waiting for live agents    ║")
    (println "╚══════════════════════════════════════════════════════════╝")
    (println)
    (println (str "  HTTP+WS:    http://127.0.0.1:" (:port result)))
    (println (str "  IRC:        localhost:" (:port irc-server)))
    (println (str "  Mission:    " mission-id))
    (println (str "  Evidence:   curl http://127.0.0.1:" (:port result) "/api/alpha/evidence"))
    (println)
    (println "  Waiting for agents to connect...")
    (println)

    ;; Shutdown hook: print evidence summary
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      (fn []
        (println)
        (println "── Evidence summary ──")
        (let [entries (vals (:entries @evidence-store))]
          (println (str "  Total entries: " (count entries)))
          (doseq [e (sort-by :evidence/at entries)]
            (println (str "  " (name (or (:evidence/claim-type e) :?))
                          " | " (name (or (:evidence/type e) :?))
                          " | " (or (:evidence/author e) "?")
                          " | " (let [b (:evidence/body e)]
                                  (cond
                                    (and (map? b) (:event b)) (name (:event b))
                                    (and (map? b) (:tool b)) (name (:tool b))
                                    (and (map? b) (:text b)) (subs (:text b) 0 (min 60 (count (:text b))))
                                    (and (map? b) (:what-worked b)) "JOINT-PAR"
                                    :else (str (first (keys b)))))))))
        (when-let [stop (:server result)] (stop))
        (when-let [stop (:stop-fn irc-server)] (stop)))))

    ;; Block forever
    @(promise)))
