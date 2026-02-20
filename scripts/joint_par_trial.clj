(ns scripts.joint-par-trial
  "Joint PAR trial: Claude + Codex paired session with IRC coordination.

   Two agents connect via WS, run parallel peripheral sessions (explore + edit),
   coordinate over IRC (#futon-par), and produce a jointly authored PAR that
   lands as a :reflection entry in the evidence landscape. The PAR is the
   crossing point where two independent linear evidence histories converge.

   Exercises the full tri-layer stack:
     transport (IRC + WS) → peripherals (explore + edit) → evidence (shared
     landscape with cross-agent reply chains)

   Run:
     clojure -Sdeps '{:paths [\"src\" \"resources\" \"library\" \".\"]}' -M -m scripts.joint-par-trial

   Connect an IRC client to observe:
     irssi -c localhost -p 6667 -n observer
     /join #futon-par

   Environment:
     FUTON3C_PORT     — HTTP+WS server port (default: 5056)
     FUTON3C_IRC_PORT — IRC server port (default: 6667)"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.registry :as preg]
            [futon3c.runtime.agents :as runtime]
            [futon3c.transport.http :as http]
            [futon3c.transport.irc :as irc]
            [futon3c.transport.ws :as ws]
            [org.httpkit.server :as hk])
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent CompletableFuture]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env [k default]
  (or (System/getenv k) default))

(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(defn- now-str [] (str (Instant/now)))

(def ^:private cwd (System/getProperty "user.dir"))

(defn- pass [label]
  (println (str "  ✓ " label)))

(defn- fail [label & [detail]]
  (println (str "  ✗ " label (when detail (str " — " detail))))
  (System/exit 1))

(defn- section [label]
  (println)
  (println (str "── " label " ──")))

;; =============================================================================
;; WS client (Java 11+ built-in)
;; =============================================================================

(defn- make-ws-listener
  [frames errors opened-p closed-p]
  (let [!buffer (atom (StringBuilder.))]
    (reify WebSocket$Listener
      (onOpen [_ web-socket]
        (deliver opened-p web-socket)
        (.request web-socket 1))
      (onText [_ web-socket data last]
        (.append ^StringBuilder @!buffer data)
        (when last
          (swap! frames conj (.toString ^StringBuilder @!buffer))
          (reset! !buffer (StringBuilder.)))
        (.request web-socket 1)
        (CompletableFuture/completedFuture nil))
      (onClose [_ _web-socket _status-code _reason]
        (deliver closed-p true)
        (CompletableFuture/completedFuture nil))
      (onError [_ _web-socket error]
        (swap! errors conj (str error))))))

(defn- ws-connect!
  [^HttpClient client url]
  (let [frames (atom [])
        errors (atom [])
        opened-p (promise)
        closed-p (promise)
        listener (make-ws-listener frames errors opened-p closed-p)
        ws-client (.join (.buildAsync (.newWebSocketBuilder client)
                                      (URI/create url)
                                      listener))]
    {:ws ws-client :frames frames :errors errors
     :opened opened-p :closed closed-p}))

(defn- send-json! [^WebSocket ws payload]
  (.join (.sendText ws (json/generate-string payload) true)))

(defn- wait-for-frame
  [frames pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [found (some (fn [raw]
                          (let [f (json/parse-string raw true)]
                            (when (pred f) f)))
                        @frames)]
        (cond
          found found
          (> (System/currentTimeMillis) deadline) nil
          :else (do (Thread/sleep 50) (recur)))))))

;; =============================================================================
;; Server setup
;; =============================================================================

(defn- find-channel-for-agent
  "Find the http-kit channel for a connected agent by scanning WS connections."
  [connections-atom agent-id]
  (some (fn [[ch state]]
          (when (and (= agent-id (:agent-id state))
                     (:connected? state))
            ch))
        @connections-atom))

(defn- start-trial-server!
  [ws-port irc-port evidence-store]
  ;; Reset registries
  (reg/reset-registry!)

  ;; Register both agents
  (runtime/register-claude!
   {:agent-id "claude-1"
    :invoke-fn (fn [_ _] {:result "joint-par-trial" :session-id nil})})
  (runtime/register-codex!
   {:agent-id "codex-1"
    :invoke-fn (fn [_ _] {:result "joint-par-trial" :session-id nil})})

  (let [periph-config (runtime/make-default-peripheral-config
                       {:cwd cwd
                        :evidence-store evidence-store
                        :timeout-ms 30000})
        ;; Start IRC server
        irc-server (irc/start-irc-server! {:port irc-port
                                           :evidence-store evidence-store})

        ;; Create relay bridge
        relay-bridge (irc/make-relay-bridge {:evidence-store evidence-store})

        ;; Wire relay bridge → IRC server
        _ ((:set-irc-send-fn! relay-bridge) (:send-to-channel! irc-server))

        ;; Atom to break circularity: on-connect needs connections, which
        ;; comes from make-ws-handler which takes on-connect
        !connections-ref (atom nil)

        on-connect
        (fn [agent-id]
          (when-let [conns @!connections-ref]
            (when-let [ch (find-channel-for-agent conns agent-id)]
              ;; Register agent in relay bridge with their WS send-fn
              ((:join-agent! relay-bridge)
               agent-id agent-id "#futon-par"
               (fn [msg] (hk/send! ch msg)))
              ;; Add virtual nick to IRC room (visible to human observers)
              ((:join-virtual-nick! irc-server) "#futon-par" agent-id))))

        opts {:patterns {:patterns/ids []}
              :peripheral-config periph-config
              :irc-interceptor (:irc-interceptor relay-bridge)
              :on-connect on-connect}
        http-handler (runtime/make-http-handler opts)
        {:keys [handler connections]} (runtime/make-ws-handler opts)
        _ (reset! !connections-ref connections)
        app (fn [req]
              (if (= "/ws" (:uri req))
                (handler req)
                (http-handler req)))
        result (http/start-server! app ws-port)]

    {:stop-ws (:server result) :ws-port (:port result)
     :stop-irc (:stop-fn irc-server) :irc-port (:port irc-server)
     :connections connections
     :relay-bridge relay-bridge
     :irc-server irc-server}))

;; =============================================================================
;; Trial phases
;; =============================================================================

(defn- phase-1-connect!
  "Connect both agents via WS and complete readiness handshake."
  [http-client port]
  (section "Phase 1: Dual-agent WS connect + readiness handshake")
  (let [clients
        (into {}
              (for [agent-id ["claude-1" "codex-1"]]
                (let [session-id (str "joint-par-" agent-id "-" (UUID/randomUUID))
                      url (str "ws://127.0.0.1:" port
                               "/ws?agent_id=" agent-id
                               "&session_id=" session-id)
                      client (ws-connect! http-client url)]
                  (when-not (deref (:opened client) 3000 nil)
                    (fail (str agent-id " WS connect") "timeout"))
                  (send-json! (:ws client) {"type" "ready"
                                             "agent_id" agent-id
                                             "session_id" session-id})
                  (let [ack (wait-for-frame (:frames client)
                                            #(= "ready_ack" (:type %))
                                            3000)]
                    (when-not ack
                      (fail (str agent-id " handshake") "no ready_ack"))
                    (pass (str agent-id " connected (session: " session-id ")")))
                  [agent-id (assoc client :session-id session-id)])))]
    ;; Small delay for on-connect hooks to register agents in relay bridge
    (Thread/sleep 200)
    clients))

(defn- phase-2-peripheral!
  "Run parallel peripheral sessions: Claude explores, Codex edits."
  [clients]
  (section "Phase 2: Parallel peripheral sessions")

  ;; --- Claude: explore ---
  (let [claude (:ws (get clients "claude-1"))]
    (reset! (:frames (get clients "claude-1")) [])
    (send-json! claude {"type" "peripheral_start" "peripheral_id" "explore"})
    (let [started (wait-for-frame (:frames (get clients "claude-1"))
                                  #(= "peripheral_started" (:type %))
                                  5000)]
      (when-not started
        (fail "claude-1 peripheral_start" "no peripheral_started"))
      (pass (str "claude-1 explore peripheral started (session: " (:session_id started) ")")))

    ;; Read README
    (reset! (:frames (get clients "claude-1")) [])
    (send-json! claude {"type" "tool_action" "tool" "read"
                          "args" [(str cwd "/README.md")]})
    (let [result (wait-for-frame (:frames (get clients "claude-1"))
                                 #(= "tool_result" (:type %)) 5000)]
      (when-not result (fail "claude-1 read" "no tool_result"))
      (if (.contains ^String (str (:result result)) "Futon3c")
        (pass "claude-1 read README.md — found 'Futon3c'")
        (fail "claude-1 read" "README doesn't mention Futon3c")))

    ;; Glob peripherals
    (reset! (:frames (get clients "claude-1")) [])
    (send-json! claude {"type" "tool_action" "tool" "glob"
                          "args" ["src/futon3c/peripheral/*.clj"]})
    (let [result (wait-for-frame (:frames (get clients "claude-1"))
                                 #(= "tool_result" (:type %)) 5000)]
      (when-not result (fail "claude-1 glob" "no tool_result"))
      (if (.contains ^String (str (:result result)) "explore.clj")
        (pass "claude-1 glob peripheral/*.clj — found explore.clj")
        (fail "claude-1 glob" (str "expected explore.clj in: " (:result result)))))

    ;; Grep for PeripheralRunner
    (reset! (:frames (get clients "claude-1")) [])
    (send-json! claude {"type" "tool_action" "tool" "grep"
                          "args" ["defprotocol PeripheralRunner"
                                  "src/futon3c/peripheral/runner.clj"]})
    (let [result (wait-for-frame (:frames (get clients "claude-1"))
                                 #(= "tool_result" (:type %)) 5000)]
      (when-not result (fail "claude-1 grep" "no tool_result"))
      (if (.contains ^String (str (:result result)) "PeripheralRunner")
        (pass "claude-1 grep found PeripheralRunner protocol")
        (fail "claude-1 grep" (str "expected PeripheralRunner in: " (:result result))))))

  ;; --- Codex: edit ---
  (let [codex (:ws (get clients "codex-1"))]
    (reset! (:frames (get clients "codex-1")) [])
    (send-json! codex {"type" "peripheral_start" "peripheral_id" "edit"})
    (let [started (wait-for-frame (:frames (get clients "codex-1"))
                                  #(= "peripheral_started" (:type %))
                                  5000)]
      (when-not started
        (fail "codex-1 peripheral_start" "no peripheral_started"))
      (pass (str "codex-1 edit peripheral started (session: " (:session_id started) ")")))

    ;; Write summary file (relative path within edit scope: docs/)
    (reset! (:frames (get clients "codex-1")) [])
    (send-json! codex {"type" "tool_action" "tool" "write"
                          "args" ["docs/joint-par-trial-output.md"
                                  (str "# Joint PAR Trial Summary\n\n"
                                       "Generated by codex-1 during joint PAR trial.\n"
                                       "Claude explored: README.md, peripheral/*.clj, PeripheralRunner.\n"
                                       "Codex wrote this summary file.\n"
                                       "Coordination via IRC #futon-par.\n")]})
    (let [result (wait-for-frame (:frames (get clients "codex-1"))
                                 #(= "tool_result" (:type %)) 5000)]
      (when-not result (fail "codex-1 write" "no tool_result"))
      (pass "codex-1 wrote docs/joint-par-trial-output.md"))))

(defn- phase-3-stop-peripherals!
  "Stop both peripheral sessions."
  [clients]
  (section "Phase 3: Peripheral stop — harvest fruit")

  (doseq [agent-id ["claude-1" "codex-1"]]
    (let [client (get clients agent-id)]
      (reset! (:frames client) [])
      (send-json! (:ws client) {"type" "peripheral_stop"
                                  "reason" "joint-par-trial-complete"})
      (let [stopped (wait-for-frame (:frames client)
                                    #(= "peripheral_stopped" (:type %))
                                    5000)]
        (when-not stopped
          (fail (str agent-id " peripheral_stop") "no peripheral_stopped"))
        (pass (str agent-id " peripheral stopped"))))))

(defn- phase-4-irc-coordination!
  "Exchange PAR fragments over IRC."
  [clients]
  (section "Phase 4: PAR coordination over IRC (#futon-par)")

  (let [exchanges
        [["claude-1" "PAR:what-worked: Peripheral read/glob/grep executed cleanly against live codebase. Evidence chain complete (1 goal, 3 steps, 1 conclusion). Self-referential exploration validated."]
         ["codex-1"  "PAR:what-worked: Edit peripheral accepted write to scoped path. Summary file produced from explore findings."]
         ["claude-1" "PAR:what-didnt: Could not verify evidence persistence to XTDB (AtomBackend only). No cross-peripheral evidence linking yet (explore and edit chains are independent)."]
         ["codex-1"  "PAR:what-didnt: Edit scope constraint meant summary had to go to /tmp, not into the repo docs/ directory."]
         ["codex-1"  "PAR:synthesize: Emitting joint PAR now."]]]

    (doseq [[agent-id text] exchanges]
      (let [client (get clients agent-id)]
        (reset! (:frames client) [])
        (send-json! (:ws client) {"type" "irc_response"
                                    "channel" "#futon-par"
                                    "text" text})
        ;; Small delay for evidence emission and relay
        (Thread/sleep 100)
        (pass (str agent-id " → #futon-par: " (subs text 0 (min 60 (count text))) "..."))))))

(defn- phase-5-joint-par!
  "Emit the joint PAR as a :reflection entry — the crossing point."
  [evidence-store mission-id]
  (section "Phase 5: Joint PAR emission — the crossing point")

  ;; Find both agents' conclusion evidence entries
  (let [all-entries (vals (:entries @evidence-store))
        conclusions (filter (fn [e]
                              (and (= :conclusion (:evidence/claim-type e))
                                   (= :coordination (:evidence/type e))
                                   (some #{:peripheral} (:evidence/tags e))))
                            all-entries)
        claude-conclusion (first (filter #(= "claude-1" (:evidence/author %)) conclusions))
        codex-conclusion (first (filter #(= "codex-1" (:evidence/author %)) conclusions))]

    (when-not claude-conclusion
      (fail "Joint PAR" "missing claude-1 conclusion evidence"))
    (when-not codex-conclusion
      (fail "Joint PAR" "missing codex-1 conclusion evidence"))

    (pass (str "Found crossing refs: claude-1=" (:evidence/id claude-conclusion)
               ", codex-1=" (:evidence/id codex-conclusion)))

    ;; Emit the joint PAR — the node where two independent histories cross
    (let [par-id (str "par-joint-" (UUID/randomUUID))
          par-entry
          {:evidence/id par-id
           :evidence/subject {:ref/type :mission :ref/id mission-id}
           :evidence/type :reflection
           :evidence/claim-type :conclusion
           :evidence/author "codex-1"
           :evidence/at (now-str)
           :evidence/body
           {:what-worked (str "Dual peripheral sessions (explore+edit) executed cleanly. "
                              "IRC coordination produced visible, evidence-backed exchange. "
                              "Self-evidencing pattern validated: the system documented its own "
                              "functioning through its own infrastructure.")
            :what-didnt (str "XTDB persistence not exercised (AtomBackend only). "
                             "Edit scope constraint limits output location. "
                             "PAR negotiation was turn-based, not concurrent.")
            :suggestions ["Add XTDB variant of trial for durable persistence"
                          "Consider hop protocol to unify explore→edit into single evidence chain"
                          "Explore CRDT-based concurrent PAR editing in Emacs"]
            :contributors [{:agent-id "claude-1"
                            :contributed [:what-worked :what-didnt]
                            :via :irc}
                           {:agent-id "codex-1"
                            :contributed [:what-worked :what-didnt :suggestions]
                            :via :irc}]
            :synthesized-by "codex-1"
            :coordination-channel "#futon-par"
            :mission-id mission-id
            ;; The crossing: references to both agents' peripheral conclusions
            :crossing-refs {:claude-1 (:evidence/id claude-conclusion)
                            :codex-1 (:evidence/id codex-conclusion)}}
           :evidence/tags [:mission :joint-par :reflection :par]
           :evidence/session-id (str "mission-" mission-id)}]

      (estore/append* evidence-store par-entry)
      (pass (str "Joint PAR emitted: " par-id))
      (pass "PAR subject: {:ref/type :mission}")
      (pass (str "Crossing refs point to both agents' conclusions"))
      par-entry)))

(defn- phase-6-verify!
  "Query evidence and verify the complete diamond structure."
  [evidence-store mission-id]
  (section "Phase 6: Evidence verification — the diamond")

  (let [all-entries (vals (:entries @evidence-store))
        by-type (group-by :evidence/type all-entries)
        by-claim (group-by :evidence/claim-type all-entries)
        by-author (group-by :evidence/author all-entries)

        ;; Peripheral evidence
        claude-entries (filter #(and (= "claude-1" (:evidence/author %))
                                     (contains? (set (:evidence/tags %)) :peripheral))
                               all-entries)
        codex-entries (filter #(and (= "codex-1" (:evidence/author %))
                                    (contains? (set (:evidence/tags %)) :peripheral))
                              all-entries)

        ;; IRC evidence
        irc-posts (get by-type :forum-post)

        ;; PAR
        reflections (filter #(= :reflection (:evidence/type %)) all-entries)
        joint-par (first (filter #(= :mission
                                      (:ref/type (:evidence/subject %)))
                                 reflections))]

    (pass (str "Total evidence entries: " (count all-entries)))

    ;; Claude explore chain
    (if (>= (count claude-entries) 5)
      (pass (str "claude-1 explore chain: " (count claude-entries) " entries "
                 "(goal + 3 steps + conclusion)"))
      (fail "claude-1 explore chain" (str "expected ≥5, got " (count claude-entries))))

    ;; Codex edit chain
    (if (>= (count codex-entries) 3)
      (pass (str "codex-1 edit chain: " (count codex-entries) " entries "
                 "(goal + 1 step + conclusion)"))
      (fail "codex-1 edit chain" (str "expected ≥3, got " (count codex-entries))))

    ;; IRC coordination
    (if (>= (count irc-posts) 5)
      (pass (str "IRC :forum-post entries: " (count irc-posts)
                 " (5 PAR exchanges)"))
      (fail "IRC evidence" (str "expected ≥5 forum-posts, got " (count irc-posts))))

    ;; Verify IRC entries have correct subject
    (let [irc-subjects (map #(get-in % [:evidence/subject :ref/type]) irc-posts)
          all-thread? (every? #(= :thread %) irc-subjects)]
      (if all-thread?
        (pass "All IRC entries have subject {:ref/type :thread}")
        (fail "IRC subjects" (str "expected all :thread, got " (distinct irc-subjects)))))

    ;; Joint PAR
    (if joint-par
      (do
        (pass "Joint PAR :reflection entry found on mission subject")
        (let [body (:evidence/body joint-par)]
          (if (:what-worked body)
            (pass "PAR has :what-worked")
            (fail "PAR shape" "missing :what-worked"))
          (if (:what-didnt body)
            (pass "PAR has :what-didnt")
            (fail "PAR shape" "missing :what-didnt"))
          (if (seq (:suggestions body))
            (pass (str "PAR has " (count (:suggestions body)) " suggestions"))
            (fail "PAR shape" "missing :suggestions"))
          (if (:crossing-refs body)
            (pass (str "PAR crossing-refs: " (pr-str (:crossing-refs body))))
            (fail "PAR shape" "missing :crossing-refs — no evidence of coordination"))))
      (fail "Joint PAR" "no :reflection entry on mission subject"))

    ;; Print the diamond
    (println)
    (println "  Evidence diamond:")
    (println)
    (println (str "    claude-1:  goal → step × 3 → conclusion ─┐"))
    (println (str "                                               ├──→ joint PAR"))
    (println (str "    codex-1:   goal → step × 1 → conclusion ─┘"))
    (println (str "                    (session subjects)     (mission subject)"))
    (println)
    (println "  Full evidence timeline:")
    (doseq [e (sort-by :evidence/at all-entries)]
      (println (str "    " (name (or (:evidence/claim-type e) :?))
                    " | " (name (or (:evidence/type e) :?))
                    " | " (or (:evidence/author e) "?")
                    " | " (some-> (:evidence/at e) str (subs 11 19))
                    " | " (let [b (:evidence/body e)]
                            (cond
                              (and (map? b) (:event b)) (name (:event b))
                              (and (map? b) (:tool b)) (name (:tool b))
                              (and (map? b) (:text b)) (subs (:text b) 0 (min 50 (count (:text b))))
                              (and (map? b) (:what-worked b)) "JOINT-PAR"
                              :else (str (first (keys b))))))))))

;; =============================================================================
;; Main
;; =============================================================================

(let [ws-port (parse-int (env "FUTON3C_PORT" "5056") 5056)
      irc-port (parse-int (env "FUTON3C_IRC_PORT" "6667") 6667)
      mission-id (str "joint-par-" (.toString (java.util.UUID/randomUUID)))
      evidence-store (atom {:entries {} :order []})
      http-client (HttpClient/newHttpClient)]

  (println)
  (println "╔══════════════════════════════════════════════════════════╗")
  (println "║  futon3c joint PAR trial                                ║")
  (println "║  Two agents coordinate over IRC to produce a shared    ║")
  (println "║  PAR — the crossing point of independent histories.    ║")
  (println "╚══════════════════════════════════════════════════════════╝")
  (println)
  (println (str "  cwd:         " cwd))
  (println (str "  ws-port:     " ws-port))
  (println (str "  irc-port:    " irc-port))
  (println (str "  mission-id:  " mission-id))

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
                                   :contributors [{:agent-id "claude-1" :role :explore}
                                                  {:agent-id "codex-1" :role :edit}]
                                   :channel "#futon-par"}
                   :evidence/tags [:mission :joint-par]
                   :evidence/session-id (str "mission-" mission-id)})

  (let [server (start-trial-server! ws-port irc-port evidence-store)]
    (try
      (pass (str "HTTP+WS server on port " (:ws-port server)))
      (pass (str "IRC server on port " (:irc-port server)))

      (let [clients (phase-1-connect! http-client (:ws-port server))]
        (phase-2-peripheral! clients)
        (phase-3-stop-peripherals! clients)
        (Thread/sleep 200) ;; evidence persistence delay
        (phase-4-irc-coordination! clients)
        (Thread/sleep 200)
        (phase-5-joint-par! evidence-store mission-id)
        (phase-6-verify! evidence-store mission-id)

        ;; Close WS connections
        (doseq [[_ client] clients]
          (.sendClose ^WebSocket (:ws client) 1000 "trial-complete")))

      (section "Result")
      (println)
      (println "  All phases passed. Two histories crossed at the PAR.")
      (println (str "  Connect to IRC localhost:" (:irc-port server)
                    " #futon-par to observe."))
      (println)

      (finally
        (when-let [stop (:stop-ws server)] (stop))
        (when-let [stop (:stop-irc server)] (stop))
        (shutdown-agents)
        (System/exit 0)))))
