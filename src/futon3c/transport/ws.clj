(ns futon3c.transport.ws
  "WebSocket adapter — connection lifecycle mapped to the social pipeline.

   Per realtime/connection-state-machine (L2): connections follow a three-state
   model — :connecting (after on-open), :connected (after readiness handshake),
   :disconnected (after on-close when previously :connected). Only the
   :connected state permits message dispatch. Spurious on-close events during
   TLS handshake (while still :connecting) are ignored without cleanup.

   Per realtime/rendezvous-handshake (R7): WebSocket connections must complete
   a readiness handshake before any messages are processed. The handshake
   verifies agent presence via S-presence.

   Per realtime/structured-events-only (R9): all WS frames are typed JSON
   maps with a 'type' field.

   Pattern references:
   - realtime/connection-state-machine (L2): three-state lifecycle
   - realtime/rendezvous-handshake (R7): readiness handshake before dispatch
   - realtime/request-param-resilience (L1, L3): extract-params on WS upgrade
   - realtime/single-authority-registration (L6): one connection per agent-id
   - realtime/structured-events-only (R9): typed JSON frames only"
  (:require [futon3c.transport.protocol :as proto]
            [futon3c.agency.registry :as reg]
            [futon3c.social.mode :as mode]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.presence :as presence]
            [futon3c.peripheral.registry :as preg]
            [futon3c.peripheral.runner :as runner]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]
           [java.util UUID]))

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- now-str [] (str (Instant/now)))

(defn- error? [x]
  (and (map? x) (contains? x :error/code)))

(defn- transport-error
  "Create a SocialError from the WebSocket transport layer."
  [code message]
  {:error/component :transport
   :error/code code
   :error/message message
   :error/at (now-str)})

(defn- resolve-peripheral-id
  "Resolve which peripheral to start for a connection.
   Uses explicit peripheral-id from frame, or falls back to agent type default."
  [explicit-id agent-id registry]
  (or explicit-id
      (let [agent-entry (get-in registry [:agents agent-id])]
        (dispatch/select-peripheral (:type agent-entry) nil))))

(defn- stop-peripheral!
  "Stop an active peripheral session. Returns the stop result.
   Mutates !connections to clear peripheral state.
   Idempotent: atomically claims the peripheral via compare-and-set!, so only
   one caller executes runner/stop even under close/stop races (Codex #4)."
  [!connections ch _conn reason]
  (loop []
    (let [before @!connections
          current (get before ch)]
      (if-not (:peripheral current)
        nil
        (let [claimed {:peripheral (:peripheral current)
                       :state-atom (:peripheral-state current)}
              after (update before ch dissoc :peripheral :peripheral-state :peripheral-id)]
  (if (compare-and-set! !connections before after)
            (runner/stop (:peripheral claimed) @(:state-atom claimed) reason)
            (recur)))))))

(defn- live-registry
  "Merge static registry snapshot with live registered agents so WS traffic sees
   HTTP-registered/federated agents. Mirrors the HTTP transport behavior."
  [config]
  (let [static-reg (or (:registry config) {})
        live-status (reg/registry-status)
        live-agents (into {}
                          (map (fn [[id {:keys [capabilities type]}]]
                                 [id (cond-> {:capabilities (vec capabilities)}
                                       (some? type) (assoc :type type))]))
                          (:agents live-status))
        merged-agents (merge (:agents static-reg) live-agents)]
    (assoc static-reg :agents merged-agents)))

;; =============================================================================
;; Connection state
;;
;; Per L4 (AsyncChannel doesn't support metadata): per-connection state is
;; stored in a plain map keyed by channel object, not as metadata on the
;; channel. The connections atom is {:channel -> state-map}.
;;
;; State map:
;;   {:channel    <channel-or-id>
;;    :agent-id   <string or nil>     — set from params, confirmed at handshake
;;    :session-id <string or nil>     — set at handshake
;;    :connected? false               — L2: true only after readiness handshake
;;    :opened-at  <instant string>}
;; =============================================================================

;; =============================================================================
;; Lifecycle callbacks — testable without http-kit
;; =============================================================================

(defn make-ws-callbacks
  "Create WebSocket lifecycle callbacks for the social pipeline.

   Callbacks can be used directly in tests (with mock send-fn/close-fn)
   or wrapped by make-ws-handler for http-kit.

   Per realtime/connection-state-machine (L2): on-open puts the connection
   in :connecting state. Only a successful readiness handshake (R7) transitions
   to :connected. Messages are rejected until :connected.

   config:
     :registry      — AgentRegistryShape
     :patterns      — PatternLibrary
     :send-fn       — (fn [ch data]) for sending frames (default: hk/send!)
     :close-fn      — (fn [ch]) for closing connection (default: hk/close)
     :on-connect    — (fn [agent-id]) optional hook after successful handshake
     :on-disconnect — (fn [agent-id]) optional hook after close of connected session
     :irc-interceptor — (fn [ch conn parsed]) optional hook for IRC relay frames;
                        if it returns truthy, normal dispatch is skipped

   Returns:
     {:on-open    (fn [ch request])  — call on WebSocket upgrade
      :on-receive (fn [ch data])     — call on incoming frame
      :on-close   (fn [ch status])   — call on connection close
      :connections atom}             — connection registry for introspection"
  [config]
  (let [!connections (atom {})
        send-fn (or (:send-fn config) hk/send!)
        close-fn (or (:close-fn config) hk/close)
        registry-view #(live-registry config)
        patterns (:patterns config)
        on-connect-hook (:on-connect config)
        on-disconnect-hook (:on-disconnect config)
        irc-interceptor (:irc-interceptor config)]

    {:connections !connections

     ;; -- on-open --
     ;; L3: extract params from upgrade request (handles missing :query-string)
     ;; L2: start in :connecting state, NOT :connected
     :on-open
     (fn on-open [ch request]
       (let [params (proto/extract-params request)
             state {:channel ch
                    :agent-id (:agent-id params)
                    :session-id (:session-id params)
                    :connected? false
                    :transport :websocket
                    :opened-at (now-str)}]
         (swap! !connections assoc ch state)))

     ;; -- on-receive --
     ;; R7: readiness handshake must complete before message dispatch
     ;; R9: all frames must be typed JSON
     :on-receive
     (fn on-receive [ch data]
       (let [conn (get @!connections ch)]
         (if (nil? conn)
           ;; Unknown channel — defensive guard
           (send-fn ch (proto/render-ws-frame
                        (transport-error :unknown-connection
                                         "Connection not registered")))
           (let [parsed (proto/parse-ws-message data)]
             (if (error? parsed)
               (send-fn ch (proto/render-ws-frame parsed))
               (cond
                 ;; --- IRC interceptor (relay frames) ---
                 (and irc-interceptor
                      (= :irc-response (:ws/type parsed))
                      (:connected? conn))
                 (irc-interceptor ch conn parsed)

                 :else
                 (case (:ws/type parsed)

                 ;; --- Readiness handshake (R7) ---
                 :ready
                 (let [agent-id (:agent-id parsed)
                       session-id (or (:session-id parsed)
                                      (str "sess-" (UUID/randomUUID)))
                       ;; Build AgentConnection for S-presence verification
                       conn-event {:conn/id (str "ws-" (UUID/randomUUID))
                                   :conn/transport :websocket
                                   :conn/agent-id {:id/value agent-id
                                                   :id/type :continuity}
                                   :conn/at (now-str)
                                   :conn/metadata {:ready true}}
                       result (presence/verify conn-event (registry-view))]
                   (if (error? result)
                     ;; Handshake failed — send error, close connection
                     (do
                       (send-fn ch (proto/render-ws-frame result))
                       (close-fn ch))
                     ;; Handshake succeeded — mark :connected, send ack
                     (do
                       (swap! !connections assoc ch
                              (assoc conn
                                     :agent-id agent-id
                                     :session-id session-id
                                     :connected? true))
                       (send-fn ch (proto/render-ready-ack))
                       (when on-connect-hook
                         (on-connect-hook agent-id)))))

                 ;; --- Message dispatch ---
                 :message
                 (if-not (:connected? conn)
                   ;; R7: reject messages before handshake
                   (send-fn ch (proto/render-ws-frame
                                (transport-error :not-ready
                                                 "Readiness handshake required before sending messages")))
                   ;; Connected — classify and dispatch
                     (let [;; Override :msg/from with authenticated agent-id
                         ;; (prevents impersonation over WS)
                         agent-id (:agent-id conn)
                         message (assoc parsed
                                        :msg/from {:id/value agent-id
                                                   :id/type :continuity})
                         classified (mode/classify message patterns)
                         registry (registry-view)]
                     (if (error? classified)
                       (send-fn ch (proto/render-ws-frame classified))
                       (let [result (dispatch/dispatch classified registry)]
                         (send-fn ch (proto/render-ws-frame result))))))

                 ;; --- Peripheral session: start (Seam 4) ---
                 :peripheral-start
                 (if-not (:connected? conn)
                   (send-fn ch (proto/render-ws-frame
                                (transport-error :not-ready
                                                 "Readiness handshake required before starting peripheral")))
                   (if (:peripheral conn)
                     (send-fn ch (proto/render-ws-frame
                                  (transport-error :peripheral-already-active
                                                   "A peripheral session is already active on this connection")))
                     (let [registry (registry-view)
                           agent-id (:agent-id conn)
                           pid (resolve-peripheral-id (:peripheral-id parsed) agent-id registry)
                           periph-config (:peripheral-config registry)]
                       (cond
                         (nil? periph-config)
                         (send-fn ch (proto/render-ws-frame
                                      (transport-error :no-peripheral-config
                                                       "Registry has no peripheral config")))
                         (nil? pid)
                         (send-fn ch (proto/render-ws-frame
                                      (transport-error :unknown-peripheral
                                                       "Could not resolve peripheral for agent")))
                         (not (contains? preg/peripheral-ids pid))
                         (send-fn ch (proto/render-ws-frame
                                      (transport-error :unknown-peripheral
                                                       (str "Unknown peripheral: " (name pid)))))
                         :else
                         (let [session-id (or (:session-id conn)
                                             (str "sess-" (UUID/randomUUID)))
                               {:keys [backend evidence-store]} periph-config
                               peripheral (preg/make-peripheral pid backend)
                               context (cond-> {:session-id session-id
                                                :agent-id agent-id}
                                         evidence-store (assoc :evidence-store evidence-store))
                               start-result (runner/start peripheral context)]
                           (if (error? start-result)
                             (send-fn ch (proto/render-ws-frame start-result))
                             (do
                               (swap! !connections assoc ch
                                      (assoc conn
                                             :peripheral peripheral
                                             :peripheral-state (atom (:state start-result))
                                             :peripheral-id pid))
                               (send-fn ch (proto/render-peripheral-started pid session-id)))))))))

                 ;; --- Peripheral session: tool action (Seam 4) ---
                 :tool-action
                 (if-not (:connected? conn)
                   (send-fn ch (proto/render-ws-frame
                                (transport-error :not-ready
                                                 "Readiness handshake required")))
                   (if-not (:peripheral conn)
                     (send-fn ch (proto/render-ws-frame
                                  (transport-error :no-active-peripheral
                                                   "No peripheral session active — send peripheral_start first")))
                     (let [peripheral (:peripheral conn)
                           state-atom (:peripheral-state conn)
                           action {:tool (:tool parsed) :args (:args parsed)}
                           ;; Serialize step+reset to prevent lost updates
                           ;; under concurrent tool_action frames (Codex #3)
                           step-result (locking state-atom
                                         (let [result (runner/step peripheral @state-atom action)]
                                           (when-not (error? result)
                                             (reset! state-atom (:state result)))
                                           result))]
                       (if (error? step-result)
                         (send-fn ch (proto/render-ws-frame step-result))
                         (send-fn ch (proto/render-tool-result
                                      (:tool parsed)
                                      true
                                      (:result step-result)))))))

                 ;; --- Peripheral session: stop (Seam 4) ---
                 :peripheral-stop
                 (if-not (:connected? conn)
                   (send-fn ch (proto/render-ws-frame
                                (transport-error :not-ready
                                                 "Readiness handshake required")))
                   (if-not (:peripheral conn)
                     (send-fn ch (proto/render-ws-frame
                                  (transport-error :no-active-peripheral
                                                   "No peripheral session active to stop")))
                     (let [pid (:peripheral-id conn)
                           reason (:reason parsed)
                           stop-result (stop-peripheral! !connections ch conn reason)]
                       (if (error? stop-result)
                         (send-fn ch (proto/render-ws-frame stop-result))
                         (send-fn ch (proto/render-peripheral-stopped
                                      pid (:fruit stop-result) reason))))))

                 ;; --- Unknown frame type ---
                 (send-fn ch (proto/render-ws-frame
                              (transport-error :invalid-frame
                                               (str "Unexpected frame type: "
                                                    (:ws/type parsed))))))))))))

     ;; -- on-close --
     ;; L2: only run disconnect cleanup when transitioning from :connected
     ;; to :disconnected. Ignore close events while still :connecting
     ;; (spurious close during TLS handshake).
     ;; Seam 4: if a peripheral session is active, stop it gracefully.
     :on-close
     (fn on-close [ch _status]
       (let [conn (get @!connections ch)]
         ;; Seam 4: stop active peripheral before disconnect
         (when (and conn (:peripheral conn))
           (stop-peripheral! !connections ch conn "connection-closed"))
         ;; L2: only clean up if truly connected (handshake completed)
         (when (and conn (:connected? conn))
           (when on-disconnect-hook
             (on-disconnect-hook (:agent-id conn))))
         ;; Remove from connections regardless of state
         (swap! !connections dissoc ch)))}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn make-ws-handler
  "Create a Ring handler for WebSocket connections via http-kit.

   config: same as make-ws-callbacks.

   Returns {:handler ring-fn, :connections atom}.
   The handler upgrades HTTP requests to WebSocket via hk/as-channel."
  [config]
  (let [{:keys [on-open on-receive on-close connections]}
        (make-ws-callbacks config)]
    {:handler
     (fn [request]
       (hk/as-channel request
         {:on-open (fn [ch] (on-open ch request))
          :on-receive on-receive
          :on-close on-close}))
     :connections connections}))

(defn connected-agents
  "Return a vector of agent-ids for all connected (handshake-completed) connections.
   Useful for health checks and connection tracking."
  [connections-atom]
  (->> (vals @connections-atom)
       (filter :connected?)
       (map :agent-id)
       (remove nil?)
       vec))
