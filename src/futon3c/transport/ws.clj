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
            [futon3c.social.mode :as mode]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.presence :as presence]
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

   Returns:
     {:on-open    (fn [ch request])  — call on WebSocket upgrade
      :on-receive (fn [ch data])     — call on incoming frame
      :on-close   (fn [ch status])   — call on connection close
      :connections atom}             — connection registry for introspection"
  [config]
  (let [!connections (atom {})
        send-fn (or (:send-fn config) hk/send!)
        close-fn (or (:close-fn config) hk/close)
        registry (:registry config)
        patterns (:patterns config)
        on-connect-hook (:on-connect config)
        on-disconnect-hook (:on-disconnect config)]

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
                       result (presence/verify conn-event registry)]
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
                         classified (mode/classify message patterns)]
                     (if (error? classified)
                       (send-fn ch (proto/render-ws-frame classified))
                       (let [result (dispatch/dispatch classified registry)]
                         (send-fn ch (proto/render-ws-frame result))))))

                 ;; --- Unknown frame type ---
                 (send-fn ch (proto/render-ws-frame
                              (transport-error :invalid-frame
                                               (str "Unexpected frame type: "
                                                    (:ws/type parsed)))))))))))

     ;; -- on-close --
     ;; L2: only run disconnect cleanup when transitioning from :connected
     ;; to :disconnected. Ignore close events while still :connecting
     ;; (spurious close during TLS handshake).
     :on-close
     (fn on-close [ch _status]
       (let [conn (get @!connections ch)]
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
