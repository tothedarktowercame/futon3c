(ns futon3c.transport.peripheral-events
  "Canonical helper for server-emitted peripheral UI events over the
   authenticated WS sender registry."
  (:require [futon3c.transport.protocol :as proto]
            [futon3c.transport.ws.invoke :as ws-invoke]))

(defn send-peripheral-event!
  "Send a best-effort server-emitted peripheral_event frame to AGENT-ID.
   Returns true when the frame was accepted by the registered WS sender, false
   when no sender exists."
  [agent-id peripheral-id event payload]
  (ws-invoke/send-frame! agent-id
                         (proto/peripheral-event-frame peripheral-id event payload)))
