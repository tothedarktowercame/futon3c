(ns scripts.ws-send
  "Send a single WS frame and print the response(s).

   Usage:
     clojure -Sdeps '{:paths [\"src\" \"resources\" \"library\" \".\"]}' \
       -M -m scripts.ws-send <agent-id> <session-id> <json-frame> [wait-ms]

   Examples:
     # Connect and start explore peripheral:
     ws-send claude-1 sess-1 '{\"type\":\"peripheral_start\",\"peripheral_id\":\"explore\"}'

     # Run a tool:
     ws-send claude-1 sess-1 '{\"type\":\"tool_action\",\"tool\":\"read\",\"args\":[\"README.md\"]}'

     # Send IRC message:
     ws-send claude-1 sess-1 '{\"type\":\"irc_response\",\"channel\":\"#futon-par\",\"text\":\"hello\"}'

   The client connects, sends the ready handshake, sends the frame,
   prints all received frames, and exits after wait-ms (default 3000)."
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util.concurrent CompletableFuture]))

(defn- make-listener [frames opened-p]
  (let [!buf (atom (StringBuilder.))]
    (reify WebSocket$Listener
      (onOpen [_ ws]
        (deliver opened-p ws)
        (.request ws 1))
      (onText [_ ws data last]
        (.append ^StringBuilder @!buf data)
        (when last
          (let [text (.toString ^StringBuilder @!buf)]
            (swap! frames conj text)
            (println text))
          (reset! !buf (StringBuilder.)))
        (.request ws 1)
        (CompletableFuture/completedFuture nil))
      (onClose [_ _ code reason]
        (println (str "WS closed: " code " " reason))
        (CompletableFuture/completedFuture nil))
      (onError [_ _ err]
        (println (str "WS error: " err))))))

(let [args *command-line-args*
      agent-id (nth args 0)
      session-id (nth args 1)
      json-frame (nth args 2)
      wait-ms (if (> (count args) 3)
                (Integer/parseInt (nth args 3))
                3000)
      port (or (System/getenv "FUTON3C_PORT") "5056")
      url (str "ws://127.0.0.1:" port
               "/ws?agent_id=" agent-id
               "&session_id=" session-id)
      frames (atom [])
      opened (promise)
      client (HttpClient/newHttpClient)
      ws (.join (.buildAsync (.newWebSocketBuilder client)
                             (URI/create url)
                             (make-listener frames opened)))]
  ;; Wait for connection
  (deref opened 3000 nil)

  ;; Send ready handshake
  (.join (.sendText ws (str "{\"type\":\"ready\",\"agent_id\":\"" agent-id
                            "\",\"session_id\":\"" session-id "\"}") true))
  (Thread/sleep 500)

  ;; Send the actual frame
  (when (and json-frame (not= json-frame "ready-only"))
    (println (str ">>> " json-frame))
    (.join (.sendText ws json-frame true)))

  ;; Wait for responses
  (Thread/sleep wait-ms)
  (.sendClose ws 1000 "done")
  (Thread/sleep 200)
  (shutdown-agents)
  (System/exit 0))
