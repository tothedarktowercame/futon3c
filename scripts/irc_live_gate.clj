(ns scripts.irc-live-gate
  "Live 3-way IRC gate: Agency + IRC server on Linode, Claude local, Codex external.

   Validates the full IRC relay path:
   1. Agency starts on port 7070 (WS + HTTP)
   2. IRC server starts on port 6667
   3. Claude connects via WS locally, joins #futon via relay bridge
   4. Programmatic IRC client connects as 'joe', joins #futon
   5. Joe sends PRIVMSG → relayed to Claude via WS → Claude responds → relayed to IRC
   6. Evidence entries created for all messages
   7. PASS if all signals observed

   Configuration via env vars:
   - FUTON3C_PORT              (default: 7070)
   - FUTON3C_IRC_PORT          (default: 6667)
   - FUTON3C_BIND_HOST         (default: 0.0.0.0)
   - FUTON3C_PUBLIC_WS_BASE    (for external Codex, optional)
   - FUTON3C_SESSION_ID        (default: sess-irc-live)
   - FUTON3C_WAIT_MS           (default: 30000)"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.registry :as preg]
            [futon3c.peripheral.tools :as tools]
            [futon3c.runtime.agents :as runtime]
            [futon3c.social.persist :as persist]
            [futon3c.transport.irc :as irc]
            [futon3c.transport.ws :as ws]
            [org.httpkit.server :as hk])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.net InetSocketAddress Socket URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util.concurrent CompletableFuture]))

(defn- now-str [] (str (Instant/now)))

(defn- env [k default]
  (or (System/getenv k) default))

(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(defn- ensure! [pred message data]
  (when-not pred
    (throw (ex-info message data))))

(defn- wait-until [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 50) (recur))))))

;; =============================================================================
;; WS client helpers (same as dual_agent_ws_live_gate.clj)
;; =============================================================================

(defn- mk-listener [frames errors opened-p closed-p]
  (reify WebSocket$Listener
    (onOpen [_ web-socket]
      (deliver opened-p web-socket)
      (.request web-socket 1))
    (onText [_ web-socket data _last]
      (swap! frames conj (str data))
      (.request web-socket 1)
      (CompletableFuture/completedFuture nil))
    (onClose [_ _web-socket _status-code _reason]
      (deliver closed-p true)
      (CompletableFuture/completedFuture nil))
    (onError [_ _web-socket error]
      (swap! errors conj (str error)))))

(defn- start-ws-client! [client url]
  (let [frames (atom [])
        errors (atom [])
        opened-p (promise)
        closed-p (promise)
        listener (mk-listener frames errors opened-p closed-p)
        ws-client (.join (.buildAsync (.newWebSocketBuilder client)
                                      (URI/create url)
                                      listener))]
    {:ws ws-client :frames frames :errors errors
     :opened opened-p :closed closed-p :url url}))

(defn- send-json! [^WebSocket web-socket payload]
  (.join (.sendText web-socket (json/generate-string payload) true)))

(defn- find-frame [frames pred]
  (some (fn [f] (when (pred f) f))
        (map #(json/parse-string % true) @frames)))

(defn- wait-for-frame [frames pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [frame (find-frame frames pred)]
        frame
        (if (> (System/currentTimeMillis) deadline)
          nil
          (do (Thread/sleep 50) (recur)))))))

;; =============================================================================
;; IRC client helpers (raw TCP)
;; =============================================================================

(defn- start-irc-client! [host port nick]
  (let [socket (Socket.)
        _ (.connect socket (InetSocketAddress. host (int port)) 3000)
        reader (BufferedReader. (InputStreamReader. (.getInputStream socket) "UTF-8"))
        writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream socket) "UTF-8"))
        received (atom [])
        running (atom true)
        reader-thread (future
                        (try
                          (while @running
                            (when-let [line (.readLine reader)]
                              (swap! received conj line)
                              ;; Auto-respond to PING
                              (when (str/starts-with? line "PING")
                                (let [token (subs line 5)]
                                  (locking writer
                                    (.write writer (str "PONG " token "\r\n"))
                                    (.flush writer))))))
                          (catch Exception _)))]
    ;; Register
    (locking writer
      (.write writer (str "NICK " nick "\r\n"))
      (.write writer (str "USER " nick " 0 * :" nick "\r\n"))
      (.flush writer))
    (Thread/sleep 500)
    {:socket socket :reader reader :writer writer
     :received received :running running :reader-thread reader-thread}))

(defn- irc-send! [{:keys [writer]} line]
  (locking writer
    (.write writer (str line "\r\n"))
    (.flush writer)))

(defn- irc-close! [{:keys [socket running reader-thread]}]
  (reset! running false)
  (try (.close socket) (catch Exception _))
  (future-cancel reader-thread))

(defn- wait-for-irc-line [received pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [line (some (fn [l] (when (pred l) l)) @received)]
        line
        (if (> (System/currentTimeMillis) deadline)
          nil
          (do (Thread/sleep 50) (recur)))))))

;; =============================================================================
;; Main gate
;; =============================================================================

(let [ws-port (parse-int (env "FUTON3C_PORT" "7070") 7070)
      irc-port (parse-int (env "FUTON3C_IRC_PORT" "6667") 6667)
      bind-host (env "FUTON3C_BIND_HOST" "0.0.0.0")
      session-id (env "FUTON3C_SESSION_ID" "sess-irc-live")
      wait-ms (parse-int (env "FUTON3C_WAIT_MS" "30000") 30000)
      claude-id "claude-1"
      evidence-store (atom {:entries {} :order []})]

  (println "IRC Live Gate Configuration:")
  (println "  ws-port:" ws-port)
  (println "  irc-port:" irc-port)
  (println "  bind-host:" bind-host)
  (println "  session-id:" session-id)
  (println "  wait-ms:" wait-ms)

  (reg/reset-registry!)
  (persist/reset-sessions!)

  (runtime/register-claude!
    {:agent-id claude-id
     :invoke-fn (fn [prompt prior-session-id]
                  {:result (str "claude-ok:" prompt)
                   :session-id prior-session-id
                   :exit-code 0})})

  (let [periph-config {:backend (tools/make-mock-backend)
                       :peripherals (preg/load-peripherals)}
        relay-bridge (irc/make-relay-bridge {:evidence-store evidence-store})
        opts {:patterns {:patterns/ids [:alleycat/irc-live]}
              :peripheral-config periph-config
              :send-fn hk/send!
              :close-fn hk/close
              :irc-interceptor (:irc-interceptor relay-bridge)}
        http-handler (runtime/make-http-handler opts)
        {:keys [handler connections]} (runtime/make-ws-handler opts)
        app (fn [req]
              (cond
                (and (= :get (:request-method req))
                     (= "/agency/connected" (:uri req)))
                {:status 200
                 :headers {"Content-Type" "application/json"}
                 :body (json/generate-string
                        {"connected_agents" (ws/connected-agents connections)
                         "count" (count (ws/connected-agents connections))})}
                (= "/agency/ws" (:uri req))
                (handler req)
                :else
                (http-handler req)))
        ;; Start Agency (WS + HTTP)
        ws-stop (hk/run-server app {:ip bind-host :port ws-port})
        _ (Thread/sleep 200)

        ;; Start IRC server
        irc-server (irc/start-irc-server!
                    {:port irc-port
                     :bind-host bind-host
                     :relay-fn (:relay-fn relay-bridge)
                     :evidence-store evidence-store})
        ;; Wire the bridge's IRC send function to the IRC server
        _ ((:set-irc-send-fn! relay-bridge) (:send-to-channel! irc-server))

        http-client (HttpClient/newHttpClient)]

    (try
      (println "Agency started at:" (str bind-host ":" ws-port))
      (println "IRC server started at:" (str bind-host ":" irc-port))
      (println)

      ;; === Step 1: Claude connects via WS ===
      (println "=== Step 1: Claude connecting via WS... ===")
      (let [claude-url (str "ws://127.0.0.1:" ws-port "/agency/ws?agent-id=" claude-id "&session-id=" session-id)
            claude-ws (start-ws-client! http-client claude-url)]
        (ensure! (some? (deref (:opened claude-ws) 3000 nil))
                 "Claude WS open timeout"
                 {:url (:url claude-ws) :errors @(:errors claude-ws)})
        (send-json! (:ws claude-ws) {"type" "ready" "agent_id" claude-id "session_id" session-id})
        (ensure! (some? (wait-for-frame (:frames claude-ws) #(= "ready_ack" (:type %)) 3000))
                 "Claude missing ready_ack"
                 {:frames @(:frames claude-ws)})
        (println "Claude connected via WS.")

        ;; Register Claude in the relay bridge — use server-side http-kit channel
        ;; to send frames TO Claude (not the client-side WebSocket object)
        (Thread/sleep 200)
        (let [claude-ch (some (fn [[ch conn]]
                                (when (and (= claude-id (:agent-id conn))
                                           (:connected? conn))
                                  ch))
                              @connections)]
          (ensure! (some? claude-ch)
                   "Could not find Claude's http-kit channel in connections"
                   {:connections (count @connections)})
          ((:join-agent! relay-bridge) claude-id "claude" "#futon"
           (fn [data] (hk/send! claude-ch data))))

        ;; === Step 2: Joe connects via IRC ===
        (println)
        (println "=== Step 2: Joe connecting via IRC... ===")
        (let [joe-irc (start-irc-client! "127.0.0.1" irc-port "joe")]
          (ensure! (wait-for-irc-line (:received joe-irc)
                                      #(re-find #"001" %) 3000)
                   "Joe IRC registration timeout"
                   {:received @(:received joe-irc)})
          (println "Joe connected to IRC as 'joe'.")

          ;; Joe joins #futon
          (irc-send! joe-irc "JOIN #futon")
          (Thread/sleep 500)
          (ensure! (wait-for-irc-line (:received joe-irc)
                                      #(re-find #"JOIN #futon" %) 2000)
                   "Joe JOIN timeout"
                   {:received @(:received joe-irc)})
          (println "Joe joined #futon.")

          ;; === Step 3: Joe sends a message ===
          (println)
          (println "=== Step 3: Joe sends PRIVMSG to #futon... ===")
          (irc-send! joe-irc "PRIVMSG #futon :Hey Claude, what's the status?")
          (Thread/sleep 500)

          ;; Claude should have received irc_message via WS
          (let [irc-msg (wait-for-frame (:frames claude-ws)
                                        #(= "irc_message" (:type %))
                                        3000)]
            (println "Claude received:" irc-msg)
            (ensure! (some? irc-msg)
                     "Claude did not receive IRC message"
                     {:frames @(:frames claude-ws)})
            (ensure! (= "#futon" (:channel irc-msg))
                     "Wrong channel in relay"
                     {:frame irc-msg})
            (ensure! (= "joe" (:from irc-msg))
                     "Wrong sender in relay"
                     {:frame irc-msg})
            (ensure! (= "Hey Claude, what's the status?" (:text irc-msg))
                     "Wrong text in relay"
                     {:frame irc-msg}))
          (println "IRC → WS relay verified.")

          ;; === Step 4: Claude responds via WS ===
          (println)
          (println "=== Step 4: Claude responds via irc_response... ===")
          (send-json! (:ws claude-ws) {"type" "irc_response"
                                       "channel" "#futon"
                                       "text" "All systems green. Evidence store has 42 entries."})
          (Thread/sleep 500)

          ;; Joe should see Claude's response on IRC
          (let [response-line (wait-for-irc-line
                               (:received joe-irc)
                               #(and (re-find #"PRIVMSG #futon" %)
                                     (re-find #"All systems green" %))
                               3000)]
            (println "Joe received:" response-line)
            (ensure! (some? response-line)
                     "Joe did not receive Claude's response on IRC"
                     {:received @(:received joe-irc)})
            (ensure! (re-find #":claude" response-line)
                     "Response not from claude nick"
                     {:line response-line}))
          (println "WS → IRC relay verified.")

          ;; === Step 5: Verify evidence ===
          (println)
          (println "=== Step 5: Verifying evidence... ===")
          (let [entries (vals (:entries @evidence-store))
                futon-entries (filter #(= {:ref/type :thread :ref/id "irc/#futon"}
                                          (:evidence/subject %))
                                      entries)]
            (println "Evidence entries for irc/#futon:" (count futon-entries))
            (ensure! (>= (count futon-entries) 2)
                     "Expected at least 2 evidence entries (joe message + claude response)"
                     {:count (count futon-entries) :entries futon-entries})
            (let [authors (set (map :evidence/author futon-entries))]
              (ensure! (contains? authors "joe")
                       "Missing evidence from joe"
                       {:authors authors})
              (ensure! (contains? authors "claude")
                       "Missing evidence from claude"
                       {:authors authors})))
          (println "Evidence threading verified.")

          ;; Cleanup
          (irc-close! joe-irc)
          (try (.join (.sendClose ^WebSocket (:ws claude-ws) WebSocket/NORMAL_CLOSURE "done"))
               (catch Exception _)))

        (println)
        (println "PASS: IRC 3-way live gate complete."))
      (finally
        ((:stop-fn irc-server))
        (ws-stop)))))
