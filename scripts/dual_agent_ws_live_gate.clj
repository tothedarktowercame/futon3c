(ns scripts.dual-agent-ws-live-gate
  "Live dual-agent WS gate with optional external Claude (e.g., Linode).

   This script keeps the local futon3c server as the single routing authority
   and validates:
   - Codex action -> peripheral/run-chain -> edit
   - Claude readiness over WS
   - Optional Claude action -> peripheral/run-chain -> explore

   Configuration via env vars:
   - FUTON3C_PORT                  (default: 7070)
   - FUTON3C_BIND_HOST             (default: 0.0.0.0)
   - FUTON3C_PUBLIC_WS_BASE        (required when FUTON3C_EXTERNAL_CLAUDE=true)
   - FUTON3C_SESSION_ID            (default: sess-alleycat-live)
   - FUTON3C_WAIT_MS               (default: 300000)
   - FUTON3C_EXTERNAL_CLAUDE       (default: true)
   - FUTON3C_REQUIRE_CLAUDE_ACTION (default: true)
   - FUTON3C_USE_REAL_BACKEND      (default: false)
   - FUTON3C_REAL_BACKEND_CWD      (default: current working directory)"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.registry :as preg]
            [futon3c.peripheral.tools :as tools]
            [futon3c.runtime.agents :as runtime]
            [futon3c.social.persist :as persist]
            [futon3c.transport.ws :as ws]
            [org.httpkit.server :as hk])
  (:import [java.net InetSocketAddress Socket URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util.concurrent CompletableFuture]))

(defn- now-str [] (str (Instant/now)))

(defn- env
  [k default]
  (or (System/getenv k) default))

(defn- parse-int
  [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(defn- parse-bool
  [s default]
  (if (nil? s)
    default
    (contains? #{"1" "true" "yes" "on"}
               (str/lower-case (str s)))))

(defn- ensure!
  [pred message data]
  (when-not pred
    (throw (ex-info message data))))

(defn- wait-until
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 50) (recur))))))

(defn- find-frame
  [frames pred]
  (some (fn [frame]
          (when (pred frame) frame))
        (map #(json/parse-string % true) @frames)))

(defn- wait-for-frame
  [frames pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [frame (find-frame frames pred)]
        frame
        (if (> (System/currentTimeMillis) deadline)
          nil
          (do (Thread/sleep 50) (recur)))))))

(defn- normalize-ws-base
  [s]
  (some-> s
          (str/replace #"/+$" "")))

(defn- start-server!
  [handler bind-host port]
  (let [stop-fn (hk/run-server handler {:ip bind-host :port port})
        _ (Thread/sleep 120)
        probe-host (if (or (= bind-host "0.0.0.0")
                           (= bind-host "::")
                           (str/blank? bind-host))
                     "127.0.0.1"
                     bind-host)
        listening? (try
                     (with-open [sock (Socket.)]
                       (.connect sock (InetSocketAddress. probe-host (int port)) 1000)
                       true)
                     (catch Exception _ false))]
    (if listening?
      {:server stop-fn :host bind-host :port port :started-at (now-str)}
      (do
        (stop-fn)
        (throw (ex-info "Server started but probe failed"
                        {:bind-host bind-host :probe-host probe-host :port port}))))))

(defn- mk-listener
  [frames errors opened-p closed-p]
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

(defn- start-client!
  [client url]
  (let [frames (atom [])
        errors (atom [])
        opened-p (promise)
        closed-p (promise)
        listener (mk-listener frames errors opened-p closed-p)
        ws-client (.join (.buildAsync (.newWebSocketBuilder client)
                                      (URI/create url)
                                      listener))]
    {:ws ws-client
     :frames frames
     :errors errors
     :opened opened-p
     :closed closed-p
     :url url}))

(defn- send-json!
  [^WebSocket web-socket payload]
  (.join (.sendText web-socket (json/generate-string payload) true)))

(defn- periph-backend
  [use-real-backend? cwd]
  (if use-real-backend?
    (rb/make-real-backend {:cwd cwd :timeout-ms 120000})
    (tools/make-mock-backend)))

(let [port (parse-int (env "FUTON3C_PORT" "7070") 7070)
      bind-host (env "FUTON3C_BIND_HOST" "0.0.0.0")
      public-ws-base (normalize-ws-base (System/getenv "FUTON3C_PUBLIC_WS_BASE"))
      session-id (env "FUTON3C_SESSION_ID" "sess-alleycat-live")
      wait-ms (parse-int (env "FUTON3C_WAIT_MS" "300000") 300000)
      external-claude? (parse-bool (System/getenv "FUTON3C_EXTERNAL_CLAUDE") true)
      require-claude-action? (parse-bool (System/getenv "FUTON3C_REQUIRE_CLAUDE_ACTION") true)
      use-real-backend? (parse-bool (System/getenv "FUTON3C_USE_REAL_BACKEND") false)
      backend-cwd (env "FUTON3C_REAL_BACKEND_CWD" (System/getProperty "user.dir"))
      codex-id "codex-1"
      claude-id "claude-1"
      local-ws-base (str "ws://127.0.0.1:" port)
      codex-local-url (str local-ws-base "/agency/ws?agent-id=" codex-id "&session-id=" session-id)
      claude-public-url (str (or public-ws-base local-ws-base)
                             "/agency/ws?agent-id=" claude-id "&session-id=" session-id)]

  (ensure! (or (not external-claude?) (some? public-ws-base))
           "FUTON3C_PUBLIC_WS_BASE is required when FUTON3C_EXTERNAL_CLAUDE=true"
           {:external-claude external-claude?
            :example "wss://<your-linode-host>:7070"})

  (println "Live Gate Configuration:")
  (println "  bind-host:" bind-host)
  (println "  port:" port)
  (println "  external-claude?:" external-claude?)
  (println "  require-claude-action?:" require-claude-action?)
  (println "  use-real-backend?:" use-real-backend?)
  (println "  session-id:" session-id)
  (println "  wait-ms:" wait-ms)

  (reg/reset-registry!)
  (persist/reset-sessions!)

  ;; Keep registry entries explicit. WS handshake verifies these IDs.
  (runtime/register-codex!
    {:agent-id codex-id
     :invoke-fn (fn [prompt prior-session-id]
                  {:result (str "codex-ok:" prompt)
                   :session-id prior-session-id
                   :exit-code 0})})

  (runtime/register-claude!
    {:agent-id claude-id
     :invoke-fn (fn [prompt prior-session-id]
                  {:result (str "claude-ok:" prompt)
                   :session-id prior-session-id
                   :exit-code 0})})

  (let [periph-config {:backend (periph-backend use-real-backend? backend-cwd)
                       :peripherals (preg/load-peripherals)}
        !sent (atom [])
        !connections-ref (atom nil)
        send-fn (fn [ch data]
                  (let [conn (when-let [connections @!connections-ref]
                               (get @connections ch))
                        parsed (try (json/parse-string data true) (catch Exception _ nil))]
                    (swap! !sent conj {:at (now-str)
                                       :agent-id (:agent-id conn)
                                       :frame parsed
                                       :raw data})
                    (hk/send! ch data)))
        close-fn hk/close
        opts {:patterns {:patterns/ids [:alleycat/live]}
              :peripheral-config periph-config
              :send-fn send-fn
              :close-fn close-fn}
        http-handler (runtime/make-http-handler opts)
        {:keys [handler connections]} (runtime/make-ws-handler opts)
        _ (reset! !connections-ref connections)
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
        server (start-server! app bind-host port)
        client (HttpClient/newHttpClient)]
    (try
      (println "Server started at:" (str bind-host ":" port))
      (println "Codex local URL:" codex-local-url)
      (when external-claude?
        (println "Claude (Linode) connect URL:" claude-public-url)
        (println "Waits for claude-1 readiness and action frames from that WS connection."))

      ;; Always run local codex client for deterministic edit-route validation.
      (let [codex (start-client! client codex-local-url)]
        (ensure! (some? (deref (:opened codex) 2000 nil))
                 "Codex WS open timeout"
                 {:url (:url codex) :errors @(:errors codex)})

        (send-json! (:ws codex) {"type" "ready" "agent_id" codex-id "session_id" session-id})
        (ensure! (some? (wait-for-frame (:frames codex) #(= "ready_ack" (:type %)) 3000))
                 "Codex missing ready_ack"
                 {:frames @(:frames codex) :errors @(:errors codex)})

        (send-json! (:ws codex) {"type" "message"
                                 "msg_id" "alleycat-codex-live-1"
                                 "payload" "fix failing integration test"
                                 "to" codex-id})
        (let [codex-receipt (wait-for-frame
                             (:frames codex)
                             #(and (= "receipt" (:type %))
                                   (= "alleycat-codex-live-1" (:msg_id %)))
                             3000)]
          (println "Codex receipt:" codex-receipt)
          (ensure! (some? codex-receipt)
                   "Missing codex receipt"
                   {:frames @(:frames codex) :errors @(:errors codex)})
          (ensure! (= "peripheral/run-chain" (:route codex-receipt))
                   "Codex route mismatch"
                   {:receipt codex-receipt})
          (ensure! (= "edit" (:peripheral_id codex-receipt))
                   "Codex peripheral mismatch"
                   {:receipt codex-receipt}))

        (if external-claude?
          (do
            ;; Gate on external Claude connection
            (ensure! (wait-until #(contains? (set (ws/connected-agents connections)) claude-id) wait-ms)
                     "Claude did not connect in time"
                     {:connected (ws/connected-agents connections)
                      :claude-url claude-public-url})
            (println "Claude connected.")
            (when require-claude-action?
              (println "Waiting for Claude action receipt (msg_id alleycat-claude-live-1 expected)...")
              (ensure! (wait-until
                        #(some (fn [{:keys [agent-id frame]}]
                                 (and (= claude-id agent-id)
                                      (= "receipt" (:type frame))
                                      (= "alleycat-claude-live-1" (:msg_id frame))
                                      (= "peripheral/run-chain" (:route frame))
                                      (= "explore" (:peripheral_id frame))))
                               @!sent)
                        wait-ms)
                       "Claude action receipt not observed in time"
                       {:hint "From Claude WS client send message frame with msg_id=alleycat-claude-live-1, payload string, to=claude-1"
                        :connected (ws/connected-agents connections)}))
            (println "External Claude gate passed."))
          ;; Local claude mode for deterministic self-check.
          (let [claude-local-url (str local-ws-base "/agency/ws?agent-id=" claude-id "&session-id=" session-id)
                claude (start-client! client claude-local-url)]
            (ensure! (some? (deref (:opened claude) 2000 nil))
                     "Claude local WS open timeout"
                     {:url (:url claude) :errors @(:errors claude)})
            (send-json! (:ws claude) {"type" "ready" "agent_id" claude-id "session_id" session-id})
            (ensure! (some? (wait-for-frame (:frames claude) #(= "ready_ack" (:type %)) 3000))
                     "Claude local missing ready_ack"
                     {:frames @(:frames claude) :errors @(:errors claude)})
            (send-json! (:ws claude) {"type" "message"
                                      "msg_id" "alleycat-claude-live-1"
                                      "payload" "investigate flaky test cluster"
                                      "to" claude-id})
            (let [claude-receipt (wait-for-frame
                                  (:frames claude)
                                  #(and (= "receipt" (:type %))
                                        (= "alleycat-claude-live-1" (:msg_id %)))
                                  3000)]
              (println "Claude receipt:" claude-receipt)
              (ensure! (some? claude-receipt)
                       "Missing claude local receipt"
                       {:frames @(:frames claude) :errors @(:errors claude)})
              (ensure! (= "peripheral/run-chain" (:route claude-receipt))
                       "Claude route mismatch"
                       {:receipt claude-receipt})
              (ensure! (= "explore" (:peripheral_id claude-receipt))
                       "Claude peripheral mismatch"
                       {:receipt claude-receipt}))
            (.join (.sendClose ^WebSocket (:ws claude) WebSocket/NORMAL_CLOSURE "done"))))

        (.join (.sendClose ^WebSocket (:ws codex) WebSocket/NORMAL_CLOSURE "done"))
        (println "PASS: live dual-agent gate complete."))
      (finally
        ((:server server))))))
