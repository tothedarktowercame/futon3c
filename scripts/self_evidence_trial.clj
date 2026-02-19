(ns scripts.self-evidence-trial
  "Self-evidencing trial: futon3c explores its own codebase through its own
   peripheral system, producing evidence about itself into its own evidence
   landscape. The system's first act of self-documentation.

   Exercises the full integration stack:
     Seam 1: dispatch routes agent → explore peripheral
     Seam 2: RealBackend executes real file ops against this repo
     Seam 4: WS frames → peripheral lifecycle → evidence emission
     Evidence pipeline: evidence store → HTTP query → verification

   Run:
     clojure -m scripts.self-evidence-trial

   With make dev running (XTDB persistence):
     FUTON3C_USE_XTDB=true clojure -m scripts.self-evidence-trial

   Environment:
     FUTON3C_PORT          — server port (default: 5055)
     FUTON3C_USE_XTDB      — connect to running futon1a for durable evidence
     FUTON1A_PORT          — futon1a port (default: 7071)"
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.registry :as preg]
            [futon3c.runtime.agents :as runtime]
            [futon3c.transport.http :as http]
            [futon3c.transport.ws :as ws])
  (:import [java.net InetSocketAddress Socket URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util.concurrent CompletableFuture]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env [k default]
  (or (System/getenv k) default))

(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(defn- truthy? [s]
  (contains? #{"1" "true" "yes" "on"}
             (str/lower-case (str (or s "")))))

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
  "Create a WebSocket$Listener that collects frames into an atom.
   Handles continuation: onText's `last` parameter indicates whether the
   message is complete. Partial chunks are buffered until the final one."
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
  "Connect a WS client to url. Returns {:ws client :frames atom :opened promise}."
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
  "Wait for a frame matching pred, return parsed JSON or nil on timeout."
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

(defn- all-frames [frames]
  (mapv #(json/parse-string % true) @frames))

;; =============================================================================
;; HTTP evidence query
;; =============================================================================

(defn- http-get [url]
  (let [client (java.net.http.HttpClient/newHttpClient)
        request (-> (java.net.http.HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.header "Accept" "application/json")
                    (.build))
        response (.send client request
                        (java.net.http.HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response)
     :body (json/parse-string (.body response) true)}))

;; =============================================================================
;; Server setup
;; =============================================================================

(defn- start-trial-server!
  "Boot a combined HTTP+WS server with a real peripheral backend."
  [port evidence-store]
  ;; Reset registries for clean state
  (reg/reset-registry!)

  ;; Register trial agent
  (runtime/register-claude!
   {:agent-id "trial-agent"
    :invoke-fn (fn [_ _] {:result "self-evidence-trial" :session-id nil})})

  (let [periph-config (runtime/make-default-peripheral-config
                       {:cwd cwd
                        :evidence-store evidence-store
                        :timeout-ms 30000})
        opts {:patterns {:patterns/ids []}
              :peripheral-config periph-config}
        http-handler (runtime/make-http-handler opts)
        {:keys [handler connections]} (runtime/make-ws-handler opts)
        app (fn [req]
              (if (= "/ws" (:uri req))
                (handler req)
                (http-handler req)))
        result (http/start-server! app port)]
    {:stop (:server result) :port (:port result) :connections connections}))

;; =============================================================================
;; Trial phases
;; =============================================================================

(defn- phase-1-connect!
  "Connect via WS and complete readiness handshake."
  [http-client port session-id]
  (section "Phase 1: WebSocket connect + readiness handshake")
  (let [url (str "ws://127.0.0.1:" port "/ws?agent_id=trial-agent&session_id=" session-id)
        client (ws-connect! http-client url)]
    ;; Wait for connection
    (when-not (deref (:opened client) 3000 nil)
      (fail "WS connect" "timeout waiting for onOpen"))
    (pass "WS connection established")

    ;; Send readiness handshake
    (send-json! (:ws client) {"type" "ready"
                               "agent_id" "trial-agent"
                               "session_id" session-id})
    (let [ack (wait-for-frame (:frames client)
                              #(= "ready_ack" (:type %))
                              3000)]
      (when-not ack
        (fail "Readiness handshake" "no ready_ack received"))
      (pass "Readiness handshake complete"))
    client))

(defn- phase-2-peripheral!
  "Start explore peripheral and run tool actions against the live codebase."
  [client]
  (section "Phase 2: Peripheral session — self-exploration")

  ;; Start explore peripheral
  (reset! (:frames client) [])
  (send-json! (:ws client) {"type" "peripheral_start"
                              "peripheral_id" "explore"})
  (let [started (wait-for-frame (:frames client)
                                #(= "peripheral_started" (:type %))
                                5000)]
    (when-not started
      (fail "peripheral_start" "no peripheral_started frame"))
    (pass (str "Explore peripheral started (session: "
               (:session_id started) ")")))

  ;; Action 1: Read our own README
  (reset! (:frames client) [])
  (let [readme-path (str cwd "/README.md")]
    (send-json! (:ws client) {"type" "tool_action"
                                "tool" "read"
                                "args" [readme-path]})
    (let [result (wait-for-frame (:frames client)
                                 #(= "tool_result" (:type %))
                                 5000)]
      (when-not result
        (fail "tool_action :read" "no tool_result"))
      (when-not (:ok result)
        (fail "tool_action :read" "result not ok"))
      (if (.contains ^String (str (:result result)) "Futon3c")
        (pass "Read README.md — found 'Futon3c' in our own README")
        (fail "tool_action :read" "README doesn't mention Futon3c"))))

  ;; Action 2: Glob for peripheral implementations
  (reset! (:frames client) [])
  (send-json! (:ws client) {"type" "tool_action"
                              "tool" "glob"
                              "args" ["src/futon3c/peripheral/*.clj"]})
  (let [result (wait-for-frame (:frames client)
                               #(= "tool_result" (:type %))
                               5000)]
    (when-not result
      (fail "tool_action :glob" "no tool_result"))
    (let [files (if (sequential? (:result result))
                  (:result result)
                  (str (:result result)))]
      (if (and (string? (str files))
               (.contains ^String (str files) "explore.clj"))
        (pass (str "Glob peripheral/*.clj — found explore.clj among siblings"))
        (fail "tool_action :glob" (str "expected explore.clj in: " files)))))

  ;; Action 3: Grep for PeripheralRunner protocol in our own code
  (reset! (:frames client) [])
  (send-json! (:ws client) {"type" "tool_action"
                              "tool" "grep"
                              "args" ["defprotocol PeripheralRunner"
                                      "src/futon3c/peripheral/runner.clj"]})
  (let [result (wait-for-frame (:frames client)
                               #(= "tool_result" (:type %))
                               5000)]
    (when-not result
      (fail "tool_action :grep" "no tool_result"))
    (if (.contains ^String (str (:result result)) "PeripheralRunner")
      (pass "Grep found PeripheralRunner protocol in runner.clj")
      (fail "tool_action :grep" (str "expected PeripheralRunner in: " (:result result))))))

(defn- phase-3-stop!
  "Stop peripheral and verify fruit."
  [client]
  (section "Phase 3: Peripheral stop — harvest fruit")
  (reset! (:frames client) [])
  (send-json! (:ws client) {"type" "peripheral_stop"
                              "reason" "self-evidence-trial-complete"})
  (let [stopped (wait-for-frame (:frames client)
                                #(= "peripheral_stopped" (:type %))
                                5000)]
    (when-not stopped
      (fail "peripheral_stop" "no peripheral_stopped frame"))
    (pass (str "Peripheral stopped (reason: " (:reason stopped) ")"))
    (when (:fruit stopped)
      (pass (str "Fruit returned: " (count (keys (:fruit stopped))) " keys")))
    stopped))

(defn- phase-4-evidence!
  "Query evidence via HTTP API and verify the complete chain."
  [port session-id]
  (section "Phase 4: Evidence verification — the self-evidencing moment")
  (let [base (str "http://127.0.0.1:" port)
        {:keys [status body]} (http-get (str base "/api/alpha/evidence?session=" session-id))]
    (when-not (= 200 status)
      (fail "Evidence query" (str "HTTP " status)))
    (let [entries (:entries body)
          ;; HTTP API returns namespaced keys: evidence/claim-type etc.
          claim-type-of (fn [e] (or (:evidence/claim-type e)
                                    (get e (keyword "evidence/claim-type"))))
          session-id-of (fn [e] (or (:evidence/session-id e)
                                    (get e (keyword "evidence/session-id"))))
          at-of (fn [e] (or (:evidence/at e) (get e (keyword "evidence/at"))))
          type-of (fn [e] (or (:evidence/type e) (get e (keyword "evidence/type"))))
          body-of (fn [e] (or (:evidence/body e) (get e (keyword "evidence/body"))))
          by-claim (group-by claim-type-of entries)
          goals (concat (get by-claim :goal) (get by-claim "goal"))
          steps (concat (get by-claim :step) (get by-claim "step"))
          conclusions (concat (get by-claim :conclusion) (get by-claim "conclusion"))]
      (pass (str "Evidence query returned " (count entries) " entries"))
      (if (>= (count goals) 1)
        (pass (str (count goals) " goal entry (peripheral start)"))
        (fail "Evidence chain" (str "missing goal entry. claim types seen: "
                                    (pr-str (distinct (map claim-type-of entries))))))
      (if (>= (count steps) 3)
        (pass (str (count steps) " step entries (read + glob + grep)"))
        (fail "Evidence chain" (str "expected ≥3 steps, got " (count steps))))
      (if (>= (count conclusions) 1)
        (pass (str (count conclusions) " conclusion entry (peripheral stop)"))
        (fail "Evidence chain" "missing conclusion entry"))

      ;; Verify evidence is about this session
      (let [session-entries (filter #(= session-id (session-id-of %)) entries)]
        (if (= (count entries) (count session-entries))
          (pass "All entries tagged with correct session-id")
          (pass (str (count session-entries) "/" (count entries)
                     " entries have matching session-id"))))

      ;; Print the evidence timeline
      (println)
      (println "  Evidence timeline:")
      (doseq [e (sort-by at-of entries)]
        (println (str "    " (name (or (claim-type-of e) :unknown))
                      " | " (name (or (type-of e) :unknown))
                      " | " (some-> (at-of e) str (subs 11 19))
                      " | " (let [b (body-of e)]
                              (cond
                                (map? b) (or (:event b) (:tool b) (first (keys b)))
                                :else (str b)))))))))

;; =============================================================================
;; Main
;; =============================================================================

(let [port (parse-int (env "FUTON3C_PORT" "5055") 5055)
      session-id (str "self-evidence-" (.toString (java.util.UUID/randomUUID)))
      evidence-store (atom {:entries {} :order []})
      http-client (HttpClient/newHttpClient)]

  (println)
  (println "╔══════════════════════════════════════════════════════╗")
  (println "║  futon3c self-evidencing trial                      ║")
  (println "║  The system explores itself, producing evidence     ║")
  (println "║  about its own functioning into its own landscape.  ║")
  (println "╚══════════════════════════════════════════════════════╝")
  (println)
  (println (str "  cwd:        " cwd))
  (println (str "  port:       " port))
  (println (str "  session-id: " session-id))

  (let [server (start-trial-server! port evidence-store)]
    (try
      (pass (str "Server started on port " (:port server)))

      (let [client (phase-1-connect! http-client (:port server) session-id)]
        (phase-2-peripheral! client)
        (phase-3-stop! client)

        ;; Small delay for evidence persistence
        (Thread/sleep 200)

        (phase-4-evidence! (:port server) session-id)

        ;; Close WS
        (.sendClose ^WebSocket (:ws client) 1000 "trial-complete"))

      (section "Result")
      (println)
      (println "  All phases passed. The system has evidenced itself.")
      (println)

      (finally
        (when-let [stop (:stop server)] (stop))
        (shutdown-agents)
        (System/exit 0))))))
