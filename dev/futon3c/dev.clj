(ns futon3c.dev
  "Minimal dev server: boots futon1a (XTDB + HTTP) for evidence persistence.

   Agents are not registered at startup — they come alive when you launch
   a claude session (via `make claude` or the REPL).

   Environment variables:
     FUTON1A_PORT       — HTTP port for futon1a (default 7071)
     FUTON1A_DATA_DIR   — XTDB storage directory
     FUTON3C_PORT       — futon3c transport HTTP port (default 7070, 0 = disable)
     FUTON3C_DRAWBRIDGE_PORT  — Drawbridge HTTP port (default 6768, 0 = disable)
     FUTON3C_DRAWBRIDGE_BIND  — Drawbridge bind interface (default 127.0.0.1)
     FUTON3C_DRAWBRIDGE_ALLOW — Drawbridge allowlist CSV (default 127.0.0.1,::1)
     FUTON3C_ADMIN_TOKEN / ADMIN_TOKEN / .admintoken — Drawbridge auth token
     FUTON3C_PATTERNS   — comma-separated pattern IDs (default: none)
     FUTON3C_PEERS      — comma-separated peer Agency URLs for federation
     FUTON3C_SELF_URL   — this Agency's externally reachable URL"
  (:require [futon1a.system :as f1]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.mission-control.service :as mcs]
            [futon3c.agency.federation :as federation]
            [futon3c.runtime.agents :as rt]
            [futon3c.transport.http :as http]
            [repl.http :as drawbridge]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn env
  "Read an env var with optional default."
  ([k] (System/getenv k))
  ([k default] (or (System/getenv k) default)))

(defn env-int [k default]
  (if-let [s (env k)]
    (parse-long s)
    default))

(defn read-admin-token []
  (or (some-> (env "FUTON3C_ADMIN_TOKEN") str/trim not-empty)
      (some-> (env "ADMIN_TOKEN") str/trim not-empty)
      (let [f (io/file ".admintoken")]
        (when (.exists f)
          (some-> (slurp f) str/trim not-empty)))
      "change-me"))

(defn env-list [k default]
  (if-let [s (env k)]
    (->> (str/split s #",")
         (map str/trim)
         (remove str/blank?)
         vec)
    default))

(defn start-futon1a!
  "Start futon1a (XTDB + HTTP). Returns system map with :node, :store, :stop!, etc."
  []
  (let [port (env-int "FUTON1A_PORT" 7071)
        data-dir (env "FUTON1A_DATA_DIR"
                      (str (System/getProperty "user.home")
                           "/code/storage/futon1a/default"))]
    (println (str "[dev] Starting futon1a (XTDB: " data-dir ")..."))
    (let [sys (f1/start! {:data-dir data-dir :port port})]
      (println (str "[dev] futon1a: http://localhost:" (:http/port sys)))
      sys)))

(defn start-futon3c!
  "Start futon3c transport HTTP. Returns {:server stop-fn :port p} or nil if disabled."
  [xtdb-node]
  (let [port (env-int "FUTON3C_PORT" 7070)]
    (when (pos? port)
      (let [pattern-ids (if-let [s (env "FUTON3C_PATTERNS")]
                          (mapv keyword (remove empty? (.split s ",")))
                          [])
            opts {:patterns {:patterns/ids pattern-ids}
                  :xtdb-node xtdb-node}
            handler (rt/make-http-handler opts)
            result (http/start-server! handler port)]
        (println (str "[dev] futon3c: http://localhost:" (:port result)
                      " (patterns: " (if (seq pattern-ids) pattern-ids "none") ")"))
        result))))

(defn start-drawbridge!
  "Start Drawbridge endpoint used by fubar/portal style tooling.
   Returns {:stop stop-fn :port p} or nil when disabled."
  []
  (let [port (env-int "FUTON3C_DRAWBRIDGE_PORT" 6768)]
    (when (pos? port)
      (let [bind (env "FUTON3C_DRAWBRIDGE_BIND" "127.0.0.1")
            allow (env-list "FUTON3C_DRAWBRIDGE_ALLOW" ["127.0.0.1" "::1"])
            token (read-admin-token)
            stop-fn (drawbridge/start! {:port port
                                        :bind bind
                                        :allow allow
                                        :token token})]
        {:stop stop-fn
         :port port
         :bind bind}))))

(defn -main [& _args]
  (let [f1-sys (start-futon1a!)
        evidence-store (xb/make-xtdb-backend (:node f1-sys))
        _ (mcs/configure! {:evidence-store evidence-store})
        f3c-sys (start-futon3c! (:node f1-sys))
        bridge-sys (start-drawbridge!)
        ;; Federation: configure peers and install announcement hook
        _ (federation/configure-from-env!)
        _ (federation/install-hook!)
        fed-peers (federation/peers)
        fed-self (federation/self-url)]
    (println)
    (println "[dev] Evidence API (futon3c transport → XTDB backend)")
    (println "[dev]   GET  /api/alpha/evidence          — query entries")
    (println "[dev]   GET  /api/alpha/evidence/:id       — single entry")
    (println "[dev]   GET  /api/alpha/evidence/:id/chain — reply chain")
    (println "[dev]   POST /api/alpha/evidence          — append entry")
    (println "[dev]   POST /api/alpha/agents            — register agent")
    (println "[dev]   GET  /api/alpha/agents            — list agents")
    (println)
    (if (seq fed-peers)
      (do (println (str "[dev] Federation: self=" fed-self " peers=" fed-peers))
          (println "[dev]   Agents registered locally will be announced to peers."))
      (println "[dev] Federation: no peers configured (set FUTON3C_PEERS, FUTON3C_SELF_URL)"))
    (println)
    (println "[dev] No agents registered. Use `make claude`, `make codex`, or REPL:")
    (println "[dev]   (require '[futon3c.runtime.agents :as rt])")
    (println "[dev]   (rt/register-claude! {:agent-id \"claude-1\" :invoke-fn ...})")
    (println "[dev]   (rt/register-codex!  {:agent-id \"codex-1\"  :invoke-fn ...})")
    (println "[dev]   (rt/register-tickle! {:agent-id \"tickle-1\" :invoke-fn ...})")
    (println)
    (println "[dev] Mission control service (Drawbridge, no cold-start per query)")
    (println "[dev]   (require '[futon3c.mission-control.service :as mcs])")
    (println "[dev]   (mcs/list-sessions)")
    (println "[dev]   (mcs/run-review! {:author \"joe\"})")
    (println)
    (println "[dev] Press Ctrl-C to stop.")

    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      ^Runnable
      (fn []
        (println "\n[dev] Shutting down...")
        (when-let [stop (:stop bridge-sys)] (stop))
        (when-let [stop (:server f3c-sys)] (stop))
        ((:stop! f1-sys))
        (println "[dev] Stopped."))))

    ;; Block forever
    @(promise)))
