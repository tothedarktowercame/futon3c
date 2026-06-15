(ns futon3c.agency.agent-pouch
  "Persistent warm Claude process pouch.

   One pouch owns one long-lived `claude --print --input-format stream-json`
   process per agent-id. A turn writes one JSON user event to stdin and reads
   stdout until the matching `result` event. Callers keep the cold invoke path as
   fallback; this namespace only manages warm process lifecycle."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.dev.config :as config]
            [futon3c.util.cwd :as cwd])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.time Instant]
           [java.util.concurrent TimeUnit TimeoutException]))

(defonce ^:private !pouches (atom {}))
(defonce ^:private !registry-lock (Object.))

(def ^:private default-timeout-ms (* 30 60 1000))
(def ^:private default-idle-ttl-ms (* 30 60 1000))
(def ^:private default-max-warm 8)

(defn- bool-prop-or-env [k default]
  (let [prop (System/getProperty k)]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (config/env-bool k default))))

(defn- long-prop-or-env [k default]
  (let [raw (or (System/getProperty k) (config/env k))]
    (try
      (if (str/blank? (str raw))
        default
        (long (Long/parseLong (str/trim (str raw)))))
      (catch Throwable _
        default))))

(defn enabled?
  "Load-dark Kangaroo flag. Default OFF."
  []
  (bool-prop-or-env "FUTON3C_KANGAROO" false))

(defn idle-ttl-ms []
  (long-prop-or-env "FUTON3C_KANGAROO_IDLE_TTL_MS" default-idle-ttl-ms))

(defn max-warm []
  (long-prop-or-env "FUTON3C_KANGAROO_MAX_WARM" default-max-warm))

;; =============================================================================
;; Joey gate — warm only SMALL sessions; a monster needs an explicit override
;; =============================================================================
;;
;; A pouch keeps the PROCESS warm but cannot keep Anthropic's server-side prompt
;; cache warm (5-min TTL). So warming a giant transcript is a token-cost trap: every
;; turn past the cache window re-prefills the whole history regardless of process
;; warmth. Policy: warm a "joey" (small session) by default; a "monster" (transcript
;; over the threshold) stays COLD unless overridden (allow-monster! / env).

(def ^:private default-joey-max-bytes (* 2 1024 1024))   ;; 2 MiB

(defn joey-max-bytes []
  (long-prop-or-env "FUTON3C_KANGAROO_JOEY_MAX_BYTES" default-joey-max-bytes))

(defn session-transcript-bytes
  "Best-effort byte size of SESSION-ID's transcript file
   (~/.claude/projects/.../<sid>.jsonl). nil when not found — a brand-new session
   has no transcript yet."
  [session-id]
  (let [sid (some-> session-id str str/trim not-empty)]
    (when sid
      (let [projects (io/file (System/getProperty "user.home") ".claude" "projects")]
        (when (.isDirectory projects)
          (some (fn [d]
                  (let [f (io/file d (str sid ".jsonl"))]
                    (when (.isFile f) (.length f))))
                (.listFiles projects)))))))

(defonce ^:private !monster-allowlist (atom #{}))

(defn allow-monster!
  "Operator override: warm AGENT-ID's pouch even if its session is a monster."
  [agent-id] (swap! !monster-allowlist conj (str agent-id)) true)

(defn disallow-monster! [agent-id] (swap! !monster-allowlist disj (str agent-id)) true)

(defn- env-monster-allowlist
  "Per-agent overrides that survive a JVM restart (the in-memory allowlist does
   not): comma-separated agent ids in FUTON3C_KANGAROO_MONSTER_ALLOWLIST."
  []
  (let [raw (or (System/getProperty "FUTON3C_KANGAROO_MONSTER_ALLOWLIST")
                (config/env "FUTON3C_KANGAROO_MONSTER_ALLOWLIST"))]
    (into #{}
          (comp (map str/trim) (remove str/blank?))
          (str/split (str (or raw "")) #","))))

(defn monster-allowed?
  "True when monsters may be warmed for AGENT-ID — globally (env), per-agent
   durable (env allowlist), or per-agent in-memory (allow-monster!)."
  [agent-id]
  (or (bool-prop-or-env "FUTON3C_KANGAROO_ALLOW_MONSTERS" false)
      (contains? (env-monster-allowlist) (str agent-id))
      (contains? @!monster-allowlist (str agent-id))))

(defn joey-eligible?
  "True when AGENT-ID's SESSION-ID may be warmed: an override is set, the transcript
   is at or under joey-max-bytes, or its size is unknown (a fresh session is a joey
   until it grows). A monster returns false, keeping the caller on the cold path."
  [agent-id session-id]
  (or (monster-allowed? agent-id)
      (let [bytes (session-transcript-bytes session-id)]
        (or (nil? bytes) (<= (long bytes) (long (joey-max-bytes)))))))

(defonce ^:private !monster-logged (atom #{}))

(defn note-monster-cold!
  "Log once per (agent, session) that a monster is being kept cold, so the decision
   is visible (loud-failure discipline). Returns the transcript bytes."
  [agent-id session-id]
  (let [bytes (session-transcript-bytes session-id)
        k [(str agent-id) (some-> session-id str)]]
    (when (and bytes (not (contains? @!monster-logged k)))
      (swap! !monster-logged conj k)
      (println (format "[kangaroo] %s session %.1fMB exceeds joey-max %.1fMB — staying COLD (no pouch). Override: (futon3c.agency.agent-pouch/allow-monster! \"%s\")"
                       (str agent-id) (/ (double bytes) 1048576.0)
                       (/ (double (joey-max-bytes)) 1048576.0) (str agent-id)))
      (flush))
    bytes))

(defn- now-ms [] (System/currentTimeMillis))
(defn- now [] (str (Instant/now)))

(defn- prompt-str [prompt]
  (cond
    (string? prompt) prompt
    (map? prompt) (or (:prompt prompt) (:text prompt) (json/generate-string prompt))
    :else (str prompt)))

(defn- text-from-assistant [event]
  (let [content (get-in event [:message :content])]
    (cond
      (string? content) content
      (sequential? content) (apply str (keep (fn [block]
                                               (when (= "text" (:type block))
                                                 (:text block)))
                                             content))
      :else "")))

(defn- tool-names-from-assistant
  "Tool names invoked in this assistant event (empty when none)."
  [event]
  (let [content (get-in event [:message :content])]
    (when (sequential? content)
      (keep (fn [block]
              (when (= "tool_use" (:type block))
                (:name block)))
            content))))

(defn- no-text-summary
  "Legible stand-in for a turn that produced tool calls but no text. Surfaces the
   tool names so a tool-last turn is inspectable from any surface instead of an
   opaque '[…produced no text response]' (M-agency-hardening, no-text-path
   appendix). Strictly more information than the old placeholder, never less."
  [tool-names]
  (let [names (->> tool-names (remove nil?) distinct vec)]
    (if (seq names)
      (str "[no text — called: " (str/join ", " names) "]")
      "[no text or tool calls in this turn]")))

(defn- alive? [pouch]
  (boolean (and (:process pouch)
                (.isAlive ^Process (:process pouch)))))

(defn- destroy-pouch! [pouch]
  (when-let [w (:writer pouch)]
    (try (.close ^BufferedWriter w) (catch Throwable _)))
  (when-let [r (:reader pouch)]
    (try (.close ^BufferedReader r) (catch Throwable _)))
  (when-let [p (:process pouch)]
    (try
      (when (.isAlive ^Process p)
        (.destroy ^Process p)
        (when-not (.waitFor ^Process p 200 TimeUnit/MILLISECONDS)
          (.destroyForcibly ^Process p)))
      (catch Throwable _))))

(defn evict!
  "Evict AGENT-ID's pouch if present."
  [agent-id]
  (let [aid (str agent-id)]
    (when-let [pouch (get @!pouches aid)]
      (destroy-pouch! pouch)
      (swap! !pouches dissoc aid)
      true)))

(defn clear!
  "Destroy all warm pouches. Intended for tests/dev."
  []
  (doseq [pouch (vals @!pouches)]
    (destroy-pouch! pouch))
  (reset! !pouches {})
  true)

(defn evict-idle!
  "Evict pouches idle longer than TTL-MS. A pouch mid-turn (:in-flight?) is
   never idle, however old its last-used stamp — destroying it would kill the
   live turn. Returns evicted agent ids."
  ([] (evict-idle! (idle-ttl-ms)))
  ([ttl-ms]
   (let [cutoff (- (now-ms) (long ttl-ms))
         evicted (atom [])]
     (doseq [[aid pouch] @!pouches
             :when (and (not (:in-flight? pouch))
                        (< (long (:last-used-ms pouch 0)) cutoff))]
       (when (evict! aid)
         (swap! evicted conj aid)))
     @evicted)))

(defn- enforce-cap! []
  (let [limit (max 1 (long (max-warm)))
        pouches (remove (fn [[_ p]] (:in-flight? p)) @!pouches)]
    (when (> (count pouches) limit)
      (doseq [[aid _] (->> pouches
                           (sort-by (fn [[_ p]] (:last-used-ms p 0)))
                           (take (- (count pouches) limit)))]
        (evict! aid)))))

(defn- stderr-drainer [proc sink]
  (future
    (try
      (with-open [r (BufferedReader. (InputStreamReader. (.getErrorStream ^Process proc)))]
        (loop []
          (when-let [line (.readLine r)]
            (swap! sink #(vec (take-last 20 (conj (or % []) line))))
            (recur))))
      (catch Throwable _ nil))))

(defn- spawn-pouch!
  [agent-id {:keys [claude-bin session-id model cwd permission-mode]}]
  (let [aid (str agent-id)
        args (cond-> [(or claude-bin "claude")
                      "--print"
                      "--input-format" "stream-json"
                      "--output-format" "stream-json"
                      "--verbose"
                      "--permission-mode" (or permission-mode "bypassPermissions")]
               model (into ["--model" model])
               (seq (str session-id)) (into ["--resume" (str session-id)]))
        pb (doto (ProcessBuilder. ^java.util.List (vec args))
             (.redirectInput java.lang.ProcessBuilder$Redirect/PIPE)
             (.redirectOutput java.lang.ProcessBuilder$Redirect/PIPE)
             (.redirectError java.lang.ProcessBuilder$Redirect/PIPE))
        _ (when-let [d (cwd/resolve-cwd cwd)] (.directory pb (io/file d)))
        proc (.start pb)
        stderr (atom [])
        pouch {:agent-id aid
               :process proc
               :writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream proc)))
               :reader (BufferedReader. (InputStreamReader. (.getInputStream proc)))
               :lock (Object.)
               :session-id session-id
               :claude-bin (or claude-bin "claude")
               :model model
               :cwd cwd
               :spawned-at (now)
               :spawned-at-ms (now-ms)
               :last-used-ms (now-ms)
               :turn-count 0
               :stderr stderr}]
    (stderr-drainer proc stderr)
    pouch))

(defn- compatible? [pouch {:keys [session-id model cwd claude-bin]}]
  (and (or (nil? session-id)
           (= (some-> session-id str) (some-> (:session-id pouch) str)))
       (= (some-> model str) (some-> (:model pouch) str))
       (= (some-> cwd str) (some-> (:cwd pouch) str))
       (= (some-> (or claude-bin "claude") str) (some-> (:claude-bin pouch) str))))

(defn- ensure-pouch! [agent-id opts]
  (let [aid (str agent-id)]
    (locking !registry-lock
      (evict-idle!)
      (let [existing (get @!pouches aid)]
        (if (and existing (alive? existing) (compatible? existing opts))
          existing
          (do
            (when existing (destroy-pouch! existing))
            (let [pouch (spawn-pouch! aid opts)]
              (swap! !pouches assoc aid pouch)
              (enforce-cap!)
              pouch)))))))

(defn- user-line [prompt]
  (json/generate-string
   {:type "user"
    :message {:role "user"
              :content [{:type "text" :text (prompt-str prompt)}]}}))

(defn- read-turn* [pouch on-event]
  (let [text (StringBuilder.)
        tools (java.util.ArrayList.)
        sid (atom (:session-id pouch))]
    (loop []
      (let [line (.readLine ^BufferedReader (:reader pouch))]
        (when (nil? line)
          (throw (ex-info "pouch process closed stdout before result"
                          {:agent-id (:agent-id pouch)})))
        (if (str/blank? line)
          (recur)
          (let [event (json/parse-string line true)]
            (when on-event
              ;; Observability hook (e.g. surfacing tool activity to the
              ;; registry); a hook failure must never kill the turn.
              (try (on-event event) (catch Throwable _)))
            (case (:type event)
              "assistant"
              (let [t (text-from-assistant event)]
                (when-not (str/blank? t)
                  (.append text t))
                (doseq [n (tool-names-from-assistant event)] (.add tools n))
                (recur))

              "result"
              (do
                (when-let [event-sid (:session_id event)]
                  (reset! sid event-sid))
                {:result (let [s (str text)]
                           (if (str/blank? s)
                             ;; tool-last / no-text turn: surface what was called
                             ;; instead of an opaque placeholder.
                             (no-text-summary (vec tools))
                             s))
                 :session-id @sid
                 :pouch/warm? true
                 :pouch/agent-id (:agent-id pouch)})

              "error"
              (throw (ex-info "pouch stream emitted an error event"
                              {:agent-id (:agent-id pouch) :event event}))

              (recur))))))))

(defn- drain-pending!
  "Discard any stale buffered output on the pouch reader before a new turn.
   Under feed-turn!'s per-pouch lock the process is idle between turns, so
   anything readable here is orphaned output from a prior anomaly (an
   unconsumed `result`). Dropping it guarantees each turn reads ITS OWN
   output — otherwise a single unconsumed result shifts every later response
   one turn behind (M-agency-hardening: warm-pouch response desync, 2026-06-11).
   Returns the number of stale lines dropped (0 in the normal case)."
  [pouch]
  (let [^BufferedReader r (:reader pouch)]
    (loop [n 0]
      (if (.ready r)
        (if (nil? (.readLine r))
          n
          (recur (inc n)))
        n))))

(defn- read-turn-with-timeout [pouch timeout-ms on-event]
  (let [f (future (read-turn* pouch on-event))
        v (deref f (long timeout-ms) ::timeout)]
    (if (= v ::timeout)
      (do
        (future-cancel f)
        (throw (TimeoutException. (str "pouch feed timed out after " timeout-ms "ms"))))
      v)))

(defn feed-turn!
  "Feed PROMPT to AGENT-ID's warm pouch and read until the result event.

   Options:
   :claude-bin, :session-id, :model, :cwd, :permission-mode, :timeout-ms,
   :on-event (fn [parsed-event] — called for every stream event; exceptions
   are swallowed so observability can't kill the turn).
   Throws on spawn/feed/read failure so callers can cold-fallback."
  [agent-id prompt {:keys [timeout-ms on-event] :as opts}]
  (let [pouch (ensure-pouch! agent-id opts)
        timeout (or timeout-ms default-timeout-ms)
        lock (:lock pouch)]
    (locking lock
      (try
        (when-not (alive? pouch)
          (throw (ex-info "pouch process is not alive" {:agent-id (str agent-id)})))
        ;; Mark in-flight + touch last-used BEFORE the (possibly very long) turn,
        ;; so idle-eviction/cap enforcement never destroys a pouch mid-turn.
        (swap! !pouches
               (fn [m] (cond-> m
                         (contains? m (str agent-id))
                         (update (str agent-id) assoc
                                 :in-flight? true :last-used-ms (now-ms)))))
        ;; Resync guard: drop any stale buffered output before writing, so this
        ;; turn reads its own result (not a prior turn's). A no-op normally.
        (let [drained (drain-pending! pouch)]
          (when (pos? drained)
            (println (str "[pouch] " (str agent-id) " drained " drained
                          " stale line(s) before turn — resynced response alignment"))
            (flush)))
        (.write ^BufferedWriter (:writer pouch) (str (user-line prompt) "\n"))
        (.flush ^BufferedWriter (:writer pouch))
        (let [result (read-turn-with-timeout pouch timeout on-event)]
          (swap! !pouches update (str agent-id)
                 #(when %
                    (assoc %
                           :session-id (:session-id result)
                           :last-used-ms (now-ms)
                           :turn-count (inc (long (:turn-count % 0))))))
          result)
        (catch Throwable t
          (evict! agent-id)
          (throw t))
        (finally
          ;; Only clear the flag on a still-registered pouch — after an evict!
          ;; (error path) there is no entry, and update would reinstate a nil one.
          (swap! !pouches
                 (fn [m] (cond-> m
                           (contains? m (str agent-id))
                           (update (str agent-id) dissoc :in-flight?)))))))))

(defn snapshot []
  (into {}
        (map (fn [[aid pouch]]
               [aid {:agent-id aid
                     :alive? (alive? pouch)
                     :in-flight? (boolean (:in-flight? pouch))
                     :session-id (:session-id pouch)
                     :spawned-at (:spawned-at pouch)
                     :last-used-ms (:last-used-ms pouch)
                     :turn-count (:turn-count pouch)
                     :pid (try (.pid ^Process (:process pouch))
                               (catch Throwable _ nil))
                     :session-bytes (session-transcript-bytes (:session-id pouch))
                     :joey? (joey-eligible? aid (:session-id pouch))
                     :stderr @(:stderr pouch)}]))
        @!pouches))
