(ns futon3c.agency.agent-pouch
  "Persistent warm Claude process pouch.

   One pouch owns one long-lived `claude --print --input-format stream-json`
   process per agent-id. A turn writes one JSON user event to stdin and reads
   stdout until the matching `result` event. Callers keep the cold invoke path as
   fallback; this namespace only manages warm process lifecycle."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.dev.config :as config])
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

(defn monster-allowed?
  "True when monsters may be warmed for AGENT-ID — globally (env) or per-agent (allowlist)."
  [agent-id]
  (or (bool-prop-or-env "FUTON3C_KANGAROO_ALLOW_MONSTERS" false)
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
  "Evict pouches idle longer than TTL-MS. Returns evicted agent ids."
  ([] (evict-idle! (idle-ttl-ms)))
  ([ttl-ms]
   (let [cutoff (- (now-ms) (long ttl-ms))
         evicted (atom [])]
     (doseq [[aid pouch] @!pouches
             :when (< (long (:last-used-ms pouch 0)) cutoff)]
       (when (evict! aid)
         (swap! evicted conj aid)))
     @evicted)))

(defn- enforce-cap! []
  (let [limit (max 1 (long (max-warm)))
        pouches @!pouches]
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
        _ (when cwd (.directory pb (io/file cwd)))
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

(defn- read-turn* [pouch]
  (let [text (StringBuilder.)
        sid (atom (:session-id pouch))]
    (loop []
      (let [line (.readLine ^BufferedReader (:reader pouch))]
        (when (nil? line)
          (throw (ex-info "pouch process closed stdout before result"
                          {:agent-id (:agent-id pouch)})))
        (if (str/blank? line)
          (recur)
          (let [event (json/parse-string line true)]
            (case (:type event)
              "assistant"
              (let [t (text-from-assistant event)]
                (when-not (str/blank? t)
                  (.append text t))
                (recur))

              "result"
              (do
                (when-let [event-sid (:session_id event)]
                  (reset! sid event-sid))
                {:result (let [s (str text)]
                           (if (str/blank? s)
                             "[Claude used tools but produced no text response]"
                             s))
                 :session-id @sid
                 :pouch/warm? true
                 :pouch/agent-id (:agent-id pouch)})

              (recur))))))))

(defn- read-turn-with-timeout [pouch timeout-ms]
  (let [f (future (read-turn* pouch))
        v (deref f (long timeout-ms) ::timeout)]
    (if (= v ::timeout)
      (do
        (future-cancel f)
        (throw (TimeoutException. (str "pouch feed timed out after " timeout-ms "ms"))))
      v)))

(defn feed-turn!
  "Feed PROMPT to AGENT-ID's warm pouch and read until the result event.

   Options:
   :claude-bin, :session-id, :model, :cwd, :permission-mode, :timeout-ms.
   Throws on spawn/feed/read failure so callers can cold-fallback."
  [agent-id prompt {:keys [timeout-ms] :as opts}]
  (let [pouch (ensure-pouch! agent-id opts)
        timeout (or timeout-ms default-timeout-ms)
        lock (:lock pouch)]
    (locking lock
      (try
        (when-not (alive? pouch)
          (throw (ex-info "pouch process is not alive" {:agent-id (str agent-id)})))
        (.write ^BufferedWriter (:writer pouch) (str (user-line prompt) "\n"))
        (.flush ^BufferedWriter (:writer pouch))
        (let [result (read-turn-with-timeout pouch timeout)]
          (swap! !pouches update (str agent-id)
                 #(when %
                    (assoc %
                           :session-id (:session-id result)
                           :last-used-ms (now-ms)
                           :turn-count (inc (long (:turn-count % 0))))))
          result)
        (catch Throwable t
          (evict! agent-id)
          (throw t))))))

(defn snapshot []
  (into {}
        (map (fn [[aid pouch]]
               [aid {:agent-id aid
                     :alive? (alive? pouch)
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
