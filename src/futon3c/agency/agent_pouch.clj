(ns futon3c.agency.agent-pouch
  "Persistent warm Claude process pouch.

   One pouch owns one long-lived `claude --print --input-format stream-json`
   process per agent-id. A turn writes one JSON user event to stdin and reads
   stdout until the matching `result` event. Callers keep the cold invoke path as
   fallback; this namespace only manages warm process lifecycle."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.job-tree :as job-tree]
            [futon3c.agency.turn-queue :as turn-queue]
            [futon3c.dev.config :as config]
            [futon3c.util.cwd :as cwd])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.time Instant]
           [java.util.concurrent ArrayBlockingQueue LinkedBlockingQueue
            RejectedExecutionException ThreadFactory ThreadPoolExecutor
            TimeUnit TimeoutException]
           [java.util.concurrent.locks ReentrantLock]))

(defonce ^:private !pouches (atom {}))
(defonce ^:private !registry-lock (Object.))

(def ^:private default-timeout-ms (* 60 60 1000))
(def ^:private default-missing-trailer-grace-ms 90000)
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

(defn demux?
  "Load-dark flag for the demultiplexing pouch reader (D6, E-unsolicited-pouch-turns).
   Default OFF; the OFF path is byte-for-byte the original synchronous read."
  []
  (bool-prop-or-env "FUTON3C_POUCH_DEMUX" false))

(defonce ^:private !unsolicited-sink (atom nil))

(def agent-initiated-marker
  "Visible attribution for a pouch turn that no operator turn solicited."
  "[AGENT-INITIATED — NOT A REPLY]")

(defn make-unsolicited-sink
  "Adapt DELIVER! to the `(fn [agent-id turn])` pouch sink contract.
   DELIVER! receives a surface-neutral map whose speaker contains the mandatory
   agent-initiated marker; operator-surface adapters must preserve that speaker."
  [deliver!]
  (fn [agent-id turn]
    (deliver! {:agent-id (str agent-id)
               :session-id (:session-id turn)
               :speaker (str agent-id " " agent-initiated-marker)
               :text (str (:result turn))})))

(defn set-unsolicited-sink!
  "Register (fn [agent-id turn]) for turns the agent took that no feed-turn!
   solicited — a background-task completion re-invoking it. Without a sink
   they are logged. They must never be discarded silently (the old
   drain-pending! behaviour) nor returned to the next caller (the desync)."
  [f]
  (reset! !unsolicited-sink f))

(defonce ^:private unsolicited-delivery-executor
  ;; The demux thread is the pouch's sole stdout reader. It must never execute
  ;; an operator-surface adapter inline: Emacs, a socket, or a test sink may
  ;; block indefinitely. A bounded queue also prevents an unavailable surface
  ;; from turning a burst of autonomous completions into unbounded memory use.
  (ThreadPoolExecutor.
   1 1 0 TimeUnit/MILLISECONDS
   (ArrayBlockingQueue. 256)
   (reify ThreadFactory
     (newThread [_ runnable]
       (doto (Thread. ^Runnable runnable "pouch-unsolicited-delivery")
         (.setDaemon true))))))

(defn idle-ttl-ms []
  (long-prop-or-env "FUTON3C_KANGAROO_IDLE_TTL_MS" default-idle-ttl-ms))

(defn missing-trailer-grace-ms
  "How long a solicited, text-only assistant event may remain quiescent before
   the demux treats the absent protocol `result` as a missing trailer.

   This is deliberately longer than the observed pause between an intermediate
   prose block and its following tool call. Any later protocol event cancels the
   inference. Set FUTON3C_POUCH_MISSING_TRAILER_GRACE_MS to tune in tests or
   operations."
  []
  (long-prop-or-env "FUTON3C_POUCH_MISSING_TRAILER_GRACE_MS"
                    default-missing-trailer-grace-ms))

(defn max-warm []
  (long-prop-or-env "FUTON3C_KANGAROO_MAX_WARM" default-max-warm))

(defn max-rss-mb
  "Per-pouch RSS ceiling in MB. Long-lived warm pouches grow without bound
   (two kernel-OOM box deaths on 2026-07-04: pouch processes at 12 GB and
   16.4 GB anon RSS); pouches over this ceiling are recycled at the next
   sweep — the cold-fallback path resumes the session from its transcript."
  []
  (long-prop-or-env "FUTON3C_KANGAROO_MAX_RSS_MB" 4096))

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

;; -- E-monster-to-joey: compact a monster's COLD transcript so it can warm -----
;; Flag-gated (default OFF). Compaction rewrites the transcript IN PLACE under the
;; same session-id (agent clock/evidence/pouch key untouched), backing up the
;; original; safe because this fires only at the monster-cold decision, where the
;; session has no warm writer. See scripts/compact_session.py.

(def ^:private default-compact-script
  "/home/joe/code/futon3c/scripts/compact_session.py")

(defn compact-monsters-enabled?
  "Flag (default ON as of 2026-07-01, FUTON3C_KANGAROO_COMPACT_MONSTERS): compact a
   monster's cold transcript so it becomes a warm-able joey instead of cold-spawning
   every turn. Verified live on real agents (E-monster-to-joey). Set the flag to
   false to disable."
  []
  (bool-prop-or-env "FUTON3C_KANGAROO_COMPACT_MONSTERS" true))

(defn- compact-script []
  (or (some-> (System/getProperty "FUTON3C_KANGAROO_COMPACT_SCRIPT") str/trim not-empty)
      (some-> (config/env "FUTON3C_KANGAROO_COMPACT_SCRIPT") str/trim not-empty)
      default-compact-script))

(defn- compact-target-mib []
  (or (some-> (or (System/getProperty "FUTON3C_KANGAROO_COMPACT_TARGET_MIB")
                  (config/env "FUTON3C_KANGAROO_COMPACT_TARGET_MIB"))
              str/trim not-empty)
      "1.7"))

(defn compact-session!
  "Run the recency-shell compactor on SESSION-ID's cold transcript, in place
   (backup kept). Returns true on success. Loud-failure: logs and returns false on
   any error. Only meaningful on a COLD session (no warm writer)."
  [session-id]
  (let [sid    (some-> session-id str str/trim not-empty)
        script (compact-script)]
    (when (and sid (.isFile (io/file script)))
      (try
        (let [sh (requiring-resolve 'clojure.java.shell/sh)
              {:keys [exit out err]} (sh "python3" script sid
                                         "--target-mib" (compact-target-mib))]
          (println (format "[kangaroo] compact %s: exit=%s %s" sid (str exit)
                           (str/trim (str (or out "")
                                          (when (seq err) (str " ERR:" err))))))
          (flush)
          (zero? (long exit)))
        (catch Throwable t
          (println (str "[kangaroo] compact failed for " sid ": " (.getMessage t)))
          (flush)
          false)))))

(defn joey-eligible-or-compact?
  "Like `joey-eligible?`, but when a monster is found and compaction is enabled
   (FUTON3C_KANGAROO_COMPACT_MONSTERS), compact the cold transcript in place and
   re-check the gate — so a shrunk monster warms instead of staying cold.
   SIDE EFFECT (only on a monster + flag on): rewrites the transcript."
  [agent-id session-id]
  (or (joey-eligible? agent-id session-id)
      (and (compact-monsters-enabled?)
           (some? (some-> session-id str str/trim not-empty))
           (compact-session! session-id)
           (joey-eligible? agent-id session-id))))

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

(defn- fail-demux-waiters!
  "Atomically close DEMUX and fail its current owner plus every queued waiter.

   Registration uses the same DEMUX monitor. Therefore shutdown is linear:
   a waiter is either present here and failed, or observes :closed? and never
   registers. There is no third state where it is added after the sole reader
   has exited and can wait forever."
  [demux error]
  (when demux
    (locking demux
      (reset! (:closed? demux) true)
      ;; The current turn is removed from :waiters at init time. It must stay
      ;; reachable here or eviction can kill the process while leaving the
      ;; invoke promise parked forever (claude-2, 2026-08-06).
      (when-let [owner-atom (:owner demux)]
        (when-let [w @owner-atom]
          (reset! owner-atom nil)
          (deliver (:promise w) {:error error})))
      (let [^LinkedBlockingQueue q (:waiters demux)]
        (loop []
          (when-let [w (.poll q)]
            (deliver (:promise w) {:error error})
            (recur)))))))

(defn- destroy-pouch! [pouch]
  ;; Mark the demux closed and fail its waiters first. Terminate the subprocess
  ;; BEFORE closing its BufferedReader: BufferedReader.close synchronizes with
  ;; readLine and can itself block behind the sole reader thread until the
  ;; process produces output. Process death is what reliably releases readLine.
  (fail-demux-waiters!
   (:demux pouch)
   (ex-info "pouch was evicted while a turn was waiting"
            {:agent-id (:agent-id pouch)}))
  (when-let [w (:writer pouch)]
    (try (.close ^BufferedWriter w) (catch Throwable _)))
  (when-let [p (:process pouch)]
    (try
      (when (.isAlive ^Process p)
        (.destroy ^Process p)
        (when-not (.waitFor ^Process p 200 TimeUnit/MILLISECONDS)
          (.destroyForcibly ^Process p)))
      (catch Throwable _)))
  (when-let [r (:reader pouch)]
    (try (.close ^BufferedReader r) (catch Throwable _))))

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

(defn- pouch-rss-kb
  "Resident set size of POUCH's process in kB via `ps`, or nil when
   unreadable (dead process). NB: Java reads of /proc/<pid>/status throw
   EINVAL in this JVM (verified 2026-07-04), hence the subprocess."
  [pouch]
  (try
    (when-let [p ^Process (:process pouch)]
      (when (.isAlive p)
        (let [ps (.start (ProcessBuilder. ["ps" "-o" "rss=" "-p" (str (.pid p))]))
              out (with-open [r (clojure.java.io/reader (.getInputStream ps))]
                    (slurp r))]
          (.waitFor ps 2000 TimeUnit/MILLISECONDS)
          (some-> (re-find #"\d+" out) parse-long))))
    (catch Throwable _ nil)))

(defn evict-oversized!
  "Evict pouches whose RSS exceeds the ceiling. Mid-turn pouches are never
   killed (same rule as evict-idle!) — a ballooned pouch is recycled at the
   first sweep after its turn ends. Returns [[agent-id rss-mb] ...]."
  ([] (evict-oversized! (max-rss-mb)))
  ([limit-mb]
   (let [limit-kb (* (long limit-mb) 1024)
         evicted (atom [])]
     (doseq [[aid pouch] @!pouches
             :when (not (:in-flight? pouch))]
       (when-let [rss-kb (pouch-rss-kb pouch)]
         (when (> rss-kb limit-kb)
           (println (str "[kangaroo] " aid " pouch over RSS ceiling ("
                         (quot rss-kb 1024) "MB > " limit-mb
                         "MB); recycling — next invoke resumes cold"))
           (when (evict! aid)
             (swap! evicted conj [aid (quot rss-kb 1024)])))))
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
               :lock (ReentrantLock.)
               :session-id session-id
               :claude-bin (or claude-bin "claude")
               :model model
               :cwd cwd
               :permission-mode (or permission-mode "bypassPermissions")
               :spawned-at (now)
               :spawned-at-ms (now-ms)
               :last-used-ms (now-ms)
               :turn-count 0
               :stderr stderr
               ;; Decided once, at spawn: a pouch must not switch read models
               ;; mid-life if the flag is flipped under it.
               :demux (when (demux?)
                        {:waiters (LinkedBlockingQueue.)
                         :turns (atom 0)
                         ;; Every parsed event advances this clock. A delayed
                         ;; missing-trailer check is valid only while its event
                         ;; remains the most recent event for the same turn.
                         :activity-seq (atom 0)
                         ;; Monotone observation clock for task_notification.
                         ;; A waiter records this value when registered so the
                         ;; reader can distinguish "notification already seen,
                         ;; then operator prompt" from "operator prompt queued,
                         ;; then an already-buffered autonomous notification".
                         :notification-seq (atom 0)
                         ;; Unlike a local volatile in demux-loop!, this owner
                         ;; remains reachable by destroy-pouch!/eviction.
                         :owner (atom nil)
                         :thread (atom nil)
                         :closed? (atom false)})}]
    (stderr-drainer proc stderr)
    pouch))

(defn- compatible? [pouch {:keys [session-id model cwd claude-bin permission-mode]}]
  (and (or (nil? session-id)
           (= (some-> session-id str) (some-> (:session-id pouch) str)))
       (= (some-> model str) (some-> (:model pouch) str))
       (= (some-> cwd str) (some-> (:cwd pouch) str))
       (= (some-> (or claude-bin "claude") str) (some-> (:claude-bin pouch) str))
       ;; A permission change must not be silently absorbed by a warm pouch:
       ;; claude-2's 2026-08-10 re-registration (default -> bypassPermissions)
       ;; never took effect because the old pouch kept serving.
       (= (str (or permission-mode "bypassPermissions"))
          (str (or (:permission-mode pouch) "bypassPermissions")))))

(defn- ensure-pouch! [agent-id opts]
  (let [aid (str agent-id)]
    (locking !registry-lock
      (evict-idle!)
      (evict-oversized!)
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

(defn- control-line []
  (json/generate-string
   {:type "user"
    :message {:role "user" :content "/compact"}}))

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

(defn- read-turn-with-timeout
  "Read one turn, optionally bounded. TIMEOUT-MS nil or non-positive reads
   until the result event arrives.

   Unlike the codex adapter this keeps a default bound (see
   default-timeout-ms). The pouch is a persistent stdio protocol: abandoning a
   read leaves an unconsumed `result` that shifts every later turn one behind
   (the desync drain-pending! exists to repair), so removing the bound here
   needs a pouch-level cancel first. Tracked in README-agency-cap.md."
  [pouch timeout-ms on-event]
  (let [f (future (read-turn* pouch on-event))
        bound (when (and timeout-ms (pos? (long timeout-ms))) (long timeout-ms))]
    (if-not bound
      @f
      (let [v (deref f bound ::timeout)]
        (if (= v ::timeout)
          (do
            (future-cancel f)
            (throw (TimeoutException. (str "pouch feed timed out after " bound "ms"))))
          v)))))

;; ---------------------------------------------------------------------------
;; Demultiplexing reader (D6 — the single-READER dual of D3's single-writer).
;;
;; The pouch has a second turn source: the agent itself. A background task
;; completing re-invokes it, producing a full turn and its own `result` that no
;; feed-turn! is waiting on (measured 2026-08-03 on claude-11: 23 self-initiated
;; turns against 11 fed). read-turn* returns on the FIRST result it sees, so each
;; unsolicited one shifts every later REPL reply a turn behind.
;;
;; drain-pending! cannot fix this, and not only because "the process is idle
;; between turns" is false. Peeking at a stream you are not continuously
;; consuming cannot distinguish an orphaned COMPLETE turn from an in-flight
;; turn's PARTIAL output — and draining the latter both destroys the agent's
;; work and leaves the next read starting mid-turn. The repair has to be a
;; reader that never stops reading.
;;
;; Correlation (measured against a live pouch, 2026-08-03 — not assumed):
;;
;;    6.6  result  success             <- the fed turn ends
;;   12.9  system  task_notification   <- background task completed
;;   13.0  system  init                <- a NEW turn starts, solicited by nobody
;;   19.3  result  success
;;
;; Every turn opens with `system`/`init`, so turn boundaries are explicit; and an
;; agent-initiated turn is ANNOUNCED by a `system`/`task_notification` emitted
;; between turns. Ownership is therefore decided by the protocol, not by timing:
;; a turn whose init follows a pending task_notification belongs to no waiter.
;;
;; An earlier draft correlated by counting turns seen before the write. It was
;; wrong — the count reflects what the reader has PROCESSED, not what the process
;; has EMITTED, so a turn already sitting in the pipe was invisible and got handed
;; to the next caller: the very bug being fixed. The tests caught it.
;;
;; Assumption worth stating: one pending task_notification is taken to explain one
;; autonomous turn (the flag is reset, not decremented). If a burst of
;; notifications ever produced several distinct turns, the later ones could still
;; be mis-attributed. `system`/`task_started` is NOT a trigger — it occurs inside
;; a turn, when the agent launches the job.
;; ---------------------------------------------------------------------------

(defn- report-unsolicited! [agent-id turn]
  (if-let [f @!unsolicited-sink]
    (try
      (.execute unsolicited-delivery-executor
                ^Runnable
                (fn []
                  (try
                    (f agent-id turn)
                    (catch Throwable t
                      (println (str "[pouch] " agent-id
                                    " unsolicited delivery failed: "
                                    (.getMessage t)))
                      (flush)))))
      (catch RejectedExecutionException _
        (println (str "[pouch] " agent-id
                      " unsolicited delivery queue full — delivery rejected"))
        (flush)))
    (do (println (str "[pouch] " agent-id " unsolicited turn (agent-initiated, no"
                      " waiter): " (subs (str (:result turn))
                                         0 (min 160 (count (str (:result turn)))))))
        (flush))))

(defn- demux-loop!
  "Own the pouch's stdout for the process's whole life: segment it into turns
   and route each to its waiter, or to the unsolicited sink."
  [pouch]
  (let [{:keys [waiters turns notification-seq activity-seq] :as demux}
        (:demux pouch)
        ^BufferedReader r (:reader pouch)
        ^LinkedBlockingQueue q waiters
        aid (:agent-id pouch)
        owner (or (:owner demux) (atom nil))
        text (volatile! (StringBuilder.))
        tools (volatile! (java.util.ArrayList.))
        sid (volatile! (:session-id pouch))
        turn-id (volatile! nil)
        open? (volatile! false)
        agent-initiated? (volatile! false)]
    (letfn [(start-turn! []
              (swap! turns inc)
              ;; Bind at turn START, not at result, so a waiter's on-event
              ;; streams live and only ever sees its own turn's events.
              ;; A pending notification is not enough by itself: if the head
              ;; waiter registered AFTER that notification was observed, its
              ;; user input supersedes the notification and owns this init.
              ;; If the notification was observed AFTER waiter registration,
              ;; the init may already have been buffered before the write, so
              ;; preserve it as autonomous (the original one-behind defect).
              (let [candidate (.peek q)
                    autonomous? (and @agent-initiated?
                                     (or (nil? candidate)
                                         (< (long (:notification-seq candidate -1))
                                            (long @notification-seq))))]
                (reset! owner (when-not autonomous? (.poll q))))
              (vreset! turn-id
                       (if-let [w @owner]
                         (or (:turn-id w)
                             (str "pouch-turn-" (java.util.UUID/randomUUID)))
                         (str "pouch-autonomous-" (java.util.UUID/randomUUID))))
              (vreset! agent-initiated? false)
              (vreset! text (StringBuilder.))
              (vreset! tools (java.util.ArrayList.))
              (vreset! open? true))
            (ensure-open! [] (when-not @open? (start-turn!)))
            (finish-turn! [event trailer-inferred?]
              (when-let [s (:session_id event)] (vreset! sid s))
              (let [body (str @text)
                    turn (cond->
                           {:result (if (str/blank? body)
                                      (no-text-summary (vec @tools))
                                      body)
                            :session-id @sid
                            :turn-id @turn-id
                            :pouch/warm? true
                            :pouch/agent-id aid}
                           trailer-inferred?
                           (assoc :pouch/trailer-inferred? true))]
                (if-let [w @owner]
                  (deliver (:promise w) {:ok turn})
                  (report-unsolicited! aid turn)))
              (reset! owner nil)
              (vreset! open? false))
            (emit! [event]
              (when-let [w @owner]
                (when-let [f (:on-event w)]
                  (try (f (assoc event :turn-id @turn-id))
                       (catch Throwable _ nil)))))
            (schedule-missing-trailer! [event event-seq]
              ;; Claude's stream-json process has twice emitted a genuine final
              ;; assistant message but no terminal `result`, leaving the waiter
              ;; and HTTP stream open forever. A text-only event is only a
              ;; candidate: an intermediate paragraph followed by thinking or
              ;; a tool is cancelled by the next event's activity sequence.
              (let [candidate-owner @owner
                    candidate-turn @turn-id]
                (future
                  (Thread/sleep (missing-trailer-grace-ms))
                  (locking demux
                    (when (and @open?
                               candidate-owner
                               (identical? candidate-owner @owner)
                               (= candidate-turn @turn-id)
                               (= event-seq @activity-seq))
                      (println (str "[pouch] " aid " inferred missing result trailer for "
                                    candidate-turn " after "
                                    (missing-trailer-grace-ms) "ms quiescence"))
                      (flush)
                      (emit! {:type "result"
                              :subtype "missing_trailer_inferred"
                              :session_id @sid})
                      (finish-turn! event true))))))]
      (try
        (loop []
          (if-let [line (.readLine r)]
            (do
              (when-not (str/blank? line)
                (when-let [event (try (json/parse-string line true)
                                      (catch Throwable _ nil))]
                  (job-tree/observe-event!
                   {:agent-id aid
                    :turn-id @turn-id
                    :root-pid (.pid ^Process (:process pouch))
                    :event event})
                  (let [event-seq (swap! activity-seq inc)]
                    (case (:type event)
                    "system"
                    (condp = (str (:subtype event))
                      ;; Claude may emit more than one init record while
                      ;; starting a fresh --print process. A duplicate belongs
                      ;; to the open turn; polling q again would orphan the
                      ;; current owner and park its invoke forever.
                      "init" (do (ensure-open!) (emit! event))
                      ;; Announced between turns: the next turn to open is the
                      ;; agent answering its own background work unless a user
                      ;; prompt is subsequently registered before that init.
                      "task_notification" (do (swap! notification-seq inc)
                                              (vreset! agent-initiated? true))
                      (do (ensure-open!) (emit! event)))

                    "assistant"
                    (do (ensure-open!)
                        (emit! event)
                        (let [t (text-from-assistant event)]
                          (when-not (str/blank? t)
                            (.append ^StringBuilder @text t)
                            (when (empty? (tool-names-from-assistant event))
                              (schedule-missing-trailer! event event-seq))))
                        (doseq [n (tool-names-from-assistant event)]
                          (.add ^java.util.ArrayList @tools n)))

                    "result"
                    (locking demux
                      (ensure-open!)
                      (emit! event)
                      (finish-turn! event false))

                      (do (ensure-open!) (emit! event))))))
              (recur))
            ;; stdout closed: fail every outstanding waiter rather than
            ;; leaving them to time out one by one.
            (do (vreset! open? false)
                (fail-demux-waiters!
                 (:demux pouch)
                 (ex-info "pouch process closed stdout" {:agent-id aid})))))
        (catch Throwable t
          (fail-demux-waiters! (:demux pouch) t))))))

(defn- ensure-demux! [pouch]
  (let [{:keys [thread]} (:demux pouch)]
    (when (nil? @thread)
      (let [t (doto (Thread. ^Runnable #(demux-loop! pouch)
                             (str "pouch-demux-" (:agent-id pouch)))
                (.setDaemon true))]
        (when (compare-and-set! thread nil t)
          (.start t))))))

(defn- feed-line-demux!
  "ON path: enqueue a waiter, write LINE, and await THIS turn's result."
  [pouch line timeout-ms on-event turn-id]
  (let [{:keys [waiters closed?] :as demux} (:demux pouch)
        ^LinkedBlockingQueue q waiters]
    (ensure-demux! pouch)
    (let [w {:promise (promise)
             :on-event on-event
             :turn-id turn-id
             :notification-seq @(:notification-seq demux)}]
      ;; Pair with fail-demux-waiters!: close-and-drain and check-and-register
      ;; share one monitor, so a waiter can never appear after shutdown drained
      ;; the queue and the sole reader exited.
      (locking demux
        (when @closed?
          (throw (ex-info "pouch stdout already closed"
                          {:agent-id (:agent-id pouch)})))
        (.add q w))
      (try
        (.write ^BufferedWriter (:writer pouch) (str line "\n"))
        (.flush ^BufferedWriter (:writer pouch))
        (catch Throwable t
          (.remove q w)
          (throw t)))
      (let [v (if (and timeout-ms (pos? (long timeout-ms)))
                (deref (:promise w) (long timeout-ms) ::timeout)
                @(:promise w))]
        (cond
          (= ::timeout v)
          (do (.remove q w)
              (throw (TimeoutException.
                      (str "pouch feed timed out after " timeout-ms "ms"))))
          (:error v) (throw (:error v))
          :else (:ok v))))))

(defn- feed-turn-demux! [pouch prompt timeout-ms on-event turn-id]
  (feed-line-demux! pouch (user-line prompt) timeout-ms on-event turn-id))

(defn- run-pouch-turn!
  "Run F with the pouch's exclusive turn ownership and bookkeeping.
   WAIT? false returns ::busy rather than waiting for an active turn."
  [agent-id pouch wait? f]
  (let [aid (str agent-id)
        ^ReentrantLock lock (:lock pouch)
        acquired? (if wait? (do (.lock lock) true) (.tryLock lock))]
    (if-not acquired?
      ::busy
      (try
        (when-not (alive? pouch)
          (throw (ex-info "pouch process is not alive" {:agent-id aid})))
        (swap! !pouches
               (fn [m] (cond-> m
                         (contains? m aid)
                         (update aid assoc :in-flight? true :last-used-ms (now-ms)))))
        (when-not (:demux pouch)
          (let [drained (drain-pending! pouch)]
            (when (pos? drained)
              (println (str "[pouch] " aid " drained " drained
                            " stale line(s) before turn — resynced response alignment"))
              (flush))))
        (let [result (f)]
          (swap! !pouches update aid
                 #(when %
                    (assoc %
                           :session-id (:session-id result)
                           :last-used-ms (now-ms)
                           :turn-count (inc (long (:turn-count % 0))))))
          (when (:pouch/trailer-inferred? result)
            (evict! aid))
          result)
        (catch Throwable t
          (evict! aid)
          (throw t))
        (finally
          (swap! !pouches
                 (fn [m] (cond-> m
                           (contains? m aid)
                           (update aid dissoc :in-flight?))))
          (.unlock lock))))))

(defn feed-turn!
  "Feed PROMPT to AGENT-ID's warm pouch and read until the result event.

   Options:
   :claude-bin, :session-id, :model, :cwd, :permission-mode, :timeout-ms,
   :on-event (fn [parsed-event] — called for every stream event; exceptions
   are swallowed so observability can't kill the turn).
   Throws on spawn/feed/read failure so callers can cold-fallback."
  [agent-id prompt {:keys [timeout-ms on-event turn-id] :as opts}]
  (let [pouch (ensure-pouch! agent-id opts)
        timeout (or timeout-ms default-timeout-ms)]
    (run-pouch-turn!
     agent-id pouch true
     (fn []
        (when-not (:demux pouch)
          (.write ^BufferedWriter (:writer pouch) (str (user-line prompt) "\n"))
          (.flush ^BufferedWriter (:writer pouch)))
        (if (:demux pouch)
          (feed-turn-demux! pouch prompt timeout on-event
                            (or turn-id turn-queue/*turn-id*))
          (read-turn-with-timeout pouch timeout on-event))))))

(defn compact-pouch!
  "Send the raw /compact control to an already-warm AGENT-ID pouch."
  [agent-id {:keys [timeout-ms] :as _opts}]
  (let [aid (str agent-id)
        pouch (get @!pouches aid)]
    (cond
      (or (nil? pouch) (not (alive? pouch)))
      {:ok false :error "no warm pouch"}

      (:in-flight? pouch)
      {:ok false :error "turn in flight"}

      :else
      (let [status (atom nil)
            result-event (atom nil)
            on-event (fn [event]
                       (when (and (= "system" (:type event))
                                  (= "status" (:subtype event))
                                  (contains? event :compact_result))
                         (reset! status event))
                       (when (= "result" (:type event))
                         (reset! result-event event)))
            timeout (or timeout-ms default-timeout-ms)]
        (try
          (let [result
                (run-pouch-turn!
                 aid pouch false
                 (fn []
                   (if (:demux pouch)
                     (feed-line-demux! pouch (control-line) timeout on-event
                                       (str "pouch-compact-" (java.util.UUID/randomUUID)))
                     (do
                       (.write ^BufferedWriter (:writer pouch)
                               (str (control-line) "\n"))
                       (.flush ^BufferedWriter (:writer pouch))
                       (read-turn-with-timeout pouch timeout on-event)))))]
            (if (= ::busy result)
              {:ok false :error "turn in flight"}
              (let [compact-result (:compact_result @status)
                    compact-error (:compact_error @status)]
                {:ok (= "success" compact-result)
                 :compact-result compact-result
                 :compact-error compact-error
                 :session-id (:session-id result)
                 :usage (:usage @result-event)
                 :total-cost-usd (:total_cost_usd @result-event)})))
          (catch Throwable t
            {:ok false :error (.getMessage t)}))))))

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
