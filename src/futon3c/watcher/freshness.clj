(ns futon3c.watcher.freshness
  "D7a — substrate-2 commit-freshness alarm (M-populate-substrate-2).

   The original 5-week silent freeze (commit-ingest default-off, 2026-05-21)
   went unnoticed because NOTHING watched freshness: the cyder heartbeat measures
   whether the watcher loop *ticks*, not whether substrate-2's commits are
   *current*. This is the obsolescence-recognition / freshness-invariant shape
   (sibling of `futon3c.logic.invariant-queue-freshness`): a DERIVED artifact —
   substrate-2's last-indexed commit per repo — is STALE relative to its
   SOURCE-OF-TRUTH — the repo's git HEAD.

   `check` returns the standard probe-family shape {:outcome :ok|:violation
   :detail <map>}. `check+notify!` adds a debounced desktop notify on a
   healthy↔stale transition. Pure leaf: depends only on commit-ingest (the
   last-indexed pointer) + git + notify-send — NOT on the watcher loop (multi),
   so multi can require it without a cycle."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [babashka.http-client :as http])
  (:import [java.util.concurrent TimeUnit]))

(def ^:private FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))

(def stale-threshold-ms
  "A repo counts as stale only when its newest non-merge HEAD commit is older
   than this AND still un-ingested — long enough to clear normal per-cycle
   ingest lag (the watcher polls every ~5s), so a just-pushed commit doesn't
   false-alarm. Override via FUTON3C_FRESHNESS_THRESHOLD_MS."
  (or (some-> (System/getenv "FUTON3C_FRESHNESS_THRESHOLD_MS") Long/parseLong)
      (* 30 60 1000)))

(defn- git [repo-root & args]
  (let [{:keys [exit out]} (apply sh/sh "git" "-C" repo-root args)]
    (when (zero? exit) (not-empty (str/trim out)))))

;; Source-of-truth = `rev-parse HEAD` (NOT last-non-merge): commit-ingest records
;; `(current-head-sha repo)` = rev-parse HEAD as its last-indexed cursor (even when
;; HEAD is a merge it skips ingesting), so freshness must compare against the SAME
;; sha or every merge-headed repo false-alarms. (Caught in D7a verification.)

(defn- last-nonmerge-sha
  "The repo's newest NON-merge commit — the right source-of-truth for a store
   check, because commit-ingest skips merges, so a merge HEAD is never in the
   store (using rev-parse HEAD would false-alarm every merge-headed repo)."
  [repo-root]
  (git repo-root "rev-list" "--no-merges" "-1" "HEAD"))

(defn- commit-epoch-ms [repo-root sha]
  (some-> (git repo-root "show" "-s" "--format=%ct" sha) Long/parseLong (* 1000)))

(defn- head-in-store?
  "Is SHA's commit vertex actually present in substrate-2? This is store TRUTH
   (the same question substrate2_liveness_probe.sh asks), not the watcher's
   in-memory cursor — which can lag the store and cause cursor-lag false alarms."
  [sha]
  (let [resp (try (http/get (str FUTON1A "/api/alpha/hyperedge/hx:code/v05/commit:" sha)
                            {:throw false :timeout 5000})
                  (catch Exception _ nil))]
    (boolean (and (= 200 (:status resp))
                  (some-> (:body resp) (str/includes? "hx/type"))))))

(defn staleness
  "Vector of stale-repo maps: repos whose current git HEAD commit is NOT in
   substrate-2 AND is older than the grace threshold (genuinely un-ingested,
   not just-pushed). Uses store presence (truth) rather than the watcher cursor.
   ROOTS = [{:path :label}…]. A repo with no readable git HEAD is skipped
   (never false-alarm on a probe error)."
  ([roots] (staleness roots {}))
  ([roots {:keys [now-ms head-fn in-store?-fn epoch-fn]
           :or {now-ms       (System/currentTimeMillis)
                head-fn      last-nonmerge-sha
                in-store?-fn head-in-store?
                epoch-fn     commit-epoch-ms}}]
   (reduce
    (fn [acc {:keys [path label]}]
      (let [head (head-fn path)]
        (cond
          (nil? head)          acc          ;; can't read git → skip
          (in-store?-fn head)  acc          ;; HEAD commit is in substrate-2 → fresh
          :else
          (let [age (some->> (epoch-fn path head) (- now-ms))]
            (if (and age (> age stale-threshold-ms))
              (conj acc {:label label :head head
                         :head-age-ms age :reason :head-not-in-store})
              acc)))))                      ;; un-ingested but within grace → ok
    []
    roots)))

(defn check
  "Probe-family entry. :violation when (a) commit-ingest is OFF (the exact
   original freeze cause — short-circuit) or (b) any repo is stale past the
   threshold; :ok otherwise."
  [roots commit-ingest-on?]
  (cond
    (not commit-ingest-on?)
    {:outcome :violation
     :detail {:reason :commit-ingest-off
              :note (str "substrate-2 commit-ingest is OFF — the code/history layer is "
                         "FROZEN (the exact cause of the 2026-05-21 5-week silent freeze). "
                         "Re-enable the watcher's :commit-ingest? / "
                         "FUTON3C_MULTI_WATCHER_COMMIT_INGEST.")}}
    :else
    (let [stale (staleness roots)]
      (if (empty? stale)
        {:outcome :ok
         :detail {:note "substrate-2 is current with every repo's git HEAD"}}
        {:outcome :violation
         :detail {:reason :repos-stale
                  :stale stale
                  :note (str "substrate-2 has drifted from git HEAD: "
                             (count stale) " repo(s) have un-ingested commits older "
                             "than the staleness threshold — check commit-ingest health.")}}))))

;; ---- transition-based desktop notify (loud, debounced) ----

(defonce ^:private !last-outcome (atom nil))

(defn- notify! [title message]
  (try
    (-> (ProcessBuilder. ["notify-send" "--urgency" "critical" title message])
        .start
        (.waitFor 2000 TimeUnit/MILLISECONDS))
    (catch Throwable _ nil)))

(defn check+notify!
  "Run `check`, fire a desktop notify on a healthy↔stale TRANSITION (debounced
   via `!last-outcome`), and return the outcome. Idempotent between transitions,
   so it's safe to call from any driver (the watcher cycle and/or a probe sweep).
   Fires on any →:violation (including the first observation, since booting into
   a freeze IS the alarm); fires recovery only on :violation→:ok."
  [roots commit-ingest-on?]
  (let [o   (check roots commit-ingest-on?)
        cur (:outcome o)
        prev @!last-outcome]
    (when (not= prev cur)
      (reset! !last-outcome cur)
      (cond
        (= cur :violation)
        (notify! "futon3c — substrate-2 FRESHNESS ALARM"
                 (str (name (:reason (:detail o))) " — " (:note (:detail o))))
        (and (= cur :ok) (= prev :violation))
        (notify! "futon3c — substrate-2 freshness recovered"
                 "substrate-2 is current with git HEAD again.")))
    o))
