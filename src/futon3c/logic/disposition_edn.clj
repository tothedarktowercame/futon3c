(ns futon3c.logic.disposition-edn
  "Read `.futon-disposition.edn` per repo and resolve which paths are
   *actively opted out* of working-tree pressure (per ARGUE-2 of
   M-bounded-in-flight-state).

   Schema (per ARGUE-2):

     {:in-progress {\"path/or/glob\"
                     {:reasoning \"...\"
                      :since \"2026-05-03\"
                      :review-by <date-or-event-or-no-review-needed>}
                    ...}
      :ignored-here-but-tracked {...}   ; placeholder for tracked-volatile
      :decided-at \"2026-05-03T...\"}

   `:review-by` accepts three forms:
     - ISO-8601 date string (e.g. \"2026-05-17\"): past = self-decayed.
     - Event-form free-text string (e.g. \"M-single-entry-point
       INSTANTIATE complete\"): treated as live until a mission-state
       evaluator is wired (deferred to a future block).
     - `:no-review-needed true`: the entry never self-decays. Use
       sparingly — entries that genuinely never need review are rare.
     - Missing `:review-by` AND age (now − :since) > 30 days:
       the entry itself becomes a drain symptom (per ARGUE-2 BECAUSE).

   Glob matching uses Java NIO's `PathMatcher` with `glob:` syntax,
   which supports `*` (single segment), `**` (multi-segment), and
   `?` (single character). Path globs are matched against the
   working-tree-relative path of each candidate file.

   Mission: M-bounded-in-flight-state INSTANTIATE Block 3."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.nio.file FileSystems Path Paths]
           [java.time LocalDate]
           [java.time.format DateTimeParseException]))

(def disposition-file-name ".futon-disposition.edn")

(def review-stale-after-days
  "If an :in-progress entry has no :review-by AND is older than this
   many days since :since, the entry itself becomes a drain symptom
   (re-eligible for pressure even though it claims to be in-progress).
   Per ARGUE-2 self-decay rule."
  30)

(defn load-state
  "Read REPO-PATH/.futon-disposition.edn and return its parsed map,
   or nil if the file is absent or unreadable.

   Returns the raw EDN structure with keys preserved as keywords
   under default reader options. Validation is intentionally light
   here — the active? predicate handles missing/malformed entries
   gracefully."
  [repo-path]
  (let [f (io/file repo-path disposition-file-name)]
    (when (and f (.exists f))
      (try
        (edn/read-string (slurp f))
        (catch Throwable _ nil)))))

(defn- ^Path as-path [^String s]
  (Paths/get s (into-array String [])))

(defn- glob-match?
  "True iff REL-PATH (a String) matches GLOB (a String) under the
   `glob:` syntax of Java NIO's PathMatcher. Trailing slash on a
   glob makes it directory-like — every path under it matches."
  [glob rel-path]
  (try
    (let [normalised (cond-> glob
                       (.endsWith glob "/") (str "**"))
          fs (FileSystems/getDefault)
          matcher (.getPathMatcher fs (str "glob:" normalised))]
      (.matches matcher (as-path rel-path)))
    (catch Throwable _ false)))

(defn- ^LocalDate parse-iso-date
  "Parse YYYY-MM-DD or full ISO-8601 timestamp; return LocalDate or nil."
  [s]
  (when (string? s)
    (try
      (LocalDate/parse (subs s 0 (min 10 (count s))))
      (catch DateTimeParseException _ nil))))

(defn- review-by-live?
  "Given an entry's :review-by value AND a `now` LocalDate, decide
   whether the review-by date is still in the future (live) or has
   passed (self-decayed).

   Forms:
     - String parseable as date → live iff date >= now.
     - String not parseable as date → event-form; treat as live
       (mission-state evaluator deferred).
     - Missing/nil → governed by review-stale-after-days check at
       caller via :since fallback.
     - :no-review-needed true → always live."
  [review-by now]
  (cond
    (nil? review-by) ::no-review-by
    (= true review-by) ::no-review-by  ; defensive against schema variation
    (and (map? review-by) (true? (:no-review-needed review-by))) true
    (string? review-by)
    (if-let [parsed (parse-iso-date review-by)]
      (not (.isBefore parsed now))   ; future-or-today = live
      true)                           ; event-form: live until evaluator wired
    :else true))

(defn- entry-active?
  "True iff a single :in-progress entry should still suppress pressure
   for paths it covers, given `now` LocalDate."
  [entry-map now]
  (let [review-by (:review-by entry-map)
        result (review-by-live? review-by now)]
    (cond
      (= ::no-review-by result)
      ;; No :review-by; check age via :since vs review-stale-after-days.
      (if-let [since (parse-iso-date (:since entry-map))]
        (let [age-days (.toDays
                        (java.time.Duration/ofDays
                         (- (.toEpochDay now) (.toEpochDay since))))]
          (<= age-days review-stale-after-days))
        true)  ; no :since either; conservatively live
      :else (boolean result))))

(defn active?
  "True iff REL-PATH is *actively opted out* of pressure under STATE
   (the parsed disposition map) at LocalDate NOW.

   A path is active iff some `:in-progress` entry's glob matches
   REL-PATH AND the entry has not self-decayed."
  ([state rel-path]
   (active? state rel-path (LocalDate/now)))
  ([state rel-path now]
   (boolean
    (when-let [entries (:in-progress state)]
      (some (fn [[glob entry-map]]
              (and (glob-match? glob rel-path)
                   (entry-active? entry-map now)))
            entries)))))

(defn self-decayed-paths
  "Return [glob ...] for entries in STATE whose :review-by has
   passed (or whose :since-age has exceeded review-stale-after-days
   without a :review-by). These globs themselves become drain
   symptoms — the operator's relief valve has overstayed its
   welcome."
  ([state] (self-decayed-paths state (LocalDate/now)))
  ([state now]
   (->> (:in-progress state)
        (remove (fn [[_ entry-map]] (entry-active? entry-map now)))
        (map first)
        vec)))

(defn summary
  "Brief human-readable summary of the loaded disposition state.
   Used by the drain-channel diagnostics."
  ([state] (summary state (LocalDate/now)))
  ([state now]
   {:in-progress-count (count (:in-progress state))
    :self-decayed-count (count (self-decayed-paths state now))
    :decided-at (:decided-at state)}))
