(ns futon3c.logic.disposition-derive
  "Derive `.futon-disposition.edn`-shaped state from existing sources,
   so the operator does not have to hand-author per-path entries
   (per D-05 of M-bounded-in-flight-state).

   Sources used (file-based; no HTTP / no LLM):
     1. **Active mission references**. Walk
        `<repo>/holes/missions/M-*.md`; for each mission whose
        `Status:` resolves to a non-terminal disposition (NOT
        `:closed`, `:archived`, `:parked`), extract path references
        from its body and mark them as :in-progress with the
        mission-id as :reasoning.

   Sources reserved for later passes:
     2. Recent session-buffer activity (via emacsclient).
     3. git commit-vs-edit-mtime gap (file edited more recently
        than its tracking commit).
     4. Linked excursion bodies / lab notes.

   The derivation produces a map matching the schema that
   `disposition-edn/load-state` returns, so downstream code
   (`active?`, `self-decayed-paths`, etc.) can consume either source
   uniformly. `combined-state` merges file-based override on top of
   derived state — operator-attestation wins when both have a path.

   This namespace deliberately reuses the existing
   `archaeology.clj` parsers rather than re-parsing mission status
   from scratch (per Joe's D-05 instruction not to add another
   parser).

   Mission: M-bounded-in-flight-state INSTANTIATE D-05."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.logic.archaeology :as archaeology]))

(def terminal-dispositions
  "Mission-doc dispositions that mean 'do not derive in-progress
   from this mission's body.' Open / IDENTIFY-redux / etc. are
   non-terminal and DO contribute."
  #{:closed :archived :parked})

(defn- list-mission-files
  "Return [java.io.File ...] for `<repo>/holes/missions/M-*.md`."
  [repo-path]
  (let [missions-dir (io/file repo-path "holes" "missions")]
    (when (.isDirectory missions-dir)
      (->> (.listFiles missions-dir)
           (filter (fn [^java.io.File f]
                     (and (.isFile f)
                          (re-matches #"M-.*\.md" (.getName f)))))
           vec))))

(defn- mission-active?
  "True iff the mission at PATH has a non-terminal disposition."
  [^java.io.File mission-file]
  (try
    (let [path (.getPath mission-file)
          status (archaeology/parse-mission-doc-status path)]
      (not (contains? terminal-dispositions status)))
    (catch Throwable _ true)))   ; absence of status reads as :open by archaeology

(defn- mission-id-from-filename
  "Convert `M-foo-bar.md` → `M-foo-bar`."
  [^java.io.File f]
  (-> (.getName f) (str/replace #"\.md\z" "")))

;; --- Path-reference extraction --------------------------------------------

;; A reasonably tight regex for explicit path references — `repo/path`
;; with a recognizable extension, or under known top-level dirs.
;; Conservative by design: false negatives are fine (the derivation
;; under-claims rather than over-claims — operator-attested overrides
;; in the file always win).
(def ^:private path-reference-pattern
  #"(?x)
    (?<! [a-zA-Z0-9._/])         # boundary: not preceded by a word char
    (futon[0-9a-z]*               # repo prefix: futon0, futon3a, futon3c, ...
       (?:/[a-zA-Z0-9._-]+)+      # one or more / segments
       (?:\.[a-zA-Z0-9]+)?        # optional extension
    )
    (?![a-zA-Z0-9._])")

(defn- extract-path-references
  "Return a set of repo-prefixed path strings mentioned in TEXT.
   Conservative: matches only on `futon*/<...>` shapes.
   `re-seq` with a capture group returns vectors `[full group1 ...]`;
   we want the captured group only."
  [text]
  (when (string? text)
    (->> (re-seq path-reference-pattern text)
         (map (fn [m] (if (vector? m) (second m) m)))
         set)))

(defn- split-repo-prefix
  "`futon3c/src/foo.clj` → [`futon3c` `src/foo.clj`].
   nil for non-futon paths."
  [s]
  (when-let [m (re-matches #"(futon[0-9a-z]*)/(.*)" s)]
    [(nth m 1) (nth m 2)]))

(defn- mission-entry
  "Build the disposition entry for a path referenced by MISSION-ID
   in MISSION-FILE."
  [mission-id ^java.io.File mission-file]
  {:reasoning (format "referenced by active mission %s" mission-id)
   :since (str (java.time.LocalDate/ofEpochDay
                (long (/ (.lastModified mission-file)
                         (* 1000 60 60 24)))))
   :review-by (format "%s reaches :closed / :archived / :parked"
                      mission-id)
   :source :derived/active-mission})

;; --- Public API -----------------------------------------------------------

(defn derive-from-active-missions
  "Walk every repo in REPO-PATHS, parse its `holes/missions/M-*.md`
   files, and produce a per-repo disposition state for paths
   referenced by *active* missions (not :closed / :archived /
   :parked).

   Returns a map  repo-abs-path → disposition-state where the
   disposition-state matches the schema of
   `disposition-edn/load-state`'s return.

   The function is intentionally cheap — it walks file mtimes and
   reads files; no HTTP, no LLM.

   When MISSION-FILE-FILTER is provided it is called as
   `(filter mission-file)` and only files for which it returns
   truthy are inspected (useful for tests)."
  ([repo-paths] (derive-from-active-missions repo-paths {}))
  ([repo-paths {:keys [mission-file-filter]
                :or {mission-file-filter (constantly true)}}]
   (let [;; First pass: collect (mission-id, referenced-paths) per repo.
         all-missions
         (for [repo repo-paths
               mission-file (list-mission-files repo)
               :when (mission-file-filter mission-file)
               :when (mission-active? mission-file)]
           {:repo-path repo
            :mission-file mission-file
            :mission-id (mission-id-from-filename mission-file)
            :body (try (slurp mission-file) (catch Throwable _ ""))})
         ;; Second pass: build per-repo state from the collected refs.
         per-repo (atom {})]
     (doseq [{:keys [mission-file mission-id body]} all-missions
             ref (extract-path-references body)
             :let [[ref-repo rel-path] (split-repo-prefix ref)]
             :when (and ref-repo rel-path (not (str/blank? rel-path)))
             ref-repo-abs (filter #(re-find (re-pattern (str ref-repo "$"))
                                            (str %))
                                  repo-paths)]
       (swap! per-repo
              update-in [ref-repo-abs :in-progress]
              (fnil assoc {})
              rel-path
              (mission-entry mission-id mission-file)))
     (-> @per-repo
         (->> (reduce-kv (fn [m k v]
                           (assoc m k (assoc v :decided-at
                                              (str (java.time.Instant/now))
                                              :source :derived)))
                         {}))))))

(defn combined-state
  "Merge `file-based-state` (from `disposition-edn/load-state`) on
   top of `derived-state` (from `derive-from-active-missions`) for
   ONE repo. Operator-attested file entries win; derived entries
   fill the rest."
  [derived-state file-based-state]
  (let [derived-in-progress (or (:in-progress derived-state) {})
        file-in-progress    (or (:in-progress file-based-state) {})
        merged-in-progress (merge derived-in-progress file-in-progress)]
    (cond-> {:in-progress merged-in-progress}
      (:decided-at file-based-state)
      (assoc :decided-at (:decided-at file-based-state))

      (and (not (:decided-at file-based-state))
           (:decided-at derived-state))
      (assoc :decided-at (:decided-at derived-state))

      (:source derived-state)
      (assoc :source-derived (:source derived-state)))))
