(ns futon3c.logic.metabolic-balance
  "Drain-channel check-fn factories under family `metabolic-balance`.

   The metabolic-balance faculty (10th cognitive-faculty shape; see
   M-bounded-in-flight-state) tracks the operator's *allostatic
   balance* over time as the integral of (block-closures-at-hinges
   − allostatic-load-symptoms). Each artifact-class is a drain
   channel under this faculty.

   The first sibling instantiated here is the *graduated-tier*
   working-tree drain channel:

     - `metabolic-balance/working-tree`
        A: uncommitted edits in a git working tree.
        Pressure: max(count_eligible/N, age_eligible_max/D,
                       bytes_eligible/B), per-channel.
        Tiers: <1.0 silent; 1.0-2.0 advisory;
               2.0-4.0 high; ≥4.0 stop-the-line.
        Eligibility filter excludes: noise (gitignored),
        volatile-tracked, disposition-opted-out (per
        `.futon-disposition.edn`).

   The existing `bounded-disposition` siblings (branch, mission-doc,
   stash) read under this faculty as *single-tier* drain channels
   (per harmolodic reread; see
   `library/invariant-coherence/drain-channel-shape.flexiarg`).
   No structural change to those siblings is implied.

   Library references:
     - futon3/library/structure/block-as-futonic-revolution.flexiarg
     - futon3/library/structure/mana-allostasis.flexiarg
     - futon3/library/invariant-coherence/drain-channel-shape.flexiarg
     - futon3/library/structure/cook-ting.flexiarg
     - futon3/library/structure/hinge-point.flexiarg

   Mission: M-bounded-in-flight-state (futon3c/holes/missions/)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.logic.disposition-edn :as disposition-edn]
            [futon3c.logic.mana-session :as mana-session]
            [futon3c.logic.probe :as probe])
  (:import [java.time Instant]))

(def I-metabolic-balance
  "Canonical statement of the metabolic-balance faculty. Grep-verifiable."
  (str "I-metabolic-balance: the operator's allostatic state in the "
       "stack is the integral of (block-closures-at-hinges − "
       "allostatic-load-symptoms) over time. Net balance must remain "
       "non-negative under sustained operation. Each artifact-class "
       "is a drain channel under this faculty; channels are voices "
       "in one harmonic structure (harmolodic reread). Working-tree "
       "is the first graduated-tier sibling; bounded-disposition "
       "siblings (branch, mission-doc, stash) are single-tier "
       "instances of the same faculty."))

;; ---------------------------------------------------------------------------
;; Tier semantics (shared across drain channels)
;; ---------------------------------------------------------------------------

(def tier-thresholds
  "Boundaries between tiers. P below `silent-max` is silent; from
   there to `advisory-max` is advisory; from there to `high-max` is
   high; ≥ `high-max` is stop-the-line."
  {:silent-max   1.0
   :advisory-max 2.0
   :high-max     4.0})

(defn pressure->tier
  "Map a numeric pressure value to its tier keyword:
     :silent | :advisory | :high | :stop-the-line"
  [^double p]
  (cond
    (< p (:silent-max tier-thresholds))   :silent
    (< p (:advisory-max tier-thresholds)) :advisory
    (< p (:high-max tier-thresholds))     :high
    :else                                 :stop-the-line))

;; ---------------------------------------------------------------------------
;; metabolic-balance/working-tree
;; ---------------------------------------------------------------------------

(def default-working-tree-nominals
  "Per-dimension nominal values for the working-tree drain channel.
   Calibrated against the 2026-05-03 sweep empirical run (V-1 anchors
   in M-bounded-in-flight-state). Revisit after first INSTANTIATE
   produces more data points (see DOCUMENT QA-14)."
  {:N-count 20
   :D-age-days 7
   :B-bytes (* 10 1024 1024)})  ; 10 MB

(def default-undecided-bound
  "Soft-cap on count of in-flight items past which advisory tier
   begins regardless of age/size."
  20)

(defn- file-age-days
  "Age in days (double) of FILE on disk, computed from mtime vs
   System/currentTimeMillis. Returns 0.0 if file is absent (e.g.
   a deletion shown by git status)."
  [^java.io.File f]
  (if (and f (.exists f))
    (let [now-ms (System/currentTimeMillis)
          mtime  (.lastModified f)]
      (if (zero? mtime)
        0.0
        (double (/ (- now-ms mtime) (* 1000.0 60 60 24)))))
    0.0))

(defn- file-bytes
  "Bytes of FILE on disk; 0 if file is absent."
  [^java.io.File f]
  (if (and f (.exists f)) (.length f) 0))

(defn- list-uncommitted-paths
  "Return [{:path string :status string :age-days double :bytes long} ...]
   for paths shown in `git status --porcelain` for REPO-PATH. Status is
   the 2-character porcelain code (e.g. \" M\", \"D \", \"??\"). Empty
   when not a git repo or git fails. For deleted files, age and bytes
   are 0 (the file is gone; the index reference is what's pending)."
  [repo-path]
  (try
    (let [{:keys [exit out]}
          (shell/sh "git" "-C" repo-path "status" "--porcelain")]
      (if (zero? exit)
        (->> (str/split-lines (or out ""))
             (remove str/blank?)
             (mapv (fn [line]
                     (let [status (subs line 0 (min 2 (count line)))
                           rel (str/triml (subs line (min 3 (count line))))
                           f (io/file repo-path rel)]
                       {:status status
                        :path rel
                        :age-days (file-age-days f)
                        :bytes (file-bytes f)}))))
        []))
    (catch Throwable _ [])))

(defn- apply-eligibility-filter
  "Drop paths that should not contribute to pressure. Currently:
     - disposition-opted-out per `.futon-disposition.edn`
       (via disposition-edn/active?).
   gitignored paths are auto-excluded by `git status --porcelain`
   itself (it omits ignored files unless --ignored is passed).
   Volatile-tracked filtering (DOCUMENT QA-12) is a future refinement."
  [paths disposition-state]
  (if-not disposition-state
    (vec paths)
    (vec
     (remove (fn [{:keys [path]}]
               (disposition-edn/active? disposition-state path))
             paths))))

(defn compute-channel-pressure
  "Compute working-tree pressure from a vec of eligible path-records
   (each with at least :age-days and :bytes), under NOMINALS map of
   {:N-count int :D-age-days number :B-bytes number}.

   Pressure is `max(count/N, age/D, bytes/B)` per ARGUE-3. Returns
   {:P double :count int :max-age-days double :total-bytes long
    :tier keyword}. Pure function; no IO."
  [eligible-paths {:keys [N-count D-age-days B-bytes]
                   :as _nominals}]
  (if (empty? eligible-paths)
    {:P 0.0 :count 0 :max-age-days 0.0 :total-bytes 0 :tier :silent}
    (let [n            (count eligible-paths)
          max-age      (double (apply max 0.0
                                      (keep #(some-> (:age-days %) double)
                                            eligible-paths)))
          total-bytes  (long (reduce + 0 (keep :bytes eligible-paths)))
          P            (max (/ n (double N-count))
                            (/ max-age (double D-age-days))
                            (/ total-bytes (double B-bytes)))]
      {:P P
       :count n
       :max-age-days max-age
       :total-bytes total-bytes
       :tier (pressure->tier P)})))

(defn- compute-working-tree-pressure
  "Per-repo: list paths, filter for eligibility, compute pressure.
   Returns the map shape from compute-channel-pressure plus :repo."
  [repo-path disposition-state nominals]
  (-> (list-uncommitted-paths repo-path)
      (apply-eligibility-filter disposition-state)
      (compute-channel-pressure nominals)
      (assoc :repo repo-path)))

(defn- load-disposition-for-repo
  "Read `.futon-disposition.edn` for REPO-PATH; nil if absent."
  [repo-path]
  (disposition-edn/load-state repo-path))

(defn tier->outcome
  "Map a tier keyword to the probe-result :outcome. Silent and
   advisory are :ok (the operator notices but nothing fails);
   high and stop-the-line are :violation (boot-time evidence
   surfaces the channel). Public so downstream surfaces (HUD,
   War Machine views) can render outcome consistently with the
   check-fn."
  [tier]
  (case tier
    :silent        :ok
    :advisory      :ok
    :high          :violation
    :stop-the-line :violation))

(defn check-working-tree-pressure
  "Probe check-fn factory for `metabolic-balance/working-tree`.

   REPO-PATHS is a seq of git-repo paths to scan. Returns the
   standard probe-result shape `{:outcome :ok | :violation
   :detail <map>}`. Detail carries :tier (:silent / :advisory /
   :high / :stop-the-line) so downstream surfaces can render
   graduated drive (the ARGUE-1 distinguishing feature of this
   sibling vs the bounded-disposition single-tier siblings).

   Options:
     :nominals — per-channel nominal map; defaults to
       `default-working-tree-nominals`.
     :disposition-state-by-repo — map repo-path → state. When
       omitted, the check loads `.futon-disposition.edn` for each
       repo via `disposition-edn/load-state`. Pass an explicit map
       for hermetic testing.

   See M-bounded-in-flight-state INSTANTIATE-improvisation, Blocks 1-3."
  ([repo-paths] (check-working-tree-pressure repo-paths {}))
  ([repo-paths {:keys [nominals disposition-state-by-repo
                       session-id mana-session-base-url]
                :or {nominals default-working-tree-nominals}}]
   (fn [_evidence-store]
     (try
       (let [per-repo
             (vec
              (for [repo repo-paths
                    :when (.exists (io/file repo ".git"))]
                (let [disp (or (get disposition-state-by-repo repo)
                               (load-disposition-for-repo repo))]
                  (compute-working-tree-pressure repo disp nominals))))
             max-tier-value (reduce
                             (fn [acc {:keys [tier]}]
                               (max acc
                                    (case tier
                                      :silent 0
                                      :advisory 1
                                      :high 2
                                      :stop-the-line 3)))
                             0
                             per-repo)
             max-tier (case max-tier-value
                        0 :silent
                        1 :advisory
                        2 :high
                        3 :stop-the-line)
             outcome (tier->outcome max-tier)
             ;; Per V-6 amendment (II): drain measures per-repo;
             ;; balance aggregates per-session. When a session-id is
             ;; supplied, fetch the session-attributed mana summary
             ;; from nonstarter and surface it alongside the per-repo
             ;; drain readings. Failures (nonstarter unreachable) are
             ;; silent — the drain readings stand on their own.
             mana-opts (cond-> {} mana-session-base-url
                                  (assoc :base-url mana-session-base-url))
             mana-summary (when session-id
                            (mana-session/mana-summary session-id mana-opts))]
         {:outcome outcome
          :detail  (cond-> {:scanned-repos (count per-repo)
                            :max-tier max-tier
                            :max-pressure (apply max 0.0 (map :P per-repo))
                            :per-repo per-repo
                            :nominals nominals
                            :invariant I-metabolic-balance}
                     session-id (assoc :session-id session-id
                                       :session-mana mana-summary))})
       (catch Throwable t
         {:outcome :violation
          :detail  {:exception (str (.getName (class t)) ": "
                                    (.getMessage t))
                    :invariant I-metabolic-balance}})))))

;; ---------------------------------------------------------------------------
;; on-load wrapper (mirrors archaeology siblings' shape)
;; ---------------------------------------------------------------------------

(def default-repo-paths
  "Default futon-stack repo paths for the working-tree drain channel.
   Mirrors `futon3c.logic.locus/default-repo-paths` shape."
  ["/home/joe/code/futon0"
   "/home/joe/code/futon1a"
   "/home/joe/code/futon3"
   "/home/joe/code/futon3a"
   "/home/joe/code/futon3b"
   "/home/joe/code/futon3c"
   "/home/joe/code/futon4"
   "/home/joe/code/futon5"
   "/home/joe/code/futon5a"])

;; ---------------------------------------------------------------------------
;; Boot-time emit + print helpers (Block 4)
;; ---------------------------------------------------------------------------

(defn- emit-load-time-fired!
  "Emit a `:family-fired` evidence entry for the metabolic-balance
   load-time check. Returns the boundary receipt."
  [evidence-store family-id author {:keys [outcome detail invariant]}]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :pattern
              :ref/id (str "structural-law-family/" (name family-id))}
    :type :coordination
    :claim-type :observation
    :author author
    :body {:event :family-fired
           :family-id family-id
           :outcome outcome
           :detail (or detail {})
           :invariant invariant
           :timestamp (str (Instant/now))}}))

(defn- print-load-time-result!
  [family-id {:keys [outcome detail]}]
  (let [label (name family-id)
        max-tier (:max-tier detail)
        max-pressure (:max-pressure detail)]
    (case outcome
      :ok
      (println (str "[metabolic-balance] " label " load-time check: OK"
                    (when max-tier
                      (str " (max-tier=" (name max-tier)
                           " P=" (format "%.2f" (or max-pressure 0.0)) ")"))))

      :inactive
      (println (str "[metabolic-balance] " label " load-time check: INACTIVE ("
                    (pr-str detail) ")"))

      (do
        (println "================================================================")
        (println (str "[metabolic-balance] " label " LOAD-TIME WARNING"))
        (when max-tier
          (println (str "              max-tier: " (name max-tier)
                        "   max-P: " (format "%.2f" (or max-pressure 0.0)))))
        (when-let [violators (->> (:per-repo detail)
                                  (filter (fn [r] (#{:high :stop-the-line}
                                                   (:tier r))))
                                  vec)]
          (when (seq violators)
            (doseq [v violators]
              (println (format "              %-50s tier=%-14s P=%.2f  count=%d  max-age=%.1fd  bytes=%d"
                               (str (:repo v))
                               (name (:tier v))
                               (:P v)
                               (:count v)
                               (:max-age-days v)
                               (:total-bytes v))))))
        (println "              Boot continues; the warning is recorded as evidence.")
        (println "================================================================")))))

(defn- run-load-time-check!
  [evidence-store family-id author result {:keys [emit? print?]
                                           :or {emit? true print? true}}]
  (let [outcome (or (:outcome result) :violation)
        detail (:detail result)
        invariant (or (:invariant result) (:invariant detail))
        receipt (when emit?
                  (try
                    (emit-load-time-fired!
                     evidence-store family-id author
                     {:outcome outcome :detail detail :invariant invariant})
                    (catch Throwable t
                      {:ok false
                       :error/code :emit-failed
                       :error/exception (str (.getName (class t)) ": "
                                             (.getMessage t))})))]
    (when print?
      (print-load-time-result! family-id {:outcome outcome :detail detail}))
    (cond-> (assoc result :outcome outcome)
      receipt (assoc :emit-receipt receipt))))

(defn check-working-tree-pressure-on-load!
  "Run the working-tree drain-channel check at JVM load time and emit
   a `:family-fired` evidence entry. Mirrors the archaeology siblings'
   on-load! shape; output line begins with `[metabolic-balance]` for
   distinguishability in boot logs."
  ([evidence-store] (check-working-tree-pressure-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? repo-paths nominals
                           disposition-state-by-repo
                           session-id mana-session-base-url]
                    :or {emit? true
                         print? true
                         repo-paths default-repo-paths
                         nominals default-working-tree-nominals}}]
   (run-load-time-check!
    evidence-store
    :metabolic-balance/working-tree
    "metabolic-balance/check-working-tree-pressure-on-load!"
    ((check-working-tree-pressure
      repo-paths
      {:nominals nominals
       :disposition-state-by-repo disposition-state-by-repo
       :session-id session-id
       :mana-session-base-url mana-session-base-url})
     evidence-store)
    {:emit? emit? :print? print?})))

;; ---------------------------------------------------------------------------
;; Probe-tap registration (Block 4)
;; ---------------------------------------------------------------------------

(defn register-metabolic-balance-taps!
  "Register the metabolic-balance/* check-fns with `futon3c.logic.probe`.
   Operator-driven; intended to be called once at activation time
   alongside `register-default-taps!`,
   `register-archaeology-control-taps!`, and `register-locus-taps!`.

   Options:
     :repo-paths — override the default futon-stack repo list.
     :nominals — override the default working-tree nominals.

   Returns the set of registered family-ids.

   First sibling: metabolic-balance/working-tree (graduated-tier).
   Future siblings (when their hinges open): mission-proliferation,
   session-sprawl, branch/PR ahead-of-origin, untriaged-evidence — see
   M-bounded-in-flight-state DOCUMENT QA Q-15."
  ([] (register-metabolic-balance-taps! {}))
  ([{:keys [repo-paths nominals]
     :or {repo-paths default-repo-paths
          nominals default-working-tree-nominals}}]
   (probe/register-family-check!
    :metabolic-balance/working-tree
    (check-working-tree-pressure repo-paths {:nominals nominals}))
   #{:metabolic-balance/working-tree}))
