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

(defn- list-uncommitted-paths
  "Return [{:path string :status string :age-days number :bytes long} ...]
   for paths shown in `git status --porcelain` for REPO-PATH.
   Empty when not a git repo or git fails. Status string is the
   2-character porcelain code (e.g. \" M\", \"D \", \"??\").
   Block 2 fills the per-path mtime + bytes computation."
  [repo-path]
  (try
    (let [{:keys [exit out]}
          (shell/sh "git" "-C" repo-path "status" "--porcelain")]
      (if (zero? exit)
        (->> (str/split-lines (or out ""))
             (remove str/blank?)
             (mapv (fn [line]
                     {:status (subs line 0 (min 2 (count line)))
                      :path (str/triml (subs line (min 3 (count line))))
                      ;; Block 2 fills these:
                      :age-days nil
                      :bytes nil})))
        []))
    (catch Throwable _ [])))

(defn- apply-eligibility-filter
  "Drop paths that should not contribute to pressure:
     - gitignored (by status code)
     - volatile-tracked (Block 2 / Q-12)
     - disposition-opted-out per `.futon-disposition.edn` (Block 3)
   Block 1 is a passthrough placeholder; Blocks 2 + 3 fill it."
  [paths _disposition-state]
  paths)

(defn- compute-working-tree-pressure
  "Pressure for the working-tree channel of REPO-PATH.
   Block 1 returns nil (skeleton); Block 2 fills with
     max(count/N, age/D, bytes/B).
   Returns {:P number :count int :max-age-days number :total-bytes long}
   or nil while skeleton."
  [_repo-path _eligible-paths _nominals]
  ;; Block 2 implementation goes here.
  nil)

(defn check-working-tree-pressure
  "Probe check-fn factory for `metabolic-balance/working-tree`.

   REPO-PATHS is a seq of git-repo paths to scan. Returns the
   standard probe-result shape `{:outcome :ok | :violation
   | :inactive :detail <map>}`.

   Skeleton form (Block 1): returns :inactive with :deferred? true
   until pressure function lands in Block 2.

   Options (planned):
     :nominals — per-channel nominal map; defaults to
       `default-working-tree-nominals`.
     :disposition-state — output of `futon3c.logic.disposition-edn/load`
       (Block 3). When omitted, eligibility filter is identity.

   See M-bounded-in-flight-state INSTANTIATE-improvisation, Block 1."
  ([repo-paths] (check-working-tree-pressure repo-paths {}))
  ([repo-paths {:keys [nominals disposition-state]
                :or {nominals default-working-tree-nominals}}]
   (fn [_evidence-store]
     {:outcome :inactive
      :detail  {:deferred? true
                :reason "INSTANTIATE Block 1 skeleton; pressure function lands in Block 2."
                :scanned-repos (count repo-paths)
                :nominals nominals
                :disposition-state-present? (some? disposition-state)
                :invariant I-metabolic-balance}})))

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

(defn check-working-tree-pressure-on-load!
  "Run the working-tree drain-channel check at JVM load time.
   Skeleton wrapper — Block 4 finishes wiring (probe-tap registration,
   evidence emission). For now emits the :inactive skeleton outcome."
  ([evidence-store] (check-working-tree-pressure-on-load! evidence-store {}))
  ([evidence-store {:keys [repo-paths nominals disposition-state]
                    :or {repo-paths default-repo-paths
                         nominals default-working-tree-nominals}}]
   ((check-working-tree-pressure repo-paths
                                 {:nominals nominals
                                  :disposition-state disposition-state})
    evidence-store)))
