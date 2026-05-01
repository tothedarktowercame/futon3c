(ns futon3c.logic.archaeology
  "Subsumption-witness check-fn factories under family `archaeology-control`.

   Three sibling invariants share the namespace prefix
   `obsolescence-recognition/<artifact-class>`. Each returns the standard
   probe shape `{:outcome :ok | :violation :detail <map>}` so they slot
   directly into `futon3c.logic.probe`'s `family-check-fns` registry.

     - `obsolescence-recognition/autostash`
        A: `git stash list` per repo
        P: HEAD-reachable commit graph
        subsumption: `git stash show -p N | git apply --reverse --check` clean

     - `obsolescence-recognition/deferred-stub`
        A: deferred-stub registrations in `family-check-fns`
        P: live registrations whose check-fn is non-deferred
           (or inventory entries with :status :operational)
        subsumption: registered fn returns :inactive with :deferred? true
        AND inventory shows :status :operational for that family-id

     - `obsolescence-recognition/pipeline-tracer`
        A: open `:pipeline-tracer-item` evidence entries
        P: matching `:pipeline-tracer-closed` evidence entry, OR
           past target-date with no close
        subsumption: track-id has a closed-evidence entry; otherwise
        past-due tracers without closure are flagged as obsolete

   Library references:
     - futon3/library/invariant-coherence/subsumption-witness.flexiarg
     - futon3/library/invariant-coherence/protocol-family-naming.flexiarg

  Mission: M-archaeology-control (futon3c/holes/missions/)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store]
            [futon3c.logic.inventory :as inventory]
            [futon3c.logic.probe :as probe])
  (:import [java.time Instant]))

(def I-obsolescence-recognition
  "Canonical statement of the subsumption-witness shape. Grep-verifiable."
  (str "I-obsolescence-recognition: a latent artifact A is obsolete "
       "relative to a stronger record P when A's information content is "
       "subsumed by P, and A occupies a parallel/transient position while "
       "P is in the canonical/durable position. Each artifact-class is a "
       "sibling invariant under family archaeology-control with namespace "
       "id obsolescence-recognition/<artifact-class>."))

;; ---------------------------------------------------------------------------
;; obsolescence-recognition/autostash
;; ---------------------------------------------------------------------------

(defn- list-stashes
  "Return [{:n int :subject string :sha string} ...] for `git stash list` in REPO-PATH.
   Empty when no stashes or git fails."
  [repo-path]
  (try
    (let [{:keys [exit out]}
          (shell/sh "git" "-C" repo-path "stash" "list" "--format=%gd|%H|%s")]
      (if (zero? exit)
        (->> (str/split-lines (or out ""))
             (remove str/blank?)
             (mapv (fn [line]
                     (let [[gd sha subj] (str/split line #"\|" 3)
                           n (some-> gd
                                     (str/replace #"^stash@\{" "")
                                     (str/replace #"\}$" "")
                                     (Long/parseLong))]
                       {:n n :sha sha :subject (or subj "")}))))
        []))
    (catch Throwable _ [])))

(defn- stash-subsumed-by-head?
  "True iff `git stash show -p N | git apply --reverse --check` succeeds —
   i.e. the stash's content is already present in HEAD."
  [repo-path n]
  (try
    (let [show (shell/sh "git" "-C" repo-path "stash" "show" "-p"
                         (str "stash@{" n "}"))]
      (when (zero? (:exit show))
        (let [apply (shell/sh "git" "-C" repo-path "apply" "--reverse"
                              "--check" "-"
                              :in (or (:out show) ""))]
          (zero? (:exit apply)))))
    (catch Throwable _ false)))

(defn check-autostash-obsolescence
  "Probe check-fn for `obsolescence-recognition/autostash`.

   REPO-PATHS is a seq of git-repo paths to scan. For each, enumerate
   stashes and test whether each is subsumed by HEAD. Returns the standard
   probe-result shape."
  [repo-paths]
  (fn [_evidence-store]
    (try
      (let [obsolete
            (vec
             (for [repo repo-paths
                   :when (.exists (io/file repo ".git"))
                   stash (list-stashes repo)
                   :when (stash-subsumed-by-head? repo (:n stash))]
               {:class :autostash
                :repo repo
                :id (str "stash@{" (:n stash) "}")
                :subject (:subject stash)
                :superseded-by "HEAD"}))]
        (if (empty? obsolete)
          {:outcome :ok
           :detail {:scanned-repos (count repo-paths)
                    :obsolete-count 0
                    :invariant I-obsolescence-recognition}}
          {:outcome :violation
           :detail {:scanned-repos (count repo-paths)
                    :obsolete-count (count obsolete)
                    :obsolete-artifacts obsolete
                    :invariant I-obsolescence-recognition}}))
      (catch Throwable t
        {:outcome :violation
         :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                  :invariant I-obsolescence-recognition}}))))

;; ---------------------------------------------------------------------------
;; obsolescence-recognition/deferred-stub
;; ---------------------------------------------------------------------------

(defn- check-fn-result-deferred?
  "True iff calling CHECK-FN with a stub store returns
   `{:outcome :inactive :detail {:deferred? true ...}}`."
  [check-fn]
  (try
    (let [r (check-fn nil)]
      (and (= :inactive (:outcome r))
           (true? (get-in r [:detail :deferred?]))))
    (catch Throwable _ false)))

(defn- inventory-status-operational?
  "True iff INVENTORY-FAMILIES contains a family with :id matching
   FAMILY-ID-KW and :status :operational (or operational variants).
   Used as a proxy for 'work has been done; deferred-stub is obsolete'."
  [inventory-families family-id-kw]
  (let [target-name (when (keyword? family-id-kw) (name family-id-kw))]
    (boolean
     (some (fn [{:keys [id status]}]
             (and id status
                  (= (when (keyword? id) (name id)) target-name)
                  (#{:operational
                     :operational-but-bypassable
                     :operational-when-enabled} status)))
           inventory-families))))

(defn check-deferred-stub-obsolescence
  "Probe check-fn for `obsolescence-recognition/deferred-stub`.

   Walks `probe/family-check-fns`. For each registered family-id whose
   check-fn returns `:deferred? true`, cross-references the structural-
   law inventory: if the same family-id appears as :status :operational,
   the deferred-stub is obsolete (real implementation has landed).

   INVENTORY-PATH defaults to the structural-law-inventory.sexp; pass
   nil to skip the inventory cross-check (then no obsolescence can be
   detected — :outcome :ok always)."
  ([] (check-deferred-stub-obsolescence
       "/home/joe/code/futon3c/docs/structural-law-inventory.sexp"))
  ([inventory-path]
   (fn [_evidence-store]
     (try
       (let [registry @probe/family-check-fns
             families (when (and inventory-path
                                 (.exists (io/file inventory-path)))
                        (let [inv (-> inventory-path slurp
                                      inventory/parse-sexp-string)]
                          (inventory/extract-all-families inv)))
             obsolete
             (vec
              (for [[fid f] registry
                    :when (check-fn-result-deferred? f)
                    :when (inventory-status-operational? families fid)]
                {:class :deferred-stub
                 :family-id fid
                 :superseded-by (str "inventory :status :operational for "
                                     (pr-str fid))}))]
         (if (empty? obsolete)
           {:outcome :ok
            :detail {:registered-count (count registry)
                     :obsolete-count 0
                     :invariant I-obsolescence-recognition}}
           {:outcome :violation
            :detail {:registered-count (count registry)
                     :obsolete-count (count obsolete)
                     :obsolete-artifacts obsolete
                     :invariant I-obsolescence-recognition}}))
       (catch Throwable t
         {:outcome :violation
          :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                   :invariant I-obsolescence-recognition}})))))

;; ---------------------------------------------------------------------------
;; obsolescence-recognition/pipeline-tracer
;; ---------------------------------------------------------------------------

(defn- iso-now-str [] (str (Instant/now)))

(defn- past-target-date?
  "True iff TARGET-ISO is a parseable date that has already passed."
  [target-iso]
  (try
    (and (string? target-iso)
         (>= (count target-iso) 10)
         (neg? (compare (subs target-iso 0 10)
                        (subs (iso-now-str) 0 10))))
    (catch Throwable _ false)))

(defn- query-by-tags
  "Query the store for entries tagged with all TAG-KEYWORDS (intersection)."
  [evidence-store tag-keywords]
  (try
    (store/query* evidence-store
                  {:query/type :coordination
                   :query/tags (vec tag-keywords)})
    (catch Throwable _ [])))

(defn- entry-track-id [entry]
  (or (get-in entry [:evidence/body :track-id])
      (get-in entry [:evidence/body "track-id"])))

(defn check-pipeline-tracer-obsolescence
  "Probe check-fn for `obsolescence-recognition/pipeline-tracer`.

   Subsumption test: an open `:pipeline-tracer-item` is obsolete when
     (a) a matching `:pipeline-tracer-closed` evidence entry exists with
         the same :track-id (work concluded; tracer should retire), OR
     (b) target-date is past and no close-emit exists (tracer overdue;
         operator hasn't decided whether to extend or close)."
  []
  (fn [evidence-store]
    (try
      (let [open (query-by-tags evidence-store [:pipeline-tracer :open])
            closed (query-by-tags evidence-store [:pipeline-tracer :closed])
            closed-track-ids (set (keep entry-track-id closed))
            ;; Dedupe open by track-id (latest wins).
            open-by-track (reduce
                           (fn [acc e]
                             (let [tid (entry-track-id e)
                                   prior (get acc tid)]
                               (if (or (nil? prior)
                                       (and (:evidence/at e)
                                            (:evidence/at prior)
                                            (pos? (compare (str (:evidence/at e))
                                                           (str (:evidence/at prior))))))
                                 (assoc acc tid e)
                                 acc)))
                           {} open)
            obsolete
            (vec
             (for [[tid entry] open-by-track
                   :when tid
                   :let [target (or (get-in entry [:evidence/body :target-date])
                                    (get-in entry [:evidence/body "target-date"]))
                         closed? (contains? closed-track-ids tid)
                         past-due? (past-target-date? target)]
                   :when (or closed? past-due?)]
               {:class :pipeline-tracer
                :track-id tid
                :target-date target
                :superseded-by (cond
                                 closed? "matching :pipeline-tracer-closed entry"
                                 past-due? (str "past target-date " target
                                                "; no close-emit"))}))]
        (if (empty? obsolete)
          {:outcome :ok
           :detail {:open-count (count open-by-track)
                    :closed-count (count closed-track-ids)
                    :obsolete-count 0
                    :invariant I-obsolescence-recognition}}
          {:outcome :violation
           :detail {:open-count (count open-by-track)
                    :closed-count (count closed-track-ids)
                    :obsolete-count (count obsolete)
                    :obsolete-artifacts obsolete
                    :invariant I-obsolescence-recognition}}))
      (catch Throwable t
        {:outcome :violation
         :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                  :invariant I-obsolescence-recognition}}))))

;; ---------------------------------------------------------------------------
;; Load-time checks — promote sibling probes to :operational at JVM boot
;; ---------------------------------------------------------------------------

(def default-inventory-path
  "Default structural-law inventory path for archaeology load-time checks."
  "/home/joe/code/futon3c/docs/structural-law-inventory.sexp")

(declare default-repo-paths)

(defn- emit-load-time-fired!
  "Emit a `:family-fired` evidence entry for a load-time archaeology check.
   Returns the boundary receipt."
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
           :at (str (Instant/now))}
    :tags [:invariant-queue :family-canary :family-fired
           :load-time family-id outcome]}))

(defn- print-load-time-result!
  [family-id {:keys [outcome detail]}]
  (let [label (name family-id)]
    (case outcome
      :ok
      (println (str "[archaeology] " label " load-time check: OK"))

      :inactive
      (println (str "[archaeology] " label " load-time check: INACTIVE ("
                    (pr-str detail) ")"))

      (do
        (println "================================================================")
        (println (str "[archaeology] " label " LOAD-TIME CHECK FAILED"))
        (println (str "              detail: " (pr-str detail)))
        (println "              Boot continues; the violation is recorded as evidence.")
        (println "================================================================")))))

(defn- run-load-time-check!
  [evidence-store family-id author result {:keys [emit? print?]
                                           :or {emit? true print? true}}]
  (let [outcome (or (:outcome result) :violation)
        detail (:detail result)
        invariant (or (get result :invariant)
                      (get detail :invariant))
        receipt (when emit?
                  (try
                    (emit-load-time-fired!
                     evidence-store family-id author
                     {:outcome outcome
                      :detail detail
                      :invariant invariant})
                    (catch Throwable t
                      {:ok false
                       :error/code :emit-failed
                       :error/exception (str (.getName (class t)) ": "
                                             (.getMessage t))})))]
    (when print?
      (print-load-time-result! family-id {:outcome outcome :detail detail}))
    (cond-> (assoc result :outcome outcome)
      receipt (assoc :emit-receipt receipt))))

(defn check-autostash-on-load!
  "Run the autostash obsolescence check at JVM load time and emit a
   `:family-fired` evidence entry for the outcome."
  ([evidence-store] (check-autostash-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? repo-paths]
                    :or {emit? true
                         print? true
                         repo-paths default-repo-paths}}]
   (run-load-time-check!
    evidence-store
    :obsolescence-recognition/autostash
    "archaeology/check-autostash-on-load!"
    ((check-autostash-obsolescence repo-paths) evidence-store)
    {:emit? emit? :print? print?})))

(defn check-deferred-stub-on-load!
  "Run the deferred-stub obsolescence check at JVM load time and emit a
   `:family-fired` evidence entry for the outcome."
  ([evidence-store] (check-deferred-stub-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? inventory-path]
                    :or {emit? true
                         print? true
                         inventory-path default-inventory-path}}]
   (run-load-time-check!
    evidence-store
    :obsolescence-recognition/deferred-stub
    "archaeology/check-deferred-stub-on-load!"
    ((check-deferred-stub-obsolescence inventory-path) evidence-store)
    {:emit? emit? :print? print?})))

(defn check-pipeline-tracer-on-load!
  "Run the pipeline-tracer obsolescence check at JVM load time and emit a
   `:family-fired` evidence entry for the outcome."
  ([evidence-store] (check-pipeline-tracer-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print?]
                    :or {emit? true print? true}}]
   (run-load-time-check!
    evidence-store
    :obsolescence-recognition/pipeline-tracer
    "archaeology/check-pipeline-tracer-on-load!"
    ((check-pipeline-tracer-obsolescence) evidence-store)
    {:emit? emit? :print? print?})))

;; ---------------------------------------------------------------------------
;; bounded-disposition/stash (M-bounded-disposition)
;;
;; A different shape from subsumption-witness above:
;; per-artifact disposition + bounded undecided population.
;; See futon3/library/invariant-coherence/bounded-disposition.flexiarg.
;; ---------------------------------------------------------------------------

(def I-bounded-disposition
  "Canonical statement of the bounded-disposition shape. Grep-verifiable."
  (str "I-bounded-disposition: a population A of latent-work artifacts "
       "satisfies the invariant when every member carries a disposition "
       "record drawn from a fixed vocabulary D AND the unresolved subset "
       "stays bounded — either by count (|undecided| <= N) or by age "
       "(every artifact older than threshold T must have a non-default "
       "disposition). Two obligations per artifact-class, both part of "
       "the same invariant. Default disposition is :awaiting-decision."))

(def stash-disposition-vocabulary
  "Fixed disposition vocabulary for `bounded-disposition/stash`."
  #{:kept :parked-on-branch :dropped :awaiting-decision})

(def default-stash-disposition
  "Default disposition for a stash carrying no `[disposition: ...]` tag."
  :awaiting-decision)

(def default-undecided-bound
  "Default per-repo cap on stashes at :awaiting-decision."
  5)

(def default-old-stash-days
  "Default age (in days) past which a stash must carry a non-default
   disposition."
  14)

(defn parse-stash-disposition
  "Parse MESSAGE for a `[disposition: <vocab>]` tag (anywhere in the
   message, case-insensitive). Returns one of
   `stash-disposition-vocabulary`, defaulting to
   `default-stash-disposition` if absent or unrecognized."
  [message]
  (let [tag (when (string? message)
              (some->> (re-find #"(?i)\[\s*disposition\s*:\s*([a-z\-]+)\s*\]"
                                message)
                       second
                       str/lower-case
                       keyword))]
    (if (contains? stash-disposition-vocabulary tag)
      tag
      default-stash-disposition)))

(defn- list-stashes-with-time
  "Like `list-stashes` but augments each entry with `:age-days`
   (computed from `git stash list --format` including authordate)."
  [repo-path]
  (try
    (let [{:keys [exit out]}
          (shell/sh "git" "-C" repo-path "stash" "list"
                    "--format=%gd|%H|%at|%s")]
      (if (zero? exit)
        (->> (str/split-lines (or out ""))
             (remove str/blank?)
             (mapv (fn [line]
                     (let [[gd sha at-secs subj] (str/split line #"\|" 4)
                           n (some-> gd
                                     (str/replace #"^stash@\{" "")
                                     (str/replace #"\}$" "")
                                     (Long/parseLong))
                           authored (try
                                      (Long/parseLong (or at-secs "0"))
                                      (catch Throwable _ 0))
                           now (long (/ (System/currentTimeMillis) 1000))
                           age-secs (max 0 (- now authored))
                           age-days (long (/ age-secs (* 60 60 24)))]
                       {:n n :sha sha :subject (or subj "")
                        :age-days age-days
                        :disposition (parse-stash-disposition (or subj ""))}))))
        []))
    (catch Throwable _ [])))

(defn check-stash-disposition
  "Probe check-fn for `bounded-disposition/stash`.

   Two obligations per repo:
     - Per-stash classification: every stash either carries a
       `[disposition: kept|parked-on-branch|dropped|awaiting-decision]`
       tag (parsed via `parse-stash-disposition`) or is treated as
       `:awaiting-decision` by default.
     - Population bound: per repo, |undecided| <= UNDECIDED-BOUND AND
       no stash older than OLD-DAYS carries `:awaiting-decision`.

   Options:
     :undecided-bound — default `default-undecided-bound` (5)
     :old-days        — default `default-old-stash-days` (14)

   REPO-PATHS is a seq of git-repo paths to scan."
  ([repo-paths] (check-stash-disposition repo-paths {}))
  ([repo-paths {:keys [undecided-bound old-days]
                :or {undecided-bound default-undecided-bound
                     old-days default-old-stash-days}}]
   (fn [_evidence-store]
     (try
       (let [per-repo
             (vec
              (for [repo repo-paths
                    :when (.exists (io/file repo ".git"))]
                (let [stashes (list-stashes-with-time repo)
                      undecided (filter #(= default-stash-disposition
                                            (:disposition %)) stashes)
                      unclassified-old (filter
                                        #(and (= default-stash-disposition
                                                 (:disposition %))
                                              (> (:age-days %) old-days))
                                        stashes)
                      bound-violated? (or (> (count undecided) undecided-bound)
                                          (seq unclassified-old))]
                  {:repo repo
                   :total (count stashes)
                   :undecided-count (count undecided)
                   :unclassified-old (vec
                                      (for [s unclassified-old]
                                        {:id (str "stash@{" (:n s) "}")
                                         :subject (:subject s)
                                         :age-days (:age-days s)}))
                   :violation? bound-violated?
                   :by-disposition (->> stashes
                                        (group-by :disposition)
                                        (reduce-kv
                                         (fn [acc k vs] (assoc acc k (count vs)))
                                         {}))})))
             violations (vec (filter :violation? per-repo))]
         (if (empty? violations)
           {:outcome :ok
            :detail {:scanned-repos (count per-repo)
                     :total-stashes (reduce + 0 (map :total per-repo))
                     :undecided-bound undecided-bound
                     :old-days old-days
                     :invariant I-bounded-disposition}}
           {:outcome :violation
            :detail {:scanned-repos (count per-repo)
                     :violations violations
                     :undecided-bound undecided-bound
                     :old-days old-days
                     :invariant I-bounded-disposition}}))
       (catch Throwable t
         {:outcome :violation
          :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                   :invariant I-bounded-disposition}})))))

(defn check-stash-disposition-on-load!
  "Run the stash bounded-disposition check at JVM load time and emit a
   `:family-fired` evidence entry for the outcome."
  ([evidence-store] (check-stash-disposition-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? repo-paths undecided-bound old-days]
                    :or {emit? true
                         print? true
                         repo-paths default-repo-paths
                         undecided-bound default-undecided-bound
                         old-days default-old-stash-days}}]
   (run-load-time-check!
    evidence-store
    :bounded-disposition/stash
    "archaeology/check-stash-disposition-on-load!"
    ((check-stash-disposition repo-paths
                              {:undecided-bound undecided-bound
                               :old-days old-days})
     evidence-store)
    {:emit? emit? :print? print?})))

;; ---------------------------------------------------------------------------
;; bounded-disposition/branch
;; ---------------------------------------------------------------------------

(def branch-disposition-vocabulary
  "Fixed disposition vocabulary for `bounded-disposition/branch`."
  #{:active :merged-not-yet-deleted :parked :long-lived-release :abandoned})

(def default-branch-disposition
  "Default disposition for a branch carrying no `[disposition: ...]` tag."
  :active)

(def default-branch-undecided-bound
  "Default per-repo cap on branches at the default disposition."
  10)

(def default-old-branch-days
  "Default age (in days) past which a branch must carry a non-default
   disposition."
  30)

(defn parse-branch-disposition
  "Parse MESSAGE for a `[disposition: <vocab>]` tag (anywhere in the
   message, case-insensitive). Returns one of
   `branch-disposition-vocabulary`, defaulting to
   `default-branch-disposition` if absent or unrecognized."
  [message]
  (let [tag (when (string? message)
              (some->> (re-find #"(?i)\[\s*disposition\s*:\s*([a-z\-]+)\s*\]"
                                message)
                       second
                       str/lower-case
                       keyword))]
    (if (contains? branch-disposition-vocabulary tag)
      tag
      default-branch-disposition)))

(defn- branch-description
  [repo-path branch-name]
  (try
    (let [{:keys [exit out]}
          (shell/sh "git" "-C" repo-path "config" "--get"
                    (str "branch." branch-name ".description"))]
      (if (zero? exit)
        (str/trim (or out ""))
        ""))
    (catch Throwable _ "")))

(defn- list-branches-with-time
  "Return [{:name string :age-days int :disposition kw} ...] for local
   branches in REPO-PATH, excluding main/master."
  [repo-path]
  (try
    (let [{:keys [exit out]}
          (shell/sh "git" "-C" repo-path "for-each-ref" "refs/heads/*"
                    "--format=%(refname:short)|%(committerdate:unix)")]
      (if (zero? exit)
        (->> (str/split-lines (or out ""))
             (remove str/blank?)
             (mapv (fn [line]
                     (let [[branch-name at-secs] (str/split line #"\|" 2)
                           authored (try
                                      (Long/parseLong (or at-secs "0"))
                                      (catch Throwable _ 0))
                           now (long (/ (System/currentTimeMillis) 1000))
                           age-secs (max 0 (- now authored))
                           description (branch-description repo-path branch-name)]
                       {:name branch-name
                        :age-days (long (/ age-secs (* 60 60 24)))
                        :description description
                        :disposition (parse-branch-disposition description)})))
             (remove (comp #{"main" "master"} :name))
             vec)
        []))
    (catch Throwable _ [])))

(defn check-branch-disposition
  "Probe check-fn for `bounded-disposition/branch`.

   Two obligations per repo:
     - Per-branch classification: every non-main/master branch either
       carries a `[disposition: ...]` tag in its git config description
       or is treated as `:active` by default.
     - Population bound: per repo, |active| <= UNDECIDED-BOUND AND no
       branch older than OLD-DAYS carries the default disposition.

   Options:
     :undecided-bound — default `default-branch-undecided-bound` (10)
     :old-days        — default `default-old-branch-days` (30)

   REPO-PATHS is a seq of git-repo paths to scan."
  ([repo-paths] (check-branch-disposition repo-paths {}))
  ([repo-paths {:keys [undecided-bound old-days]
                :or {undecided-bound default-branch-undecided-bound
                     old-days default-old-branch-days}}]
   (fn [_evidence-store]
     (try
       (let [per-repo
             (vec
              (for [repo repo-paths
                    :when (.exists (io/file repo ".git"))]
                (let [branches (list-branches-with-time repo)
                      undecided (filter #(= default-branch-disposition
                                            (:disposition %)) branches)
                      unclassified-old (filter
                                        #(and (= default-branch-disposition
                                                 (:disposition %))
                                              (> (:age-days %) old-days))
                                        branches)
                      bound-violated? (or (> (count undecided) undecided-bound)
                                          (seq unclassified-old))]
                  {:repo repo
                   :total (count branches)
                   :undecided-count (count undecided)
                   :unclassified-old (vec
                                      (for [b unclassified-old]
                                        {:name (:name b)
                                         :age-days (:age-days b)}))
                   :violation? bound-violated?
                   :by-disposition (->> branches
                                        (group-by :disposition)
                                        (reduce-kv
                                         (fn [acc k vs] (assoc acc k (count vs)))
                                         {}))})))
             violations (vec (filter :violation? per-repo))]
         (if (empty? violations)
           {:outcome :ok
            :detail {:scanned-repos (count per-repo)
                     :total-branches (reduce + 0 (map :total per-repo))
                     :undecided-bound undecided-bound
                     :old-days old-days
                     :invariant I-bounded-disposition}}
           {:outcome :violation
            :detail {:scanned-repos (count per-repo)
                     :violations violations
                     :undecided-bound undecided-bound
                     :old-days old-days
                     :invariant I-bounded-disposition}}))
       (catch Throwable t
         {:outcome :violation
          :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                   :invariant I-bounded-disposition}})))))

(defn check-branch-disposition-on-load!
  "Run the branch bounded-disposition check at JVM load time and emit a
   `:family-fired` evidence entry for the outcome."
  ([evidence-store] (check-branch-disposition-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? repo-paths undecided-bound old-days]
                    :or {emit? true
                         print? true
                         repo-paths default-repo-paths
                         undecided-bound default-branch-undecided-bound
                         old-days default-old-branch-days}}]
   (run-load-time-check!
    evidence-store
    :bounded-disposition/branch
    "archaeology/check-branch-disposition-on-load!"
    ((check-branch-disposition repo-paths
                               {:undecided-bound undecided-bound
                                :old-days old-days})
     evidence-store)
    {:emit? emit? :print? print?})))

;; ---------------------------------------------------------------------------
;; bounded-disposition/mission-doc
;; ---------------------------------------------------------------------------

(def mission-doc-disposition-vocabulary
  "Fixed disposition vocabulary for `bounded-disposition/mission-doc`."
  #{:open :closed :parked :archived})

(def default-mission-doc-disposition
  "Default disposition for a mission doc carrying no Status line."
  :open)

(def default-mission-doc-undecided-bound
  "Default per-repo cap on mission docs at the default disposition."
  10)

(def default-old-mission-doc-days
  "Default age (in days) past which a mission doc must carry a Status line."
  60)

(defn- extract-status-token
  [line]
  (when (string? line)
    (some->> (re-find #"(?i)\bstatus\b[^A-Za-z0-9]+([a-z][a-z\- ]*)"
                      line)
             second
             str/trim
             str/lower-case)))

(defn- parse-status-line-disposition
  [line]
  (let [raw (extract-status-token line)
        tag (when (seq raw)
              (keyword
               (case raw
                 "done" "closed"
                 "complete" "closed"
                 "completed" "closed"
                 (str/replace raw #"[\s]+" "-"))))]
    (if (contains? mission-doc-disposition-vocabulary tag)
      tag
      default-mission-doc-disposition)))

(defn- read-mission-doc-status
  "Read the first five lines of a mission doc and return its parsed
   disposition plus whether a Status line was present."
  [mission-file-path]
  (try
    (let [lines (->> (slurp mission-file-path)
                     str/split-lines
                     (take 5))
          status-line (some #(when (re-find #"(?i)\bstatus\s*:" %) %) lines)]
      {:disposition (parse-status-line-disposition status-line)
       :status-present? (boolean (seq (extract-status-token status-line)))})
    (catch Throwable _
      {:disposition default-mission-doc-disposition
       :status-present? false})))

(defn parse-mission-doc-status
  "Parse the first five lines of MISSION-FILE-PATH for a Status line.
   Returns one of `mission-doc-disposition-vocabulary`, defaulting to
   `default-mission-doc-disposition` when absent or unrecognized."
  [mission-file-path]
  (:disposition (read-mission-doc-status mission-file-path)))

(defn- list-mission-docs-with-time
  "Return [{:path string :age-days int :disposition kw :status-present? bool} ...]
   for mission docs in REPO-PATH."
  [repo-path]
  (let [missions-dir (io/file repo-path "holes" "missions")
        now-ms (System/currentTimeMillis)]
    (if (.exists missions-dir)
      (->> (file-seq missions-dir)
           (filter #(.isFile ^java.io.File %))
           (filter #(re-matches #"M-.*\.md" (.getName ^java.io.File %)))
           (mapv (fn [^java.io.File file]
                   (let [{:keys [disposition status-present?]}
                         (read-mission-doc-status (.getPath file))
                         age-ms (max 0 (- now-ms (.lastModified file)))]
                     {:path (.getPath file)
                      :age-days (long (/ age-ms (* 1000 60 60 24)))
                      :disposition disposition
                      :status-present? status-present?}))))
      [])))

(defn check-mission-doc-disposition
  "Probe check-fn for `bounded-disposition/mission-doc`.

   Two obligations per repo:
     - Per-doc classification: every mission doc either has a Status
       line in its first five lines mapping to one of
       `mission-doc-disposition-vocabulary`, or defaults to `:open`.
     - Population bound: per repo, |open| <= UNDECIDED-BOUND AND every
       mission doc newer than OLD-DAYS carries a non-empty Status line.

   Options:
     :undecided-bound — default `default-mission-doc-undecided-bound` (10)
     :old-days        — default `default-old-mission-doc-days` (60)

   REPO-PATHS is a seq of repo paths to scan."
  ([repo-paths] (check-mission-doc-disposition repo-paths {}))
  ([repo-paths {:keys [undecided-bound old-days]
                :or {undecided-bound default-mission-doc-undecided-bound
                     old-days default-old-mission-doc-days}}]
   (fn [_evidence-store]
     (try
       (let [per-repo
             (vec
              (for [repo repo-paths
                    :when (.exists (io/file repo))]
                (let [mission-docs (list-mission-docs-with-time repo)
                      undecided (filter #(= default-mission-doc-disposition
                                            (:disposition %))
                                        mission-docs)
                      missing-status-recent (filter
                                             #(and (<= (:age-days %) old-days)
                                                   (not (:status-present? %)))
                                             mission-docs)
                      bound-violated? (or (> (count undecided) undecided-bound)
                                          (seq missing-status-recent))]
                  {:repo repo
                   :total (count mission-docs)
                   :undecided-count (count undecided)
                   :missing-status-recent (vec
                                           (for [doc missing-status-recent]
                                             {:path (:path doc)
                                              :age-days (:age-days doc)}))
                   :violation? bound-violated?
                   :by-disposition (->> mission-docs
                                        (group-by :disposition)
                                        (reduce-kv
                                         (fn [acc k vs] (assoc acc k (count vs)))
                                         {}))})))
             violations (vec (filter :violation? per-repo))]
         (if (empty? violations)
           {:outcome :ok
            :detail {:scanned-repos (count per-repo)
                     :total-mission-docs (reduce + 0 (map :total per-repo))
                     :undecided-bound undecided-bound
                     :old-days old-days
                     :invariant I-bounded-disposition}}
           {:outcome :violation
            :detail {:scanned-repos (count per-repo)
                     :violations violations
                     :undecided-bound undecided-bound
                     :old-days old-days
                     :invariant I-bounded-disposition}}))
       (catch Throwable t
         {:outcome :violation
          :detail {:exception (str (.getName (class t)) ": " (.getMessage t))
                   :invariant I-bounded-disposition}})))))

(defn check-mission-doc-disposition-on-load!
  "Run the mission-doc bounded-disposition check at JVM load time and
   emit a `:family-fired` evidence entry for the outcome."
  ([evidence-store] (check-mission-doc-disposition-on-load! evidence-store {}))
  ([evidence-store {:keys [emit? print? repo-paths undecided-bound old-days]
                    :or {emit? true
                         print? true
                         repo-paths default-repo-paths
                         undecided-bound default-mission-doc-undecided-bound
                         old-days default-old-mission-doc-days}}]
   (run-load-time-check!
    evidence-store
    :bounded-disposition/mission-doc
    "archaeology/check-mission-doc-disposition-on-load!"
    ((check-mission-doc-disposition repo-paths
                                    {:undecided-bound undecided-bound
                                     :old-days old-days})
     evidence-store)
    {:emit? emit? :print? print?})))

;; ---------------------------------------------------------------------------
;; Convenience: register-archaeology-control-taps!
;; ---------------------------------------------------------------------------

(def default-repo-paths
  "The futon-stack repos `obsolescence-recognition/autostash` scans by
   default. Override at activation time by passing :repo-paths."
  ["/home/joe/code/futon0"
   "/home/joe/code/futon1a"
   "/home/joe/code/futon3"
   "/home/joe/code/futon3a"
   "/home/joe/code/futon3b"
   "/home/joe/code/futon3c"
   "/home/joe/code/futon4"
   "/home/joe/code/futon5"
   "/home/joe/code/futon5a"])

(defn register-archaeology-control-taps!
  "Register the six `archaeology-control` family check-fns with
   `futon3c.logic.probe`. Operator-driven; intended to be called once
   at activation time alongside `register-default-taps!`.

   Three subsumption-witness siblings (obsolescence-recognition/*) plus
   three bounded-disposition siblings
   (bounded-disposition/stash|branch|mission-doc). Two
   different shapes under the same family.

   Options:
     :repo-paths      — override the default futon-stack repo list
                        for the autostash + stash-disposition checks.
     :inventory-path  — override the structural-law-inventory.sexp
                        path (set to nil to skip the deferred-stub
                        check).
     :undecided-bound — bounded-disposition/stash undecided cap.
                        Default `default-undecided-bound` (5).
     :old-days        — bounded-disposition/stash age threshold in
                        days. Default `default-old-stash-days` (14).
     :branch-undecided-bound — bounded-disposition/branch active cap.
                               Default `default-branch-undecided-bound` (10).
     :branch-old-days        — bounded-disposition/branch age threshold
                               in days. Default `default-old-branch-days` (30).
     :mission-doc-undecided-bound — bounded-disposition/mission-doc
                                    open cap. Default
                                    `default-mission-doc-undecided-bound` (10).
     :mission-doc-old-days        — bounded-disposition/mission-doc
                                    status-line recency threshold in
                                    days. Default
                                    `default-old-mission-doc-days` (60).

   Returns the set of registered family-ids."
  ([] (register-archaeology-control-taps! {}))
  ([{:keys [repo-paths inventory-path undecided-bound old-days
            branch-undecided-bound branch-old-days
            mission-doc-undecided-bound mission-doc-old-days]
     :or {repo-paths default-repo-paths
          inventory-path default-inventory-path
          undecided-bound default-undecided-bound
          old-days default-old-stash-days
          branch-undecided-bound default-branch-undecided-bound
          branch-old-days default-old-branch-days
          mission-doc-undecided-bound default-mission-doc-undecided-bound
          mission-doc-old-days default-old-mission-doc-days}}]
   (probe/register-family-check!
    :obsolescence-recognition/autostash
    (check-autostash-obsolescence repo-paths))
   (probe/register-family-check!
    :obsolescence-recognition/deferred-stub
    (check-deferred-stub-obsolescence inventory-path))
   (probe/register-family-check!
    :obsolescence-recognition/pipeline-tracer
    (check-pipeline-tracer-obsolescence))
   (probe/register-family-check!
    :bounded-disposition/stash
    (check-stash-disposition repo-paths
                             {:undecided-bound undecided-bound
                              :old-days old-days}))
   (probe/register-family-check!
    :bounded-disposition/branch
    (check-branch-disposition repo-paths
                              {:undecided-bound branch-undecided-bound
                               :old-days branch-old-days}))
   (probe/register-family-check!
    :bounded-disposition/mission-doc
    (check-mission-doc-disposition repo-paths
                                   {:undecided-bound mission-doc-undecided-bound
                                    :old-days mission-doc-old-days}))
   #{:obsolescence-recognition/autostash
     :obsolescence-recognition/deferred-stub
     :obsolescence-recognition/pipeline-tracer
     :bounded-disposition/stash
     :bounded-disposition/branch
     :bounded-disposition/mission-doc}))
