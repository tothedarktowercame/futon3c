(ns futon3c.peripheral.strategic-embedding-experiment
  "Phase-6b optional embedding experiment (M-shared-memory-control-build-test).

  Informative only.  Compares three relation proposers on held-out typed,
  reviewed edges from the Phase 4 dark WM corpus:

  - :lexical-structural  — token-overlap plus shared-pattern-family prior,
                           mirroring the operational lifecycle-aware lexical
                           proposal;
  - :generic-vector      — deterministic hashed bag-of-words vectors with
                           cosine similarity (generic embedding surrogate:
                           every token weighted equally);
  - :dedicated-vector    — hashed vectors over mission-and-control-pattern
                           specific text (hook/title tokens double-weighted,
                           pattern identity tokens included).

  Losing to the non-vector baseline causes no architectural rollback: direct
  typed endpoint retrieval remains the operational path.

  Invariant 4 (proposal is not evidence) is enforced structurally: vector
  proposers emit only proposal-shaped candidates.  `as-review-proposal`
  stamps `:witness-status :proposed`, `:attachment-status :proposed`, and
  `:promotable? false`; `proposal-substitutes-witness?` is constantly false.
  No vector output path can mint a reviewed or witnessed attachment."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def algorithm :strategic-embedding-experiment/informative-v1)
(def vector-dimensions 4096)

;; ---------------------------------------------------------------------------
;; Text featurisation
;; ---------------------------------------------------------------------------

(defn- tokenize
  [text]
  (->> (str/split (str/lower-case (str text)) #"[^a-z0-9-]+")
       (filter #(> (count %) 2))
       distinct
       vec))

(defn pattern-text
  [pattern]
  (str/join " " (keep pattern [:p4ng/id :title :description])))

(defn mission-text
  [mission]
  (str/join " " (keep mission [:mission/id :title :objective])))

;; ---------------------------------------------------------------------------
;; Proposer 1: lexical / structural baseline
;; ---------------------------------------------------------------------------

(defn- structural-prior
  "Shared-pattern-family prior: a mission that already has a reviewed edge to
  another pattern in the same p4ng family gets a small additive bonus."
  [pattern mission train-edges]
  (let [family (fn [id] (second (str/split (str id) #"/")))
        pattern-family (family (:p4ng/id pattern))
        mission-id (:mission/id mission)]
    (if (and pattern-family
             (some (fn [[p m]]
                     (and (= m mission-id)
                          (= (family p) pattern-family)))
                   train-edges))
      0.05
      0.0)))

(defn lexical-structural-score
  [pattern mission train-edges]
  (let [p-tokens (set (tokenize (pattern-text pattern)))
        m-tokens (set (tokenize (mission-text mission)))
        overlap (count (set/intersection p-tokens m-tokens))
        denom (max 1 (Math/sqrt (* (count p-tokens) (max 1 (count m-tokens)))))]
    (+ (/ overlap denom) (structural-prior pattern mission train-edges))))

;; ---------------------------------------------------------------------------
;; Proposers 2 and 3: deterministic hashed vectors
;; ---------------------------------------------------------------------------

(defn- token-index
  [token]
  (mod (.hashCode ^String token) vector-dimensions))

(defn- vectorize
  "tokens is a seq of [token weight]; collisions add (deterministically)."
  [weighted-tokens]
  (reduce (fn [acc [token weight]]
            (let [i (token-index token)]
              (assoc acc i (+ (get acc i 0.0) (double weight)))))
          {}
          weighted-tokens))

(defn- cosine
  [a b]
  (let [dot (reduce-kv (fn [s i wa]
                         (if-let [wb (find b i)]
                           (+ s (* wa (val wb)))
                           s))
                       0.0 a)
        na (Math/sqrt (reduce-kv (fn [s _ w] (+ s (* w w))) 0.0 a))
        nb (Math/sqrt (reduce-kv (fn [s _ w] (+ s (* w w))) 0.0 b))]
    (if (or (zero? na) (zero? nb))
      0.0
      (/ dot (* na nb)))))

(defn- generic-pattern-vector [pattern]
  (vectorize (map (fn [t] [t 1.0]) (tokenize (pattern-text pattern)))))

(defn- generic-mission-vector [mission]
  (vectorize (map (fn [t] [t 1.0]) (tokenize (mission-text mission)))))

(defn- dedicated-vector
  "Mission-and-control-pattern specific: title and p4ng identity tokens are
  double-weighted, hook text included."
  [entity id-key text-fn]
  (let [all (tokenize (text-fn entity))
        emphasised (set (tokenize (str (get entity id-key) " " (get entity :title) " " (get entity :hook) " ")))
        weighted (map (fn [t] [t (if (emphasised t) 2.0 1.0)]) all)]
    (vectorize weighted)))

(defn generic-vector-score [pattern mission]
  (cosine (generic-pattern-vector pattern) (generic-mission-vector mission)))

(defn dedicated-vector-score [pattern mission]
  (cosine (dedicated-vector pattern :p4ng/id pattern-text)
          (dedicated-vector mission :mission/id mission-text)))

;; ---------------------------------------------------------------------------
;; Ranking
;; ---------------------------------------------------------------------------

(def ^:private proposers
  {:lexical-structural (fn [patterns missions train-edges _held-out]
                         (for [p patterns m missions]
                           [(:p4ng/id p) (:mission/id m)
                            (lexical-structural-score p m train-edges)]))
   :generic-vector (fn [patterns missions _train-edges _held-out]
                     (for [p patterns m missions]
                       [(:p4ng/id p) (:mission/id m)
                        (generic-vector-score p m)]))
   :dedicated-vector (fn [patterns missions _train-edges _held-out]
                       (for [p patterns m missions]
                         [(:p4ng/id p) (:mission/id m)
                          (dedicated-vector-score p m)]))})

(defn rank-pairs
  "Deterministic total order: score descending, then pattern id, then mission
  id.  Returns a vector of {:pattern-id ... :mission-id ... :score ...}."
  [proposer-kw patterns missions train-edges]
  (->> ((get proposers proposer-kw) patterns missions train-edges nil)
       (map (fn [[p m s]] {:pattern-id p :mission-id m :score (double s)}))
       (sort-by (juxt (comp - :score) :pattern-id :mission-id))
       vec))

;; ---------------------------------------------------------------------------
;; Proposal-is-not-evidence guard (Invariant 4)
;; ---------------------------------------------------------------------------

(defn as-review-proposal
  "The ONLY shape a vector or lexical proposal may take.  It is never an
  attachment and never a witness."
  [candidate]
  {:proposed-by (:proposer candidate algorithm)
   :pattern-id (:pattern-id candidate)
   :mission-id (:mission-id candidate)
   :score (:score candidate)
   :witness-status :proposed
   :attachment-status :proposed
   :promotable? false
   :supporting-typed-edge nil})

(defn proposal-substitutes-witness?
  "Constantly false: a proposal cannot substitute for a typed witnessed edge."
  [_proposal]
  false)

(defn- guard-report
  []
  (let [candidate {:pattern-id "p4ng/R5-policy-evaluation"
                   :mission-id "M-aif-policy-conditioned-eig"
                   :score 0.99
                   :proposer :dedicated-vector}
        proposal (as-review-proposal candidate)]
    {:sample proposal
     :substitutes-witness? (proposal-substitutes-witness? proposal)
     :witness-status-remains-proposed? (= :proposed (:witness-status proposal))
     :attachment-status-remains-proposed? (= :proposed (:attachment-status proposal))
     :promotable? false}))

;; ---------------------------------------------------------------------------
;; Metrics
;; ---------------------------------------------------------------------------

(defn recall-at
  "Fraction of held-out edges present in the first k ranked pairs."
  [k held-out-edges ranked]
  (let [top (set (map (juxt :pattern-id :mission-id) (take k ranked)))
        held (set held-out-edges)]
    (if (empty? held)
      0.0
      (double (/ (count (set/intersection top held)) (count held))))))

(defn precision-at
  "Fraction of the first k ranked pairs that are held-out edges."
  [k held-out-edges ranked]
  (if (zero? k)
    0.0
    (let [top (take k ranked)
          held (set held-out-edges)]
      (/ (double (count (filter #(held ((juxt :pattern-id :mission-id) %)) top))) k))))

;; ---------------------------------------------------------------------------
;; Experiment entry point
;; ---------------------------------------------------------------------------

(defn- validate-fixture
  [fixture]
  (let [problems (concat
                  (when-not (seq (:patterns fixture)) [:no-patterns])
                  (when-not (seq (:missions fixture)) [:no-missions])
                  (when-not (seq (:held-out-edges fixture)) [:no-held-out-edges]))]
    (when (seq problems)
      {:status :invalid-fixture :problems (vec problems)})))

(defn run-experiment
  "Run the informative Phase 6b comparison.  Returns per-proposer recall@k and
  precision@k on held-out edges plus the proposal-is-not-evidence guard
  report.  Never emits a promotion or attachment decision."
  [fixture & {:keys [ks] :or {ks [5 10]}}]
  (or (validate-fixture fixture)
      (let [patterns (:patterns fixture)
            missions (:missions fixture)
            train-edges (vec (:train-edges fixture))
            held-out (vec (:held-out-edges fixture))]
        {:status :informative-only
         :algorithm algorithm
         :held-out-edge-count (count held-out)
         :train-edge-count (count train-edges)
         :proposers
         (into {}
               (for [proposer (sort (keys proposers))]
                 (let [ranked (rank-pairs proposer patterns missions train-edges)
                       metrics (reduce (fn [acc k]
                                         (assoc acc
                                                (keyword (str "recall@" k))
                                                (double (recall-at k held-out ranked))
                                                (keyword (str "precision@" k))
                                                (double (precision-at k held-out ranked))))
                                       {} ks)]
                   [proposer {:ranking (count ranked)
                              :metrics metrics}])))
         :bypass-guard (guard-report)
         :promote? false
         :architectural-effect :none})))
