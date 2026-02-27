(ns futon3c.portfolio.observe
  "Portfolio observation layer — gather mc-backend outputs into ~12
   normalized [0,1] channels.

   Each channel is a pure function from portfolio state to [0,1].
   Normalization constants are tunable priors representing 'what would
   be surprising' (cf. ant observe clamp01 convention).

   Source: D-2 in M-portfolio-inference.md"
  (:require [futon3c.peripheral.mission-control-backend :as mc]
            [futon3c.evidence.store :as estore]))

;; =============================================================================
;; Utilities
;; =============================================================================

(defn clamp01
  "Clamp x to [0,1]."
  [x]
  (max 0.0 (min 1.0 (double x))))

(defn rescale
  "Rescale x from [lo,hi] to [0,1], clamped."
  [x lo hi]
  (let [range (- hi lo)]
    (if (zero? range)
      0.5
      (clamp01 (/ (- (double x) lo) range)))))

;; =============================================================================
;; Normalization priors — "what would be surprising"
;; =============================================================================

(def default-priors
  "Normalization constants — calibrated so current portfolio state maps to
   ~0.5-0.7, leaving gradient in both directions.
   Recalibrated 2026-02-27: expanded from 4 repos to 7 (~64 missions,
   ~88 prototypes across 9 devmaps, ~80 gaps at current state)."
  {:evidence-per-day-cap  20.0   ; >20 evidence entries/day would be surprising
   :max-chain-cap         10.0   ; >10 dependency depth would be surprising
   :gap-cap              120.0   ; ~80 current gaps → 0.67 (room for improvement/degradation)
   :spinoff-cap           40.0   ; ~20 current spinoffs → 0.5
   :review-age-cap        14.0}) ; >14 days since last review would be surprising

;; =============================================================================
;; Raw state gathering
;; =============================================================================

(defn gather-mc-state
  "Aggregate raw portfolio data from mc-backend.
   Takes an optional pre-built portfolio review to avoid redundant scanning.
   Returns a flat map of raw values ready for channel normalization."
  ([evidence-store] (gather-mc-state evidence-store nil))
  ([evidence-store portfolio-review]
   (let [review (or portfolio-review (mc/build-portfolio-review))
         missions (:portfolio/missions review)
         coverage (:portfolio/coverage review)
         mana (:portfolio/mana review)
         gaps (:portfolio/gaps review)
         total (count missions)
         complete (count (filter #(= :complete (:mission/status %)) missions))
         blocked (count (filter #(= :blocked (:mission/status %)) missions))
         ;; Average coverage across devmaps
         avg-coverage (if (seq coverage)
                        (/ (reduce + (map :coverage/coverage-pct coverage))
                           (count coverage))
                        0.0)
         ;; Coverage trajectory: compare with last snapshot if available
         coverage-slope (when evidence-store
                          (let [diff-result (mc/portfolio-diff evidence-store)
                                diff (:diff diff-result)]
                            (when diff
                              (let [new-cov (:new-coverage diff)
                                    old-cov (:old-coverage diff)]
                                (when (and (seq new-cov) (seq old-cov))
                                  (let [new-avg (/ (reduce + (map :coverage/coverage-pct new-cov))
                                                   (count new-cov))
                                        old-avg (/ (reduce + (map :coverage/coverage-pct old-cov))
                                                   (count old-cov))]
                                    (- new-avg old-avg)))))))
         ;; Dependency depth: compute max blocked-by chain length
         dep-depth (let [blocked-by-map (into {}
                                              (keep (fn [m]
                                                      (when (:mission/blocked-by m)
                                                        [(:mission/id m)
                                                         (:mission/blocked-by m)])))
                                              missions)]
                     (if (empty? blocked-by-map)
                       0
                       ;; Simple: count missions that are blocked (depth approximation)
                       (count blocked-by-map)))
         ;; Stall count: missions with no recent evidence
         ;; Approximation: blocked + unknown status missions
         stalled (count (filter #(#{:blocked :unknown} (:mission/status %)) missions))
         ;; Spinoff pressure: count gaps that suggest new missions needed
         spinoff-candidates (count (filter #(re-find #"no mission" %) gaps))
         ;; Evidence velocity: count recent evidence entries
         evidence-per-day (when evidence-store
                            (let [recent (estore/query* evidence-store
                                                        {:query/limit 100})
                                  now-ms (System/currentTimeMillis)
                                  day-ms (* 24 60 60 1000)
                                  recent-today (count
                                                (filter
                                                 (fn [e]
                                                   (when-let [at (:evidence/at e)]
                                                     (let [t (try
                                                               (.toEpochMilli
                                                                (java.time.Instant/parse (str at)))
                                                               (catch Exception _ 0))]
                                                       (> t (- now-ms day-ms)))))
                                                 recent))]
                              recent-today))
         ;; Review age: days since last portfolio-related evidence
         ;; Check both "global" (mc-review) and "inference" (portfolio-step!) subjects
         days-since-review (when evidence-store
                             (let [global (estore/query* evidence-store
                                                         {:query/subject {:ref/type :portfolio
                                                                          :ref/id "global"}
                                                          :query/type :coordination
                                                          :query/limit 1})
                                   inference (estore/query* evidence-store
                                                            {:query/subject {:ref/type :portfolio
                                                                             :ref/id "inference"}
                                                             :query/type :coordination
                                                             :query/limit 1})
                                   latest (first (sort-by :evidence/at #(compare %2 %1)
                                                          (concat global inference)))]
                               (when-let [at (:evidence/at latest)]
                                 (try
                                   (let [then (.toEpochMilli (java.time.Instant/parse (str at)))
                                         now (System/currentTimeMillis)]
                                     (/ (- now then) (* 24.0 60 60 1000)))
                                   (catch Exception _ nil)))))
         ;; Pattern reuse: placeholder — requires pattern catalog query
         pattern-reuse-ratio 0.0]
     {:total total
      :complete complete
      :blocked blocked
      :coverage-pct avg-coverage
      :coverage-slope (or coverage-slope 0.0)
      :mana-pool-balance (or (:mana/pool-balance mana) 0.0)
      :mana-cap 1000.0  ; default cap until Nonstarter is wired
      :mana-available? (:mana/available mana)
      :evidence-per-day (or evidence-per-day 0)
      :max-chain dep-depth
      :gaps (count gaps)
      :stalled stalled
      :spinoff-candidates spinoff-candidates
      :pattern-reuse-ratio pattern-reuse-ratio
      :days-since-review (or days-since-review 999.0)})))

;; =============================================================================
;; Channel normalization — raw state → [0,1] observation vector
;; =============================================================================

(defn observe
  "Normalize portfolio state into 12 [0,1] channels.
   Takes either a pre-gathered mc-state map, or gathers it fresh.

   Returns a map of channel-keyword → double in [0,1]."
  ([mc-state] (observe mc-state default-priors))
  ([mc-state priors]
   (let [total (max 1 (:total mc-state 1))]
     {:mission-complete-ratio  (clamp01 (/ (double (:complete mc-state 0)) total))
      :coverage-pct            (clamp01 (double (:coverage-pct mc-state 0.0)))
      :coverage-trajectory     (rescale (:coverage-slope mc-state 0.0) -1.0 1.0)
      :mana-available          (clamp01 (/ (double (:mana-pool-balance mc-state 0.0))
                                           (max 1.0 (:mana-cap mc-state 1.0))))
      :blocked-ratio           (clamp01 (/ (double (:blocked mc-state 0)) total))
      :evidence-velocity       (clamp01 (/ (double (:evidence-per-day mc-state 0))
                                           (:evidence-per-day-cap priors)))
      :dependency-depth        (clamp01 (/ (double (:max-chain mc-state 0))
                                           (:max-chain-cap priors)))
      :gap-count               (clamp01 (/ (double (:gaps mc-state 0))
                                           (:gap-cap priors)))
      :stall-count             (clamp01 (/ (double (:stalled mc-state 0)) total))
      :spinoff-pressure        (clamp01 (/ (double (:spinoff-candidates mc-state 0))
                                           (:spinoff-cap priors)))
      :pattern-reuse           (clamp01 (double (:pattern-reuse-ratio mc-state 0.0)))
      :review-age              (clamp01 (/ (double (:days-since-review mc-state 0.0))
                                           (:review-age-cap priors)))
      ;; Heartbeat-derived channels (T-7): default to neutral when no heartbeat data
      :effort-prediction-error (clamp01 (double (:effort-prediction-error mc-state 0.0)))
      :bid-completion-rate     (clamp01 (double (:bid-completion-rate mc-state 0.5)))
      :unplanned-work-ratio    (clamp01 (double (:unplanned-work-ratio mc-state 0.0)))})))

;; =============================================================================
;; Heartbeat enrichment (T-7)
;; =============================================================================

(defn merge-heartbeat-summary
  "Merge heartbeat action-error summary into mc-state for observation.
   Adds :effort-prediction-error, :bid-completion-rate, :unplanned-work-ratio
   from the output of heartbeat/compute-action-errors.
   Returns mc-state unchanged when action-errors is nil."
  [mc-state action-errors]
  (if-let [summary (:summary action-errors)]
    (let [planned (max 1 (:planned summary 1))
          taken (:taken summary 0)
          unplanned (:unplanned summary 0)
          effort-err (:effort-error-sum summary 0)]
      (assoc mc-state
             ;; Normalize effort error: max distance is ~4 per action (trivial↔epic),
             ;; scale by planned count. Clamp01 catches edge cases.
             :effort-prediction-error (/ (double effort-err) (* 2.0 planned))
             :bid-completion-rate (/ (double taken) planned)
             :unplanned-work-ratio (if (pos? (+ taken unplanned))
                                     (/ (double unplanned) (+ taken unplanned))
                                     0.0)))
    mc-state))

;; =============================================================================
;; Channel metadata
;; =============================================================================

(def channel-keys
  "Ordered list of observation channel keys.
   First 12: portfolio state from mc-backend.
   Last 3: heartbeat effort data (neutral defaults when no heartbeat available)."
  [:mission-complete-ratio :coverage-pct :coverage-trajectory
   :mana-available :blocked-ratio :evidence-velocity
   :dependency-depth :gap-count :stall-count
   :spinoff-pressure :pattern-reuse :review-age
   ;; Heartbeat-derived channels (T-7)
   :effort-prediction-error :bid-completion-rate :unplanned-work-ratio])

(defn obs->vector
  "Convert observation map to ordered vector (for ML-style consumers)."
  [obs]
  (mapv #(get obs % 0.0) channel-keys))
