(ns futon3c.portfolio.policy
  "Portfolio policy layer — EFE computation and action selection.

   Computes expected free energy G(a) for each candidate action,
   then selects via softmax with temperature τ.

   G(a) = λ_pragmatic · pragmatic(a)
        + λ_epistemic · epistemic(a)
        + λ_upvote   · upvote(a)
        + λ_effort   · effort(a)

   Abstains when τ < 0.55 (from FuLab policy contract).

   Source: D-6 in M-portfolio-inference.md
   Template: futon2/src/ants/aif/policy.clj"
  (:require [futon3c.portfolio.observe :as obs]))

;; =============================================================================
;; Arena definition
;; =============================================================================

(def portfolio-arena
  "The initial action arena for portfolio management."
  {:arena/id :portfolio-management
   :arena/participants [:human :claude :codex]
   :arena/actions [:work-on :review :consolidate :upvote :wait]
   :arena/rules {:mode-gate true
                 :mana-gate true
                 :upvote-decay 0.10}})

;; =============================================================================
;; EFE weights (lambdas)
;; =============================================================================

(def default-lambdas
  "EFE term weights. From Nonstarter GFE objective + portfolio-specific."
  {:pragmatic 0.6    ; goal progress
   :epistemic 0.4    ; uncertainty reduction
   :upvote    0.3    ; collective desire signal
   :effort    0.2})  ; effort penalty

;; =============================================================================
;; EFE term computation
;; =============================================================================

(defn pragmatic-value
  "Pragmatic value: how much does this action advance goals?
   Higher for actions that address high-error channels."
  [action observation mu-sens adjacent-missions]
  (case action
    :work-on  (let [gap-error (Math/abs (- (:gap-count observation 0.5)
                                           (:gap-count mu-sens 0.5)))
                    stall-error (Math/abs (- (:stall-count observation 0.5)
                                            (:stall-count mu-sens 0.5)))
                    n-adjacent (count (filter :adjacent? adjacent-missions))]
                ;; Working on missions is most valuable when gaps/stalls are high
                ;; and there are adjacent missions to work on
                (+ (* 0.5 (+ gap-error stall-error))
                   (* 0.3 (obs/clamp01 (/ (double n-adjacent) 5.0)))
                   (* 0.2 (:gap-count observation 0.0))))
    :review   (let [review-error (Math/abs (- (:review-age observation 0.5)
                                              (:review-age mu-sens 0.5)))]
                ;; Review is valuable when review-age is high or observations are stale
                (+ (* 0.6 review-error)
                   (* 0.4 (:review-age observation 0.0))))
    :consolidate (let [spinoff-error (Math/abs (- (:spinoff-pressure observation 0.5)
                                                  (:spinoff-pressure mu-sens 0.5)))]
                   ;; Consolidation when spinoff pressure or entropy is high
                   (+ (* 0.5 spinoff-error)
                      (* 0.3 (:spinoff-pressure observation 0.0))
                      (* 0.2 (- 1.0 (:coverage-pct observation 0.5)))))
    :upvote   0.1  ; small constant — upvoting is always mildly useful
    :wait     (let [total-error (reduce + (map (fn [k]
                                                 (Math/abs (- (get observation k 0.5)
                                                              (get mu-sens k 0.5))))
                                               obs/channel-keys))]
                ;; Waiting is valuable when everything is calm (low total error)
                (* 0.5 (- 1.0 (obs/clamp01 (/ total-error 3.0)))))))

(defn epistemic-value
  "Epistemic value: how much would this action reduce uncertainty?
   Actions that produce new observations have high epistemic value."
  [action observation precision]
  (let [pi-o (:Pi-o precision)]
    (case action
      :work-on    ;; Working produces evidence → reduces uncertainty
      (let [low-precision-channels (count (filter (fn [[_k v]] (< v 0.6)) pi-o))]
        (* 0.3 (obs/clamp01 (/ (double low-precision-channels) 6.0))))
      :review     ;; Review directly refreshes all channels
      0.8
      :consolidate ;; Consolidation clarifies structure
      0.3
      :upvote     ;; Upvoting doesn't produce information
      0.05
      :wait       ;; Waiting produces no information
      0.0)))

(defn upvote-value
  "Upvote value: weighted by current vote signals.
   Placeholder: returns 0.0 until Nonstarter integration.
   Future: vote_weight × decay for each adjacent mission."
  [_action _upvote-state]
  0.0)

(defn effort-cost
  "Effort cost: how much does this action cost in mana/time?
   Lower cost → lower penalty → preferred, all else equal."
  [action]
  (case action
    :work-on     0.6   ; significant effort
    :review      0.2   ; moderate effort
    :consolidate 0.4   ; moderate-to-significant
    :upvote      0.05  ; minimal effort
    :wait        0.0)) ; zero effort

;; =============================================================================
;; Expected Free Energy
;; =============================================================================

(defn expected-free-energy
  "Compute G(a) for a single action.
   Lower G = better (minimizing free energy).

   Returns {:action, :G, :terms {term-name → value}}."
  [action observation mu-sens precision adjacent-missions upvote-state lambdas]
  (let [prag (pragmatic-value action observation mu-sens adjacent-missions)
        epis (epistemic-value action observation precision)
        upv (upvote-value action upvote-state)
        eff (effort-cost action)
        ;; G = negative of value + cost
        ;; Lower G = better, so: G = -(pragmatic + epistemic + upvote) + effort
        G (+ (* (:effort lambdas) eff)
             (- (* (:pragmatic lambdas) prag))
             (- (* (:epistemic lambdas) epis))
             (- (* (:upvote lambdas) upv)))]
    {:action action
     :G G
     :terms {:pragmatic prag
             :epistemic epis
             :upvote upv
             :effort eff}}))

;; =============================================================================
;; Softmax policy selection
;; =============================================================================

(defn softmax
  "Compute softmax probabilities from logits."
  [logits]
  (let [max-l (apply max logits)
        exps (mapv #(Math/exp (- % max-l)) logits)
        sum (reduce + exps)]
    (mapv #(/ % sum) exps)))

(defn choose-action
  "Evaluate all actions and select via softmax.

   Takes:
   - mu: belief state {:sens, :mode, :focus, :urgency}
   - precision: {:Pi-o {...}, :tau temperature}
   - observation: normalized [0,1] channel map
   - adjacent-missions: from adjacent/compute-adjacent-set
   - opts: {:upvote-state, :lambdas, :rng (random for reproducibility)}

   Returns:
   - {:action selected-action
      :policies [{:action :G :terms :probability}...]
      :tau current-temperature
      :abstain? whether tau < threshold}"
  [mu precision observation adjacent-missions opts]
  (let [lambdas (or (:lambdas opts) default-lambdas)
        upvote-state (or (:upvote-state opts) {})
        tau (:tau precision)
        abstain-threshold 0.55
        abstain? (< tau abstain-threshold)
        actions (:arena/actions portfolio-arena)
        ;; Compute G for each action
        evaluations (mapv #(expected-free-energy
                            % observation (:sens mu) precision
                            adjacent-missions upvote-state lambdas)
                          actions)
        ;; Softmax: logits = -G/τ (lower G → higher probability)
        logits (mapv (fn [e] (/ (- (:G e)) (max tau 0.1))) evaluations)
        probs (softmax logits)
        ;; Attach probabilities
        policies (mapv (fn [e p] (assoc e :probability p))
                       evaluations probs)
        ;; Select: if not abstaining, sample from distribution
        ;; For determinism in tests, use :rng seed; otherwise argmax
        selected (if abstain?
                   :wait
                   (let [rng (:rng opts)]
                     (if rng
                       ;; Stochastic: weighted sample
                       (let [r (.nextDouble rng)
                             cumulative (reductions + probs)]
                         (nth actions
                              (count (take-while #(< % r) cumulative))))
                       ;; Deterministic: argmax
                       (:action (apply max-key :probability policies)))))]
    {:action selected
     :policies policies
     :tau tau
     :abstain? abstain?}))
