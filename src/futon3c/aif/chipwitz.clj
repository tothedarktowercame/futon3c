(ns futon3c.aif.chipwitz
  "M-chipwitz-corps: the warranted-work control layer.

  At every choice point, run the warrant-finder BEFORE escalating to the
  operator. A warrant DETERMINES the choice (not merely consistent with it).
  Warrant found → log PSR and proceed. No warrant → genuine operator question
  (NAG: Named Actionable Gap) AND a library gap.

  This is the determined-fork test (collaboration-coherence/determined-fork-proto-psr)
  operationalized as the ChipWitz gate's first move.

  STAGED: this namespace provides the PURE functions for warrant-checking,
  PSR logging, NAG formatting, and autopen-rate measurement. The live wiring
  into the pilot flow (consulting the warrant-finder before surfacing operator
  questions) is pilot-flow integration work that the fold does NOT do — it
  requires Joe's call on touching the recovered pilot.

  Pure + no I/O: the warrant-finder is INJECTED (the caller provides the
  cascade-policy-for function) so this ns is testable without the constructor."
  (:require [clojure.set :as set]))

;; ── The warrant threshold (the one knob) ──────────────────────────────
;;
;; The mission doc names this as DERIVE work — 'what cascade C-score / match
;; quality counts as found?' The default is deliberately conservative:
;; a warrant DETERMINES the choice, not merely consistent with it.
;; So the top pattern must have rel > threshold AND must be a DETERMINING
;; pattern (one that prescribes a specific action, not just describes context).
;;
;; The threshold is a DEF — not a hardcoded constant — so the operator can
;; tune it. Too loose → autopen-creep; too tight → rubber-stamp asks.

(def ^:dynamic ^:private *warrant-threshold*
  "Minimum cascade rel-score for a pattern to count as a WARRANT.
  Default 0.45: the cycle-5 motivating example had expected-information-gain
  at rel ~0.5, which should have been a findable warrant. Below this, the
  pattern is too weak to DETERMINE the choice."
  0.45)

(def ^:dynamic ^:private *autopen-window*
  "Number of recent warranted-proceeds to consider for autopen-rate calculation."
  20)

;; ── b1: The determined-fork test ──────────────────────────────────────

(defn find-warrant
  "Run the warrant-finder on a circumstance and return the determining warrant
  if one exists, or nil.

  `cascade-policy-fn` — injected: (fn [psi-text budget]) → cascade map with
    :shown [{:pattern ... :rel ...}] (the cascade-policy-for interface).
  `circumstance` — {:psi <text> :choice-point {:description <str> :options [<str>...]}}
  `budget` — cascade budget (default 6)

  Returns: {:pattern-id <str> :rel <double> :cascade-size <int> :determines? true}
  or nil if no warrant found above threshold.

  A warrant DETERMINES the choice when:
  1. The top pattern's rel-score exceeds *warrant-threshold*
  2. The pattern is in the warrant-eligible set (patterns that prescribe
     action, not just describe context)"
  ([cascade-policy-fn circumstance] (find-warrant cascade-policy-fn circumstance 6))
  ([cascade-policy-fn circumstance budget]
   (let [psi (:psi circumstance)
         cascade (cascade-policy-fn psi budget)
         shown (:shown cascade)]
     (when (seq shown)
       (let [top (first (sort-by :rel > shown))
             rel (:rel top)
             pattern-id (:pattern top)]
         (when (and (number? rel) (>= rel *warrant-threshold*))
           {:pattern-id pattern-id
            :rel rel
            :cascade-size (:size cascade)
            :determines? true}))))))

(defn determined-fork?
  "The ChipWitz gate's first move: is this choice-point DETERMINED by an
  existing warrant? Returns the warrant map if yes, nil if no.

  This is the test that should have run before cycle-5's A/B fork was
  surfaced to Joe: the warrant (expected-information-gain) was findable,
  so the fork was determined, and the operator should have gotten a
  notification, not a question."
  ([cascade-policy-fn circumstance] (determined-fork? cascade-policy-fn circumstance 6))
  ([cascade-policy-fn circumstance budget]
   (find-warrant cascade-policy-fn circumstance budget)))

;; ── b1: PSR logging (the proto-PSR for a warranted proceed) ───────────

(defn make-psr
  "Build a Pattern Selection Record for a warranted proceed.
  This is the proto-PSR the cycle-5 fork should have produced:
  'choosing B because expected-information-gain prefers measured pairs.'"
  [{:keys [choice-point warrant chosen-option rationale]}]
  {:psr/choice-point (:description choice-point)
   :psr/pattern-id (:pattern-id warrant)
   :psr/pattern-rel (:rel warrant)
   :psr/chosen-option chosen-option
   :psr/rationale (or rationale
                      (str "Choosing " chosen-option " because "
                           (:pattern-id warrant) " (rel " (:rel warrant)
                           ") determines this choice."))
   :psr/at (str (java.time.Instant/now))})

;; ── b3: The NAG (Named Actionable Gap) ────────────────────────────────

(defn make-nag
  "Build a NAG (Named Actionable Gap) for a choice-point where NO warrant
  was found. This is the dual of the warranted-proceed: the patterns CANNOT
  decide, so the choice is legitimately the operator's.

  The NAG surfaces WITH its warrant context (what was tried), the specific
  undecidable gap, and the minimal unblock — 'Because of pattern P which you
  set, I can't decide this one thing; fill the gap and we roll.'"
  [{:keys [choice-point cascade-result threshold]}]
  (let [options (:options choice-point)
        top-pattern (when (seq (:shown cascade-result))
                      (:pattern (first (sort-by :rel > (:shown cascade-result)))))]
    {:nag/warranted false
     :nag/choice-point (:description choice-point)
     :nag/options options
     :nag/best-effort-pattern top-pattern
     :nag/best-effort-rel (when (:shown cascade-result)
                            (:rel (first (sort-by :rel > (:shown cascade-result)))))
     :nag/gap (str "No pattern above threshold " threshold
                   " determines this choice"
                   (when top-pattern
                     (str " (closest: " top-pattern ")")))
     :nag/unblock "Specify which option the standing contract prefers, or confirm this is genuinely operator-decided."
     :nag/at (str (java.time.Instant/now))}))

;; ── b2: The autopen rate ──────────────────────────────────────────────

(defn autopen-rate
  "Compute the autopen rate from a sequence of PXR (PSR+PUR) records.

  The autopen rate is the fraction of warranted-proceeds where the warrant
  turned out to be rubber-stamp (the proceed achieved NOTHING the warrant
  predicted). This is the honest-map metric: if everything proceeds on
  warrant, the threshold is too loose.

  `pxr-records` — a sequence of {:psr ... :pur {:outcome ... :prediction-error ...}}
  `pur-outcome-key` — the key in :pur that indicates success (default :outcome)

  Returns: {:rate <double 0.0-1.0> :n-total <int> :n-autopen <int>}

  A proceed is 'autopen' when:
  - PUR outcome is nil/absent (no outcome evidence = rubber-stamp)
  - OR prediction-error is large (> 0.5 absolute, meaning the warrant
     predicted something very different from what happened)"
  ([pxr-records] (autopen-rate pxr-records *autopen-window*))
  ([pxr-records window]
   (let [recent (take-last window pxr-records)
         n-total (count recent)
         autopen? (fn [pxr]
                    (let [pur (:pur pxr)]
                      (or (nil? (:outcome pur))
                          (nil? (:prediction-error pur))
                          (and (number? (:prediction-error pur))
                               (> (Math/abs (double (:prediction-error pur))) 0.5)))))
         n-autopen (count (filter autopen? recent))]
     {:rate (if (pos? n-total) (/ (double n-autopen) n-total) 0.0)
      :n-total n-total
      :n-autopen n-autopen})))

(defn threshold-adjustment
  "Given the current autopen rate, compute the threshold adjustment.
  High autopen rate → RAISE the threshold (make hidden over-proceeding visible).
  Low autopen rate → keep or lower (the warrants are discriminating).

  Returns: {:new-threshold <double> :direction <:raise|:lower|:hold> :reason <str>}"
  [current-threshold autopen-rate-result]
  (let [rate (:rate autopen-rate-result)
        n (:n-total autopen-rate-result)]
    (cond
      (< n 5) {:new-threshold current-threshold :direction :hold
               :reason "insufficient data (<5 PXR records) — hold threshold"}
      (> rate 0.3) {:new-threshold (min 0.8 (+ current-threshold 0.05))
                    :direction :raise
                    :reason (str "autopen rate " (format "%.1f%%" (* 100 rate))
                                 " > 30% — raising threshold to reduce rubber-stamping")}
      (< rate 0.05) {:new-threshold (max 0.3 (- current-threshold 0.05))
                     :direction :lower
                     :reason (str "autopen rate " (format "%.1f%%" (* 100 rate))
                                  " < 5% — warrants are discriminating; lowering threshold")}
      :else {:new-threshold current-threshold :direction :hold
             :reason (str "autopen rate " (format "%.1f%%" (* 100 rate))
                          " in healthy range 5-30%")})))

;; ── The full ChipWitz gate (pure, for testing) ────────────────────────

(defn chipwitz-gate
  "The full ChipWitz gate: run the determined-fork test and return either
  a warranted-proceed (with PSR) or a NAG.

  Returns: {:decision :warranted-proceed :psr <map>}
           {:decision :nag :nag <map>}

  This is the function that should sit BEFORE the operator-escalation path
  in the pilot flow. Warranted-proceeds get a notification; NAGs get a question."
  ([cascade-policy-fn circumstance] (chipwitz-gate cascade-policy-fn circumstance 6))
  ([cascade-policy-fn circumstance budget]
   (let [warrant (determined-fork? cascade-policy-fn circumstance budget)]
     (if warrant
       {:decision :warranted-proceed
        :psr (make-psr {:choice-point circumstance
                        :warrant warrant
                        :chosen-option (first (:options circumstance))})}
       (let [cascade-result (cascade-policy-fn (:psi circumstance) budget)]
         {:decision :nag
          :nag (make-nag {:choice-point circumstance
                          :cascade-result cascade-result
                          :threshold *warrant-threshold*})})))))
