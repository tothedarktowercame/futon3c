(ns futon3c.aif.loop-learning
  "Automated LOOP learning derivation — repl.spec.edn §turns :LOOP :autonomy / V6.

   v0 of the partial lift of M-a-sorry-enterprise (sorry mining) +
   M-pattern-application-diagnostic (pattern scoring) into the LOOP turn, so
   the operator-B→A learning channel survives non-interactive overnight runs.

   AUTO-MINED (not agent-supplied): the :learning block is derived mechanically
   from the cycle's own data —
     patterns-applied : the patterns the REPL operator structurally applies
                        every cycle, ∪ any fork-warrant patterns recorded in
                        the frame (M-pattern-application-diagnostic).
     sorries-mined    : the WM judgement's OWN gap-signals — missing AIF heads
                        and channel-gaps out of preferred range — minus what
                        the sorry-registry already tracks (M-a-sorry-enterprise).
                        Grounded in niche-construction: the WM's structural
                        gaps ARE the missing edge families.

   Heuristic + improvable; candidates for operator review. The interactive
   M-a-sorry-enterprise pass back-fits to (and improves on) this. :derivation
   is stamped :auto-mined so a reader can tell it from an agent-supplied block."
  (:require [futon3c.aif.repl-trace :as rt]))

(defn- g
  "Tolerant get: keyword key OR its string form (judgement comes string-keyed
   from the snapshot cache, keyword-keyed via wm-api-query JSON parse)."
  [m k] (or (get m k) (get m (name k))))

(def ^:private repl-cycle-patterns
  "Patterns the REPL operator applies structurally on every cycle (grounded in
   M-pilot-appearance §8.4.3 PSR)."
  [{:pattern "agent/sense-deliberate-act" :applied? true
    :role "R/E/P/L IS sense-deliberate-act-update at the operator-peripheral scale"}
   {:pattern "aif/predictive-coding-belief-update" :applied? true
    :role "EVAL predicts discharge; LOOP measures realised; prediction-error is the belief update"}])

(defn patterns-from-frame
  "patterns-applied = REPL-cycle structural patterns ∪ frame :fork-warrant patterns."
  [frame]
  (let [warrant (g (or frame {}) :fork-warrant)
        wp (when warrant (g warrant :patterns))]
    (vec (concat repl-cycle-patterns
                 (map (fn [p] {:pattern (g p :pattern) :applied? true :role (g p :role)})
                      (or wp []))))))

(def ^:private max-mined 12)

(defn judgement-gap-sorries
  "Mine candidate sorries from a WM judgement's own gap-signals (:priorities),
   excluding gaps already tracked in OPEN-SORRY-IDS. Bounded at max-mined;
   reports truncation in :_truncated meta on the vector when it bites."
  [judgement open-sorry-ids]
  (let [existing (set (map name open-sorry-ids))
        cands (->> (g judgement :priorities)
                   (keep (fn [p]
                           (let [t (g p :type) id (some-> (g p :id) name)]
                             (cond
                               (nil? id) nil
                               (= t "missing-head")
                               {:id (keyword "sorry" (str "aif-head-missing-" id))
                                :kind :prototyping-forward
                                :rating 1.0   ; structural gaps top the rating
                                :title (str "AIF head not readable by WM head: " id)
                                :rationale (str (g p :note) " (auto-mined from WM missing-head priority)")}
                               (= t "channel-gap")
                               {:id (keyword "sorry" (str "channel-gap-" id))
                                :kind :technical-debt
                                :rating (double (or (g p :gap) 0.0))   ; rate by gap magnitude
                                :title (str "WM channel " id " out of preferred range")
                                :rationale (str (g p :summary) " (auto-mined from WM channel-gap priority)")}
                               :else nil))))
                   (remove #(contains? existing (name (:id %))))
                   distinct
                   (sort-by :rating >)   ; highest-warrant first → top-N% promotion
                   vec)
        kept (vec (take max-mined cands))]
    (if (> (count cands) max-mined)
      (with-meta kept {:_truncated (- (count cands) max-mined)})
      kept)))

(defn- enacted-cascade-pattern
  "H3 (2026-09-17): the judgement's decision is a cascade decision or a
   typed abstention. When a cascade decision is enacted, its first acting
   pattern (first of the chosen candidate's :precedence) is the step the
   tick actually enacted; the recorded cascade posterior is consumed as the
   enacted step's marginal (:chosen-action-mass). Returns nil for
   abstentions — no pattern is invented."
  [judgement]
  (let [decision (g (or judgement {}) :decision)]
    (when (and (map? decision)
               (= "cascade-selection-posterior"
                  (some-> (get-in decision [:selection-law :applied]) name)))
      (let [action (g decision :action)
            precedence (when (map? action) (g action :precedence))
            posterior-mass (g decision :chosen-action-mass)]
        (when (seq precedence)
          (cond-> {:pattern (first precedence)
                   :applied? true
                   :role "enacted first step of the WM cascade decision"}
            posterior-mass (assoc :posterior-mass posterior-mass)))))))

(defn loop-learning-pass
  "Produce an auto-mined :learning block (repl_trace/learning shape).
     :judgement       — live WM judgement (has :priorities and, when a
                       cascade decision was enacted, its posterior and
                       enacted first step; :ranked-actions is gone with the
                       flat grain, H3 2026-09-17)
     :frame           — the cycle's frame (for :fork-warrant), optional
     :open-sorry-ids  — current registry open ids (avoid re-mining tracked gaps)"
  [{:keys [judgement frame open-sorry-ids]}]
  (let [mined (judgement-gap-sorries judgement (or open-sorry-ids []))
        trunc (:_truncated (meta mined))
        enacted (enacted-cascade-pattern judgement)]
    (rt/learning
     {:derivation :auto-mined
      :patterns-applied (cond-> (patterns-from-frame frame)
                          enacted (conj enacted))
      :sorries-mined mined
      :notes (str "auto-mined v0: patterns = REPL-cycle structure + fork-warrant; "
                  "sorries = WM judgement gap-signals (missing-heads + channel-gaps) "
                  "minus registry-tracked, :rating-sorted (gap magnitude; structural=1.0). "
                  "ADVISORY-ONLY: frame-local, NOT persisted, NOT WM-actionable — the WM "
                  "only proposes/closes PERSISTED registry sorries. Promotion of the top-rated "
                  "candidates to the registry is E-cheesemonger's gated job; closure claims must "
                  "cite a persisted sorry-id (no closing an ephemeral candidate). "
                  "The interactive M-a-sorry-enterprise pass back-fits to this."
                  (when trunc (str " [" trunc " further gap-candidates truncated at "
                                   max-mined "]")))})))
