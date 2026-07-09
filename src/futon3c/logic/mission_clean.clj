(ns futon3c.logic.mission-clean
  "Emit a MISSION as a DarkTower-spec CLean — the OUTER-loop tracker.

  A mission's 8-phase lifecycle is a `BV.seq` spine (non-commutative — you cannot
  DOCUMENT before you IDENTIFY); each phase is a TypedHole position; an unwritten
  phase is a hole HUNGRY TO BE WRITTEN (discharge = sorryProof); `document` is
  graded :payoff (the goal). A box carries a :hole iff the phase is still open for
  THIS mission, so the comb's live holes ARE the mission's frontier and \"the field
  moved\" = a phase-hole discharges (ghost/vacuous -> written / Obligation -> Empty).

  This mirrors `mathlib4/DarkTower/Examples.lean §MissionExample` and renders
  0-sorry through `futon6/scripts/clean_to_lean.py` while passing
  `futon6/scripts/clean_argcheck.bb` (G1-G8).

  The fill-state (which phases are written vs ghost/vacuous) is read straight off
  `capability-star-map-extractor/structural-hole-report`, this repo's phase-aware
  structural counter. `build-mission-clean` is pure (takes the report) so it is
  testable without the :3100 ego proxy; `emit-mission-clean!` fetches + writes.

  Deferred, tracked as `:clean/shape` metadata (see holes/excursions/
  E-scope-organism-copar.md): the mission's own scope∥organism copar. The generic
  renderer emits the informal∥formal copar; scope∥organism is a fidelity upgrade,
  not a correctness blocker. Held Open-questions are also metadata only — G3
  forbids a non-spine box, so the spine stays exactly 8 phases."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [futon3c.logic.capability-star-map-extractor :as ext]))

;; The eightfold lifecycle spine. MUST stay in sync with the extractor's private
;; `eightfold-phases` (strings) — this is the keyword image, in phase order.
(def phase-lifecycle
  [:head :identify :map :derive :argue :verify :instantiate :document])

;; Each phase :produces one artefact; the next phase :consumes it. The wires
;; carry these (G5: wire :carries must be produced by :from and consumed by :to).
(def ^:private phase-produces
  {:head        :mission-framed
   :identify    :sorry-stated
   :map         :research-map
   :derive      :construction
   :argue       :warranted-claim
   :verify      :checked-witness
   :instantiate :live-demo
   :document    :mission-documented})

(def ^:private phase-text
  {:head        "HEAD — the mission is framed: title, the scope∥organism reading, the sorry it exists to close."
   :identify    "IDENTIFY — the sorry is stated; scope-in / scope-out fixed; provisional completion criteria drafted; open questions held."
   :map         "MAP — research the terrain before building (MAP is research, not build)."
   :derive      "DERIVE — turn the mapped terrain into a construction that closes (or reduces) the sorry."
   :argue       "ARGUE — warrant the construction against the completion criteria: why it actually closes the sorry."
   :verify      "VERIFY — check the warranted claim against an ungameable witness (V2 no-teleport; earned, not fabricated)."
   :instantiate "INSTANTIATE — realize the verified capability as a live demo / substrate change."
   :document    "DOCUMENT — the mission's payoff: the loop is written up as reusable capability that descends into the stack."})

(def ^:private phase-wanted
  {:head        "the mission is framed on entry."
   :identify    "the sorry is stated and scope is fixed."
   :map         "the MAP is written for real — the first experiments land findings that fix the frame. An unwritten phase hungry to be written."
   :derive      "a real derivation exists — the move is constructed, not asserted."
   :argue       "the argument is written and holds — the claim is warranted, not merely plausible."
   :verify      "a checked witness outside the model confirms the claim — the drift is measured, not assumed zero."
   :instantiate "the capability runs on real substrate — a live instance, not a paper design."
   :document    "THE GOAL — graded :payoff. Documented as reusable capability; hungry until every prior phase discharges."})

(defn- phase-satiety
  "Static grade per §MissionExample: `document` is the payoff (the goal), the rest
   parse. Applied to OPEN holes only; discharged phases default to :canon (settled)."
  [phase]
  (if (= phase :document) :payoff :parse))

(defn- discharged?
  "A phase is discharged (Empty, no hole) iff written. HEAD is auto-discharged on
   entry — every admitted mission has a head/frame (E-mission-head), independent of
   whether the doc carries an explicit HEAD frame."
  [phase written-set]
  (or (= phase :head)
      (contains? written-set (name phase))))

(defn- phase-box
  [phase idx written-set]
  (let [base (cond-> {:id phase :method phase :text (phase-text phase)}
               (pos? idx) (assoc :consumes [(phase-produces (nth phase-lifecycle (dec idx)))])
               true       (assoc :produces (phase-produces phase)))]
    (if (discharged? phase written-set)
      base
      (assoc base :hole {:kind :sorry
                         :discharge :sorryProof
                         :satiety (phase-satiety phase)
                         :wanted (phase-wanted phase)}))))

(defn build-mission-clean
  "Pure. Given a mission-id and a `structural-hole-report` map (may be nil — then
   only HEAD is discharged, the freshly-entered baseline), return the CLean EDN map
   that passes clean_argcheck G1-G8 and renders 0-sorry via clean_to_lean.py."
  [mission-id report]
  (let [written-set (set (:phase/written report))
        boxes (map-indexed (fn [idx phase] (phase-box phase idx written-set))
                           phase-lifecycle)
        wires (for [[a b] (partition 2 1 phase-lifecycle)]
                {:from a :to b :carries (phase-produces a)})
        holes-at (set (keep #(when (:hole %) (:id %)) boxes))]
    {:clean/proof  (name mission-id)
     :clean/title  (str "Mission: " (name mission-id)
                        " — the 8-phase lifecycle as a DarkTower spec (outer-loop tracker)")
     :clean/source {:mission (str mission-id) :lifecycle phase-lifecycle}
     :clean/seq    phase-lifecycle
     :clean/boxes  (vec boxes)
     :clean/wires  (vec wires)
     :clean/copar  [{:reading :informal :is :clean/seq}
                    {:reading :formal   :is [:clean/boxes :clean/wires]}]
     :clean/shape  {:macro :eightfold-mission-lifecycle
                    :holes-at holes-at
                    :discharges-at #{}
                    ;; deferred domain copar (E-scope-organism-copar.md):
                    :readings [:scope :organism]
                    ;; held Open-questions tracked here, NOT as a spine box (G3):
                    :open-questions (long (or (:loose/open-question-count report) 0))}}))

(defn mission-clean-edn-str
  "Pretty EDN with explicit :clean/* keys (matching the corpus), not the
   #:clean{} namespaced-map collapse."
  [clean]
  (binding [*print-namespace-maps* false]
    (with-out-str (pprint/pprint clean))))

(defn emit-mission-clean!
  "Fetch the mission's structural-hole-report via the :3100 ego proxy, build its
   CLean, and write it to `out-path`. Returns {:clean … :path … :report …}. This is
   the 'upon entry' hook for the outer loop (dispatch_pilot_flight STEP 1b).

   opts:
     :refresh? true + :doc-path <path> — the LOOP-turn callback: SYNCHRONOUSLY
       reingest the (just-edited) mission doc and bust the 30s structural-cache
       BEFORE reading, so a phase discharge is observable immediately after the
       edit instead of waiting out the async watcher + cache TTL (STEP 4)."
  ([mission-id out-path] (emit-mission-clean! mission-id out-path {}))
  ([mission-id out-path opts]
   (when (:refresh? opts)
     (when-let [doc (:doc-path opts)]
       ((requiring-resolve 'futon3c.watcher.scope-reingest/reingest-now!) doc))
     (ext/invalidate-structural-cache! mission-id))
   (let [report (ext/structural-hole-report mission-id opts)
         clean  (build-mission-clean mission-id report)]
     (io/make-parents out-path)
     (spit out-path (str ";; AUTO-EMITTED mission CLean (outer-loop tracker) — futon3c.logic.mission-clean\n"
                         ";; Fill-state from structural-hole-report; regenerate on re-observe.\n"
                         (mission-clean-edn-str clean)))
     {:clean clean :path out-path :report report})))
