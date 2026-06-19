(ns futon3c.wm.operator-lane
  "E-wm-operator-lane — the lane classifier.

   Assigns each surfaced War Machine item to exactly one operator-communication
   lane from its DESCRIPTIVE attributes:

   - :silent — autonomously dischargeable; no operator surfacing needed.
   - :brief  — lands in the morning bulletin Joe PULLS; never interrupts.
   - :nag    — actively pushed; the narrow, earned interrupt.

   The nag gate is the conjunction (in-Joe's-model ∧ futon-important ∧ risk-mode)
   PLUS prior acknowledgement — novelty flows down, so a first-sighting item can
   only reach :brief and escalates to :nag only after Joe has seen it.

   Importance is read DESCRIPTIVELY (`:futon-important?`, sourced from `c_joint`
   structural centrality / a declared crux). A predicted-importance signal is
   NEVER consulted (INV-5 — no descriptive→predictive laundering).

   This classifier is verified against the independently-authored invariant model
   `futon3c.logic.wm-operator-lane-invariants` (INV-1..INV-6); see
   `holes/excursions/E-wm-operator-lane.md` §9.

   Item attribute map (all optional booleans, absent = false):
     {:in-joes-model? :futon-important? :risk-mode? :acknowledged?
      :operator-dependent? :framing-blocked? :mode}")

(def lanes #{:silent :brief :nag})

(defn nag?
  "The nag gate: all three descriptive conditions hold AND the item was already
   acknowledged (so novelty flows down to :brief on first sighting — INV-3+INV-4)."
  [{:keys [in-joes-model? futon-important? risk-mode? acknowledged?]}]
  (boolean (and in-joes-model? futon-important? risk-mode? acknowledged?)))

(defn needs-surfacing?
  "True when the item must at least reach :brief (cannot be :silent): it depends
   on the operator, is framing-blocked, or is structurally important (INV-6)."
  [{:keys [operator-dependent? framing-blocked? futon-important?]}]
  (boolean (or operator-dependent? framing-blocked? futon-important?)))

(defn classify-item
  "Return the lane (:silent | :brief | :nag) for ITEM's descriptive attributes.
   Total and mutually exclusive (INV-2). Any `:predicted-important?` key is
   ignored by construction (INV-5)."
  [item]
  (cond
    (nag? item)             :nag
    (needs-surfacing? item) :brief
    :else                   :silent))
