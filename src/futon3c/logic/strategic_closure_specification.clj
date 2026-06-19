(ns futon3c.logic.strategic-closure-specification
  "Family `strategic-closure-specification` (was :status :candidate in the
   structural-law inventory; built v0 2026-06-01, claude-2, invariant-queue audit
   Tier-B).

   Two sibling invariants:

     - strategic-sorry-next-step-sufficiently-specified
        Each strategic SORRY surfaced as a LIVE RECOMMENDATION must carry an
        honest v1 closure spec: canonical target, concrete action, and (when the
        registry grows them) step/effect/successor witnesses, backing surface,
        explicit failure interpretation.

     - under-specified-strategic-items-stay-candidate
        A strategic SORRY without a recommendation-grade v1 spec must NOT be
        promoted to a crisp War Machine next-move; it stays candidate pressure.

   Same shape + reading convention as `futon3c.logic.sorry-closures`: read the
   registry file directly (no cross-repo ns dependency), return the standard
   probe shape `{:outcome :ok | :violation :detail <map>}` for the probe family
   registry.

   ── v0 SCOPE / SCHEMA-GAP (honest, ratify before promoting to :operational) ──
   The live `futon2/resources/sorrys.edn` entries carry {:id :title :status :kind
   :resolved-at :target :resolution}. They do NOT carry discrete witness fields
   (:step/:effect/:successor witnesses, :backing-surface, :failure-interpretation)
   nor an explicit `:kind :strategic` tag. So this v0:
     (a) identifies 'strategic' sorrys HEURISTICALLY (`:kind :strategic`, or
         :strategic-vocabulary present, or 'strategic' in :title) — flagged as a
         v0 heuristic, NOT a ratified definition;
     (b) checks the v1-spec fields it CAN see today (canonical :target present +
         a non-trivial concrete action via :resolution/:action);
     (c) reports the missing witness fields as a SCHEMA GAP, not a violation —
         the invariant cannot fully bind until the registry grows those fields.
   This is why the family stays :candidate (not :operational) after this build:
   full binding needs a registry-schema decision (owner: the sorry-registry /
   War Machine line), which must not be invented unilaterally here."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private home (System/getProperty "user.home"))

(def registry-candidate-paths
  "Preferred → fallback, matching futon3c.logic.sorry-closures."
  [(str home "/code/futon2/resources/sorrys.edn")
   (str home "/code/futon2/data/sorrys.edn")])

(defn registry-path []
  (first (filter #(.exists (io/file ^String %)) registry-candidate-paths)))

(defn load-registry
  ([] (load-registry (registry-path)))
  ([path] (when path (edn/read-string (slurp path)))))

;; ── v0 strategic identification (heuristic — see ns SCHEMA-GAP note) ──────────
(defn strategic-sorry?
  "v0 heuristic. A ratified `:kind :strategic` (or explicit flag) would replace
   this; until then we recognise strategic-vocabulary linkage or a title signal."
  [s]
  (boolean
    (or (= :strategic (:kind s))
        (contains? s :strategic-vocabulary)
        (and (string? (:title s))
             (str/includes? (str/lower-case (:title s)) "strategic")))))

(defn live-recommendation?
  "A strategic SORRY counts as a live recommendation when it is still open
   (not resolved/closed) — those are the ones that would enter the War Machine
   as next moves. Resolved/foreclosed items are not live recommendations."
  [s]
  (contains? #{:open :reopened :candidate} (:status s)))

;; ── v1-spec completeness (what is checkable TODAY) ───────────────────────────
(def v1-spec-fields-present
  "The witness fields the family wants. Only :target + concrete-action are
   present in the v0 registry; the rest are recorded as schema-gap, not failure."
  {:checkable   [:canonical-target :concrete-action]
   :schema-gap  [:step-witness :effect-witness :successor-witness
                 :backing-surface :failure-interpretation]})

(defn- has-concrete-action? [s]
  (let [a (or (:action s) (:resolution s) (:next-step s))]
    (cond
      (string? a) (>= (count (str/trim a)) 12)
      (sequential? a) (boolean (seq a))          ; (str ...) resolution form
      :else false)))

(defn v1-spec-status
  "Returns {:has-target? :has-action? :checkable-complete? :schema-gap [...]}."
  [s]
  (let [has-target? (boolean (:target s))
        has-action? (has-concrete-action? s)]
    {:has-target? has-target?
     :has-action? has-action?
     :checkable-complete? (and has-target? has-action?)
     :schema-gap (:schema-gap v1-spec-fields-present)}))

(defn under-specified-strategic
  "Strategic live-recommendation SORRYs whose CHECKABLE v1-spec is incomplete
   (missing canonical target or concrete action). Returns vector of
   {:id :has-target? :has-action? :schema-gap}. These must stay candidate, not
   enter the War Machine as crisp next moves (invariant #2)."
  [registry-doc]
  (->> (:sorrys registry-doc)
       (filter strategic-sorry?)
       (filter live-recommendation?)
       (map (fn [s] (assoc (v1-spec-status s) :id (:id s))))
       (remove :checkable-complete?)
       (mapv #(dissoc % :checkable-complete?))))

(defn check
  "Probe-family entry. :ok when every strategic live-recommendation SORRY has a
   checkable v1 spec (target + concrete action); :violation lists the
   under-specified ones. Always reports :schema-gap so the partial-binding is
   honest — full binding awaits registry witness fields."
  ([] (check (load-registry)))
  ([registry-doc]
   (if (nil? registry-doc)
     {:outcome :ok :detail {:note "no sorry registry found; nothing to check"}}
     (let [under (under-specified-strategic registry-doc)]
       (if (empty? under)
         {:outcome :ok
          :detail {:schema-gap (:schema-gap v1-spec-fields-present)
                   :note "all strategic live-recommendation sorrys have checkable v1 spec; witness-field binding awaits registry schema"}}
         {:outcome :violation
          :detail {:under-specified under
                   :schema-gap (:schema-gap v1-spec-fields-present)
                   :note "strategic sorrys surfaced as live recommendations without canonical target + concrete action; keep them :candidate (invariant #2)"}})))))
