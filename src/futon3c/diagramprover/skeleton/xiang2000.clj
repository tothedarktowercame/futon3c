(ns futon3c.diagramprover.skeleton.xiang2000
  "Skeleton use case: M-象-2000's open causal questions, asked of open
  causal theories glued together (`causal.open-theory`).

  The specimen is the mission's own acceptance case, the 2026-09-24 Kimi red
  tape (M-象-2000 §Completion criterion). The record answers what happened
  and in what order; the mission names three questions the record cannot
  answer by itself:

  - derivation of R is \"a bounded session/order reconstruction, not a
    stored causal edge\" (scripts/xiang2000_p0.py);
  - the hungry hole :反事实世界模型 asks \"if P had been in force at T0,
    would the incident have happened?\", which ARGUE-0 says needs a world
    model, and P14 was narrowed because there was none;
  - the capture drop of 09-24 is to be held apart from the requisition rule,
    a causal claim the mission states by hand.

  Here each pattern that bears on the incident is a small OPEN theory with
  a declared interface; the incident is their gluing. Every answer below is
  computed from the glued theory with the existing D1 machinery, and every
  answer is only as good as the mechanisms written here: the mechanisms are
  the world model ARGUE-0 asked for, made explicit and reviewable rather
  than left implicit in a replay. They are authored from the mission text,
  not mined from the store.

  Variables (all Boolean, one per recorded act or effect):
    :limit-report        15:48 report-problem, Kimi showed a 5-hour limit
    :constraint-act      16:03 constrain, a Kimi job must name its mission
    :enforcement-proposal 16:20/16:28 propose an enforcement rule
    :requisition-commit  16:34 commit 5146606d
    :rule-in-force       the P13 rule record's projection of that commit
    :guard-in-force      a pattern P warning against enforcement notices
                         delivered as operator turns, in force at T0
    :unguarded           not guard-in-force
    :followup-wired      the followup half of the rule is live
    :notices-under-joe   the 42 notices delivered as turns under Joe's name
    :red-tape            Joe: the notices are red tape
    :analysis-refusals   kimi-1 refused analysis jobs lacking a requisition
    :capture-default-off live capture off by default after 09-23
    :capture-drop        captured turns fell from 147 to 15"
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.open-theory :as ot]
            [futon3c.diagramprover.causal.scm :as scm]))

;; ---------------------------------------------------------------------------
;; Pattern-sized open theories

(def operator-proposals
  "Report → constrain → propose → commit: the operator acts of 15:48–16:34."
  (ot/theory :operator-proposals
             {"constraint-act" "limit-report"
              "enforcement-proposal" "constraint-act"
              "requisition-commit" "enforcement-proposal"}
             :inputs ["limit-report"]
             :interface [:requisition-commit]))

(defn followup-delivery
  "An enforcement followup that delivers its notices as operator turns,
  unless a guarding pattern is in force. ENFORCEMENT-SOURCE is what the
  followup reads: the commit itself (as built) or the rule record."
  ([] (followup-delivery :requisition-commit))
  ([enforcement-source]
   (ot/theory :followup-delivery
              {"unguarded" "not guard-in-force"
               "followup-wired" (str (name enforcement-source) " and unguarded")
               "notices-under-joe" "followup-wired"
               "red-tape" "notices-under-joe"}
              :inputs ["guard-in-force" (name enforcement-source)]
              :interface [enforcement-source])))

(def rule-reporting
  "P13 as built: the rule record is projected from the commit."
  (ot/theory :rule-reporting {"rule-in-force" "requisition-commit"}
             :interface [:requisition-commit :rule-in-force]))

(def kimi-refusals
  (ot/theory :kimi-refusals {"analysis-refusals" "requisition-commit"}
             :interface [:requisition-commit]))

(def capture-default
  (ot/theory :capture-default {"capture-drop" "capture-default-off"}
             :inputs ["capture-default-off"]))

(def as-built
  "The stack as the mission describes it: the requisition gate \"never reads
  rule records\" (M-象-2000, P13 notes)."
  [operator-proposals (followup-delivery :requisition-commit)
   rule-reporting kimi-refusals capture-default])

(def rule-gated
  "The alternative wiring: enforcement reads the rule record."
  [operator-proposals (followup-delivery :rule-in-force)
   rule-reporting kimi-refusals capture-default])

(defn incident
  "The glued theory, throwing if the patterns do not compose."
  ([] (incident as-built))
  ([theories]
   (let [{:keys [glued? dag] :as r} (ot/glue theories)]
     (when-not glued? (throw (ex-info "patterns do not glue" r)))
     dag)))

;; ---------------------------------------------------------------------------
;; Receipts

(def happened
  "What the record shows: everything occurred, and no guard was in force."
  {:limit-report true :guard-in-force false :capture-default-off true})

(defn- cf [d intervention outcome]
  (let [r (scm/counterfactual d {:evidence (assoc happened :red-tape true)
                                 :intervention intervention :outcome outcome})]
    (select-keys r [:method :answer :reason])))

(defn derivation
  "Derivation of R as the causal cone of R: its ancestors in the theory."
  ([r] (derivation (incident) r))
  ([d r] (into (sorted-set) (dag/ancestors d r))))

(defn receipts []
  (let [d (incident)
        gated (incident rule-gated)]
    {:derivation
     {:requisition-commit (derivation d :requisition-commit)
      :red-tape (derivation d :red-tape)}

     :capture-drop-held-apart
     {:d-separated-from-commit? (dsep/d-separated? d :capture-drop :requisition-commit #{})
      :had-no-commit-capture-still-drops (cf d {:requisition-commit false} :capture-drop)}

     :prevention
     {:question "had guard P been in force at T0, would the red tape have occurred?"
      :answer (cf d {:guard-in-force true} :red-tape)}

     :withdrawal
     {:as-built (cf d {:rule-in-force false} :red-tape)
      :rule-gated (cf gated {:rule-in-force false} :red-tape)}

     :predictions
     (let [ci (dsep/implied-independencies d {:max-conditioning 1})]
       {:count (count ci) :sample (vec (take 5 ci))})}))
