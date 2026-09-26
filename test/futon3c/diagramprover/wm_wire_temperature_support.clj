(ns futon3c.diagramprover.wm-wire-temperature-support
  "Shared support for WIRE-23-B's temperature chain: the three wires out of
  :r14-precision-carry on the field [:beta {:record :precision}].

  The writer is futon2.aif.policy-precision-carry/advance
  (policy_precision_carry.clj): the ADVANCING return `(seal (merge ...))`
  (the branch that runs cascade-beta-update, marked by its :update entry)
  is the witness path; the `(hold ...)` branches return the previous
  record's beta with :status :held and no :update. Its readers here are
  war-machine/cascade-decision-admitted (:r9-decision,
  war_machine.clj:6825-6848: `beta-state (precision-carry/advance ...)`
  then `{:beta (:beta beta-state) :beta-state beta-state ...}` into
  policy/select-action-cascades), policy/select-action-cascades itself
  (:r9-selection-law, policy.clj:300, arg 2 `{:beta ...}`) and
  cascade-selection/selection-posterior (:r14-selection-posterior,
  cascade_selection.clj:54, arg 1 `{:beta ...}`).

  LIVE: one tick record under holes/labs/M-wm-wiring/spike/ carries :beta
  at all, tick-run-record-2026-09-26-flight-278b6988-click-1.edn (the
  other thirteen spike records and the M-futon-seams exemplars carry
  none). It carries BOTH ends of each wire: the writer's sealed record at
  [:decision :selection-certificate :policy-precision-state] (:beta 1,
  :status :held — the live tick took a hold branch; the advancing branch
  is exercised hermetically below) and, from it, the decision's recorded
  beta ([:decision :selection-certificate :beta :value]) and the selection
  law's recorded beta and posterior ([:decision :selection-law :beta],
  :softmax-weights, :applied :cascade-selection-posterior). Each wire
  test pins the record by sha256.

  The hermetic drive below runs the REAL advance on the fixture family of
  futon2's policy-precision-carry-test, takes its advancing return, and
  feeds (:beta that-return) to the real reader vars; the bad cases tamper
  :beta at each reader's door. selection-posterior has NO default
  temperature: a missing or non-positive beta refuses :invalid-temperature
  (cascade_selection.clj:74-75)."
  (:require [futon2.aif.cascade-selection :as cascade-selection]
            [futon2.aif.policy :as policy]
            [futon2.aif.policy-precision-carry :as precision-carry]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

;; The writer's end: the sealed precision-carry record's :beta.
(def writer-path [:decision :selection-certificate :policy-precision-state :beta])
;; The r9-decision reader's produced value: the decision's recorded beta.
(def decision-reader-field [:decision :selection-certificate :beta])
;; The r9-selection-law reader's produced value: the law's recorded beta;
;; also what arg 1 of selection-posterior carried on this tick.
(def law-reader-field [:decision :selection-law :beta])

(defn observe
  "The writer's :beta and the reader's value under the field in the pinned
   tick record. TAMPER rewrites the record at the reader's door (the bad
   cases) before the reader's value is taken."
  ([reader-field] (observe reader-field identity))
  ([reader-field tamper]
   (let [r (tamper (w/read-record (:path record)))]
     {:writer (get-in r writer-path)
      :reader (get-in r reader-field)})))

;; --- the hermetic drive: the real writer var, advancing branch ---------

(def token ["target" :artifact])
(def model {:q0 {#{} 1} :rates {token {:false-neg 0 :false-pos 0}} :horizon 2})
(def clock {"target" {:tau {:value 1 :status :declared}}})

(defn- policy-action [id theta]
  {:kind :cascade-candidate :id id :target "target"
   :precedence [{:id id :theta theta
                 :guard {:status :interpreted :clauses [{:present #{} :absent #{}}]}
                 :produces #{token}}]})

(def low (policy-action :low 1/2))
(def high (policy-action :high 1/4))
(def candidates [{:id low :g 0.0 :habit 1.0} {:id high :g 1.0 :habit 1.0}])
(def family (precision-carry/family
             {:action low :selection-certificate {:candidates candidates}}
             model clock))
(def admission {:status :admitted :record-sha256 "record"
                :occurrence {:action/value low}
                :present #{token} :absent #{} :unknown #{}})

(defn advance-advancing
  "Run the real policy-precision-carry/advance and return its ADVANCING
   return (the `(seal (merge ...))` branch, marked by :update); refuses to
   continue if a hold branch answered."
  []
  (let [r (precision-carry/advance {:previous nil :initialized-beta 1
                                    :model-id (:model-id family)
                                    :admission admission :family family})]
    (assert (contains? r :update)
            "witness path is the advancing branch; a hold branch sets :status :held and no :update")
    r))

(defn law-decision
  "The r9-selection-law reader: the real policy/select-action-cascades at
   the caller-declared beta, over one acting cascade candidate."
  [beta]
  (policy/select-action-cascades
   [{:cascade true :cascade-id :c1
     :action {:id :c1 :cascade-id :c1 :target "target"
              :precedence [{:id :p/a :produces #{token}}]}
     :controller-score 0.5 :rank 1}]
   {:beta beta}))

(def posterior-candidates
  [{:id :a :habit 1.0 :f 0.0 :g 0.0}
   {:id :b :habit 1.0 :f 0.0 :g 1.0}])

(defn posterior
  "The r14-selection-posterior reader: the real
   cascade-selection/selection-posterior at the given temperature."
  [beta]
  (cascade-selection/selection-posterior
   {:beta beta :candidates posterior-candidates}))
