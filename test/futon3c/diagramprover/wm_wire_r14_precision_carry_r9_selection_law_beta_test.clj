(ns futon3c.diagramprover.wm-wire-r14-precision-carry-r9-selection-law-beta-test
  "Wire [:r14-precision-carry :r9-selection-law [:beta {:record
  :precision}]]: the carried beta reaching the selection law.

  The writer is futon2.aif.policy-precision-carry/advance's advancing
  return. The reader is futon2.aif.policy/select-action-cascades
  (policy.clj:300): arg 2 `{:beta ...}` — no default, a missing or
  non-positive beta propagates selection-posterior's :invalid-temperature
  refusal — passed to cascade-selection/selection-posterior and recorded
  on the decision as :selection-law :beta, the reader's produced value
  under the field.

  This wire is VERIFIED: the pinned tick record carries both ends — the
  writer's sealed record's :beta at
  [:decision :selection-certificate :policy-precision-state :beta] and the
  law's recorded beta at [:decision :selection-law :beta] — and they are
  equal. A hermetic corroboration drives the REAL advance (advancing
  branch) into the REAL select-action-cascades and tampers :beta at the
  law's door."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temperature-support :as t]))

(defn check [] (t/observe t/law-reader-field))

(def wire
  {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r14-precision-carry-r9-selection-law-beta-test/hermetically-the-advancing-beta-is-the-laws-beta :kind :refusal
                  :product [:refusal :kind] :intervention :before-reader :expected :invalid-temperature}
  :wire [:r14-precision-carry :r9-selection-law [:beta {:record :precision}]]
   :kind :verified
   :test `the-carried-beta-reaches-the-selection-law
   :check check
   :record t/record})

(deftest the-carried-beta-reaches-the-selection-law
  (is (= (:sha256 t/record) (w/sha256-file (:path t/record)))
      "the pin is the record read")
  (let [o (check)
        law (get-in (w/read-record (:path t/record)) [:decision :selection-law])]
    (is (= 1 (:writer o)))
    (is (= :cascade-selection-posterior (:applied law)))
    (is (= :carry-beta (:tau-source law)))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (t/observe t/law-reader-field
                     #(assoc-in % t/law-reader-field {:absent :precision-carry-refused}))]
    (is (not (w/received? o)))))

(deftest a-different-beta-at-the-reader-fails-the-wire
  (let [o (t/observe t/law-reader-field
                     #(assoc-in % t/law-reader-field 2))]
    (is (= 2 (:reader o)))
    (is (not (w/received? o)))))

(deftest hermetically-the-advancing-beta-is-the-laws-beta
  (let [adv (t/advance-advancing)
        writer (:beta adv)
        d (t/law-decision writer)
        o {:writer writer :reader (get-in d [:selection-law :beta])}]
    (is (= :updated (:status adv)) "the advancing branch ran cascade-beta-update")
    (is (number? writer))
    (is (= {:value writer :status :declared} (:beta d))
        "the decision's certificate records the same beta")
    (is (w/received? o))
    ;; a typed absence at :beta at the law's door: no default temperature
    ;; is substituted; the law propagates :invalid-temperature
    (is (= :invalid-temperature
           (try (t/law-decision {:absent :precision-carry-refused})
                (catch clojure.lang.ExceptionInfo e
                  (get-in (ex-data e) [:refusal :kind])))))
    ;; a different beta at the law's door is the beta the law records
    (let [o' {:writer writer :reader (get-in (t/law-decision (* 2 writer))
                                             [:selection-law :beta])}]
      (is (some? (:reader o')))
      (is (not (w/received? o'))))))
