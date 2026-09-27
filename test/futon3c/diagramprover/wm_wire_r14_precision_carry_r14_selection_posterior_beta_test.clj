(ns futon3c.diagramprover.wm-wire-r14-precision-carry-r14-selection-posterior-beta-test
  "Wire [:r14-precision-carry :r14-selection-posterior [:beta {:record
  :precision}]]: the carried beta reaching the selection posterior.

  The writer is futon2.aif.policy-precision-carry/advance's advancing
  return. The reader is
  futon2.aif.cascade-selection/selection-posterior
  (cascade_selection.clj:54): arg 1 `{:beta ...}`, the selection
  temperature (gamma = 1/beta); its produced value is the posterior over
  candidates at that temperature. There is NO default temperature: a
  missing or non-positive beta refuses :invalid-temperature
  (cascade_selection.clj:74-75).

  This wire is VERIFIED: the pinned tick record carries both ends — the
  writer's sealed record's :beta at
  [:decision :selection-certificate :policy-precision-state :beta] and,
  produced from it, the beta the law passed to the posterior, recorded at
  [:decision :selection-law :beta] beside the posterior itself
  (:selection-law :posterior, :softmax-weights, :applied
  :cascade-selection-posterior). A hermetic corroboration drives the REAL
  advance's advancing return into the REAL selection-posterior and asserts
  the posterior's numbers move with the temperature."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temperature-support :as t]))

(defn check [] (t/observe t/law-reader-field))

(def wire
  {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r14-precision-carry-r14-selection-posterior-beta-test/hermetically-the-posterior-moves-with-the-carried-temperature :kind :value-varying
                  :product [:a] :intervention :before-reader}
  :wire [:r14-precision-carry :r14-selection-posterior [:beta {:record :precision}]]
   :kind :verified
   :test `the-carried-beta-reaches-the-posterior
   :check check
   :record t/record})

(deftest the-carried-beta-reaches-the-posterior
  (is (= (:sha256 t/record) (w/sha256-file (:path t/record)))
      "the pin is the record read")
  (let [o (check)
        law (get-in (w/read-record (:path t/record)) [:decision :selection-law])]
    (is (= 1 (:writer o)))
    (is (= :cascade-selection-posterior (:applied law)))
    (is (= :C1 (:candidate law)))
    (is (= [1.0] (vals (:softmax-weights law)))
        "one acting candidate: the recorded posterior at the carried beta")
    (is (map? (:posterior law)))
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

(deftest hermetically-the-posterior-moves-with-the-carried-temperature
  (let [adv (t/advance-advancing)
        writer (:beta adv)
        ;; the REAL reader at the carried temperature: masses sum to 1
        at-carried (t/posterior writer)
        at-double (t/posterior (* 2 writer))
        at-one (t/posterior 1)
        at-two (t/posterior 2)]
    (is (= :updated (:status adv)) "the advancing branch ran cascade-beta-update")
    (is (< (Math/abs ^double (- 1.0 (double (reduce + (vals at-carried))))) 1e-12))
    ;; the numbers, pinned: beta 1 and beta 2 over the support candidates
    (is (= {:a 0.7310585786300049 :b 0.2689414213699951} at-one))
    (is (= {:a 0.6224593312018546 :b 0.3775406687981454} at-two))
    (is (not= at-carried at-one)
        "the carried temperature, not a default, shaped the posterior")
    ;; a typed absence at :beta at the posterior's door: the reader's
    ;; typed outcome, never a substituted default temperature
    (is (= :invalid-temperature
           (try (t/posterior {:absent :precision-carry-refused})
                (catch clojure.lang.ExceptionInfo e
                  (get-in (ex-data e) [:refusal :kind])))))
    (is (= :invalid-temperature
           (try (t/posterior nil)
                (catch clojure.lang.ExceptionInfo e
                  (get-in (ex-data e) [:refusal :kind])))))
    ;; a different beta at the door gives different masses
    (is (not= (:a at-carried) (:a at-double)))))
