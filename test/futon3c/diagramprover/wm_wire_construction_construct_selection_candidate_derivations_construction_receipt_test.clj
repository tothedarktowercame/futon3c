(ns futon3c.diagramprover.wm-wire-construction-construct-selection-candidate-derivations-construction-receipt-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-support :as support]))
(defn observe [mutation] (support/derivation mutation))
(defn check [] (support/live-digest))
(def wire {:wire [:construction-construct :selection-candidate-derivations :construction-receipt]
           :kind :verified
           :test `the-observed-handoff :check check
           :live-records-read support/live-records-read
           :record (assoc (first support/live-records-read)
                          :writer-path support/receipt-path :reader-path support/digest-path)
           :note "Reader-produced payload digest commits the receipt nested under candidate :id. Both controls change only that receipt before real entry. Separate existing defect: :construction merges a nonexistent outer receipt and says :hand-admitted."})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o))
    (is (= (:writer o) (:recomputed o)))
    (is (= :machine-constructed (get-in o [:receipt :kind])))
    (is (nil? (:outer-receipt o)))
    (is (= :hand-admitted (get-in o [:entry :construction :kind]))
        "Existing defect: entry merges the outer receipt, but production nests it under :id")))
(deftest absence-before-reader-fails
  (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
