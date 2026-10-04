(ns futon3c.diagramprover.skeleton.xiang2000-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.causal.open-theory :as ot]
            [futon3c.diagramprover.skeleton.xiang2000 :as x]))

(def r (delay (x/receipts)))

(deftest derivation-of-the-commit-matches-the-hand-reconstruction
  ;; M-象-2000 acceptance table: 15:48 report, 16:03 constrain, 16:20/16:28
  ;; propose, then 16:34 commit 5146606d.
  (is (= #{:limit-report :constraint-act :enforcement-proposal}
         (get-in @r [:derivation :requisition-commit]))))

(deftest red-tape-ancestry-excludes-the-capture-default
  (let [cone (get-in @r [:derivation :red-tape])]
    (is (contains? cone :requisition-commit))
    (is (contains? cone :guard-in-force))
    (is (not (contains? cone :capture-default-off)))))

(deftest capture-drop-is-held-apart-from-the-rule
  (is (true? (get-in @r [:capture-drop-held-apart :d-separated-from-commit?])))
  (is (= {:method :deterministic-scm :answer true}
         (get-in @r [:capture-drop-held-apart :had-no-commit-capture-still-drops]))))

(deftest prevention-counterfactual-is-computed-not-asserted
  (is (= {:method :deterministic-scm :answer false}
         (get-in @r [:prevention :answer]))))

(deftest withdrawal-semantics-depend-on-what-enforcement-reads
  (testing "as built, withdrawing the rule record leaves the notices"
    (is (true? (get-in @r [:withdrawal :as-built :answer]))))
  (testing "if enforcement read the rule record, withdrawal would stop them"
    (is (false? (get-in @r [:withdrawal :rule-gated :answer])))))

(deftest the-glued-theory-makes-testable-predictions
  (is (pos? (get-in @r [:predictions :count])))
  (is (some #(= {:x :analysis-refusals :y :capture-drop :given #{}} %)
            (get-in @r [:predictions :sample]))))

(deftest patterns-that-do-not-compose-are-refused
  (testing "a second pattern claiming the commit's mechanism"
    (let [rival (ot/theory :rival {"requisition-commit" "quota-alarm"}
                           :inputs ["quota-alarm"] :interface [:requisition-commit])]
      (is (= :double-mechanism (:reason (ot/glue (conj x/as-built rival))))))))
