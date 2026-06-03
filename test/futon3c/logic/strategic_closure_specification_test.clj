(ns futon3c.logic.strategic-closure-specification-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.strategic-closure-specification :as scs]))

(def ^:private well-specified
  {:id :sorry/strat-good
   :title "Strategic: pick the next campaign keystone"
   :status :open
   :kind :strategic
   :target :M-substrate-metric
   :action "Open M-substrate-metric and fix O1 node identity before defining d."})

(def ^:private under-specified
  {:id :sorry/strat-bad
   :title "Strategic direction TBD"
   :status :open
   :kind :strategic})            ; no :target, no action

(def ^:private resolved-strategic
  {:id :sorry/strat-done
   :title "Strategic thing, already handled"
   :status :addressed
   :kind :strategic
   :resolved-at "2026-05-30"})   ; not a live recommendation → ignored

(def ^:private non-strategic
  {:id :sorry/plain
   :title "Fix a typo"
   :status :open
   :kind :prototyping-forward})  ; not strategic → ignored

(deftest strategic-identification
  (testing "heuristic recognises kind/vocabulary/title signals"
    (is (scs/strategic-sorry? {:kind :strategic}))
    (is (scs/strategic-sorry? {:strategic-vocabulary {:μ/modes []}}))
    (is (scs/strategic-sorry? {:title "this is Strategic work"}))
    (is (not (scs/strategic-sorry? {:title "fix a typo" :kind :prototyping-forward})))))

(deftest live-recommendation-filter
  (is (scs/live-recommendation? {:status :open}))
  (is (scs/live-recommendation? {:status :candidate}))
  (is (not (scs/live-recommendation? {:status :addressed})))
  (is (not (scs/live-recommendation? {:status :closed}))))

(deftest v1-spec-status-shape
  (let [good (scs/v1-spec-status well-specified)
        bad  (scs/v1-spec-status under-specified)]
    (is (:checkable-complete? good))
    (is (not (:checkable-complete? bad)))
    (testing "schema-gap is always reported (honest partial binding)"
      (is (seq (:schema-gap good)))
      (is (= (:schema-gap good) (:schema-gap bad))))))

(deftest under-specified-strategic-selection
  (let [reg {:sorrys [well-specified under-specified resolved-strategic non-strategic]}
        under (scs/under-specified-strategic reg)]
    (testing "flags only the under-specified, live, strategic sorry"
      (is (= [:sorry/strat-bad] (mapv :id under))))
    (testing "well-specified, resolved, and non-strategic are not flagged"
      (is (not (some #{:sorry/strat-good :sorry/strat-done :sorry/plain}
                     (map :id under)))))))

(deftest check-outcomes
  (testing ":violation when an under-specified strategic recommendation exists"
    (let [r (scs/check {:sorrys [well-specified under-specified]})]
      (is (= :violation (:outcome r)))
      (is (= [:sorry/strat-bad] (mapv :id (get-in r [:detail :under-specified]))))
      (is (seq (get-in r [:detail :schema-gap])))))
  (testing ":ok when all strategic live recommendations have checkable v1 spec"
    (let [r (scs/check {:sorrys [well-specified resolved-strategic non-strategic]})]
      (is (= :ok (:outcome r)))
      (is (seq (get-in r [:detail :schema-gap])))))
  (testing ":ok with note when no registry"
    (is (= :ok (:outcome (scs/check nil))))))
