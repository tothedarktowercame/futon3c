(ns futon3c.scripts.mission-scope-view-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.scripts.mission-scope-view :as view]))

(deftest projection-filters-and-normalizes-scope-rows
  (let [hyperedges [{:hx/id "hx|mission-scope|a/identify"
                     :hx/type :mission-scope/eightfold-phase
                     :hx/props {:mission "M-a"
                                :scope/id "a/identify"
                                :scope/name "IDENTIFY"
                                :scope/binder-type "eightfold-phase"
                                :scope/parent "a/root"
                                :scope/parent-state :linked
                                :anchor/state :anchored
                                :anchor/resolve-by :verbatim-search
                                :anchor/passage "## IDENTIFY\nbody"}}
                    {:hx/id "hx|mission-scope|b/identify"
                     :hx/type :mission-scope/eightfold-phase
                     :hx/props {:mission "M-b"
                                :scope/id "b/identify"
                                :scope/binder-type "eightfold-phase"}}]
        projected (view/project-hyperedges "M-a" hyperedges)
        row (first (:scopes projected))]
    (is (= 1 (:scope_count projected)))
    (is (= [{:type "eightfold-phase" :count 1}] (:type_counts projected)))
    (is (= "a/identify" (:id row)))
    (is (= "linked" (:parent_state row)))
    (is (= "anchored" (:anchor_state row)))
    (is (= "verbatim-search" (:anchor_resolve_by row)))
    (is (= "## IDENTIFY" (:passage row)))))
