(ns futon3c.arxana.design-catalogue-facts-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.design-catalogue :as d]
            [futon3c.arxana.design-catalogue-facts :as f]))

(defn rule [id qualname] {:id id :type-revision [:rule/content 1]
                          :asserted-content {:rule/pattern [:whole-sequence]
                                             :rule/rewrite {:python/qualname qualname}}})
(defn sup [successor predecessor]
  {:id (str "sup-" predecessor) :type-revision [:supersedes 1]
   :endpoints [{:role :successor :kind :clusion :target {:clusion/id successor}}
               {:role :predecessor :kind :clusion :target {:clusion/id predecessor}}]})

(deftest superseded-version-may-share-a-function
  (let [raw (rule "r-q56" "m.paper_walk._merge_right_or_splits")
        q83 (rule "r-q83" "m.paper_walk._merge_right_or_splits")]
    ;; Without the supersession the two versions are a duplicate (the live case before the fix).
    (is (= :violation (:result (d/rule-function-check (f/current-function-rules [raw q83] [])))))
    (is (= :pass (:result (d/rule-function-check (f/current-function-rules [raw q83] [(sup "r-q83" "r-q56")])))))
    ;; The supersession decides which version is current.
    (is (= ["r-q56"] (map :id (f/current-function-rules [raw q83] [(sup "r-q56" "r-q83")]))))
    ;; Two unsuperseded records for one function remain a violation.
    (is (= :violation (:result (d/rule-function-check
                                (f/current-function-rules [raw q83 (rule "r-x" "m.paper_walk._merge_right_or_splits")]
                                                          [(sup "r-q83" "r-q56")])))))))
