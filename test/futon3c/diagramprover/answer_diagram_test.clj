(ns futon3c.diagramprover.answer-diagram-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.answer-diagram :as answer]
            [futon3c.diagramprover.rmgraph :as rm]))

(defn hole [id status]
  {:id id :role id :question (str "What is " (name id) "?") :required true
   :status status :candidates [{:text (str "candidate " (name id)) :score 0.8}]
   :binding (when (= :own status) {:span [0 4] :text "ours"})
   :offered (when (= :offered status) {:party "reader" :deadline nil})})

(def thesis
  {:slot "public-benefit" :text "A public project benefits people."
   :question-parts [{:id :benefit :label "benefit"}]
   :sentences [{:id :s1 :frames [{:id :f1 :predicate {:lemma "benefit" :span [17 25]}
                                  :holes [(hole :project :free) (hole :benefit :free)
                                          (hole :people :free) (hole :mechanism :free)]}]}]
   :fills [{:part :benefit :frames [:f1]}]})

(def revision
  (assoc thesis :sentences
         [{:id :s1 :frames [{:id :f1 :predicate {:lemma "benefit" :span [17 25]}
                             :holes [(hole :project :own) (hole :benefit :own)
                                     (hole :people :own) (hole :mechanism :offered)]}]}]))

(deftest public-benefit-diagrams-are-valid
  (let [a (answer/answer->diagram thesis)
        b (answer/answer->diagram revision)]
    (is (rm/rm-valid? (:graph a)))
    (is (rm/rm-valid? (:graph b)))
    (is (= 4 (count (:inputs (:graph a)))))
    (is (= 1 (count (:inputs (:graph b)))))
    (is (= 4 (count (filter #(= :read-never-written (:finding %))
                            (answer/findings thesis)))))
    (is (re-find #"project:OPEN" (answer/render thesis)))
    (is (re-find #"mechanism:OFFERED" (answer/render revision)))))

(deftest check-names-gaps-and-extras
  (let [unfilled (assoc thesis :fills [])
        kinds (set (map :finding (answer/findings unfilled)))]
    (is (contains? kinds :part-never-reached))
    (is (contains? kinds :box-reaches-no-part))))
