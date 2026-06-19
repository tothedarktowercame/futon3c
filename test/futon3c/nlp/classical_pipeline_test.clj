(ns futon3c.nlp.classical-pipeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.nlp.classical-pipeline :as nlp]))

(deftest explicit-token-resolves
  (let [r (nlp/resolve-turn nlp/fixture-profiles
                            {:text "please clock this to M-foo"})]
    (is (= :resolved (:kind r)))
    (is (= {:campaign-id nil :mission-id "M-foo" :excursion-id nil}
           (:target r)))
    (is (= "explicit" (get-in r [:witness :rule])))))

(deftest term-similarity-proposes-only
  (let [r (nlp/resolve-turn nlp/fixture-profiles
                            {:text "the alpha beta gamma design needs the resolver"})]
    (is (= :proposal (:kind r)))
    (is (= {:campaign-id nil :mission-id "M-alpha-beta-gamma" :excursion-id nil}
           (:target r)))
    (is (= "term-similarity" (get-in r [:witness :rule])))
    (is (true? (get-in r [:witness :proposal-only])))))

(deftest off-topic-turn-proposes-nothing
  (is (nil? (nlp/resolve-turn nlp/fixture-profiles
                              {:text "make tea after lunch"}))))

(deftest affect-before-after-fixture
  (let [rows (mapv nlp/affect-row (take 5 nlp/fixture-turns))
        by-id (into {} (map (juxt :id identity) rows))]
    (testing "documented failures fixed"
      (is (= "-" (get-in by-id ["fixture-please" :after])))
      (is (= "-" (get-in by-id ["fixture-interesting" :after])))
      (is (= "-" (get-in by-id ["fixture-not-happy" :after]))))
    (testing "true joy and inspiration fire"
      (is (= "joy" (get-in by-id ["fixture-joy" :after])))
      (is (= "inspiration" (get-in by-id ["fixture-inspiration" :event-type]))))))
