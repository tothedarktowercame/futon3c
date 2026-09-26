(ns futon3c.diagramprover.rewrite-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.matcher :as matcher]
            [futon3c.diagramprover.rewrite :as rewrite]
            [futon3c.diagramprover.rule :as rule]))

(defn- chain [& values]
  (reduce graph/compose
          (map #(graph/generator % [:wire] [:wire]) values)))

(deftest dpo-rewrites-a-subchain
  (let [rewrite-rule (rule/make-rule (chain :f :g) (chain :h))
        host (chain :f :g :k)
        applications (rewrite/rule-applications rewrite-rule host)
        result (:graph (first applications))]
    (is (= 1 (count applications)))
    (is (some? (matcher/find-iso result (chain :h :k))))))

(deftest non-applicable-rule-is-an-empty-relation
  (let [rewrite-rule (rule/make-rule (chain :absent) (chain :replacement))
        applications (rewrite/rule-applications rewrite-rule (chain :f :g))]
    (is (= () applications))
    (is (not (nil? applications)))))

(deftest successful-rewrites-preserve-the-host-boundary
  (let [rewrite-rule (rule/make-rule (chain :f :g) (chain :h))
        host (chain :f :g :k)
        result (:graph (first (rewrite/rule-applications rewrite-rule host)))]
    (is (= (graph/domain host) (graph/domain result)))
    (is (= (graph/codomain host) (graph/codomain result)))))

(deftest induced-match-embeds-the-rhs
  (let [rewrite-rule (rule/make-rule (chain :f :g) (chain :h))
        {:keys [graph match]}
        (first (rewrite/rule-applications rewrite-rule (chain :f :g :k)))]
    (testing "the DPO result carries a total RHS-to-result match"
      (is (= graph (:codomain match)))
      (is (= (:rhs rewrite-rule) (:domain match)))
      (is (matcher/total? match)))))
