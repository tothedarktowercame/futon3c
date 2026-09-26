(ns futon3c.diagramprover.matcher-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.matcher :as matcher]))

(defn- disjoint-generators [values]
  (let [[g inputs outputs]
        (reduce
         (fn [[g inputs outputs] value]
           (let [[g in] (graph/add-vertex g {:vtype :wire})
                 [g out] (graph/add-vertex g {:vtype :wire})
                 [g _] (graph/add-edge g [in] [out] {:value value})]
             [g (conj inputs in) (conj outputs out)]))
         [(graph/make-graph) [] []] values)]
    (-> g (graph/set-inputs inputs) (graph/set-outputs outputs))))

(deftest matching-is-complete
  (testing "one f has two embeddings in two disconnected copies"
    (is (= 2 (count (matcher/matches
                     (disjoint-generators [:f])
                     (disjoint-generators [:f :f]))))))
  (testing "two symmetric f components in three have P(3,2)=6 embeddings"
    (is (= 6 (count (matcher/matches
                     (disjoint-generators [:f :f])
                     (disjoint-generators [:f :f :f])))))))

(defn- chain-with-return-path [return-path?]
  (let [[g a] (graph/add-vertex (graph/make-graph) {:vtype :wire})
        [g b] (graph/add-vertex g {:vtype :wire})
        [g c] (graph/add-vertex g {:vtype :wire})
        [g _] (graph/add-edge g [a] [b] {:value :f})
        [g _] (graph/add-edge g [b] [c] {:value :g})
        g (-> g (graph/set-inputs [a]) (graph/set-outputs [c]))]
    (if return-path?
      (let [[g x] (graph/add-vertex g {:vtype :wire})
            [g _] (graph/add-edge g [c] [x] {:value :outside-1})
            [g _] (graph/add-edge g [x] [a] {:value :outside-2})]
        g)
      g)))

(deftest non-convex-matches-are-rejected
  (let [pattern (chain-with-return-path false)
        host (chain-with-return-path true)]
    (is (= 1 (count (matcher/matches pattern host {:convex? false}))))
    (is (empty? (matcher/matches pattern host)))))

(deftest graph-isomorphism-honours-ordered-boundary
  (let [graph (graph/generator :f [:wire] [:wire])]
    (is (some? (matcher/find-iso graph graph)))
    (is (nil? (matcher/find-iso graph
                                (graph/generator :g [:wire] [:wire]))))))
