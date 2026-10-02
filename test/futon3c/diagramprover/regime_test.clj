(ns futon3c.diagramprover.regime-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.regime :as regime]))

(def sig {:f {:in [:a] :out [:a]} :copy {:in [:a] :out [:a :a]}
          :new {:in [] :out [:a] :identity :creates}})

(defn- sort-of [vt] (if (vector? vt) (first vt) vt))
(defn- owner-of [vt] (when (vector? vt) (second vt)))

(defn- chain
  "x --f--> y, as an open diagram over sort [:a o]."
  [o]
  (let [g (graph/make-graph)
        [g x] (graph/add-vertex g {:vtype [:a o]})
        [g y] (graph/add-vertex g {:vtype [:a o]})
        [g _] (graph/add-edge g [x] [y] {:value :f})]
    (-> g (graph/set-inputs [x]) (graph/set-outputs [y]))))

(deftest clean-chain
  (let [laws {:signature sig :regime {} :sort-of sort-of :owner-of owner-of}]
    (is (regime/clean? (regime/check (chain "p") laws)))))

(deftest copy-is-duplication-under-linear-and-lawful-under-cartesian
  (let [g (graph/make-graph)
        [g x] (graph/add-vertex g {:vtype :d})
        [g y] (graph/add-vertex g {:vtype :d})
        [g z] (graph/add-vertex g {:vtype :d})
        [g _] (graph/add-edge g [x] [y] {:value :use})
        [g _] (graph/add-edge g [x] [z] {:value :use})
        g (-> g (graph/set-inputs [x]) (graph/set-outputs [y z]))]
    (is (= [:duplicated] (map :finding (regime/linearity-findings g {} sort-of))))
    (is (= [] (regime/linearity-findings g {:d :cartesian} sort-of)))))

(deftest unconsumed-and-unproduced-linear-wires
  (let [g (graph/make-graph)
        [g x] (graph/add-vertex g {:vtype :d})
        [g y] (graph/add-vertex g {:vtype :d})]
    ;; x is produced (input) but never consumed; y is neither.
    (is (= [:discarded :discarded]
           (map :finding (regime/linearity-findings (graph/set-inputs g [x]) {} sort-of))))
    (is (= [:conjured] (map :finding (regime/linearity-findings
                                      (-> g (graph/set-outputs [y]) (graph/remove-vertex x))
                                      {} sort-of))))))

(deftest identity-and-concurrency
  (let [g (chain "p")
        [g y2] (graph/add-vertex g {:vtype [:a "p"]})
        [g _] (graph/add-edge g [(first (:inputs g))] [y2] {:value :f})]
    (is (= [{:finding :concurrent-identity :owner "p" :concurrent-pairs 1}]
           (regime/concurrency-findings g owner-of))))
  (let [g (graph/make-graph)
        [g x] (graph/add-vertex g {:vtype [:a "p"]})
        [g y] (graph/add-vertex g {:vtype [:a "q"]})
        [g _] (graph/add-edge g [x] [y] {:value :f})]
    (is (= [:identity-not-preserved]
           (map :finding (regime/identity-findings g sig owner-of))))))

(deftest unknown-and-ill-typed-generators
  (let [g (graph/make-graph)
        [g x] (graph/add-vertex g {:vtype :a})
        [g y] (graph/add-vertex g {:vtype :b})
        [g _] (graph/add-edge g [x] [y] {:value :f})
        [g _] (graph/add-edge g [y] [] {:value :mystery})]
    (is (= [:ill-typed :unknown-generator]
           (map :finding (regime/signature-findings g sig sort-of))))))
