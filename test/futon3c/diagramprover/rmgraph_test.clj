(ns futon3c.diagramprover.rmgraph-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.rmgraph :as rm]))

(defn- shared-graph [source-order]
  (let [[graph p] (rm/add-vertex (rm/make-graph) {:value :p})
        [graph q] (rm/add-vertex graph {:value :q})
        [graph r] (rm/add-vertex graph {:value :r})
        ids {:p p :q q}
        graph (first (rm/add-edge graph (mapv ids source-order) [r]
                                  {:value :consumer}))]
    graph))

(deftest unordered-tentacles-canonicalize-equally
  (let [left (shared-graph [:p :q])
        right (shared-graph [:q :p])]
    (is (rm/rm-valid? left))
    (is (= (rm/canonicalize left) (rm/canonicalize right)))
    (is (= (rm/canonicalize left) (rm/canonicalize left)))))

(defn- tied-source-graph [source-shape]
  (let [[graph first-wire] (rm/add-vertex (rm/make-graph) {:value :w})
        [graph second-wire] (rm/add-vertex graph {:value :w})
        [graph sink] (rm/add-vertex graph {:value :sink})
        wires {:first first-wire :second second-wire}]
    (first (rm/add-edge graph (mapv wires source-shape) [sink] {:value :f}))))

(deftest tied-payload-non-isomorphism-is-distinguished
  ;; Forces the exhaustive tied-partition fallback: multiplicity is invariant.
  (let [doubled (tied-source-graph [:first :first])
        shared (tied-source-graph [:first :second])]
    (is (not= (rm/canonicalize doubled) (rm/canonicalize shared)))))

(defn- tied-iso-graph [swap-wires?]
  (let [[graph first-wire] (rm/add-vertex (rm/make-graph) {:value :w})
        [graph second-wire] (rm/add-vertex graph {:value :w})
        [graph left-sink] (rm/add-vertex graph {:value :left-sink})
        [graph right-sink] (rm/add-vertex graph {:value :right-sink})
        [left-wire right-wire] (if swap-wires?
                                 [second-wire first-wire]
                                 [first-wire second-wire])
        uses [[left-wire left-sink :left-use]
              [right-wire right-sink :right-use]]
        uses (if swap-wires? (reverse uses) uses)]
    (reduce (fn [result [wire sink value]]
              (first (rm/add-edge result [wire] [sink] {:value value})))
            graph uses)))

(deftest tied-payload-isomorphism-is-identified
  ;; Both vertex permutation and edge insertion order differ.
  (let [left (tied-iso-graph false)
        right (tied-iso-graph true)]
    (is (not= left right))
    (is (= (rm/canonicalize left) (rm/canonicalize right)))))

(deftest sharing-and-discard-are-legal
  (let [[graph producer] (rm/add-vertex (rm/make-graph) {:value :producer})
        [graph left] (rm/add-vertex graph {:value :left})
        [graph right] (rm/add-vertex graph {:value :right})
        graph (first (rm/add-edge graph [producer] [left] {:value :left-use}))
        graph (first (rm/add-edge graph [producer] [right] {:value :right-use}))]
    (is (rm/rm-valid? graph))
    (is (= 2 (count (rm/out-edges graph producer))))
    (is (empty? (rm/out-edges graph left)))))

(deftest rejects-left-amonogamy
  (let [[graph node] (rm/add-vertex (rm/make-graph) {:value :node})
        graph (first (rm/add-edge graph [] [node] {:value :first-producer}))
        graph (first (rm/add-edge graph [] [node] {:value :second-producer}))]
    (is (false? (rm/rm-valid? graph)))))
(deftest rejects-cycles
  (let [[graph a] (rm/add-vertex (rm/make-graph) {:value :a})
        [graph b] (rm/add-vertex graph {:value :b})
        graph (first (rm/add-edge graph [a] [b] {:value :ab}))
        graph (first (rm/add-edge graph [b] [a] {:value :ba}))]
    (is (false? (rm/rm-valid? graph)))))
