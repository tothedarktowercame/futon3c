(ns futon3c.diagramprover.critical-pairs
  "Inclusion overlaps: a whole LHS convexly embedded in another LHS.
  Not a complete enumeration of partial hypergraph overlaps or a confluence proof."
  (:refer-clojure :exclude [descendants])
  (:require [futon3c.diagramprover.matcher :as matcher]
            [futon3c.diagramprover.rewrite :as rewrite]))

(defn applications
  "Existing matcher/DPO semantics, with an optional :guard (match -> boolean).
  Guards are checked before rewriting, including during descendant exploration."
  [rule host]
  (for [m (matcher/match-rule rule host)
        :when (if-let [guard (:guard rule)] (guard m) true)]
    (assoc (rewrite/dpo rule m) :lhs-match m)))

(defn- iso? [a b] (boolean (matcher/find-iso a b)))

(defn descendants
  "Explore all descendants modulo boundary-preserving isomorphism, bounded by
  max-states. Exhaustion is :complete; a bound is :inconclusive, never non-joining.
  Normal forms and intermediate states are retained so joinability does not
  assume termination or confluence of the supplied rules."
  [rules start max-states]
  (loop [pending [start] seen [] normals []]
    (cond
      (empty? pending) {:status :complete :states seen :normal-forms normals}
      (some #(iso? (peek pending) %) seen) (recur (pop pending) seen normals)
      (>= (count seen) max-states)
      {:status :inconclusive :states seen :normal-forms normals}
      :else
      (let [g (peek pending)
            nexts (mapv :graph (mapcat #(applications % g) rules))]
        (recur (into (pop pending) nexts) (conj seen g)
               (cond-> normals (empty? nexts) (conj g)))))))

(defn critical-pairs
  "Check both inclusion directions. Reports overlap, matches, one-step results,
  normal forms and :joining/:non-joining/:inconclusive. Two orientations of a
  root overlap may both be reported. Only complete disjoint reachable sets
  establish non-joining; max-states defaults to 100 per branch."
  ([a b] (critical-pairs a b [a b] {}))
  ([a b rules {:keys [max-states] :or {max-states 100}}]
   (vec
    (for [[outer inner] [[a b] [b a]]
          root (applications outer (:lhs outer))
          :when (matcher/surjective? (:lhs-match root))
          sub (applications inner (:lhs outer))
          :let [left (descendants rules (:graph root) max-states)
                right (descendants rules (:graph sub) max-states)
                common (some (fn [g] (when (some #(iso? g %) (:states right)) g))
                             (:states left))]]
      {:rules [(:name outer) (:name inner)]
       :overlap (:lhs outer)
       :matches [(:lhs-match root) (:lhs-match sub)]
       :results [(:graph root) (:graph sub)]
       :normalisations [left right]
       :join common
       :status (cond common :joining
                     (every? #(= :complete (:status %)) [left right]) :non-joining
                     :else :inconclusive)}))))
