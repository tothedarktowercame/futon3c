(ns futon3c.diagramprover.causal.idalg-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.idalg :as idalg]))

(defn- free-variables
  "Variables an estimand mentions that no enclosing sum binds."
  [expr]
  (case (:op expr)
    :constant #{}
    :cond (set/union #{(:variable expr)} (set (:given expr))
                     (if-let [source (:source expr)] (free-variables source) #{}))
    :product (apply set/union #{} (map free-variables (:terms expr)))
    :sum (set/difference (free-variables (:expr expr)) (set (:variables expr)))))

;; Line 2 sums out W and M (not ancestors of Y); line 7 then factors the
;; district {Z,Y}.  Built from the original joint, that factor was
;; P(Y | Z,W,X,M): conditioned on W and M after they were summed out, and
;; W <-> Y makes that a different number.  The right answer is the
;; back-door formula sum_z P(z) P(y | z,x).
(deftest line-7-factors-the-current-distribution
  (let [g {:nodes #{:Z :X :W :Y :M}
           :directed #{[:Z :X] [:X :Y] [:Z :W] [:X :M] [:W :M]}
           :bidirected #{#{:Z :M} #{:Z :Y} #{:W :Y}}}
        result (idalg/identify-effect g :X :Y)]
    (is (:identifiable? result))
    (is (= #{:X :Y} (free-variables (:estimand result)))
        (idalg/formula (:estimand result)))))
