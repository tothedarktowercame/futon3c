(ns futon3c.metric.e1-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.metric.e1 :as sut]))

(def ^:private mission-a "futon3c-d/mission/a")
(def ^:private mission-b "futon3c-d/mission/b")
(def ^:private mission-c "futon3c-d/mission/c")
(def ^:private sorry-a "futon2/sorry/a")
(def ^:private file-a "futon3c-d/file/src/a.clj")

(def ^:private edges
  [{:hx/type "code/v05/mission-cross-ref"
    :hx/endpoints [mission-a mission-b]}
   {:hx/type "code/v05/related-mission"
    :hx/endpoints [mission-b mission-c]}
   {:hx/type "code/v05/file->mission"
    :hx/endpoints [file-a mission-b]}
   {:hx/type "code/v05/sorry->related-missions"
    :hx/endpoints [sorry-a mission-a]}
   {:hx/type "code/v05/mission-cross-ref"
    :hx/endpoints [mission-a mission-b]}
   {:hx/type "code/v05/irrelevant"
    :hx/endpoints [mission-a :outside]}])

(defn- close?
  [a b]
  (< (abs (- (double a) (double b))) 1.0e-9))

(deftest builds-feeds-mu-multigraph
  (let [graph (sut/build-undirected-graph edges)]
    (is (= #{mission-b sorry-a}
           (set (get graph mission-a))))
    (is (= 2 (count (filter #{mission-b} (get graph mission-a)))))
    (is (= #{mission-a mission-c file-a}
           (set (get graph mission-b))))
    (is (nil? (get graph :outside)))))

(deftest lazy-random-walk-measure-preserves-multiedge-mass
  (let [graph (sut/build-undirected-graph edges)
        mu (sut/lazy-random-walk-measure graph mission-a)]
    (is (close? 1.0 (reduce + (vals mu))))
    (is (close? 0.5 (get mu mission-a)))
    (is (close? (/ 1.0 3.0) (get mu mission-b)))
    (is (close? (/ 1.0 6.0) (get mu sorry-a)))
    (is (= {:isolated 1.0}
           (sut/lazy-random-walk-measure graph :isolated)))))

(deftest hop-distance-covers-connected-and-disconnected-cases
  (let [graph (sut/build-undirected-graph edges)]
    (is (= 0 (sut/hop-distance graph mission-a mission-a)))
    (is (= 1 (sut/hop-distance graph mission-a mission-b)))
    (is (= 2 (sut/hop-distance graph sorry-a mission-b)))
    (is (= 3 (sut/hop-distance graph sorry-a mission-c)))
    (is (nil? (sut/hop-distance graph mission-a :elsewhere)))))

(deftest strain-rollup-exposes-node-level-e1-fields
  (let [rollup (sut/strain-rollup [{:edge :e1 :kappa 0.2}
                                   {:edge :bridge :kappa -0.7}
                                   {:edge :e2 :kappa 0.1}])]
    (is (= -0.7 (:curvature/min-incident-kappa rollup)))
    (is (= :bridge (:curvature/strain-edge rollup)))
    (is (:curvature/strain? rollup))
    (is (close? (/ -0.4 3.0) (:curvature/mean-incident-kappa rollup))))
  (testing "isolated node"
    (is (= {:curvature/min-incident-kappa nil
            :curvature/mean-incident-kappa nil
            :curvature/strain-edge nil
            :curvature/strain? false}
           (sut/strain-rollup [])))))

(deftest o3-proposal-composition-requires-strain-unresolvedness-and-actionability
  (let [base {:curvature/strain? true
              :curvature/min-incident-kappa -0.8
              :resolution-state/resolvedness 0.25
              :resolution-state/actionable? true}]
    (is (sut/propose-here? base))
    (is (close? 0.6 (sut/action-intensity base)))
    (is (not (sut/propose-here? (assoc base :resolution-state/resolvedness 1.0))))
    (is (not (sut/propose-here? (assoc base :resolution-state/resolvedness :unknown))))
    (is (not (sut/propose-here? (assoc base :curvature/strain? false))))
    (is (not (sut/propose-here? (assoc base :resolution-state/actionable? false))))
    (is (not (sut/propose-here? (assoc base
                                        :resolution-state/actionable? false
                                        :actionable? true))))))
