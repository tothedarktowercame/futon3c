(ns futon3c.apm.cascade-dry-run-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.cascade-dry-run :as dry-run]))

(deftest snapshot-memory-ids-preserve-shelf-order
  (is (= ["m2" "m1"]
         (dry-run/snapshot-memory-ids
          {:snapshot/memories [{:memory-id "m2"} {:memory-id "m1"}]}))))

(deftest cascade-summary-measures-expanded-routes
  (let [summary
        (dry-run/cascade-summary
         {:expanded-available 4 :expanded-count 3 :truncated? true
          :seed-patterns ["seed/a" "seed/b"]
          :routes [["leaf" {:route :leaf :hops 0}]
                   ["why-1" {:route :why-hop :hops 1
                             :pattern "math-strategy/missing-dependency-protocol"}]
                   ["why-2" {:route :why-hop :hops 2 :pattern "pattern/other"}]
                   ["co" {:route :co-incidence :hops 2 :pattern "pattern/co"}]]
          :pattern-surfaces {"pattern/other" {:body "content"} "pattern/co" {}}})]
    (is (= {:why-hop 2 :co-incidence 1} (:route-histogram summary)))
    (is (= {1 1, 2 2} (:hop-distribution summary)))
    (is (= {[:why-hop 1] 1 [:why-hop 2] 1 [:co-incidence 2] 1}
           (:route-hop-distribution summary)))
    (is (= 1 (:missing-dependency-why-count summary)))
    (is (= 2 (:distinct-why-reachable-patterns summary)))
    (is (= 2 (:pattern-surface-count summary)))
    (is (true? (:nil-or-empty-pattern-surface? summary)))))
