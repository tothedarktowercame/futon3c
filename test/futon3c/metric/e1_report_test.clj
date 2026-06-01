(ns futon3c.metric.e1-report-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.metric.e1-report :as sut]))

(def ^:private m-a "futon3c-d/mission/a")
(def ^:private m-b "futon3c-d/mission/b")
(def ^:private m-c "futon3c-d/mission/c")
(def ^:private m-d "futon3c-d/mission/d")
(def ^:private sorry-a "futon2/sorry/a")
(def ^:private file-a "futon3c-d/file/src/a.clj")

(def ^:private fixture-by-type
  {"code/v05/mission-cross-ref"
   [{:hx/type "code/v05/mission-cross-ref" :hx/endpoints [m-a m-b]}
    {:hx/type "code/v05/mission-cross-ref" :hx/endpoints [m-b m-c]}]
   "code/v05/related-mission"
   [{:hx/type "code/v05/related-mission" :hx/endpoints [sorry-a m-a]}
    {:hx/type "code/v05/related-mission" :hx/endpoints [m-c m-d]}
    {:hx/type "code/v05/related-mission" :hx/endpoints [m-c m-d]}]
   "code/v05/file->mission"
   [{:hx/type "code/v05/file->mission" :hx/endpoints [file-a m-b]}]})

(defn- fixture-fetcher
  [hx-type _opts]
  (vec (get fixture-by-type hx-type [])))

(deftest report-summarizes-bounded-e1-graph
  (let [r (sut/report {:fetcher fixture-fetcher
                       :relation-types (keys fixture-by-type)
                       :top-bridges 10})]
    (is (= :substrate-metric/e1-r1 (:report r)))
    (is (= 6 (:feeds-mu-edge-count r)))
    (is (= 6 (:node-count r)))
    (is (= {:mission 4 :sorry 1 :file 1}
           (:node-count-by-type r)))
    (is (= 1 (:component-count r)))
    (is (= [6] (map :size (:largest-components r))))
    (is (= 4 (:bridge-candidate-count r)))
    (is (not-any? #(= #{m-c m-d} (set (:edge %)))
                  (:candidate-bridges r)))
    (is (= [8 8 5 5] (map :bridge-score (:candidate-bridges r))))))

(deftest node-type-classification-covers-e1-grains
  (is (= :mission (sut/node-type "futon3c-d/mission/substrate-metric")))
  (is (= :sorry (sut/node-type "futon2/sorry/example")))
  (is (= :pattern (sut/node-type "futon3c-d/pattern/example")))
  (is (= :file (sut/node-type "futon3c-d/file/src/example.clj")))
  (is (= :unknown (sut/node-type "x"))))
