(ns futon3c.wm.promote-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.promote :as promote]))

(def bundle
  {:judgement {:ranked-actions
               [{:rank 1 :G-total -5.0 :action {:type :address-sorry :target "sorry/x"}}
                {:rank 2 :G-total -4.0 :action {:type :advance-mission :target "M-first-flights"}}
                {:rank 3 :G-total -3.0 :action {:type :advance-mission :target "M-y"}}]}})

(deftest promote-reorders-visibly-without-faking-G
  (let [out (promote/apply-operator-promote
             bundle {:sentinel {:targets ["M-first-flights"] :until "2099-01-01T00:00:00Z"
                                :reason "test" :by "joe"}})
        ras (get-in out [:judgement :ranked-actions])]
    (is (= "M-first-flights" (get-in (first ras) [:action :target])) "promoted to top")
    (is (true? (:operator-promoted (first ras))) "visibly tagged")
    (is (= -4.0 (:G-total (first ras))) "honest G preserved — reordered, not rescored")
    (is (= [1 2 3] (mapv :rank ras)) "reranked contiguously")
    (is (= 1 (get-in out [:judgement :operator-promote :hits])))))

(deftest no-sentinel-is-identity
  (is (= bundle (promote/apply-operator-promote bundle {:sentinel nil}))))
