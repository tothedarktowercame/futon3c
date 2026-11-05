(ns futon3c.agency.promise-history-compare-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.promise-history-compare :as compare]))

(defn- row [n predecessor payload at]
  {:evidence/id (str "promise-history:" n)
   :evidence/type (if (= n 1) :promise/park-made :promise/dependency-terminated)
   :evidence/author "agent"
   :evidence/at at
   :evidence/body (cond-> {:history/format 3
                           :history/payload-edn payload
                           :history/promise-id "park:compare"
                           :history/promise-sequence n}
                    predecessor (assoc :history/predecessor predecessor))})

(def row-1 (row 1 nil "{:record {:id \"park:compare\"}}" "2026-11-05T17:11:00Z"))
(def row-2 (row 2 {:sequence 1 :id "promise-history:1" :type :promise/park-made}
                "{:record {:id \"park:compare\"} :step 2}" "2026-11-05T17:12:00Z"))
(def row-3 (row 3 {:sequence 2 :id "promise-history:2"
                   :type :promise/dependency-terminated}
                "{:record {:id \"park:compare\"} :step 3}" "2026-11-05T17:13:00Z"))

(deftest equal-populations-have-empty-difference
  (is (= {:only-evidence [] :only-log [] :mismatched []
          :pre-dual-write-evidence-by-event-time {}
          :pre-dual-write-evidence-count 0
          :chain-check-evidence [] :chain-check-log []}
         (compare/compare-history [row-1 row-2 row-3] [row-1 row-2 row-3]))))

(deftest payload-mismatch-and-missing-middle-are-both-reported
  (let [changed-row-3 (assoc-in row-3 [:evidence/body :history/payload-edn]
                                "{:record :different}")
        result (compare/compare-history [row-1 row-2 row-3]
                                        [row-1 changed-row-3])]
    (is (= ["promise-history:2"] (:only-evidence result)))
    (is (= ["promise-history:3"] (:mismatched result)))
    (is (empty? (:chain-check-evidence result)))
    (is (= [{:promise-id "park:compare" :reason :missing-transition
             :sequence 2 :predecessor-id "promise-history:2"
             :predecessor-type :promise/dependency-terminated}]
           (:chain-check-log result)))))

(deftest pre-dual-write-only-evidence-is-counted-by-event-time
  (let [legacy (row 1 nil "legacy" "2026-11-05T17:09:00Z")
        result (compare/compare-history [legacy] [])]
    (is (empty? (:only-evidence result)))
    (is (= 1 (:pre-dual-write-evidence-count result)))
    (is (= {"2026-11-05T17:09:00Z" 1}
           (:pre-dual-write-evidence-by-event-time result)))))
