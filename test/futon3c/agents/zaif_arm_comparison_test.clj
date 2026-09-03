(ns futon3c.agents.zaif-arm-comparison-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zaif-actand :as q-actand]
            [futon3c.agents.zaif-arm-comparison :as comparison]))

(def calibration-path
  "../futon2/holes/labs/M-zaif-harness/calibration-sessions.edn")
(def fixture-dir
  "../futon2/holes/labs/wm-contract/runs/U12-c-mis-falsifier/node-fixtures")

(defn- corpora []
  {:calibration-sessions (q-actand/read-calibration-sessions calibration-path)
   :mission-fixtures (->> (file-seq (io/file fixture-dir))
                          (filter #(re-find #"\.edn$" (.getName %)))
                          (sort-by #(.getName %))
                          (mapv #(q-actand/read-calibration-sessions
                                  (.getPath %))))})

(deftest whole-comparison-is-deterministic
  (let [inputs (corpora)]
    (is (= (comparison/comparison-report inputs)
           (comparison/comparison-report inputs)))))

(deftest every-ordering-probe-is-explicit
  (is (= {:A :pass
          :B :pass
          :C :not-applicable-typed-refusal
          :calibration-table-a4 :pass}
         (into {} (map (fn [[column probe]] [column (:status probe)])
                       comparison/ordering-probes)))))

(deftest digest-stability-is-counted
  (let [cells (:cells (comparison/comparison-report (corpora)))]
    (is (= 0 (reduce + (for [[_ corpora] cells
                             [_ cell] corpora]
                         (get-in cell [:digest-stability :violations])))))
    (is (= (* 4 (+ 114 39))
           (reduce + (for [[_ corpora] cells
                           [_ cell] corpora]
                       (get-in cell [:digest-stability :comparisons])))))))

(deftest real-calibration-pin-has-verbatim-comparison-outcome
  (let [report (comparison/comparison-report (corpora))
        pin (:live-pin report)]
    ;; Live calibration pin: e-0cae94f2-9ca8-4863-9251-44278445a5f7.
    (is (= "e-0cae94f2-9ca8-4863-9251-44278445a5f7" (:record-id pin)))
    (is (= :calibration-table-a4 (:column pin)))
    (is (= 0.2513144282809061 (get-in pin [:result :act-value])))
    (is (= q-actand/query-id (get-in pin [:result :provenance :query])))
    (is (some #{"e-0cae94f2-9ca8-4863-9251-44278445a5f7"}
              (get-in pin [:result :provenance :record-ids])))))

(deftest headline-and-refusal-counts-are-measured
  (let [report (comparison/comparison-report (corpora))]
    (is (= {:calibration-sessions 114 :mission-node-fixtures 39}
           (:corpus-counts report)))
    (is (= {:A {:calibration-sessions 0 :mission-node-fixtures 0}
            :B {:calibration-sessions 0 :mission-node-fixtures 0}
            :C {:calibration-sessions 0 :mission-node-fixtures 0}
            :calibration-table-a4 {:calibration-sessions 3
                                   :mission-node-fixtures 0}}
           (into {}
                 (for [[column corpora] (:cells report)]
                   [column (into {} (for [[corpus cell] corpora]
                                      [corpus (:distinct-values cell)]))]))))
    (is (= 114 (get-in report [:cells :A :calibration-sessions
                               :refusals :q-actand/missing-input])))
    (is (= 39 (get-in report [:cells :B :mission-node-fixtures
                              :refusals :q-actand/missing-input])))
    (is (= 114 (get-in report [:cells :C :calibration-sessions
                               :refusals :q-actand/no-typed-source])))))
