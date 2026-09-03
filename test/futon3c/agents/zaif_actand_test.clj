(ns futon3c.agents.zaif-actand-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zaif-actand :as actand]))

(def calibration-path
  "../futon2/holes/labs/M-zaif-harness/calibration-sessions.edn")

(defn- sessions []
  (actand/read-calibration-sessions calibration-path))

(deftest shared-q-actand-record-validator-contract
  "This test is the shared both-grains contract. The future WM :mission
  instantiation must cite this test verbatim and add its real rows here."
  (let [rows (actand/calibration-table (sessions))
        mission-row {:grain :mission
                     :actand :M-example
                     :action :continue
                     :observables #{:progress :stalled}
                     :density {:progress 3/4 :stalled 1/4}
                     :counts {:progress 3 :stalled 1}
                     :provenance {:source :wm/tick-fixtures
                                  :query :q-actand/mission-v1
                                  :record-ids ["tick-example"]}}]
    (is (every? actand/q-actand-record? rows))
    (is (actand/q-actand-record? mission-row))
    (is (false? (actand/q-actand-record? (assoc (first rows) :density {}))))))

(deftest calibration-live-pin
  (let [row (actand/calibration-density
             (sessions)
             {:route :gamma :correction-label true}
             :ask)]
    ;; Tracked calibration pin: e-0cae94f2-9ca8-4863-9251-44278445a5f7.
    (is (= {:gold-judged 18 :not-gold-judged 10} (:counts row)))
    (is (= {:gold-judged 9/14 :not-gold-judged 5/14} (:density row)))
    (is (some #{"e-0cae94f2-9ca8-4863-9251-44278445a5f7"}
              (get-in row [:provenance :record-ids])))
    (is (= actand/query-id (get-in row [:provenance :query])))))

(deftest zero-support-is-a-typed-refusal
  (is (= {:q-actand/refusal :q-actand/no-typed-source
          :grain :arm-session
          :actand {:route :unobserved :correction-label true}
          :action :ask}
         (actand/calibration-density
          (sessions) {:route :unobserved :correction-label true} :ask))))

(deftest provenance-is-required
  (let [row (first (actand/calibration-table (sessions)))]
    (is (false? (actand/q-actand-record? (dissoc row :provenance))))))

(deftest calibration-materialisation-is-deterministic
  (let [bytes (slurp calibration-path)
        parsed-a (edn/read-string bytes)
        parsed-b (edn/read-string bytes)]
    (is (= (actand/calibration-table parsed-a)
           (actand/calibration-table parsed-b)))))

(deftest missing-input-is-a-typed-refusal
  (is (= {:q-actand/refusal :q-actand/missing-input
          :field :gold_judged}
         (actand/calibration-table
          [{:id "incomplete" :route :gamma :is_correction true}]))))
