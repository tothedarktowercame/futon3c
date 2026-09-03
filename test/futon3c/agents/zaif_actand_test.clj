(ns futon3c.agents.zaif-actand-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zaif-actand :as actand]
            [futon3c.agents.zaif-controller :as controller]
            [futon3c.agents.zaif-inputs :as inputs]))

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
          [{:id "incomplete" :route :gamma :is_correction true}])))
  (let [row (first (actand/calibration-table (sessions)))]
    (is (= {:q-actand/refusal :q-actand/missing-input
            :field [:density :gold-judged]}
           (actand/demo-bridge
            (-> row
                (update :density dissoc :gold-judged)
                (update :observables disj :gold-judged)
                (update :counts dissoc :gold-judged)))))))

(deftest real-density-reaches-controller-act-value
  (let [row (actand/calibration-density
             (sessions)
             {:route :gamma :correction-label true}
             :ask)
        bridged (actand/demo-bridge row)
        hydrated (inputs/hydrate-inputs {:actand-query-result bridged})
        decision (controller/decide hydrated)
        expected (* (:act-pragmatic-scale controller/constants)
                    (:gamma-used decision)
                    (- (Math/log (double 9/14)) (Math/log 0.5)))]
    ;; After-pin source: calibration session
    ;; e-0cae94f2-9ca8-4863-9251-44278445a5f7 contributes to this real row,
    ;; whose counts are {:gold-judged 18 :not-gold-judged 10}.
    ;; Before-pin: live decision e-0f2f9aec-6240-40e9-a25a-e45d9452076f
    ;; recorded :task-belief {}, :g-terms {:act 0.0}, and :gamma-used 1.0.
    ;; Shipped :act-pragmatic-scale is 1.0 (controller/constants).
    (is (= {:gold-judged 18 :not-gold-judged 10} (:counts row)))
    (is (some #{"e-0cae94f2-9ca8-4863-9251-44278445a5f7"}
              (get-in row [:provenance :record-ids])))
    (is (= bridged (:task-belief hydrated)))
    (is (= (:provenance row) (get-in hydrated [:task-belief :provenance])))
    (is (= :q-actand/demo-bridge-a4
           (get-in hydrated [:task-belief :bridge])))
    (is (nil? (get-in hydrated [:task-belief :refused])))
    (is (not (zero? (get-in decision [:g-terms :act]))))
    (is (= expected (get-in decision [:g-terms :act])))))

(deftest task-belief-persists-and-reconstructs-across-two-turns
  (let [row (actand/calibration-density
             (sessions)
             {:route :gamma :correction-label true}
             :ask)
        turn-one-belief (actand/demo-bridge row)
        turn-one-inputs (inputs/hydrate-inputs
                         {:actand-query-result turn-one-belief})
        turn-one (controller/decide turn-one-inputs)
        persisted-bytes (pr-str turn-one-belief)
        reconstructed-belief (edn/read-string persisted-bytes)
        turn-two-inputs (inputs/hydrate-inputs
                         {:actand-query-result reconstructed-belief})
        turn-two (controller/decide turn-two-inputs)]
    ;; Tracked calibration record
    ;; e-0cae94f2-9ca8-4863-9251-44278445a5f7 is quoted verbatim here: it is
    ;; one of the 18 :gold-judged records in the persisted density's 28 ids.
    (is (= {:gold-judged 18 :not-gold-judged 10} (:counts row)))
    (is (contains? (set (get-in reconstructed-belief
                                [:provenance :record-ids]))
                   "e-0cae94f2-9ca8-4863-9251-44278445a5f7"))
    (is (= turn-one-belief reconstructed-belief))
    (is (= :q-actand/demo-bridge-a4 (:bridge reconstructed-belief)))
    (is (= 0.2513144282809061 (get-in turn-one [:g-terms :act])))
    (is (= (get-in turn-one [:g-terms :act])
           (get-in turn-two [:g-terms :act])))
    (is (= (get-in turn-one-inputs [:task-belief :provenance])
           (get-in turn-two-inputs [:task-belief :provenance])))))

(deftest r3-belief-update-is-directional-and-typed-empty-is-unchanged
  (let [live-row (actand/calibration-density
                  (sessions)
                  {:route :gamma :correction-label true}
                  :ask)
        planted-low (assoc live-row
                           :density {:gold-judged 1/2 :not-gold-judged 1/2}
                           :counts {:gold-judged 1 :not-gold-judged 1})
        planted-high (assoc live-row
                            :density {:gold-judged 3/4 :not-gold-judged 1/4}
                            :counts {:gold-judged 3 :not-gold-judged 1})
        low-belief (:task-belief
                    (inputs/hydrate-inputs
                     {:actand-query-result (actand/demo-bridge planted-low)}))
        high-belief (:task-belief
                     (inputs/hydrate-inputs
                      {:actand-query-result (actand/demo-bridge planted-high)}))
        empty-before (:task-belief (inputs/hydrate-inputs {}))
        empty-after (:task-belief (inputs/hydrate-inputs
                                   {:actand-query-result nil}))]
    ;; Tracked calibration record e-0cae94f2-9ca8-4863-9251-44278445a5f7
    ;; pins the real row from which both directional plants retain provenance.
    (is (= {:gold-judged 18 :not-gold-judged 10} (:counts live-row)))
    (is (some #{"e-0cae94f2-9ca8-4863-9251-44278445a5f7"}
              (get-in live-row [:provenance :record-ids])))
    (is (= 0.0 (:act-value low-belief)))
    (is (= (- (Math/log 0.75) (Math/log 0.5))
           (:act-value high-belief)))
    (is (> (:act-value high-belief) (:act-value low-belief)))
    (is (= (:provenance live-row) (:provenance high-belief)))
    (is (= {:absence :d8/task-belief-actand-source-absent} empty-before))
    (is (= empty-before empty-after))))
