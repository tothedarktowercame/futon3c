(ns futon3c.logic.mission-clean-test
  "The mission->CLean emitter (outer-loop tracker). Verifies the pure builder
   produces a DarkTower-spec CLean that satisfies clean_argcheck's gates and the
   §MissionExample grading. The full 0-sorry / argcheck round-trip is validated
   out-of-band against futon6/scripts (clean_to_lean.py + clean_argcheck.bb)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.mission-clean :as mc]))

(def ^:private m-learning-loop-report
  "M-learning-loop @ 2026-07-09 (Checkpoint 3): identify written; map the live
   frontier; derive..document ghost; one held Open-question."
  {:phase/written ["identify"]
   :phase/ghost ["map" "derive" "argue" "verify" "instantiate" "document"]
   :phase/vacuous []
   :loose/open-question-count 1})

(deftest builds-eightfold-spine
  (let [clean (mc/build-mission-clean :M-learning-loop m-learning-loop-report)]
    (testing "G3 — :clean/seq equals the in-order box :method vector"
      (is (= (:clean/seq clean) (mapv :method (:clean/boxes clean))))
      (is (= mc/phase-lifecycle (:clean/seq clean))))
    (testing "spine stays exactly 8 phases (no floating open-questions box)"
      (is (= 8 (count (:clean/boxes clean))))
      (is (= 1 (get-in clean [:clean/shape :open-questions]))))))

(deftest holes-track-fill-state
  (let [clean (mc/build-mission-clean :M-learning-loop m-learning-loop-report)
        boxes (into {} (map (juxt :id identity)) (:clean/boxes clean))
        open? #(contains? (get boxes %) :hole)]
    (testing "HEAD auto-discharged on entry; written phases discharged"
      (is (not (open? :head)))
      (is (not (open? :identify))))
    (testing "ghost/vacuous phases are open (hungry)"
      (is (every? open? [:map :derive :argue :verify :instantiate :document])))
    (testing "G8 — :holes-at agrees with boxes carrying holes"
      (is (= (get-in clean [:clean/shape :holes-at])
             (set (keep #(when (:hole %) (:id %)) (:clean/boxes clean)))))
      (is (= #{} (get-in clean [:clean/shape :discharges-at]))))))

(deftest satiety-grading-matches-mission-example
  (let [clean (mc/build-mission-clean :M-learning-loop m-learning-loop-report)
        boxes (into {} (map (juxt :id identity)) (:clean/boxes clean))
        satiety #(get-in boxes [% :hole :satiety])]
    (testing "§MissionExample: document is the payoff (the goal), other open phases parse"
      (is (= :payoff (satiety :document)))
      (is (every? #(= :parse (satiety %)) [:map :derive :argue :verify :instantiate])))
    (testing "every open hole is a well-formed sorry hole (G6)"
      (is (every? (fn [b] (or (not (:hole b))
                              (and (= :sorry (get-in b [:hole :kind]))
                                   (= :sorryProof (get-in b [:hole :discharge])))))
                  (:clean/boxes clean))))))

(deftest wires-carry-produced-consumed
  (testing "G5 — each wire carries what :from produces and :to consumes"
    (let [clean (mc/build-mission-clean :M-learning-loop m-learning-loop-report)
          boxes (into {} (map (juxt :id identity)) (:clean/boxes clean))]
      (doseq [{:keys [from to carries]} (:clean/wires clean)]
        (is (= carries (get-in boxes [from :produces])))
        (is (contains? (set (get-in boxes [to :consumes])) carries))))))

(deftest nil-report-is-fresh-entry-baseline
  (testing "nil report (proxy down / fresh entry): only HEAD discharged"
    (let [clean (mc/build-mission-clean :M-fresh nil)
          boxes (into {} (map (juxt :id identity)) (:clean/boxes clean))]
      (is (not (contains? (get boxes :head) :hole)))
      (is (every? #(contains? (get boxes %) :hole)
                  [:identify :map :derive :argue :verify :instantiate :document]))
      (is (= 0 (get-in clean [:clean/shape :open-questions]))))))
