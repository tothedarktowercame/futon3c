(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-rates-test
  "WIRE-23-C2 wire 3: [:r6-sourced-rates :r9-measured-a-version [:rates {:record :sourced-rates}]].
  Same harness as the status wire; the reader's produced value is the
  target-qualified rate [:rates [target :t]] on the measured-A record.
  WITNESSED-HERMETICALLY: no live record carries either end
  (support/measured-live-records-read)."
  (:require [futon3c.diagramprover.wm-wire-rates-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c2-support :as support]))

(def positive (delay (support/measured :rates :none)))
(defn check [] (let [o @positive] {:writer (:writer o) :reader (:reader o)}))

(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-rates-test/reader-record-retains-the-intervened-carrier
                         :kind :record :product [:rates]
                         :intervention :before-reader}
           :wire [:r6-sourced-rates :r9-measured-a-version [:rates {:record :sourced-rates}]]
           :kind :witnessed-hermetically :test `the-sourced-rates-reach-measured-a-version
           :check check
           :live-records-read support/measured-live-records-read
           :note "Writer: observation_rates.clj sourced-rates (:rates {token {:false-neg r :false-pos r}}). Reader: war_machine.clj measured-a-version qualifies them to [target token] on the measured-A record. Hermetic: the producer's return is persisted nowhere and no tick record here carries :measured-a."})

(deftest the-sourced-rates-reach-measured-a-version
  (let [o @positive]
    (is (= {:false-neg 1/10 :false-pos 1/5} (:writer o))
        "the real producer's rate for :t over the admitted :C4 labels")
    (is (= :wm/measured-a-v1 (get-in o [:result :schema])))
    (is (w/received? (check)) (pr-str (check)))))

(deftest absent-rates-before-reader
  (let [o (support/measured :rates :absent)]
    (is (nil? (:reader o))
        "a producer return without :rates contributes no qualified rate")
    (is (= {} (:rates (:result o))))
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest different-rates-before-reader
  (let [o (support/measured :rates :different)]
    (is (= {:false-neg 1/2 :false-pos 1/5} (:reader o))
        "the qualified rate is the tampered producer value, not the writer's")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest live-records-lack-both-ends
  (support/assert-live-records support/measured-live-records-read :measured-a))

(deftest reader-record-retains-the-intervened-carrier
  (let [before (products/measured-record identity)
        after (products/measured-record products/changed-rates)
        qualify #(into {} (map (fn [[token rates]] [["rates-wire" token] rates])) %)]
    (is (seq (:writer before)))
    (is (= (:writer before) (:writer after)))
    (is (= (qualify (:writer before)) (get-in before [:record :rates])))
    (is (= (qualify (:carrier after)) (get-in after [:record :rates])))
    (is (= {:false-neg 5/12 :false-pos 5/12}
           (get-in after [:record :rates ["rates-wire" :t/wanted]])))
    (is (not= (get-in before [:record :rates]) (get-in after [:record :rates])))
    (is (every? #(string? (get-in % [:record :rates-sha])) [before after]))
    (is (not= (get-in before [:record :rates-sha]) (get-in after [:record :rates-sha])))
    (println :rates-digest (get-in before [:record :rates-sha])
             :after (get-in after [:record :rates-sha]))))
