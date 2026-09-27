(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-status-test
  "WIRE-23-C2 wire 2: [:r6-sourced-rates :r9-measured-a-version [:status {:record :sourced-rates}]].
  The REAL observation-rates/sourced-rates over ten admitted :C4 labels,
  reaching the REAL war-machine/measured-a-version exactly as
  flight-conditioning-step-test's produced-measured-a drives it; the
  producer's return is tampered by with-redefs around the real var. A
  :sourced status reads as the measured-A record; any other status reads as
  the typed absence {:status :absent :reason :sourcing-refused :refusals …}
  carrying the producer's status. WITNESSED-HERMETICALLY: no live record
  carries either end (support/measured-live-records-read)."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c2-support :as support]))

(def positive (delay (support/measured :status :none)))
(defn check [] (let [o @positive] {:writer (:writer o) :reader (:reader o)}))

(def wire {:wire [:r6-sourced-rates :r9-measured-a-version [:status {:record :sourced-rates}]]
           :kind :witnessed-hermetically :test `the-sourced-status-reaches-measured-a-version
           :check check
           :live-records-read support/measured-live-records-read
           :note "Writer: observation_rates.clj sourced-rates (:status :sourced). Reader: war_machine.clj measured-a-version (:6361): a :sourced status produces the measured-A record, anything else {:status :absent :reason :sourcing-refused}. Hermetic: the producer's return is persisted nowhere and no tick record here carries :measured-a."})

(deftest the-sourced-status-reaches-measured-a-version
  (let [o @positive]
    (is (= :sourced (:writer o)))
    (is (= :wm/measured-a-v1 (get-in o [:result :schema]))
        "a :sourced producer status reads as the measured-A record")
    (is (= (:rates (:producer o))
           {:t {:false-neg 1/10 :false-pos 1/5}})
        "the real producer measured the admitted labels")
    (is (w/received? (check)) (pr-str (check)))))

(deftest absent-status-before-reader
  (let [o (support/measured :status :absent)]
    (is (= {:status :absent :reason :sourcing-refused}
           (select-keys (:result o) [:status :reason]))
        "a producer return without :status reads as the sourcing-refused absence")
    (is (contains? (:refusals (:result o)) support/measured-target))
    (is (= :absent (:reader o)))
    (is (w/typed-absence? (:result o))
        "the reader's produced value is the typed sourcing-refused absence")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest different-status-before-reader
  (let [o (support/measured :status :different)]
    (is (= {:status :absent :reason :sourcing-refused}
           (select-keys (:result o) [:status :reason])))
    (is (= :sourcing-refused-differently
           (get-in o [:result :refusals support/measured-target :status]))
        "the typed absence carries the producer's actual status")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest live-records-lack-both-ends
  (support/assert-live-records support/measured-live-records-read :measured-a))
