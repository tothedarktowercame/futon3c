(ns futon3c.diagramprover.wm-wire-r7-flight-call-flight-run-increment-test
  "Wire [:r7-flight-call :flight-run :increment]: the habit increment on
  wc-verdict-fn's return (flight_runner.clj:913-952) reaching flight/run!,
  which merges it onto the click's enactments entry.

  No live record carries either end: every flight record under spike/
  records its one enactment as a typed absence ({:absent
  :no-dispatch-configured} or {:absent :no-decision}), no
  [:enactments i :increment] appears anywhere, and wc-verdict-fn's return
  is never persisted apart from run!'s copy of it. WITNESSED-HERMETICALLY:
  a real one-click flight/run! with the real enact-fn (the lane-8
  driver's exemplar-backed fixture) and the real wc-verdict-fn (real
  checker, real enactment-habit/increment)."
  (:require [futon3c.diagramprover.wm-wire-continued-enact-products :as continued]
            [futon3c.diagramprover.wm-wire-summary-conditioning-products :as conditioning]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support]))

(def live-records-read
  [(assoc support/flight-278b6988
          :why "its one enactment is {:absent :no-dispatch-configured}: no wc call ran, no :increment on the entry; the writer's return was never persisted apart from the reader's copy")
   (assoc support/flight-ada87008
          :why "same: no :increment or :publication-observed on any enactments entry in any spike flight record")])

(defn check [] (support/increment-observe identity))

(def wire
  {:wire [:r7-flight-call :flight-run :increment]
   :second-layer {:test `policy-key-selects-the-continued-posterior
                  :kind :value-varying :product [:enactments 1 :step :p-o]
                  :intervention :before-reader}
   :kind :witnessed-hermetically
   :test `the-writers-increment-reaches-the-enactments-entry
   :check check
   :live-records-read live-records-read})

(deftest the-writers-increment-reaches-the-enactments-entry
  (let [{:keys [writer] :as o} (check)]
    (is (some? writer))
    (is (= ["click-1" :cand/a-registry-first] (:record-id writer))
        "the real enactment-habit/increment's receipt for the hermetic enactment")
    (is (contains? writer :delta))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-field-does-not-witness-the-wire
  (let [o (support/increment-observe (constantly {:absent :not-carried}))]
    (is (= {:absent :not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-increment-than-the-writers-does-not-witness-the-wire
  (let [o (support/increment-observe (fn [inc] (assoc inc :delta 42)))]
    (is (some? (:reader o)))
    (is (not (w/typed-absence? (:reader o))))
    (is (not (w/received? o)) "present, not absent, but not the receipt the writer wrote")))

(deftest the-live-records-carry-no-increment
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [f (:flight (w/read-record (:path (first live-records-read))))]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments f))))
    (is (not-any? #(and (map? %) (contains? % :increment))
                  (tree-seq coll? seq f))
        "no [:enactments i :increment] on the record")))

(deftest increment-policy-key-is-carried-into-the-step
  ;; run!:572 copies the key; conditioning-step:299 uses it to find a prior
  ;; chain. With no prior enactments, both keys use the same initial belief.
  (let [[a b] (conditioning/pair :increment)
        sa (get-in a [:record :enactments 0 :step])
        sb (get-in b [:record :enactments 0 :step])]
    (is (= (:run-record a) (:run-record b)))
    (is (= (:click a) (:click b)))
    (is (= (dissoc (:increment a) :policy-key) (dissoc (:increment b) :policy-key)))
    (doseq [r [a b]]
      (is (= (:increment r) (get-in r [:record :enactments 0 :increment])))
      (is (= (get-in r [:increment :policy-key]) (get-in r [:record :enactments 0 :step :policy-key]))))
    (is (= :present (:status sa) (:status sb)))
    (is (not= (:policy-key sa) (:policy-key sb)))
    (is (= (dissoc sa :policy-key) (dissoc sb :policy-key)))
    (println :increment-conditioning (pr-str (mapv #(select-keys % [:policy-key :q :p-o :f]) [sa sb])))))

(deftest policy-key-selects-the-continued-posterior
  (let [{:keys [first a b]} (continued/chain-pair)]
    (is (= :present (:status first) (:status a) (:status b)))
    (is (= (:q first) (get-in a [:s-prev :value])))
    (is (= :chain (get-in a [:s-prev :source])))
    (is (= {:value {#{} 1} :source :initial-belief} (:s-prev b)))
    (is (= (select-keys a [:b :o :measured-a :occurrence])
           (select-keys b [:b :o :measured-a :occurrence])))
    (is (= 11/12 (:p-o a)))
    (is (= 1/12 (:p-o b)))
    (is (not= (:q a) (:q b)))
    (is (< (:f a) (:f b)))
    (println :continued-policy-key (pr-str (mapv #(select-keys % [:s-prev :q :p-o :f]) [a b])))))
