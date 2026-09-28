(ns futon3c.diagramprover.wm-wire-producer-publication-increment-observe
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w] [futon3c.diagramprover.wm-wire-publication-support :as support]
            [futon3c.diagramprover.wm-wire-summary-conditioning-products :as conditioning]
            [futon3c.diagramprover.wm-wire-continued-enact-products :as continued])
  (:import [java.security MessageDigest]))
(defn build-record []
  (let [o (support/increment-observe identity) a (support/increment-observe (constantly {:absent :not-carried}))
        d (support/increment-observe #(assoc % :delta 42)) [pa pb] (conditioning/pair :increment)
        sa (get-in pa [:record :enactments 0 :step]) sb (get-in pb [:record :enactments 0 :step])
        {first-step :first ca :a cb :b} (continued/chain-pair)
        pins [support/flight-278b6988 support/flight-ada87008] live (:flight (w/read-record (:path (first pins))))]
    {:producer 'futon3c.diagramprover.wm-wire-producer-publication-increment-observe-test
     :operation ['futon2.aif.flight/run! 'futon3c.diagramprover.wm-wire-summary-conditioning-products/pair 'futon3c.diagramprover.wm-wire-continued-enact-products/chain-pair]
     :inputs [{:tamper [:identity :absent :delta-42]} {:pair :increment} {:chain-pair true}]
     :fields {:writer (:writer o) :reader (:reader o) :writer-present? (some? (:writer o)) :writer-typed-absence? (w/typed-absence? (:writer o)) :received? (w/received? o)
              :record-id? (= ["click-1" :cand/a-registry-first] (get-in o [:writer :record-id])) :delta-present? (contains? (:writer o) :delta)
              :absent {:reader (:reader a) :received? (w/received? a)} :different {:reader-present? (some? (:reader d)) :typed? (w/typed-absence? (:reader d)) :received? (w/received? d)}
              :live {:pins? (every? #(= (:sha256 %) (w/sha256-file (:path %))) pins) :typed-enactment? (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments live))) :increment-absent? (not-any? #(and (map? %) (contains? % :increment)) (tree-seq coll? seq live))}
              :conditioning {:run-record? (= (:run-record pa) (:run-record pb)) :click? (= (:click pa) (:click pb)) :increments-without-key? (= (dissoc (:increment pa) :policy-key) (dissoc (:increment pb) :policy-key))
                             :increment-copied? (every? #(= (:increment %) (get-in % [:record :enactments 0 :increment])) [pa pb])
                             :key-copied? (every? #(= (get-in % [:increment :policy-key]) (get-in % [:record :enactments 0 :step :policy-key])) [pa pb])
                             :present? (= :present (:status sa) (:status sb)) :keys-differ? (not= (:policy-key sa) (:policy-key sb)) :steps-without-key? (= (dissoc sa :policy-key) (dissoc sb :policy-key))}
              :continued {:present? (= :present (:status first-step) (:status ca) (:status cb)) :a-prev? (= (:q first-step) (get-in ca [:s-prev :value])) :a-chain? (= :chain (get-in ca [:s-prev :source]))
                          :b-initial? (= {:value {#{} 1} :source :initial-belief} (:s-prev cb)) :common-inputs? (= (select-keys ca [:b :o :measured-a :occurrence]) (select-keys cb [:b :o :measured-a :occurrence]))
                          :a-po? (= 11/12 (:p-o ca)) :b-po? (= 1/12 (:p-o cb)) :q-differ? (not= (:q ca) (:q cb)) :f-order? (< (:f ca) (:f cb))}}
     :left-out {:temporary-enactment-dir "fixture cleans it" :full-conditioning-records "only named relations are checked"}}))
(defn- txt [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write! [x] (let [s (txt x) f (io/file "test/fixtures/wire-producers" (str "publication-increment-observe@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "exists" {}))) (spit f s) (println f)))
(defn- paths [x] (letfn [(g [p v] (if (map? v) (mapcat (fn [[k x]] (g (conj p k) x)) v) [p]))] (g [] x)))
(deftest increment-producer (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [e (edn/read-string (slurp (first (filter #(.startsWith (.getName %) "publication-increment-observe@") (.listFiles (io/file "test/fixtures/wire-producers"))))))] (doseq [p (paths (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
