(ns futon3c.diagramprover.wm-wire-producer-rates-products-measured-product-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w] [futon3c.diagramprover.wm-wire-publication-support :as support]
            [futon3c.diagramprover.wm-wire-rates-products-support :as products])
  (:import [java.security MessageDigest]))
(defn build-record []
  (let [o (support/measurement-observe identity)
        a (support/measurement-observe #(assoc % support/measurement-token {:absent :not-carried}))
        d (support/measurement-observe #(update-in % [support/measurement-token :false-neg :numerator] inc))
        before (products/measured-product :measurement identity)
        after (products/measured-product :measurement #(zipmap (keys %) (repeat :absent)))
        pins [support/tick-278b6988 support/tick-e70b4baf]]
    {:producer 'futon3c.diagramprover.wm-wire-producer-rates-products-measured-product-test
     :operation ['futon2.aif.observation-rates/sourced-rates 'futon2.aif.wm.cascade-decision/measured-a-version]
     :inputs {:target support/measurement-target :token support/measurement-token :tamper-modes [:none :absent :different]
              :product-mutations [:identity :all-absent]}
     :fields {:writer (:writer o) :reader (:reader o) :writer-present? (some? (:writer o)) :writer-typed-absence? (w/typed-absence? (:writer o))
              :received? (w/received? o) :schema? (= :wm/measured-a-v1 (get-in o [:measured-a :schema]))
              :false-neg-measured? (pos? (get-in (:writer o) [:false-neg :denominator] 0)) :false-pos-measured? (pos? (get-in (:writer o) [:false-pos :denominator] 0))
              :absent {:reader (:reader a) :received? (w/received? a)}
              :different {:reader-present? (some? (:reader d)) :reader-typed-absence? (w/typed-absence? (:reader d)) :received? (w/received? d)}
              :live {:pins-valid? (every? #(= (:sha256 %) (w/sha256-file (:path %))) pins)
                     :measured-a-absent? (every? (fn [{:keys [path]}] (not-any? #(and (map? %) (contains? % :measured-a)) (tree-seq coll? seq (w/read-record path)))) pins)}
              :second-layer {:before-schema? (= :wm/measured-a-v1 (:schema before))
                             :zero-of-five? (every? #(= {:numerator 0 :denominator 5} (:false-neg %)) (vals (:measurement before)))
                             :absent-refusal? (= {:status :absent :reason :no-measured-rates} after)}}
     :left-out {:full-measured-record "reader checks schema, cells, carrier refusal and equality only"}}))
(defn- txt [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write! [x] (let [s (txt x) f (io/file "test/fixtures/wire-producers" (str "rates-products-measured-product@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "exists" {}))) (spit f s) (println f)))
(defn- paths [x] (letfn [(g [p v] (if (map? v) (mapcat (fn [[k x]] (g (conj p k) x)) v) [p]))] (g [] x)))
(deftest measurement-product-producer (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [e (edn/read-string (slurp (first (filter #(.startsWith (.getName %) "rates-products-measured-product@") (.listFiles (io/file "test/fixtures/wire-producers"))))))] (doseq [p (paths (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
