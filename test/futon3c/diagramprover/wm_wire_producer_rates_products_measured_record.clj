(ns futon3c.diagramprover.wm-wire-producer-rates-products-measured-record-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c2-support :as support]
            [futon3c.diagramprover.wm-wire-rates-products-support :as products])
  (:import [java.security MessageDigest]))
(defn build-record []
  (let [o (support/measured :rates :none) a (support/measured :rates :absent) d (support/measured :rates :different)
        before (products/measured-record identity) after (products/measured-record products/changed-rates)
        qualify #(into {} (map (fn [[token rates]] [["rates-wire" token] rates])) %)]
    (support/assert-live-records support/measured-live-records-read :measured-a)
    {:producer 'futon3c.diagramprover.wm-wire-producer-rates-products-measured-record-test
     :operation ['futon2.aif.observation-rates/sourced-rates 'futon2.aif.wm.cascade-decision/measured-a-version]
     :inputs {:field :rates :mutations [:none :absent :different]}
     :fields {:writer (:writer o) :reader (:reader o) :writer-present? (some? (:writer o)) :writer-typed-absence? (w/typed-absence? (:writer o))
              :received? (w/received? o) :writer-rate? (= {:false-neg 1/10 :false-pos 1/5} (:writer o)) :schema? (= :wm/measured-a-v1 (get-in o [:result :schema]))
              :absent {:reader-absent? (nil? (:reader a)) :rates-empty? (= {} (:rates (:result a))) :received? (w/received? (select-keys a [:writer :reader]))}
              :different {:reader-rate? (= {:false-neg 1/2 :false-pos 1/5} (:reader d)) :received? (w/received? (select-keys d [:writer :reader]))}
              :live-records-lack? true
              :second-layer {:writer-present? (boolean (seq (:writer before))) :writers-equal? (= (:writer before) (:writer after))
                             :before-qualified? (= (qualify (:writer before)) (get-in before [:record :rates]))
                             :after-qualified? (= (qualify (:carrier after)) (get-in after [:record :rates]))
                             :changed-rate? (= {:false-neg 5/12 :false-pos 5/12} (get-in after [:record :rates ["rates-wire" :t/wanted]]))
                             :records-differ? (not= (get-in before [:record :rates]) (get-in after [:record :rates]))
                             :hashes-strings? (every? #(string? (get-in % [:record :rates-sha])) [before after])
                             :hashes-differ? (not= (get-in before [:record :rates-sha]) (get-in after [:record :rates-sha]))}}
     :left-out {:full-records "reader checks named rate and digest relations"}}))
(defn- txt [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write! [x] (let [s (txt x) f (io/file "test/fixtures/wire-producers" (str "rates-products-measured-record@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "exists" {}))) (spit f s) (println f)))
(defn- paths [x] (letfn [(go [p v] (if (map? v) (mapcat (fn [[k x]] (go (conj p k) x)) v) [p]))] (go [] x)))
(deftest rates-product-producer (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [e (edn/read-string (slurp (first (filter #(.startsWith (.getName %) "rates-products-measured-record@") (.listFiles (io/file "test/fixtures/wire-producers"))))))] (doseq [p (paths (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
