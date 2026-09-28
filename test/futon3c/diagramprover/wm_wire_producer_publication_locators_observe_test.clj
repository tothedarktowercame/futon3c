(ns futon3c.diagramprover.wm-wire-producer-publication-locators-observe-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w] [futon3c.diagramprover.wm-wire-publication-support :as support])
  (:import [java.security MessageDigest]))
(defn build-record []
  (let [o (support/locators-observe identity)
        a (support/locators-observe #(assoc-in % [:cascade-problem :locators] {:absent :not-carried}))
        d (support/locators-observe #(assoc-in % [:cascade-problem :locators support/locator-token :path] "src/futon2/aif/flight_runner.clj"))
        pins [(assoc support/tick-278b6988 :why :writer-only) (assoc support/tick-e70b4baf :why :writer-only)]
        live (w/read-record (:path (first pins)))]
    {:producer 'futon3c.diagramprover.wm-wire-producer-publication-locators-observe-test
     :operation ['futon2.aif.mission-reading/publish-locator! 'futon2.aif.cascade-problems/assemble 'futon2.aif.wm.cascade-decision/measured-a-version]
     :inputs {:target support/locator-target :token support/locator-token :locator support/flight-source-locator :mutations [:none :absent :different]}
     :fields {:writer (:writer o) :reader (:reader o) :writer-present? (some? (:writer o)) :writer-typed-absence? (w/typed-absence? (:writer o)) :received? (w/received? o)
              :published-locator? (= support/flight-source-locator (get (:writer o) support/locator-token)) :class-c3? (boolean (some #{:C3} (:classes o)))
              :measured? (pos? (get-in o [:measurement :false-neg :denominator] 0))
              :absent {:reader (:reader a) :received? (w/received? a)}
              :different {:reader-present? (some? (:reader d)) :reader-typed-absence? (w/typed-absence? (:reader d)) :received? (w/received? d)}
              :live {:pins-valid? (every? #(= (:sha256 %) (w/sha256-file (:path %))) pins)
                     :writer-present? (boolean (some #(and (map? %) (contains? % :observation-locators)) (tree-seq coll? seq live)))
                     :reader-absent? (not-any? #(and (map? %) (contains? % :measured-a)) (tree-seq coll? seq live))}}
     :left-out {:temporary-store "publication fixture cleans its temporary store; readers check the published locator and measured relations"}}))
(defn- txt [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write! [x] (let [s (txt x) f (io/file "test/fixtures/wire-producers" (str "publication-locators-observe@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "exists" {}))) (spit f s) (println f)))
(defn- paths [x] (letfn [(g [p v] (if (map? v) (mapcat (fn [[k x]] (g (conj p k) x)) v) [p]))] (g [] x)))
(deftest locators-producer (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [e (edn/read-string (slurp (first (filter #(.startsWith (.getName %) "publication-locators-observe@") (.listFiles (io/file "test/fixtures/wire-producers"))))))] (doseq [p (paths (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
