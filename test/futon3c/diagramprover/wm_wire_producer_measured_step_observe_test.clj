(ns futon3c.diagramprover.wm-wire-producer-measured-step-observe-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire-measured-support :as support])
  (:import [java.security MessageDigest]))
(def producer 'futon3c.diagramprover.wm-wire-producer-measured-step-observe-test) (def operation 'futon2.aif.flight/run!) (def wire-id [:flight-run :flight-steps-source [:step {:record :enactment-entry}]])
(defn- observation [mutation]
  (let [o (support/step-observe mutation)]
    (assoc o :carrier (when (:carrier o) :temporary-flight-record-path))))
(defn build-record [] {:producer producer :operation operation :inputs {:mutations [:none :absent :different]} :wires {wire-id {:primary (observation :none) :interventions {:absent (observation :absent) :different (observation :different)}}} :left-out {:carrier-path "fresh temporary flight record; reader does not check its concrete pathname"}})
(def stem "measured-step-observe")
(defn- text [x] (str (pr-str x) "\n")) (defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- files [] (filter #(.startsWith (.getName %) (str stem "@")) (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write! [r] (let [s (text r) f (io/file "test/fixtures/wire-producers" (str stem "@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "record exists" {}))) (spit f s) (println f)))
(defn- leaves [x] (letfn [(walk [p v] (if (map? v) (mapcat (fn [[k z]] (walk (conj p k) z)) v) [p]))] (walk [] x)))
(deftest producer-test (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [fs (files)] (is (= 1 (count fs))) (let [e (edn/read-string (slurp (first fs)))] (doseq [p (leaves e)] (testing (pr-str p) (is (= (get-in e p) (get-in a p))))))))))
