(ns futon3c.diagramprover.wm-wire-producer-rates-observe-g30-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-rates-support :as support])
  (:import [java.security MessageDigest]))
(def producer 'futon3c.diagramprover.wm-wire-producer-rates-observe-g30-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-lane)
(def wire-id [:r4-kernel :fpi-policy-free-energy :rates])
(defn- observation [tamper] (support/observe :fpi-rates tamper))
(defn build-record []
  {:producer producer :operation operation
   :inputs {:wire-kind :fpi-rates :mutations [:none :absent :different]}
   :live-records support/live-records-read
   :wires {wire-id {:primary (observation identity)
                    :interventions {:absent (observation (constantly {:absent :not-carried}))
                                    :different (observation #(support/different :fpi-rates %))}}}
   :left-out {:temporary-label-store "created per run; no reader checks its path"
              :ranked-lane-result "reader checks only the rates writer and policy-free-energy reader"}})
(defn- text [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255))
                                (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- files [] (filter #(.startsWith (.getName %) "rates-observe-g30@")
                         (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write! [r]
  (let [s (text r) f (io/file "test/fixtures/wire-producers" (str "rates-observe-g30@" (subs (sha s) 0 12) ".edn"))]
    (when (.exists f) (throw (ex-info "producer record already exists" {:file (str f)})))
    (spit f s) (println (.getPath f))))
(defn- leaves [x]
  (letfn [(walk [p v] (if (map? v) (mapcat (fn [[k x]] (walk (conj p k) x)) v) [p]))]
    (walk [] x)))
(deftest rates-observe-g30-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! actual)
        (let [fs (files)]
          (is (= 1 (count fs)) "exactly one immutable G30 record")
          (let [expected (edn/read-string (slurp (first fs)))]
            (doseq [p (leaves expected)]
              (testing (pr-str p) (is (= (get-in expected p) (get-in actual p))))))))))
