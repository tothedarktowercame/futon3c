(ns futon3c.diagramprover.wm-wire-producer-measured-cleanup-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.belief :as belief]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as support]
            [futon3c.diagramprover.wm-wire-fold-in-support :as fold-in]
            [futon3c.diagramprover.wm-wire-measured-support :as measured])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-measured-cleanup-test)
;; The packet named the operation
;; "flight/run!+flight/run!+judge+fold+war-machine/judge". The source says:
;; the reader's support calls are support/judge (which runs wm/judge with
;; hermetic input ports) and measured/cleanup; the witness observes the
;; loop belief at the real three-argument belief/predict-observation, and
;; the :different carrier is built by wm/apply-arena-belief-events over
;; belief/initial-belief-state. The record names what the source says.
(def operation ['futon2.report.war-machine/judge
                'futon2.aif.belief/predict-observation
                'futon2.report.war-machine/apply-arena-belief-events])

(defn- observe [mutation]
  (let [root (w/tmp-dir "belief-prediction-wire-")
        predict belief/predict-observation
        captured (atom [])
        other (wm/apply-arena-belief-events
               (belief/initial-belief-state ["known"])
               [(assoc (first fold-in/events) :type :foreclosed)])]
    (try
      (with-redefs [belief/predict-observation
                    (fn [& args]
                      ;; Observe only judge's three-argument call. The real reader's
                      ;; recursive arity calls still run their original bodies.
                      (if (= 3 (count args))
                        (let [[value tags context] args
                              received (case mutation :none value :absent nil :different other)
                              expected (predict value tags context)
                              actual (predict received tags context)]
                          (swap! captured conj {:writer value :reader received
                                                :expected-predictions expected
                                                :predictions actual})
                          actual)
                        (apply predict args)))]
        (support/judge root {:annotation-graph {:health 0.9}}))
      (assoc (first @captured) :calls (count @captured))
      (finally (measured/cleanup root)))))

(defn build-record []
  (support/assert-live-pins)
  {:producer producer
   :operation operation
   :inputs {:support-judge 'futon3c.diagramprover.wm-wire-fold-out-support/judge
            :support-assert-live-pins 'futon3c.diagramprover.wm-wire-fold-out-support/assert-live-pins
            :support-cleanup 'futon3c.diagramprover.wm-wire-measured-support/cleanup
            :scan {:annotation-graph {:health 0.9}}
            :fold-in-event (first fold-in/events)
            :mutations [:none :absent :different]}
   :live-records-read support/live-records-read
   :cases (into {} (map (juxt identity observe)) [:none :absent :different])
   :left-out {:judge-return "support/judge's full judge result is not checked by the reader; only the belief and predictions captured at the real three-argument predict-observation call are"
              :trace-dir "w/tmp-dir gives a per-run temporary directory, removed by measured/cleanup; no reader checks the path"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "measured-cleanup@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "measured-cleanup@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest measured-cleanup-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable measured-cleanup record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path)
                     (get-in actual path))))))))))
