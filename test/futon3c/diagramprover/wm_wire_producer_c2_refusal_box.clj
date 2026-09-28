(ns futon3c.diagramprover.wm-wire-producer-c2-refusal-box
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-c2-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-c2-refusal-box-test)
(def operation
  {:writer 'futon2.aif.wm.cascade-decision/cascade-decision
   :test-vars ['futon2.aif.gate-refusal-abstention-test/the-real-gate-refusal-is-the-ticks-typed-abstention
               'futon2.aif.judge-refusal-abstention-test/the-real-judge-refusal-is-the-ticks-typed-abstention]})
(def cases
  [{:which :gate :wire-id [:r9-decision :gate-refusal-test :kind]}
   {:which :judge :wire-id [:r9-decision :r9-judge-refusal-test :kind]}])

(defn- case-fields [{:keys [which]}]
  {:primary (support/refusal-box which :none)
   :interventions {:absent (support/refusal-box which :absent)
                   :different (support/refusal-box which :different)}})

(defn build-record []
  (support/assert-live-records support/refusal-live-records-read :measured-a)
  {:producer producer
   :operation operation
   :inputs {:runtime-defaults :wm-wire-r9-support/hermetic-runner-defaults
            :cases cases :mutations [:none :absent :different]
            :live-pins (mapv #(select-keys % [:path :sha256]) support/refusal-live-records-read)}
   :live-records-verified? true
   :wires (into {} (map (juxt :wire-id case-fields) cases))
   :left-out {:phase-timestamps "the hermetic runner emits clock values; neither test box reads them"
              :temporary-run-stores "the refusal boxes use hermetic stores; no reader checks their paths"
              :full-test-reports "the readers check the consumed equality value and report type retained here"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "c2-refusal-box@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers" (str "c2-refusal-box@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest c2-refusal-box-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable c2-refusal-box record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (select-keys expected [:live-records-verified? :wires]))]
            (testing (pr-str path)
              (is (= (get-in expected path) (get-in actual path))))))))))
