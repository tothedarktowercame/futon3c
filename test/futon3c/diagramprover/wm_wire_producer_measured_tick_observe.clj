(ns futon3c.diagramprover.wm-wire-producer-measured-tick-observe
  "Producer for the measured-tick-observe wire group. Runs
  futon3c.diagramprover.wm-wire-measured-support/tick-observe (whose product
  operation is futon2.aif.flight/conditioning-step reading the persisted
  tick) once per field (:measured-a, :measurement, :rates) and mode
  (:none, :absent, :different, and :missing for :measured-a), and asserts
  the result equals the committed content-addressed record. Writes the
  record only when WM_WIRE_PRODUCER_WRITE=1; never overwrites."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-measured-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-measured-tick-observe-test)
(def operation 'futon2.aif.flight/conditioning-step)

(def wires
  {:measured-a [:r9-decision :flight-conditioning-step :measured-a]
   :measurement [:r9-measured-a-version :flight-conditioning-step [:measurement {:record :measured-a}]]
   :rates [:r9-measured-a-version :flight-conditioning-step [:rates {:record :measured-a}]]})

(def modes
  {:measured-a [:none :absent :different :missing]
   :measurement [:none :absent :different]
   :rates [:none :absent :different]})

(defn- observe [field mode]
  (let [o (support/tick-observe field mode)]
    {:writer (:writer o)
     :reader (:reader o)
     :received? (w/received? o)
     :writer-present? (some? (:writer o))
     :reader-present? (some? (:reader o))
     :step-status (get-in o [:step :status])
     :step-reason (get-in o [:step :reason])
     :step-tokens (get-in o [:step :tokens])
     :token (:token o)
     :rates (:rates o)
     :measurement (:measurement o)}))

(defn build-record []
  {:producer producer :operation operation
   :inputs {:support 'futon3c.diagramprover.wm-wire-measured-support/tick-observe
            :wires wires :modes modes}
   :fields {:wires (into {}
                         (for [[field wire-id] wires]
                           [wire-id {:field field
                                     :modes (into {}
                                                  (for [mode (modes field)]
                                                    [mode (observe field mode)]))}]))}
   :left-out {:temporary-paths
              "tick-observe's :carrier is a per-run temporary file; readers never check it, so it is not recorded."
              :unchecked-step-fields
              "The readers check only [:step :status], [:step :reason] and [:step :tokens]; the step's other fields (:p-o, :q, :f, :b, ...) are deterministic in this fixture but unchecked, so they are not recorded."}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers" (str "measured-tick-observe@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- leaf-paths [m]
  (mapcat (fn [[k v]]
            (if (map? v) (map #(into [k] %) (leaf-paths v)) [[k]]))
          m))

(deftest measured-tick-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "measured-tick-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (is (= producer (:producer expected)))
        (is (= operation (:operation expected)))
        (is (= (:inputs actual) (:inputs expected)))
        (doseq [path (leaf-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
