(ns futon3c.diagramprover.wm-wire-producer-rates-observe-g28
  "Producer for the [:r6-sourced-rates :r6-cascade-lane :measurement] wire.
  Runs the real wm-cd/cascade-lane over wm-wire-rates-support's problem with
  ten real pinned C3 subjects admitted through the real store and reader,
  captures the measurement carrier sourced-rates hands the lane and the
  measurement the reader records, and asserts the observation equals the
  committed record. Sibling of the rates-observe producer (PRODUCER-18,
  adjudication-rates and sourced-rates cases); this group's record stem is
  rates-observe-g28."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-rates-products-support :as products]
            [futon3c.diagramprover.wm-wire-rates-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-rates-observe-g28-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-lane)
(def stem "rates-observe-g28")
(def wire-id [:r6-sourced-rates :r6-cascade-lane :measurement])
(def mutations [:absent :different])

(defn- observation [mutation]
  (let [o (support/observe :measurement
                           (case mutation
                             :none identity
                             :absent (constantly {:absent :not-carried})
                             :different #(support/different :measurement %)))]
    (assoc o :received? (w/received? o))))

(defn- recorded [lane-product]
  (get-in lane-product [:cascade-scoring :rates-provenance :measurement]))

(defn- second-layer []
  (let [before (products/lane-product :measurement identity)
        after (products/lane-product :measurement
                                     #(assoc-in % [:t/wanted :false-neg :numerator] 2))]
    {:before {:writer (:writer before) :carrier (:carrier before)
              :recorded (recorded before) :G-efe (:G-efe before)}
     :after {:writer (:writer after) :carrier (:carrier after)
             :recorded (recorded after) :G-efe (:G-efe after)}
     :writer-unchanged? (= (:writer before) (:writer after))
     :writer-is-carrier-before? (= (:writer before) (:carrier before) (recorded before))
     :carrier-is-recorded-after? (= (:carrier after) (recorded after))
     :recorded-changed? (not= (recorded before) (recorded after))
     :g-efe-present? (boolean (seq (:G-efe before)))
     :g-efe-numeric? (every? number? (concat (:G-efe before) (:G-efe after)))
     :g-efe-unchanged? (= (:G-efe before) (:G-efe after))}))

(defn- population []
  (let [view @support/admitted-view
        lane (support/lane view)
        rates (get-in (meta (:ranked lane)) [:cascade-scoring :precision-model :rates])]
    {:subjects (:subjects view)
     :label-count (count (:labels view))
     :prior (:prior view)
     :rates rates}))

(defn- below-minimum []
  (let [view (support/label-view 4)
        lane (support/lane view)]
    {:excluded (:excluded view)
     :labels-empty? (empty? (:labels view))
     :subjects-c3-absent? (not (contains? (:subjects view) :C3))
     :t-wanted (get (support/measurement lane) :t/wanted)}))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:support-observe 'futon3c.diagramprover.wm-wire-rates-support/observe
            :support-different 'futon3c.diagramprover.wm-wire-rates-support/different
            :support-label-view 'futon3c.diagramprover.wm-wire-rates-support/label-view
            :support-lane 'futon3c.diagramprover.wm-wire-rates-support/lane
            :support-measurement 'futon3c.diagramprover.wm-wire-rates-support/measurement
            :support-assert-live-records 'futon3c.diagramprover.wm-wire-rates-support/assert-live-records
            :support-lane-product 'futon3c.diagramprover.wm-wire-rates-products-support/lane-product
            :problem 'futon3c.diagramprover.wm-wire-rates-support/problem
            :population "futon2.aif.observation-label-reader-test pinned subjects, 5 present + 5 absent (and 4 present + 5 absent for the below-minimum case), admitted through the real store and reader"
            :wire-id wire-id :mutations (into [:none] mutations)
            :second-layer-mutation "assoc-in [:t/wanted :false-neg :numerator] 2"}
   :wires {wire-id {:primary (observation :none)
                    :interventions (into {} (map (juxt identity observation) mutations))
                    :second-layer (second-layer)}}
   :population {:admitted (population) :below-minimum (below-minimum)}
   :live-records-read support/live-records-read
   :left-out {:ranked-lane-result "readers check only the measurement/rates ends and the :G-efe vector, not the whole ranked lane result"
              :label-view-store "the label population store lives in a per-run temporary directory; no recorded value contains a path, so no relation was needed"
              :live-record-verification "readers re-verify the live pins themselves (sha256 and absence of :measurement/:adjudication-rates keys) through w/sha256-file and w/read-record; the entries are carried for the wire map's :live-records-read"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) (str stem "@"))
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str stem "@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest rates-observe-g28-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable rates-observe-g28 record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [section [:wires :population]]
            (doseq [path (leaf-paths (get expected section))]
              (testing (pr-str path)
                (is (= (get-in expected (into [section] path))
                       (get-in actual (into [section] path))))))))))))
