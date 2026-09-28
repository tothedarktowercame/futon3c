(ns futon3c.diagramprover.wm-wire-producer-rates-observe
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-rates-products-support :as products]
            [futon3c.diagramprover.wm-wire-rates-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-rates-observe-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-lane)
(def cases
  [{:wire-kind :adjudication-rates
    :wire-id [:r6-cascade-lane :r4-kernel :adjudication-rates]
    :seam :adjudication-rates}
   {:wire-kind :sourced-rates
    :wire-id [:r6-sourced-rates :r6-cascade-lane [:rates {:record :sourced-rates}]]
    :seam :rates}])

(defn- observation [wire-kind mutation]
  (support/observe wire-kind
                   (case mutation
                     :none identity
                     :absent (constantly {:absent :not-carried})
                     :different #(support/different wire-kind %))))

(defn- lane-second-layer [seam]
  (let [before (products/lane-product seam identity)
        after (products/lane-product seam products/changed-rates)
        g (:G-efe before) g-prime (:G-efe after)]
    {:before {:G-efe g}
     :after {:G-efe g-prime}
     :g-efe-present? (boolean (seq g))
     :g-efe-numeric? (every? number? (concat g g-prime))
     :counts [(count g) (count g-prime)]
     :g-efe-differ? (not= g g-prime)}))

(defn- case-fields [{:keys [wire-kind seam]}]
  {:primary (observation wire-kind :none)
   :interventions {:absent (observation wire-kind :absent)
                   :different (observation wire-kind :different)}
   :second-layer (lane-second-layer seam)})

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:support-observe 'futon3c.diagramprover.wm-wire-rates-support/observe
            :support-different 'futon3c.diagramprover.wm-wire-rates-support/different
            :support-lane-product 'futon3c.diagramprover.wm-wire-rates-products-support/lane-product
            :support-assert-live-records 'futon3c.diagramprover.wm-wire-rates-support/assert-live-records
            :problem 'futon3c.diagramprover.wm-wire-rates-support/problem
            :population "futon2.aif.observation-label-reader-test pinned subjects, 5 present + 5 absent, admitted through the real store and reader"
            :cases cases
            :second-layer-mutations {:before :identity :after :futon3c.diagramprover.wm-wire-rates-products-support/changed-rates}
            :mutations [:none :absent :different]}
   :live-records-read support/live-records-read
   :wires (into {} (map (juxt :wire-id case-fields) cases))
   :left-out {:ranked-lane-result "readers check only the scoped rate ends observe extracts from the lane result, not the whole ranked vector"
              :lane-product-writer-carrier-cascade-scoring "readers check only the :G-efe vectors of the lane product"
              :temporary-paths "the label population store lives in a per-run temporary directory; no recorded value contains a path, so no relation was needed"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "rates-observe@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "rates-observe@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest rates-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable rates-observe record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path)
                     (get-in actual path))))))))))
