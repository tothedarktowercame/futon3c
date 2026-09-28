(ns futon3c.diagramprover.wm-wire-producer-construction-kernel
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-construction-products :as products]
            [futon3c.diagramprover.wm-wire-construction-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-construction-kernel-test)
(def operation 'futon2.aif.efe/rank-cascade-actions)
(def wire-id [:construction-assemble-one :r4-kernel [:want {:record :cascade-spec}]])

(defn- observation [mutation]
  (let [o (support/kernel mutation)]
    {:writer (:writer o)
     :reader (:reader o)
     :ranked-present? (boolean (seq (:ranked o)))}))

(defn- score-fields []
  (let [before (products/score-product :want :none)
        after (products/score-product :want :different)
        v (:scores before)
        v-prime (:scores after)]
    {:before-scores v
     :after-scores v-prime
     :competing? (< 1 (count v))
     :same-count? (= (count v) (count v-prime))
     :numeric? (every? number? (concat v v-prime))
     :changed? (not= v v-prime)}))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:field :want :mutations [:none :absent :different]}
   :live-records support/live-records-read
   :wires {wire-id {:primary (observation :none)
                    :interventions {:absent (observation :absent)
                                    :different (observation :different)}
                    :second-layer (score-fields)}}
   :left-out {:ranked-entries
              "the reader checks only that ranking is nonempty; scores checked by the reader are retained separately"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "construction-kernel@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record)
        sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "construction-kernel@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x)
              (mapcat (fn [[k v]] (walk (conj path k) v)) x)
              [path]))]
    (walk [] value)))

(deftest construction-kernel-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable construction-kernel record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (select-keys expected [:live-records :wires]))]
            (testing (pr-str path)
              (is (= (get-in expected path) (get-in actual path))))))))))
