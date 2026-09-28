(ns futon3c.diagramprover.wm-wire-producer-construction-decision
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-construction-products :as products]
            [futon3c.diagramprover.wm-wire-construction-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-construction-decision-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-decision)
(def cases
  [{:field :want :wire-id [:construction-assemble-one :r9-decision [:want {:record :cascade-problem}]]}
   {:field :construction-receipt :wire-id [:construction-construct :r9-decision :construction-receipt]}])

(defn- observation [field mutation]
  (let [o (support/decision field mutation)]
    {:writer (:writer o) :reader (:reader o)
     :action-present? (some? (get-in o [:result :decision :action]))
     :refusal (get-in o [:result :refusal])}))

(defn- want-product []
  (let [before (products/decision-product :want :none)
        after (products/decision-product :want :different)
        v (:scores before) v-prime (:scores after)]
    {:before before :after after
     :competing-before? (< 1 (count v))
     :candidate-counts [(count v) (count v-prime)]
     :scores-numeric? (every? number? (concat v v-prime))
     :scores-differ? (not= v v-prime)}))

(defn- case-fields [{:keys [field]}]
  (cond-> {:primary (observation field :none)
           :interventions {:absent (observation field :absent)
                           :different (observation field :different)}}
    (= field :want) (assoc :second-layer (want-product))))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:target :futon2.report.cascade-decision-test/tick-1-target
            :sources :futon2.report.cascade-decision-test/tick-1-sources
            :decision-options :futon2.report.cascade-decision-test/live-c-opts
            :cases cases :mutations [:none :absent :different]}
   :wires (into {} (map (juxt :wire-id case-fields) cases))
   :left-out {:full-ranked-decision "readers check only action presence and the recorded writer/reader and score products"
              :temporary-paths "construction fixtures use no reader-checked temporary path"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "construction-decision@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "construction-decision@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest construction-decision-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable construction-decision record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (:wires expected))]
            (testing (pr-str path)
              (is (= (get-in expected (into [:wires] path))
                     (get-in actual (into [:wires] path)))))))))))
