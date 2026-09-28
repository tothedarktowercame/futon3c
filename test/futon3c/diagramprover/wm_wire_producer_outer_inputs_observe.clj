(ns futon3c.diagramprover.wm-wire-producer-outer-inputs-observe
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.outer-cascade :as outer-cascade]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-outer-inputs-support :as outer-support]
            [futon3c.diagramprover.wm-wire-outer-record-products-15a :as record-products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-outer-inputs-observe-test)
(def operation 'futon2.aif.outer-cascade/select)
(def fields
  [:clock-lineage
   :next-step
   :publication-observed
   :enactment-records
   :pair-overlap])

(defn- primary [field]
  (let [ordinary (outer-support/observe field :none)
        absent (outer-support/observe field :absent)
        missing (outer-support/observe field :missing)
        different (outer-support/observe field :different)]
    {:writer (:writer ordinary)
     :reader (:reader ordinary)
     :received? (w/received? ordinary)
     :unchanged-law? (:unchanged-law? ordinary)
     :law-uses (get-in ordinary [:record :law-uses])
     :interventions
     {:absent {:received? (w/received? absent)
               :reader (:reader absent)
               :unchanged-law? (:unchanged-law? absent)}
      :missing {:received? (w/received? missing)
                :reader (:reader missing)
                :unchanged-law? (:unchanged-law? missing)}
      :different {:reader-present? (some? (:reader different))
                  :received? (w/received? different)
                  :unchanged-law? (:unchanged-law? different)}}}))

(defn- record-relations [field]
  (let [entries (record-products/entries)
        per-entry? (contains? #{:next-step :pair-overlap} field)
        values (record-products/carriers field entries)
        path (cond-> [:target-selection :inputs field] per-entry? (conj "A"))
        results (mapv (fn [value]
                        (outer-cascade/select
                         (cond-> {:field {:considered entries :feasible entries :exclusions []}
                                  :seed 42}
                           per-entry? (assoc-in [:field :feasible 0 field] value)
                           (not per-entry?) (assoc field value))))
                      values)
        baseline (first results)
        law-paths [:support :posterior :g :g-defined-on :law :draw :chosen]]
    {:mixed-law? (= :mixed (get-in baseline [:target-selection :law]))
     :posterior-varies? (not= (get-in baseline [:target-selection :posterior "A"])
                              (get-in baseline [:target-selection :posterior "B"]))
     :first-two-values-differ? (not= (first values) (second values))
     :values-recorded? (every? true? (map #(= %1 (get-in %2 path)) values results))
     :selection-law-stable?
     (every? true?
             (map #(= (select-keys (:target-selection baseline) law-paths)
                      (select-keys (:target-selection %) law-paths))
                  results))
     :choice-stable? (every? #(= (:chosen-target baseline) (:chosen-target %)) results)
     :third-is-absence? (if (= 3 (count values)) (contains? (last values) :absent) true)
     :third-recorded? (if (= 3 (count values)) (= (last values) (get-in (last results) path)) true)}))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:fields fields :tamper-modes [:none :absent :missing :different]}
   :fields {:live-reader-absent? (outer-support/live-reader-absent?)
            :wires (into {} (map (juxt identity primary) fields))
            :second-layer
            (into {} (for [field (remove #{:clock-lineage} fields)]
                       [field (record-relations field)]))}
   :left-out {:selection-products "readers assert the individually named recording and selection-law relations"
              :clock-http-response "the writer uses isolated HTTP ports; the durable clock properties it writes are recorded"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "outer-inputs-observe@" (subs (sha256 text) 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- checked-field-paths [record-fields]
  (concat
   [[:live-reader-absent?]]
   (mapcat
    (fn [field]
      (concat
       (map #(vector :wires field %) [:writer :reader :received? :unchanged-law? :law-uses])
       (for [mode [:absent :missing]
             k [:received? :reader :unchanged-law?]]
         [:wires field :interventions mode k])
       (for [k [:reader-present? :received? :unchanged-law?]]
         [:wires field :interventions :different k])))
    fields)
   (for [field (remove #{:clock-lineage} fields)
         k (keys (get-in record-fields [:second-layer field]))]
     [:second-layer field k])))

(deftest outer-inputs-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "outer-inputs-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
