(ns futon3c.diagramprover.wm-wire-producer-plan-observe-test
  "Producer for the plan-observe group: one run of the support's real
  outer-loop/plan-from-field! calls (and the entry/loop product helpers the
  readers' second layers used), recorded once. The ten wire readers then
  read the record and load no product code. See
  storage/test-registry/producer-2/REPORT.md."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-entry-products-10a :as entry-products]
            [futon3c.diagramprover.wm-wire-loop-products-12a :as loop-products]
            [futon3c.diagramprover.wm-wire-plan-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-plan-observe-test)
(def operation 'futon2.aif.outer-loop/plan-from-field!)

(def wire-ids
  [[:loop-entry :loop-plan :trigger]
   [:r1-outer-cascade :flight-entry :target-selection]
   [:r1-outer-cascade :flight-plan :chosen-target]
   [:r1-outer-cascade :flight-plan :draw-seed]
   [:r1-outer-cascade :flight-plan :target-selection]
   [:r1-outer-cascade :loop-plan :chosen-target]
   [:r1-outer-cascade :loop-plan :draw-seed]
   [:r1-outer-cascade :loop-plan :target-selection]
   [:r1-outer-cascade :r1-test :chosen-target]
   [:r1-outer-cascade :r1-test :target-selection]])

(def wire-args
  {[:loop-entry :loop-plan :trigger]
   {:reader :trigger :field :trigger :different :different-clock}
   [:r1-outer-cascade :flight-entry :target-selection]
   {:reader :entry :field :target-selection :different {:chosen "M-other"}}
   [:r1-outer-cascade :flight-plan :chosen-target]
   {:reader :plan :field :chosen-target :different "M-other"}
   [:r1-outer-cascade :flight-plan :draw-seed]
   {:reader :plan :field :draw-seed :different 43}
   [:r1-outer-cascade :flight-plan :target-selection]
   {:reader :plan :field :target-selection :different {:chosen "M-other"}}
   [:r1-outer-cascade :loop-plan :chosen-target]
   {:reader :loop :field :chosen-target :different "M-other"}
   [:r1-outer-cascade :loop-plan :draw-seed]
   {:reader :loop :field :draw-seed :different 43}
   [:r1-outer-cascade :loop-plan :target-selection]
   {:reader :loop :field :target-selection :different {:chosen "M-other"}}
   [:r1-outer-cascade :r1-test :chosen-target]
   {:reader :test :field :chosen-target :different "M-other"}
   [:r1-outer-cascade :r1-test :target-selection]
   {:reader :test :field :target-selection :different {:chosen "M-other"}}})

(defn- live-records-pinned?
  "The boolean content of support/assert-live-records: each pin's SHA-256
  matches and no entry in the record carries a selector key."
  []
  (every? (fn [{:keys [path sha256]}]
            (and (= sha256 (w/sha256-file path))
                 (let [r (w/read-record path)]
                   (not-any? #(and (map? %) (some (set (keys %))
                                                  [:chosen-target :draw-seed :target-selection]))
                             (tree-seq coll? seq r)))))
          support/live-records-read))

(defn- primary [wire-id]
  (let [{:keys [reader field different]} (get wire-args wire-id)
        none (support/observe reader field identity)
        absent (support/observe reader field #(assoc % field {:absent :not-carried}))
        changed (support/observe reader field #(assoc % field different))]
    {:writer (:writer none)
     :reader (:reader none)
     :received? (w/received? none)
     :interventions
     {:absent {:writer-present? (some? (:writer absent))
               :reader-typed-absence? (w/typed-absence? (:reader absent))
               :received? (w/received? absent)}
      :different {:writer-present? (some? (:writer changed))
                  :reader-present? (some? (:reader changed))
                  :received? (w/received? changed)}}}))

(defn- entry-relations []
  (into {}
        (for [mission entry-products/missions
              :let [r (entry-products/products mission :target-selection)]]
          [(:target mission)
           {:written-equal-products? (= (:written r) (:products r))
            :products-differ? (apply not= (:products r))
            :flights-equal? (apply = (:flights r))
            :wants-equal? (apply = (:wants r))
            :wants-nonempty? (boolean (seq (get-in r [:wants 0 :wants])))
            :targets-echo? (= [(:target mission) (:target mission)] (:target r))
            :paths-echo? (= [(:path mission) (:path mission)] (:path r))}])))

(defn- loop-chosen-relations []
  (let [[a b] (loop-products/products :chosen-target)
        pa (get-in a [:result :plan]) pb (get-in b [:result :plan])
        mission-for (fn [p] (first (filter #(= (:target %) (:requisition p))
                                           loop-products/missions)))]
    {:relations
     {:written-equal? (= (:written a) (:written b))
      :requisitions-differ? (not= (:requisition pa) (:requisition pb))
      :want-sources-match-missions?
      (every? (fn [p]
                (let [m (mission-for p)]
                  (= (select-keys m [:repo :path])
                     (select-keys (:want-source p) [:repo :path]))))
              [pa pb])
      :want-source-paths-differ? (not= (get-in pa [:want-source :path])
                                       (get-in pb [:want-source :path]))
      :in-view-counts-3-and-6? (= #{3 6} (set (map #(count (get-in % [:wants :in-view]))
                                                   [pa pb])))
      :wants-differ? (not= (:wants pa) (:wants pb))}
     :missing-target
     (let [[_ mb] (loop-products/products :missing)]
       {:planner-not-called? (= {} (:handed mb))
        :plan-typed-absence? (= {:absent :chosen-target-not-in-field
                                 :chosen-target "M-not-in-field"
                                 :missing [:considered-entry]}
                                (get-in mb [:result :plan]))
        :target-selection-kept? (some? (get-in mb [:result :target-selection]))})}))

(defn- loop-draw-seed-relations []
  (let [[a b] (loop-products/products :draw-seed)
        pa (get-in a [:result :plan]) pb (get-in b [:result :plan])]
    {:written-equal? (= (:written a) (:written b))
     :placement-seeds-differ? (not= (get-in pa [:placement :draw-seed])
                                    (get-in pb [:placement :draw-seed]))
     :handed-seed-placed? (= (get-in b [:handed :draw-seed])
                             (get-in pb [:placement :draw-seed]))
     :other-placement-fields-equal? (= (update pa :placement dissoc :draw-seed)
                                       (update pb :placement dissoc :draw-seed))}))

(defn- loop-target-selection-relations []
  (let [[a b] (loop-products/products :target-selection)
        pa (get-in a [:result :plan]) pb (get-in b [:result :plan])]
    {:written-equal? (= (:written a) (:written b))
     :placement-selections-differ? (not= (get-in pa [:placement :target-selection])
                                         (get-in pb [:placement :target-selection]))
     :handed-selection-placed? (= (get-in b [:handed :target-selection])
                                  (get-in pb [:placement :target-selection]))
     :selection-record-matches-placement? (= (:target-selection pb)
                                             (get-in pb [:placement :target-selection]))
     :other-fields-equal? (= (update (dissoc pa :target-selection) :placement dissoc :target-selection)
                             (update (dissoc pb :target-selection) :placement dissoc :target-selection))}))

(defn build-record []
  {:producer producer :operation operation
   :inputs {:wire-ids wire-ids :modes [:none :absent :different]
            :support-calls (into {}
                                 (map (fn [[id {:keys [reader field]}]] [id [reader field]]))
                                 wire-args)}
   :fields {:live-records-pinned? (live-records-pinned?)
            :wires (into {} (map (juxt identity primary) wire-ids))
            :second-layer
            {[:r1-outer-cascade :flight-entry :target-selection] (entry-relations)
             [:r1-outer-cascade :loop-plan :chosen-target] (loop-chosen-relations)
             [:r1-outer-cascade :loop-plan :draw-seed] (loop-draw-seed-relations)
             [:r1-outer-cascade :loop-plan :target-selection] (loop-target-selection-relations)}}
   :left-out {}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers" (str "plan-observe@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- leaf-paths [m]
  (mapcat (fn [[k v]]
            (if (map? v) (map #(into [k] %) (leaf-paths v)) [[k]]))
          m))

(defn- checked-field-paths [fields]
  (concat
   [[:live-records-pinned?]]
   (mapcat
    (fn [wire-id]
      (concat
       (map #(vector :wires wire-id %) [:writer :reader :received?])
       (for [field [:writer-present? :reader-typed-absence? :received?]]
         [:wires wire-id :interventions :absent field])
       (for [field [:writer-present? :reader-present? :received?]]
         [:wires wire-id :interventions :different field])
       (for [path (leaf-paths (get-in fields [:second-layer wire-id] {}))]
         (into [:second-layer wire-id] path))))
    wire-ids)))

(deftest plan-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "plan-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
