(ns futon3c.diagramprover.wm-wire-producer-temporal-courier-publication-paths
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-envelope-products-13a :as envelope]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as courier]
            [futon3c.diagramprover.wm-wire-temporal-run-products-14b :as run-products]
            [futon3c.diagramprover.wm-wire-temporal-storage-products :as storage])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-temporal-courier-publication-paths-test)
(def operation 'futon2.aif.flight/run!)
(def wire-ids
  [[:flight-run :flight-judge-opts [:temporal-receipt {:record :enactment-entry}]]
   [:r0-enact-step :flight-run :temporal-receipt]
   [:temporal-finalize :temporal-envelope [:temporal-receipt {:record :enactment}]]
   [:temporal-finalize :temporal-write-once [:temporal-receipt {:record :enactment}]]])

(defn- stable [value]
  (cond
    (map? value) (into (empty value) (map (fn [[k v]] [k (stable v)])) value)
    (vector? value) (mapv stable value)
    (set? value) (set (map stable value))
    (seq? value) (mapv stable value)
    (and (string? value) (.contains value "/temporal-courier-wire-")) :temporary-path
    :else value))

(defn- primary [wire-id]
  (let [none (courier/observe wire-id :none)]
    {:writer (stable (:writer none))
     :reader (stable (:reader none))
     :product-present? (some? (:product none))
     :received? (w/received? (update-vals (select-keys none [:writer :reader]) stable))
     :interventions
     (into {}
           (for [mode [:absent :different]
                 :let [result (courier/observe wire-id mode)]]
             [mode {:writer-present? (some? (:writer result))
                    :received? (w/received? (update-vals (select-keys result [:writer :reader]) stable))}]))}))

(defn- publication-paths-fields []
  (let [{:keys [published refused persisted read]} (courier/publication-paths)]
    {:published published
     :refused refused
     :persisted (stable persisted)
     :read-selected (stable (select-keys read [:status :reason :detail]))}))

(defn- judge-relations []
  (let [{:keys [inputs products expected envelopes absence]} (run-products/judge-products)
        posteriors (mapv #(get-in % [:record :posterior]) envelopes)]
    {:expected-a-reaches-reader? (= (first expected) (first envelopes))
     :expected-b-reaches-reader? (= (second expected) (second envelopes))
     :posterior-counts? (= [1 0] (mapv #(get % #{[:courier :done]} 0) (take 2 posteriors)))
     :posteriors-differ? (not= (first posteriors) (second posteriors))
     :absence-reaches-reader? (= absence (last envelopes))
     :absent-posterior-nil? (nil? (last posteriors))
     :inputs-differ-only-in-receipt? (apply = (map #(update-in % [:enactments 1] dissoc :temporal-receipt) inputs))
     :products-differ-only-in-previous? (apply = (map #(update % :flight dissoc :temporal-previous) products))}))

(defn- run-relations []
  (let [{:keys [values stored other-products products]} (run-products/run-products :temporal-receipt)]
    {:values-present? (every? some? values)
     :values-differ? (not= (first values) (second values))
     :stored-values? (= values stored)
     :other-products-stable? (= (first other-products) (second other-products))
     :no-progress? (= [:no-progress :no-progress] (mapv :status products))}))

(defn- envelope-relations []
  (let [{:keys [original changed a b]} (envelope/products :temporal-receipt)]
    {:other-record-fields-equal? (= (dissoc original :temporal-receipt) (dissoc changed :temporal-receipt))
     :a-basis-posterior? (= :posterior (get-in a [:envelope :basis]))
     :b-envelope-nil? (nil? (:envelope b))
     :b-read-absent? (= {:status :absent :reason :no-previous-posterior}
                        (select-keys (:read b) [:status :reason]))
     :b-publication-equals-read? (= (:publication b) (:read b))
     :b-consumed-refusal? (= :no-previous-posterior (get-in b [:consumed :conditioning-status]))
     :b-consumed-no-belief? (not (contains? (:consumed b) :continuation-belief))}))

(defn- storage-relations []
  (let [{:keys [final absent a b repeat event-repeat disk-a disk-b]} (storage/products)]
    {:only-receipt-changed? (= (dissoc final :temporal-receipt) (dissoc absent :temporal-receipt))
     :final-persisted? (= final disk-a (:record a) (:record repeat) (:record event-repeat))
     :absent-persisted? (= absent disk-b (:record b))
     :disk-statuses? (= [:published :absent]
                        (mapv #(get-in % [:temporal-receipt :status]) [disk-a disk-b]))
     :posterior-stable? (= (:temporal-posterior disk-a) (:temporal-posterior disk-b))
     :receipt-a-matches? (= (:temporal-receipt disk-a) (dissoc (:receipt a) :record-path :digest))
     :receipt-b-matches? (= (:temporal-receipt disk-b) (dissoc (:receipt b) :record-path :digest))
     :digests-differ? (not= (get-in a [:receipt :digest]) (get-in b [:receipt :digest]))
     :repeat-refused? (= :temporal-record-already-exists (get-in repeat [:receipt :reason]))
     :event-repeat-refused? (= :event-already-consumed (get-in event-repeat [:receipt :reason]))}))

(defn build-record []
  {:producer producer :operation operation
   :inputs {:wire-ids wire-ids :modes [:none :absent :different]}
   :fields {:live-absent? (courier/live-absent?)
            :publication-paths (publication-paths-fields)
            :wires (into {} (map (juxt identity primary) wire-ids))
            :second-layer
            {(nth wire-ids 0) (judge-relations)
             (nth wire-ids 1) (run-relations)
             (nth wire-ids 2) (envelope-relations)
             (nth wire-ids 3) (storage-relations)}}
   :left-out {:temporary-paths "replaced by :temporary-path; readers assert the recorded writer/reader relation"
              :products "readers check product presence, so the record stores :product-present?"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "temporal-courier-publication-paths@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- checked-field-paths [fields]
  (concat
   [[:live-absent?]]
   (map #(vector :publication-paths %) [:published :refused :persisted :read-selected])
   (mapcat
    (fn [wire-id]
      (concat
       (map #(vector :wires wire-id %) [:writer :reader :product-present? :received?])
       (for [mode [:absent :different]
             field [:writer-present? :received?]]
         [:wires wire-id :interventions mode field])
       (for [field (keys (get-in fields [:second-layer wire-id]))]
         [:second-layer wire-id field])))
    wire-ids)))

(deftest temporal-courier-publication-paths-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "temporal-courier-publication-paths@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
