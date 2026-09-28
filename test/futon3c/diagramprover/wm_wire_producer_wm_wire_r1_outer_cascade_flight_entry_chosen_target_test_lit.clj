(ns futon3c.diagramprover.wm-wire-producer-wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit
  "Producer for the [:r1-outer-cascade :flight-entry :chosen-target] wire.
  Runs the real futon2.aif.outer-cascade/select (the writer) over the
  reader's FIELD with seed 42, then the real
  futon2.aif.flight-driver/resolve-target (the reader) handed the writer's
  :chosen-target — plus the unable-choice and different-target
  interventions — and the second-layer pipeline
  (wm-wire-target-identity-products-10a2/products, normalized: function
  values -> :<fn>, the per-run store path -> \"<store>\"), and asserts the
  observation equals the committed record."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.outer-cascade :as oc]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-identity-products-10a2 :as products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit-test)
(def operation '[futon2.aif.outer-cascade/select futon2.aif.flight-driver/resolve-target])
(def stem "wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit")
(def wire-id [:r1-outer-cascade :flight-entry :chosen-target])
(def mutations [:unable :different])

(def field
  "The reader's FIELD: two eligible targets and one a requisition makes
  ineligible (outer_cascade_test.clj's fixture shape)."
  {:considered [{:target "M-a" :kind :mission} {:target "M-b" :kind :mission}
                {:target "M-c" :kind :mission}]
   :feasible [{:target "M-b" :kind :mission :next-step :ready :eligible true}
              {:target "M-c" :kind :mission :next-step :read-criteria :eligible false
               :ineligible-reason :requisition/mooted}
              {:target "M-a" :kind :mission :next-step :ask-interpretation :eligible true}]
   :exclusions []})

(defn- choose [field seed]
  (oc/select {:field field :seed seed :trigger :wallclock-cron}))

(defn- primary []
  (let [sel (choose field 42)
        writer (:chosen-target sel)
        r (driver/resolve-target {:chosen-target writer})]
    {:writer writer
     :reader (:target r)
     :target-source (:target-source r)
     :received? (w/received? {:writer writer :reader (:target r)})}))

(defn- unable []
  (let [f (update field :feasible
                  (fn [fs] (mapv #(assoc % :eligible false :ineligible-reason :requisition/mooted) fs)))
        sel (choose f 3)]
    {:chosen-target-present? (contains? sel :chosen-target)
     :chosen (get-in sel [:target-selection :chosen])
     :received? (w/received? {:writer (get-in sel [:target-selection :chosen])
                              :reader (get-in sel [:target-selection :chosen])})}))

(defn- different []
  (let [writer (:chosen-target (choose field 42))
        other (first (disj #{"M-a" "M-b"} writer))
        r (driver/resolve-target {:chosen-target other})]
    {:writer writer
     :other other
     :reader (:target r)
     :received? (w/received? {:writer writer :reader (:target r)})}))

(defn- normalize [root x]
  (walk/postwalk
   (fn [v]
     (cond
       (fn? v) :<fn>
       (and (string? v) root (.contains ^String v root)) (str/replace v root "<store>")
       :else v))
   x))

(defn- second-layer []
  (let [p (products/products)
        root (get-in p [:tokens :flights 0 :want-source :store])
        n (normalize root p)
        [fa fb] (:flights (:tokens n))]
    {:targets products/targets
     :locator products/locator
     :products n
     :flights-equal-modulo-target? (= (dissoc fa :target) (dissoc fb :target))}))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:field 'futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-chosen-target-test/field
            :seed 42
            :unable-seed 3
            :second-layer 'futon3c.diagramprover.wm-wire-target-identity-products-10a2/products
            :wire-id wire-id
            :mutations (into [:none] mutations)}
   :wires {wire-id {:primary (primary)
                    :interventions {:unable (unable) :different (different)}
                    :second-layer (second-layer)}}
   :left-out {:raw-products "the products map carries function values (:read-text, :observe) and the per-run store path; the record keeps the normalized form (fns -> :<fn>, store path -> \"<store>\") plus :flights-equal-modulo-target?, computed before normalization, so the reader's equality relation is preserved"
              :live-record-verification "the reader re-verifies the live pins itself (sha256 and the absence of :chosen-target anywhere) through w/sha256-file and w/read-record; the entries are carried for the wire map's :live-records-read"}})

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

(deftest wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable record for this stem")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (:wires expected))]
            (testing (pr-str path)
              (is (= (get-in expected (into [:wires] path))
                     (get-in actual (into [:wires] path)))))))))))
