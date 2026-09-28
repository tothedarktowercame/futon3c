(ns futon3c.diagramprover.wm-wire-producer-ask-out-live-census-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support]
            [futon3c.diagramprover.wm-wire-click-products-8a :as products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-ask-out-live-census-test)
(def operation
  {:writer 'futon2.aif.flight/click-wants
   :readers ['futon2.aif.flight/judge-opts
             'futon2.aif.wm.construction-inputs/flight-assembly-input]})

(def cases
  [{:reader :opts :field :universe
    :wire-id [:flight-click-wants :flight-judge-opts :universe]}
   {:reader :opts :field :wants
    :wire-id [:flight-click-wants :flight-judge-opts :wants]}
   {:reader :assembly :field :universe
    :wire-id [:flight-click-wants :tick-flight-assembly :universe]}
   {:reader :assembly :field :wants
    :wire-id [:flight-click-wants :tick-flight-assembly :wants]}])

(defn- stable-click [reader field mutation]
  (select-keys (support/click reader field mutation) [:writer :reader]))

(defn- product-fields [reader field]
  (let [r (products/products reader field)
        [before after] (:products r)]
    {:products (:products r)
     :written (:written r)
     :before-present? (boolean (seq before))
     :written-equals-products? (= (:written r) (:products r))
     :products-differ? (not= before after)
     :other-output-unchanged? (apply = (:unchanged r))
     :context (:context r)}))

(defn- case-fields [{:keys [reader field]}]
  {:primary (stable-click reader field :none)
   :interventions
   {:absent (stable-click reader field :absent)
    :different (stable-click reader field :different)}
   :second-layer (product-fields reader field)})

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:target-id support/target-id
            :mission-fixture "mission-criteria/M-futon-seams@futon3c-d05cb755.md"
            :cases (mapv #(select-keys % [:reader :field :wire-id]) cases)
            :mutations [:none :absent :different]}
   :live-census (support/live-census)
   :wires (into {} (map (juxt :wire-id case-fields) cases))
   :left-out {:temporary-store-paths "the click support creates fresh hermetic stores; no reader checks their paths"
              :flight-store-records "the readers check only the named writer/reader fields and preservation relations"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "ask-out-live-census@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "ask-out-live-census@" (subs sha 0 12) ".edn"))]
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

(deftest ask-out-live-census-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable ask-out-live-census record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (select-keys expected [:live-census :wires]))]
            (testing (pr-str path)
              (is (= (get-in expected path) (get-in actual path))))))))))
