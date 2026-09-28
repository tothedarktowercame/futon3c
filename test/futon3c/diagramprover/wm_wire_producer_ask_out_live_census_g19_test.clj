(ns futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g19-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g19-test)
(def operation
  {:writer 'futon2.aif.want-interpretation/merge-published
   :readers ['futon2.aif.cascade-problems/assemble-one
             'futon2.aif.interpretation-construction/construct]})

(def cases
  [{:kind :assemble
    :wire-id [:ask-merge-published :construction-assemble-one :interpretations]}
   {:kind :construct
    :wire-id [:ask-merge-published :construction-construct :interpretations]}])

(defn- observation [kind mutation]
  (let [o (support/interpretations kind mutation)]
    {:writer (:writer o)
     :reader (:reader o)
     :assembled-kind (get-in o [:assembled :kind])
     :constructed-status (get-in o [:constructed :status])
     :constructed-kind (get-in o [:constructed :kind])
     :constructed-precedence (get-in o [:constructed :candidates 0 :precedence])
     :tokens-contain-document? (contains? (:tokens o) support/document)
     :constructed-candidates-present?
     (boolean (seq (get-in o [:assembled :constructed-candidates])))
     :reader-contains-argue? (contains? (or (:reader o) #{}) support/argue)}))

(defn- case-fields [{:keys [kind]}]
  {:primary (observation kind :none)
   :interventions {:absent (observation kind :absent)
                   :different (observation kind :different)}})

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:target-id support/target-id
            :mission-fixture "mission-criteria/M-futon-seams@futon3c-d05cb755.md"
            :cases cases
            :mutations [:none :absent :different]}
   :live-census (support/live-census)
   :wires (into {} (map (juxt :wire-id case-fields) cases))
   :left-out
   {:temporary-store-paths
    "the support creates fresh hermetic stores; no reader checks their paths"
    :full-assembled-and-constructed-values
    "readers check the retained statuses, precedence, token and candidate relations"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "ask-out-live-census-g19@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record)
        sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "ask-out-live-census-g19@"
                           (subs sha 0 12) ".edn"))]
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

(deftest ask-out-live-census-g19-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files))
            "exactly one immutable ask-out-live-census-g19 record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (select-keys expected [:live-census :wires]))]
            (testing (pr-str path)
              (is (= (get-in expected path) (get-in actual path))))))))))
