(ns futon3c.diagramprover.wm-wire-producer-fold-out-decision-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as fold-support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-fold-out-decision-test)
(def operations
  ['futon2.aif.wm.cascade-decision/cascade-decision
   'futon2.aif.flight/run!
   'futon2.report.war-machine/judge
   'futon2.aif.enactment-habit/fold])
(def kinds [:conditioning-steps :enactment-fold])

(defn- primary [kind]
  (let [ordinary (fold-support/decision kind :none)
        absent (fold-support/decision kind :absent)
        different (fold-support/decision kind :different)]
    {:writer (:writer ordinary)
     :reader (:reader ordinary)
     :writer-present? (some? (:writer ordinary))
     :writer-typed-absence? (w/typed-absence? (:writer ordinary))
     :received? (w/received? ordinary)
     :ordinary
     (case kind
       :conditioning-steps
       {:flight-step-present? (= :present (get-in ordinary [:flight :enactments 0 :step :status]))
        :admitted-prefix? (boolean (some #(= :admitted (:conditioning-status %))
                                         (vals (:prefixes ordinary))))}
       :enactment-fold
       {:one-record-sample? (= {:records 1 :samples 1} (:reader ordinary))
        :receipt-present? (= :present (get-in ordinary [:read :receipt :status]))})
     :interventions
     {:absent
      (merge {:received? (w/received? absent)}
             (case kind
               :conditioning-steps
               {:all-no-flight-records?
                (every? #(= :no-flight-records (:conditioning-status %))
                        (vals (:prefixes absent)))}
               :enactment-fold
               {:zero-records-samples? (= {:records 0 :samples 0} (:reader absent))
                :no-enactment-fold? (= :no-enactment-fold (get-in absent [:read :receipt :reason]))}))
      :different
      (merge {:received? (w/received? different)
              :writer-reader-differ? (not= (:writer different) (:reader different))}
             (case kind
               :conditioning-steps
               {:f-incremented? (= (inc (get-in different [:writer :f]))
                                    (get-in different [:reader :f]))}
               :enactment-fold
               {:two-records-samples? (= {:records 2 :samples 2} (:reader different))}))}}))

(defn build-record []
  (fold-support/assert-live-pins)
  (let [primary-fields (into {} (map (juxt identity primary) kinds))]
    {:producer producer
     :operation operations
     :inputs {:kinds kinds :modes [:none :absent :different]}
     :fields {:live-pins-valid? true
              :wires primary-fields
              :second-layer
              {:conditioning-steps (get-in primary-fields [:conditioning-steps :interventions :absent])
               :enactment-fold (get-in primary-fields [:enactment-fold :interventions :different])}}
     :left-out {:temporary-flight-and-tick-paths "the fixture persists under a temporary directory; readers check step and receipt values"
                :decision-results "readers check the individually named prefix, fold, receipt, and carrier relations"}}))

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "fold-out-decision@" (subs (sha256 text) 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- checked-field-paths [fields]
  (concat
   [[:live-pins-valid?]]
   (for [kind kinds
         field [:writer :reader :writer-present? :writer-typed-absence? :received?]]
     [:wires kind field])
   (for [kind kinds
         field (keys (get-in fields [:wires kind :ordinary]))]
     [:wires kind :ordinary field])
   (for [kind kinds
         mode [:absent :different]
         field (keys (get-in fields [:wires kind :interventions mode]))]
     [:wires kind :interventions mode field])
   (for [kind kinds
         field (keys (get-in fields [:second-layer kind]))]
     [:second-layer kind field])))

(deftest fold-out-decision-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "fold-out-decision@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
