(ns futon3c.diagramprover.wm-wire-producer-construction-live-digest-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-construction-live-digest-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-decision)
(def wire-id [:construction-construct :selection-candidate-derivations :construction-receipt])

(defn build-record []
  (let [ordinary (support/live-digest)
        absent (support/derivation :absent)
        different (support/derivation :different)]
    {:producer producer
     :operation operation
     :inputs {:functions ['futon3c.diagramprover.wm-wire-construction-support/derivation
                          'futon3c.diagramprover.wm-wire-construction-support/live-digest]
              :mutations [:none :absent :different]}
     :fields
     {:wire-id wire-id
      :source-record (assoc (first support/live-records-read)
                            :writer-path support/receipt-path
                            :reader-path support/digest-path)
      :writer (:writer ordinary)
      :reader (:reader ordinary)
      :recomputed (:recomputed ordinary)
      :writer-present? (some? (:writer ordinary))
      :writer-typed-absence? (w/typed-absence? (:writer ordinary))
      :received? (w/received? ordinary)
      :writer-recomputed? (= (:writer ordinary) (:recomputed ordinary))
      :receipt-machine-constructed? (= :machine-constructed
                                       (get-in ordinary [:receipt :kind]))
      :outer-receipt-absent? (nil? (:outer-receipt ordinary))
      :entry-retains-receipt? (= :machine-constructed
                                 (get-in ordinary [:entry :construction :kind]))
      :interventions
      {:absent {:received? (w/received? absent)}
       :different {:reader-present? (some? (:reader different))
                   :received? (w/received? different)}}
      :second-layer {:reader-present? (some? (:reader different))
                     :received? (w/received? different)}}
     :left-out {}}))

(defn- record-text [record] (str (pr-str record) "\n"))

(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "construction-live-digest@"
                           (subs (sha256 text) 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(def checked-field-paths
  [[:wire-id]
   [:source-record]
   [:writer]
   [:reader]
   [:recomputed]
   [:writer-present?]
   [:writer-typed-absence?]
   [:received?]
   [:writer-recomputed?]
   [:receipt-machine-constructed?]
   [:outer-receipt-absent?]
   [:entry-retains-receipt?]
   [:interventions :absent :received?]
   [:interventions :different :reader-present?]
   [:interventions :different :received?]
   [:second-layer :reader-present?]
   [:second-layer :received?]])

(deftest construction-live-digest-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first
                              (filter #(.startsWith (.getName %)
                                                    "construction-live-digest@")
                                      (.listFiles
                                       (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path checked-field-paths]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
