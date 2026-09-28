(ns futon3c.diagramprover.wm-wire-producer-selection-out-refusal
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-selection-out-refusal-test)

(defn build-record []
  (let [ordinary (support/refusal :none)
        absent (support/refusal :absent)
        different (support/refusal :different)]
    {:producer producer
     :operation 'futon2.aif.wm.cascade-decision/cascade-decision
     :inputs {:live-c {:derived {:refusals [{:kind :source-not-available}]}}
              :mutations [:none :absent :different]}
     :fields {:census-present? (boolean (seq (support/census)))
              :writer (:writer ordinary) :reader (:reader ordinary)
              :writer-present? (some? (:writer ordinary))
              :writer-typed-absence? (w/typed-absence? (:writer ordinary))
              :received? (w/received? ordinary)
              :reader-live-c-refused? (= :live-c-refused (:reader ordinary))
              :interventions
              {:absent {:received? (w/received? absent) :reader-absent? (nil? (:reader absent))}
               :different {:received? (w/received? different)
                           :reader-different-refusal? (= :different-refusal (:reader different))}}}
     :left-out {:runner-result "the reader checks only the refusal kind copied through the real runner checkpoint"
                :census "the reader checks only that the pinned-record census is nonempty"}}))

(defn- text [x] (str (pr-str x) "\n"))
(defn- sha256 [s]
  (apply str (map #(format "%02x" (bit-and % 0xff))
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write-record! [record]
  (let [content (text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "selection-out-refusal@" (subs (sha256 content) 0 12) ".edn"))]
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file content) (println (.getPath file))))

(def field-paths
  [[:census-present?] [:writer] [:reader] [:writer-present?]
   [:writer-typed-absence?] [:received?] [:reader-live-c-refused?]
   [:interventions :absent :received?] [:interventions :absent :reader-absent?]
   [:interventions :different :received?]
   [:interventions :different :reader-different-refusal?]])

(deftest selection-out-refusal-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "selection-out-refusal@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path field-paths]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
