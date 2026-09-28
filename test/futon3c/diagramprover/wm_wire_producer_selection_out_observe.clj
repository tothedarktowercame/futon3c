(ns futon3c.diagramprover.wm-wire-producer-selection-out-observe-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-selection-handoff-products :as products]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-selection-out-observe-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-decision)
(def cases
  [{:kind :candidate-increment :wire-id [:r9-selection-law :r7-increment :candidate]}
   {:kind :candidate-checker :wire-id [:r9-selection-law :wc-checker :candidate]}
   {:kind :verdict :wire-id [:wc-checker :r7-increment :wc-verdict]}])

(defn- observation [kind mutation]
  (let [o (support/observe kind mutation)]
    (case kind
      :candidate-increment
      {:writer (:writer o) :reader (:reader o)
       :delta (get-in o [:result :delta]) :policy-key (get-in o [:result :policy-key])}
      :candidate-checker
      {:writer (:writer o) :reader (:reader o) :verdict (:verdict o)}
      :verdict
      {:writer (:writer o) :reader (:reader o)
       :delta (get-in o [:result :delta]) :wc-failures (get-in o [:result :wc-failures])})))

(defn- increment-relations []
  (let [a (products/increment-product false)
        b (products/increment-product true)]
    {:before {:candidate (:candidate a) :posterior (:posterior a)
              :receipt-record-id (get-in a [:receipt :record-id])}
     :after {:candidate (:candidate b) :posterior (:posterior b)
             :receipt-record-id (get-in b [:receipt :record-id])}
     :other-inputs-equal? (= (:other-inputs a) (:other-inputs b))
     :candidates-differ? (not= (:candidate a) (:candidate b))
     :posteriors-differ? (not= (:posterior a) (:posterior b))
     :before-candidate-recorded? (= (:candidate a) (get-in a [:receipt :record-id 1]))
     :after-candidate-recorded? (= (:candidate b) (get-in b [:receipt :record-id 1]))
     :deltas-one? (= 1 (get-in a [:receipt :delta]) (get-in b [:receipt :delta]))
     :other-receipt-fields-equal? (= (dissoc (:receipt a) :record-id)
                                     (dissoc (:receipt b) :record-id))}))

(defn- case-fields [{:keys [kind]}]
  (cond-> {:primary (observation kind :none)
           :interventions {:absent (observation kind :absent)
                           :different (observation kind :different)}}
    (= kind :candidate-increment) (assoc :second-layer (increment-relations))))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:selection-fixture :futon2.report.cascade-decision-test/live-c-opts
            :checker {:path "proof2a_check.clj" :args ["--wc" "--edn"]}
            :cases cases :mutations [:none :absent :different]}
   :census (support/census)
   :wires (into {} (map (juxt :wire-id case-fields) cases))
   :left-out {:temporary-checker-files "checker input files live in a fresh hermetic directory; no reader checks their paths"
              :full-decision-and-enactment "readers check the selected candidate, verdict and increment outputs retained above"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "selection-out-observe@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "selection-out-observe@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest selection-out-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable selection-out-observe record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (select-keys expected [:census :wires]))]
            (testing (pr-str path)
              (is (= (get-in expected path) (get-in actual path))))))))))
