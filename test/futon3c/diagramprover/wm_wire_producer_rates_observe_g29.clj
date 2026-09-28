(ns futon3c.diagramprover.wm-wire-producer-rates-observe-g29
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-rates-products-support :as products]
            [futon3c.diagramprover.wm-wire-rates-support :as support])
  (:import [java.security MessageDigest]))
(def producer 'futon3c.diagramprover.wm-wire-producer-rates-observe-g29-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-lane)
(def wire-id [:r6-sourced-rates :r6-test :measurement])
(defn- observation [tamper]
  (support/observe :test-measurement tamper))
(defn- report-fields [mutate]
  (let [r (products/test-report mutate)
        reports (filter #(= "the C4 token carries its counts" (:message %)) (:reports r))]
    {:calls (:calls r) :types (mapv :type reports)
     :errors? (boolean (some #(= :error (:type %)) (:reports r)))}))
(defn build-record []
  {:producer producer :operation operation
   :inputs {:wire-kind :test-measurement :mutations [:none :absent :different]
            :report-mutations [:identity :wanted-absent]}
   :live-records support/live-records-read
   :wires {wire-id
           {:primary (observation identity)
            :interventions {:absent (observation (constantly {:absent :not-carried}))
                            :different (observation #(support/different :test-measurement %))}
            :second-layer {:before (report-fields identity)
                           :after (report-fields #(assoc % :t/wanted :absent))}}}
   :left-out {:temporary-label-store "created per run; no reader checks its path"
              :unselected-test-reports "reader checks the named C4 count report and absence of errors"}})
(defn- text [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255))
                                (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- files [] (filter #(.startsWith (.getName %) "rates-observe-g29@")
                         (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write! [r]
  (let [s (text r) f (io/file "test/fixtures/wire-producers" (str "rates-observe-g29@" (subs (sha s) 0 12) ".edn"))]
    (when (.exists f) (throw (ex-info "producer record already exists" {:file (str f)})))
    (spit f s) (println (.getPath f))))
(defn- leaves [x]
  (letfn [(walk [p v] (if (map? v) (mapcat (fn [[k x]] (walk (conj p k) x)) v) [p]))]
    (walk [] x)))
(deftest rates-observe-g29-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! actual)
        (let [fs (files)]
          (is (= 1 (count fs)) "exactly one immutable G29 record")
          (let [expected (edn/read-string (slurp (first fs)))]
            (doseq [p (leaves expected)]
              (testing (pr-str p) (is (= (get-in expected p) (get-in actual p))))))))))
