(ns futon3c.diagramprover.wm-wire-producer-publication-r0-test-observe-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support])
  (:import [java.security MessageDigest]))

(def present-observation
  {:observed true :at "run-pub"
   :evidence {:status :receipt-committed
              :repair/id "occ-t"
              :repair/discharged? true}})

(defn build-record []
  (let [positive (support/r0-test-observe identity)
        forged {:observed true :at "run-pub" :evidence {:repair/id "forged"}}
        different (support/r0-test-observe (constantly forged))
        absence {:absent :no-repair-obligation-for-target :target "M-t"}
        absent (support/r0-test-observe (constantly absence))]
    {:producer 'futon3c.diagramprover.wm-wire-producer-publication-r0-test-observe-test
     :operation ['futon2.aif.flight-enact-test/a-discharged-repair-obligation-is-observed-as-published
                 'futon2.aif.flight-runner/observe-publication-fn]
     :inputs {:repair/id "occ-t"
              :repair/publication [{:status :receipt-committed
                                    :repair/id "occ-t"
                                    :repair/discharged? true}]
              :tamper [:identity forged absence]}
     :fields {:writer (:writer positive)
              :reader (:reader positive)
              :report-type (:report-type positive)
              :writer-present? (= present-observation (:writer positive))
              :reader-present? (not (w/typed-absence? (:reader positive)))
              :received? (w/received? positive)
              :different {:writer (:writer different)
                          :reader (:reader different)
                          :report-type (:report-type different)}
              :absent {:writer (:writer absent)
                       :reader (:reader absent)
                       :report-type (:report-type absent)
                       :reader-typed-absence? (w/typed-absence? (:reader absent))
                       :received? (w/received? absent)}}
     :left-out {:temporary-run-record-directory "the futon2 fixture deletes it after the test var runs"
                :clojure-test-report "only the assertion's consumed value and report type are checked"}}))

(defn- record-text [x] (str (pr-str x) "\n"))
(defn- sha256 [s]
  (apply str (map #(format "%02x" (bit-and % 255))
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write-record! [x]
  (let [text (record-text x)
        file (io/file "test/fixtures/wire-producers"
                      (str "publication-r0-test-observe@" (subs (sha256 text) 0 12) ".edn"))]
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println file)))
(defn- leaf-paths [x]
  (letfn [(walk [path value]
            (if (map? value)
              (mapcat (fn [[k v]] (walk (conj path k) v)) value)
              [path]))]
    (walk [] x)))

(deftest r0-test-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [file (first (filter #(.startsWith (.getName %) "publication-r0-test-observe@")
                                (.listFiles (io/file "test/fixtures/wire-producers"))))
            expected (edn/read-string (slurp file))]
        (doseq [path (leaf-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
