(ns futon3c.diagramprover.wm-wire-producer-publication-publication-observe
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support])
  (:import [java.security MessageDigest]))

(defn build-record []
  (let [positive (support/publication-observe identity)
        absent (support/publication-observe #(dissoc % :repair/publication))
        different (support/publication-observe
                   #(assoc % :repair/publication
                           [{:status :publication-refused
                             :repair/id "occ-wire"
                             :reason :publication-error}]))
        pins [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
               :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"}
              {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
               :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"}]
        run-record (w/read-record (:path (first pins)))
        flight (:flight (w/read-record (:path (second pins))))]
    {:producer 'futon3c.diagramprover.wm-wire-producer-publication-publication-observe-test
     :operation ['futon2.aif.full-loop-runner/persist-run-record!
                 'futon2.aif.flight-runner/observe-publication-fn]
     :inputs {:click-id "wire-pub"
              :repair-id "occ-wire"
              :repair/publication support/publication-entries
              :tamper [:identity :remove-publication :publication-refused]}
     :fields {:writer (:writer positive)
              :reader (:reader positive)
              :observation (:observation positive)
              :writer-present? (some? (:writer positive))
              :writer-typed-absence? (w/typed-absence? (:writer positive))
              :received? (w/received? positive)
              :absent {:reader (:reader absent)
                       :observation (:observation absent)
                       :received? (w/received? absent)}
              :different {:reader (:reader different)
                          :observation (:observation different)
                          :received? (w/received? different)}
              :live {:pins? (every? #(= (:sha256 %) (w/sha256-file (:path %))) pins)
                     :writer-present? (sequential? (:repair/publication run-record))
                     :enactments (mapv :enactment (:enactments flight))}}
     :left-out {:temporary-run-record-directory "the fixture deletes it after the reader runs"}}))

(defn- record-text [x] (str (pr-str x) "\n"))
(defn- sha256 [s]
  (apply str (map #(format "%02x" (bit-and % 255))
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write-record! [x]
  (let [text (record-text x)
        file (io/file "test/fixtures/wire-producers"
                      (str "publication-publication-observe@" (subs (sha256 text) 0 12) ".edn"))]
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println file)))
(defn- leaf-paths [x]
  (letfn [(walk [path value]
            (if (map? value)
              (mapcat (fn [[k v]] (walk (conj path k) v)) value)
              [path]))]
    (walk [] x)))

(deftest publication-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [file (first (filter #(.startsWith (.getName %) "publication-publication-observe@")
                                (.listFiles (io/file "test/fixtures/wire-producers"))))
            expected (edn/read-string (slurp file))]
        (doseq [path (leaf-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
