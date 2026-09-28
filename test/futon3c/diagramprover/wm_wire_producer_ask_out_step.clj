(ns futon3c.diagramprover.wm-wire-producer-ask-out-step
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-ask-out-step-test)
;; The packet named the operation "flight assembly/click". The reader's
;; support call is support/step, whose product entry point is
;; flight/conditioning-step run on the inputs of a persisted offline run
;; (see wm_wire_ask_out_support.clj). The record names what the source says.
(def operation 'futon2.aif.flight/conditioning-step)

(defn- case-fields [mutation]
  (let [o (support/step mutation)]
    (cond-> {:writer (:writer o)
             :reader (:reader o)
             :step (select-keys (:step o) [:status :reason :b :q :measured-a])}
      (= mutation :absent)
      (assoc :assembled {:refusals [{:kind (get-in o [:assembled :refusals 0 :kind])}]}))))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:support-step 'futon3c.diagramprover.wm-wire-ask-out-support/step
            :support-live-census 'futon3c.diagramprover.wm-wire-ask-out-support/live-census
            :support-live-records-read 'futon3c.diagramprover.wm-wire-ask-out-support/live-records-read
            :target-id support/target-id
            :pattern-id support/pattern-id
            :mutations [:none :absent :different]}
   :live-records-read support/live-records-read
   :live-census (support/live-census)
   :cases (into {} (map (juxt identity case-fields)) [:none :absent :different])
   :left-out {:step-other-keys "the step map also carries :p-o :schema :o :s-prev :f :policy-key :target :occurrence; readers check only :status :reason :b :q :measured-a"
              :assembled-and-decision-and-sources "readers check only [:assembled :refusals 0 :kind] of the assembly; the cascade decision and merged sources are not checked"
              :persisted-run-record "persist-run-record! writes into a per-run temporary directory; the record carries temporary paths and a run timestamp, and no reader checks it"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "ask-out-step@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "ask-out-step@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest ask-out-step-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable ask-out-step record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path)
                     (get-in actual path))))))))))
