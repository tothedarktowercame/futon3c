(ns futon3c.diagramprover.wm-wire-producer-fold-out-simple-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as fold-support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-fold-out-simple-test)
(def operations
  ['futon2.aif.flight/run!
   'futon2.report.war-machine/judge
   'futon2.aif.enactment-habit/fold])
(def kinds [:carry :belief :prediction])

(defn- primary [kind]
  (let [ordinary (fold-support/simple kind :none)
        absent (fold-support/simple kind :absent)
        different (fold-support/simple kind :different)]
    {:writer (:writer ordinary)
     :reader (:reader ordinary)
     :writer-present? (some? (:writer ordinary))
     :writer-typed-absence? (w/typed-absence? (:writer ordinary))
     :received? (w/received? ordinary)
     :ordinary
     (case kind
       :carry {:reader-is-belief-pre? (= (:reader ordinary) (get-in ordinary [:result :belief-pre]))}
       :belief {:events-present? (boolean (seq (:events ordinary)))
                :input-changed? (not= (:input ordinary) (:reader ordinary))}
       :prediction {:present? (= :present (get-in ordinary [:error :status]))
                    :numeric-error? (number? (get-in ordinary [:error :error]))})
     :interventions
     {:absent
      (merge {:received? (w/received? absent)}
             (case kind
               :carry {:fresh-belief? (= (:fresh absent) (:reader absent))}
               :belief {}
               :prediction {:refused? (= :refused (get-in absent [:error :status]))
                            :reason? (= :malformed-prediction-triple
                                        (get-in absent [:error :reason]))}))
      :different
      (merge {:received? (w/received? different)
              :writer-reader-differ? (not= (:writer different) (:reader different))}
             (if (= kind :prediction)
               {:present? (= :present (get-in different [:error :status]))}
               {}))}}))

(defn- carry-relations []
  (let [a (fold-support/simple :carry :none)
        b (fold-support/simple :carry :different)]
    {:ordinary-carried? (= (:writer a) (:reader a) (get-in a [:result :belief-pre]))
     :different-carried? (= (:reader b) (get-in b [:result :belief-pre]))
     :readers-differ? (not= (:reader a) (:reader b))
     :domain-stable? (= #{"known"} (set (keys (:reader a))) (set (keys (:reader b))))
     :fresh-stable? (= (:fresh a) (:fresh b))}))

(defn build-record []
  (fold-support/assert-live-pins)
  (let [primary-fields (into {} (map (juxt identity primary) kinds))]
    {:producer producer
     :operation operations
     :inputs {:kinds kinds :modes [:none :absent :different]}
     :fields {:live-pins-valid? true
              :wires primary-fields
              :second-layer
              {:carry (carry-relations)
               :belief (get-in primary-fields [:belief :interventions :different])
               :prediction (get-in primary-fields [:prediction :interventions :absent])}}
     :left-out {:temporary-trace-paths "the trace paths are temporary; readers assert the resulting carried values"
                :judge-results "readers check the individually named carry, belief-event, and prediction-error relations"}}))

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "fold-out-simple@" (subs (sha256 text) 0 12) ".edn"))]
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

(deftest fold-out-simple-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "fold-out-simple@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
