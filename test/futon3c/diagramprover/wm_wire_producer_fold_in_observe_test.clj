(ns futon3c.diagramprover.wm-wire-producer-fold-in-observe-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-depth-products :as depth-products]
            [futon3c.diagramprover.wm-wire-fold-in-support :as fold-support]
            [futon3c.diagramprover.wm-wire-fold-products-4a :as fold-products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-fold-in-observe-test)
(def operation 'futon2.report.war-machine/judge)
(def kinds [:brief :horizon :driver :carry])

(defn- primary [kind]
  (let [ordinary (fold-support/observe kind :none)
        absent (fold-support/observe kind :absent)
        different (fold-support/observe kind :different)]
    {:writer (:writer ordinary)
     :reader (:reader ordinary)
     :writer-present? (some? (:writer ordinary))
     :writer-typed-absence? (w/typed-absence? (:writer ordinary))
     :received? (w/received? ordinary)
     :no-error? (nil? (get-in ordinary [:result :wire-error]))
     :interventions
     {:absent
      (merge {:received? (w/received? absent)}
             (case kind
               :brief {:reader-not-carried? (= {:absent :not-carried} (:reader absent))
                       :no-error? (nil? (get-in absent [:result :wire-error]))}
               :horizon {:reader-one? (= 1 (:reader absent))}
               :driver {:observation-absent? (= :observation-absent
                                                (get-in absent [:result :belief-aggregation-events 0 :reason]))
                        :every-channel-omitted? (= :every-channel-omitted
                                                   (get-in absent [:result :micro-step-trace 0 :aggregated-driver-unknown]))
                        :fresh-belief? (= (:fresh absent) (get-in absent [:result :belief]))}
               :carry {:fresh-belief? (= (:fresh absent) (:reader absent))}))
      :different
      (merge {:received? (w/received? different)
              :no-error? (nil? (get-in different [:result :wire-error]))}
             (case kind
               :brief {:writer-reader-differ? (not= (:writer different) (:reader different))}
               :horizon {:reader-four? (= 4 (:reader different))
                         :cascade-horizon-three? (= 3 (get-in different [:result :cascade-horizon :value]))}
               :driver {:negative-reader? (neg? (:reader different))
                        :belief-changed? (not= (get-in ordinary [:result :belief])
                                               (get-in different [:result :belief]))}
               :carry {:not-fresh? (not= (:fresh different) (:reader different))}))}}))

(defn- updated-relations [kind]
  (let [a (fold-products/updated kind :none)
        b (fold-products/updated kind :different)]
    {:a-no-error? (nil? (get-in a [:result :result :wire-error]))
     :b-no-error? (nil? (get-in b [:result :result :wire-error]))
     :observation-stable? (= fold-products/scan (:observation a) (:observation b))
     :a-events? (boolean (seq (:events a)))
     :b-events? (boolean (seq (:events b)))
     :priors-differ? (not= (:prior a) (:prior b))
     :a-updated? (not= (:prior a) (:posterior a))
     :b-updated? (not= (:prior b) (:posterior b))
     :posteriors-differ? (not= (:posterior a) (:posterior b))}))

(defn- depth-relations []
  (let [a (depth-products/depth-product :none)
        b (depth-products/depth-product :different)]
    {:depths? (= [3 4] [(:reader a) (:reader b)])
     :rank-inputs-stable? (= (:rank-inputs a) (:rank-inputs b))
     :scores-present? (boolean (seq (:scores a)))
     :scores-stable? (= (:scores a) (:scores b))
     :posterior-present? (boolean (seq (:posterior a)))
     :posterior-stable? (= (:posterior a) (:posterior b))
     :cascade-horizon-stable? (= (get-in a [:result :cascade-horizon])
                                  (get-in b [:result :cascade-horizon]))}))

(defn build-record []
  (fold-support/assert-live-pins)
  (let [primary-fields (into {} (map (juxt identity primary) kinds))]
    {:producer producer
     :operation operation
     :inputs {:kinds kinds :modes [:none :absent :different]}
     :fields {:live-pins-valid? true
              :wires primary-fields
              :second-layer
              {:brief (updated-relations :brief)
               :horizon (depth-relations)
               :driver (get-in primary-fields [:driver :interventions :different])
               :carry (updated-relations :carry)}}
     :left-out {:temporary-trace-path "the carry trace lives in a temporary directory; readers check the carried belief, not its path"
                :judge-results "readers check the individually named error, belief, depth, driver, score, and posterior relations"}}))

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "fold-in-observe@" (subs (sha256 text) 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- checked-field-paths [fields]
  (concat
   [[:live-pins-valid?]]
   (for [kind kinds
         field [:writer :reader :writer-present? :writer-typed-absence? :received? :no-error?]]
     [:wires kind field])
   (for [kind kinds
         mode [:absent :different]
         field (keys (get-in fields [:wires kind :interventions mode]))]
     [:wires kind :interventions mode field])
   (for [kind kinds
         field (keys (get-in fields [:second-layer kind]))]
     [:second-layer kind field])))

(deftest fold-in-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "fold-in-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
