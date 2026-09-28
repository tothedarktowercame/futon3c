(ns futon3c.diagramprover.wm-wire-producer-token-input-observe
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-belief-products-support :as belief-products]
            [futon3c.diagramprover.wm-wire-token-continuation-products :as continuation-products]
            [futon3c.diagramprover.wm-wire-token-input-support :as token-support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-token-input-observe-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-decision)
(def hops
  [:initialization-temporal
   :legacy-initialization
   :temporal-decision
   :decision-kernel
   :decision-dispatch])

(defn- primary [hop]
  (let [ordinary (token-support/observe hop :none)
        absent (token-support/observe hop :absent)
        different (token-support/observe hop :different)]
    {:writer (:writer ordinary)
     :reader (:reader ordinary)
     :received? (w/received? ordinary)
     :interventions
     {:absent {:writer-present? (some? (:writer absent))
               :received? (w/received? absent)}
      :different {:writer-differs-from-carrier? (not= {#{} 1} (:writer different))
                  :received? (w/received? different)}}}))

(defn- receipt-relations [hop]
  (let [a (continuation-products/receipt-product hop :none)
        b (continuation-products/receipt-product hop :different)]
    {:inputs-stable? (= (:inputs a) (:inputs b))
     :other-fields-stable? (= (:other-fields a) (:other-fields b))
     :a-supplied-carried? (= (:supplied a) (:belief a))
     :b-supplied-carried? (= (:supplied b) (:belief b))
     :beliefs-differ? (not= (:belief a) (:belief b))
     :no-updates? (= [] (:updates a) (:updates b))}))

(defn- decision-relations []
  (let [a (continuation-products/decision-product :none)
        b (continuation-products/decision-product :different)]
    {:inputs-stable? (= (:decision-inputs a) (:decision-inputs b))
     :a-no-exception? (nil? (:exception a))
     :b-no-exception? (nil? (:exception b))
     :one-call-each? (= 1 (count (:calls a)) (count (:calls b)))
     :other-inputs-stable? (= (mapv :other-inputs (:calls a))
                               (mapv :other-inputs (:calls b)))
     :a-supplied-carried? (= (:supplied a) (:incoming a))
     :b-supplied-carried? (= (:supplied b) (:incoming b))
     :scores-differ? (not= (mapv :scores (:calls a)) (mapv :scores (:calls b)))}))

(defn- belief-relations [hop]
  (let [[before after] (get @belief-products/pairs hop)]
    {:writer-stable? (= (:writer before) (:writer after))
     :before-writer-carried? (= (:writer before) (:carrier before))
     :carrier-differs? (not= (:carrier before) (:carrier after))
     :controls-stable? (= (:controls before) (:controls after))
     :three-candidates? (= 3 (count (get-in before [:controls :candidates])))
     :horizon-three? (= 3 (get-in before [:controls :horizon]))
     :beta-declared? (= {:value 1 :status :declared} (get-in before [:controls :beta]))
     :before-incoming? (= (:carrier before) (:incoming before))
     :after-incoming? (= (:carrier after) (:incoming after))
     :three-scores? (= 3 (count (:G before)) (count (:G after)))
     :numeric-scores? (every? number? (concat (:G before) (:G after)))
     :scores-differ? (not= (:G before) (:G after))
     :dispatch-matches-kernel?
     (if (= hop :dispatch)
       (= (mapv :G (get @belief-products/pairs :kernel)) [(:G before) (:G after)])
       true)}))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:hops hops :modes [:none :absent :different]}
   :fields {:live-reader-absent? (token-support/live-reader-absent?)
            :wires (into {} (map (juxt identity primary) hops))
            :second-layer
            {:initialization-temporal (receipt-relations :initialization-temporal)
             :legacy-initialization (receipt-relations :legacy-initialization)
             :temporal-decision (decision-relations)
             :decision-kernel (belief-relations :kernel)
             :decision-dispatch (belief-relations :dispatch)}}
   :left-out {:decision-products "the readers assert named input, carrier, and score relations; those relations are recorded individually"
              :generated-clock-citation "the existing decision fixture excludes its occurrence id because it is not a scoring input"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "token-input-observe@" (subs (sha256 text) 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- checked-field-paths [fields]
  (concat
   [[:live-reader-absent?]]
   (mapcat (fn [hop]
             (concat
              (map #(vector :wires hop %) [:writer :reader :received?])
              [[:wires hop :interventions :absent :writer-present?]
               [:wires hop :interventions :absent :received?]
               [:wires hop :interventions :different :writer-differs-from-carrier?]
               [:wires hop :interventions :different :received?]]))
           hops)
   (for [hop hops
         field (keys (get-in fields [:second-layer hop]))]
     [:second-layer hop field])))

(deftest token-input-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "token-input-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
