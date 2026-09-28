(ns futon3c.diagramprover.wm-wire-producer-temperature-observe
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.policy-precision-carry :as precision]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-carried-precision-products :as products]
            [futon3c.diagramprover.wm-wire-temperature-support :as temperature])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-temperature-observe-test)
(def reader-value-path (conj temperature/decision-reader-field :value))

(defn build-record []
  (let [ordinary (temperature/observe reader-value-path)
        absent (temperature/observe reader-value-path
                                    #(assoc-in % temperature/decision-reader-field
                                               {:absent :precision-carry-refused}))
        different (temperature/observe reader-value-path
                                       #(assoc-in % temperature/decision-reader-field
                                                  {:value 2 :status :declared}))
        live (w/read-record (:path temperature/record))
        precision-state (get-in live [:decision :selection-certificate :policy-precision-state])
        coherent (products/observe :unchanged)
        beta-only (products/observe :beta-only)
        coherent-three (products/observe :coherent-three)
        ra (:result coherent) rb (:result coherent-three) scores (:scores ra)
        companions [:beta :gamma :tau :initialized-beta :sha256]]
    {:producer producer :operation 'futon2.aif.wm.cascade-decision/cascade-decision
     :inputs {:reader-value-path reader-value-path
              :tamper-values [{:absent :precision-carry-refused}
                              {:value 2 :status :declared}]
              :product-modes [:unchanged :beta-only :coherent-three]}
     :fields
     {:source-record temperature/record
      :writer (:writer ordinary) :reader (:reader ordinary)
      :writer-present? (some? (:writer ordinary))
      :writer-typed-absence? (w/typed-absence? (:writer ordinary))
      :received? (w/received? ordinary)
      :hash-matches? (= (:sha256 temperature/record)
                        (w/sha256-file (:path temperature/record)))
      :writer-is-one? (= 1 (:writer ordinary))
      :precision-schema? (= :wm/precision-carry-v1 (:schema precision-state))
      :precision-held? (= :held (:status precision-state))
      :declared-beta? (= {:value 1 :status :declared}
                         (get-in live temperature/decision-reader-field))
      :interventions
      {:absent {:received? (w/received? absent)}
       :different {:reader (:reader different) :received? (w/received? different)}}
      :second-layer
      {:written-intact? (precision/intact? (:written coherent))
       :written-carriers-equal? (= (:written coherent) (:carrier coherent)
                                   (:written beta-only))
       :beta-only-increment? (= (:carrier coherent) (update (:carrier beta-only) :beta dec))
       :reader-inputs-equal? (= (:reader-input coherent) (:reader-input beta-only))
       :beta-is-one? (= 1 (get-in coherent [:result :beta]))
       :posterior-keys? (= #{:A :B} (set (keys (get-in coherent [:result :posterior]))))
       :mismatch-refusal? (= {:kind :precision-consumption-mismatch}
                             (get-in beta-only [:result :refusal]))}
      :supplementary
      {:carriers-intact? (every? precision/intact? [(:carrier coherent) (:carrier coherent-three)])
       :coherent-three-written? (= (:written coherent-three) (:carrier coherent-three))
       :non-companions-equal? (= (apply dissoc (:carrier coherent) companions)
                                 (apply dissoc (:carrier coherent-three) companions))
       :reader-inputs-equal? (= (:reader-input coherent) (:reader-input coherent-three))
       :scores-equal? (= (:scores ra) (:scores rb))
       :a-score-below-b? (< (:A scores) (:B scores))
       :betas-one-three? (= [1 3] [(:beta ra) (:beta rb)])
       :higher-beta-flattens? (< 0.5 (get-in rb [:posterior :A])
                                (get-in ra [:posterior :A]))
       :softmax-formula? (every?
                          (fn [r]
                            (< (Math/abs (- (get-in r [:posterior :A])
                                            (/ 1.0 (+ 1.0 (Math/exp
                                                           (/ (- (:A scores) (:B scores))
                                                              (:beta r)))))))
                               1e-12))
                          [ra rb])}}
     :left-out {:full-live-record "the reader checks only the pinned precision state and declared beta"
                :product-records "the reader checks the named seal, refusal, score and posterior relations"}}))

(defn- text [x] (str (pr-str x) "\n"))
(defn- sha256 [s]
  (apply str (map #(format "%02x" (bit-and % 0xff))
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write-record! [record]
  (let [content (text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "temperature-observe@" (subs (sha256 content) 0 12) ".edn"))]
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file content) (println (.getPath file))))

(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest temperature-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "temperature-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (leaf-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
