(ns futon3c.diagramprover.wm-wire-producer-wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.policy :as policy]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-handoff-products :as products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit-test)
(def operation 'futon2.aif.wm.cascade-decision/cascade-decision)
(def wire-id [:r9-selection-law :r9-decision :per-policy-argmax])

(defn- step [id target]
  {:id id :target target :guard {:clauses [{:present #{} :absent #{}}]} :produces #{}})

(defn- ranked [cascade-id target g & steps]
  {:action {:kind :cascade-candidate :id cascade-id :target target
            :precedence (vec steps)
            :construction-receipt {:kind :fixture :id cascade-id}
            :interpretation-receipts {cascade-id {:kind :fixture}}}
   :cascade true :cascade-id cascade-id :controller-score g})

(def roster
  [(ranked :cas/b "M-t" 0.5 (step :p/b "M-t"))
   (ranked :cas/a1 "M-t" 1.0 (step :p/a "M-t") (step :p/c "M-t"))
   (ranked :cas/a2 "M-t" 1.0 (step :p/a "M-t") (step :p/d "M-t"))])

(defn- decide []
  (policy/select-action-cascades
   roster {:beta 1 :novelty-inputs {}
           :cascade-habit-path (str (io/file (w/tmp-dir "row9-producer-") "absent.edn"))}))

(defn- observe [tamper]
  (let [decision (decide)
        read-decision (tamper decision)]
    {:writer (get-in decision [:selection-law :per-policy-argmax])
     :reader (let [value (get-in read-decision [:selection-law :per-policy-argmax])]
               (if (nil? value) {:absent :field-not-carried} value))
     :action (get-in read-decision [:selection-law :per-policy-argmax :action])}))

(def live-records-read
  [{:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "carries the writer end and an unrefused candidate-derivations map; the read action is not echoed"}])

(defn build-record []
  (let [ordinary (observe identity)
        absent (observe #(assoc-in % [:selection-law :per-policy-argmax]
                                  {:absent :no-argmax}))
        different (observe #(assoc-in % [:selection-law :per-policy-argmax :action :id]
                                       :cas/other))
        {:keys [path sha256]} (first live-records-read)
        live (w/read-record path)
        product-a (products/decision-product false)
        product-b (products/decision-product true)]
    {:producer producer :operation operation
     :inputs {:roster roster :beta 1 :novelty-inputs {}
              :tamper-modes [:none :absent :different]}
     :fields
     {:wire-id wire-id
      :writer (:writer ordinary) :reader (:reader ordinary) :action (:action ordinary)
      :writer-present? (some? (:writer ordinary))
      :writer-typed-absence? (w/typed-absence? (:writer ordinary))
      :received? (w/received? ordinary)
      :mode-is-cas-b? (= :cas/b (get-in ordinary [:writer :action :id]))
      :action-present? (some? (:action ordinary))
      :action-is-writer-action? (= (:action ordinary) (get-in ordinary [:writer :action]))
      :interventions
      {:absent {:reader-typed-absence? (w/typed-absence? (:reader absent))
                :action-absent? (nil? (:action absent))
                :received? (w/received? absent)}
       :different {:reader-present? (some? (:reader different))
                   :writer-reader-differ? (not= (:writer different) (:reader different))
                   :received? (w/received? different)}}
      :live-record {:source (first live-records-read)
                    :hash-matches? (= sha256 (w/sha256-file path))
                    :writer-id-is-c1? (= :C1 (get-in live [:decision :selection-law :per-policy-argmax :action :id]))
                    :candidate-derivations-present? (map? (get-in live [:decision :selection-certificate :candidate-derivations]))}
      :second-layer
      {:score-counts-three? (= 3 (count (distinct (:scores product-a)))
                               (count (distinct (:scores product-b))))
       :a-supplied-recorded? (= (:supplied product-a) (:recorded product-a))
       :b-supplied-recorded? (= (:supplied product-b) (:recorded product-b))
       :action-id-changed? (not= (get-in product-a [:recorded :action :id])
                                 (get-in product-b [:recorded :action :id]))
       :posterior-changed? (not= (:posterior product-a) (:posterior product-b))
       :a-status-absent? (nil? (get-in product-a [:derivations :status]))
       :derivations-equal? (= (:derivations product-a) (:derivations product-b))}}
     :left-out {:cascade-habit-path "a fresh temporary path only isolates the literal selection fixture"}}))

(defn- text [record] (str (pr-str record) "\n"))
(defn- sha256 [s]
  (apply str (map #(format "%02x" (bit-and % 0xff))
                  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write-record! [record]
  (let [content (text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit@"
                           (subs (sha256 content) 0 12) ".edn"))]
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file content) (println (.getPath file))))

(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x)
              (mapcat (fn [[k v]] (walk (conj path k) v)) x)
              [path]))]
    (walk [] value)))

(deftest selection-law-argmax-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %)
                                                        "wm-wire-r9-selection-law-decision-per-policy-argmax-test-lit@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (leaf-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
