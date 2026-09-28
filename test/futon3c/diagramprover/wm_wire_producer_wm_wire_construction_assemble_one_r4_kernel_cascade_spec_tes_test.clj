(ns futon3c.diagramprover.wm-wire-producer-wm-wire-construction-assemble-one-r4-kernel-cascade-spec-tes-test
  "Producer for the [:construction-assemble-one :r4-kernel :cascade-spec] wire.
  Runs the real assemble -> assemble-one over cascade-decision-test's tick-1
  sources, then efe/rank-cascade-actions with the :cascade-spec option
  cascade-lane forwards, and asserts the result equals the committed record."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.cascade-model-manifest :as manifest]
            [futon2.aif.cascade-policy :as policy]
            [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-products :as products]
            [futon3c.diagramprover.wm-wire-construction-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-construction-assemble-one-r4-kernel-cascade-spec-tes-test)
(def operation 'futon2.aif.efe/rank-cascade-actions)
(def stem "wm-wire-construction-assemble-one-r4-kernel-cascade-spec-tes")
(def wire-id [:construction-assemble-one :r4-kernel :cascade-spec])
(def mutations [:absent :different :missing-want])

(defn- observe [mutation]
  (let [p (get-in @support/assembled [:problems 0 :cascade-problem])
        pair (get-in @support/assembled [:problems 0 :constructed-candidates 0])
        candidate {:kind :cascade-candidate :id (:candidate-id pair)
                   :precedence (mapv #(policy/token-interpretation % (get-in p [:interpretations %]))
                                     (:precedence pair))
                   :construction-receipt (:construction-receipt pair)}
        writer (:cascade-spec p)
        carrier (case mutation
                  :none writer
                  :absent {:absent :no-cascade-spec}
                  :different (assoc writer :want #{:test-covers-missing-total-repos})
                  :missing-want (dissoc writer :want))
        ranked (efe/rank-cascade-actions
                {:cascade-belief (manifest/observed-belief
                                  (set (for [[k v] (:facts p) :when (true? v)] k)))}
                [candidate] {:cascade-spec carrier :horizon-steps (:horizon-steps p)
                             :f-prefix-production? true})
        reader (get-in (meta ranked) [:cascade-scoring :spec-in])
        derived (get-in (meta ranked) [:cascade-scoring :spec])
        o {:writer writer :reader reader}]
    {:writer writer :reader reader
     :writer-present? (some? writer)
     :received? (w/received? o)
     :ranked-present? (boolean (seq ranked))
     :ranked-kind (get-in ranked [:kind])
     :reader-differs-from-derived? (not= reader derived)}))

(defn- second-layer []
  (let [before (products/score-product :cascade-spec :none)
        after (products/score-product :cascade-spec :different)
        v (:scores before) v-prime (:scores after)]
    {:before before :after after
     :competing-before? (< 1 (count v))
     :candidate-counts [(count v) (count v-prime)]
     :scores-numeric? (every? number? (concat v v-prime))
     :scores-differ? (not= v v-prime)}))

(def live-records-read
  (filter #(re-find #"tick-run-record.*(278b6988|7f89646a|e70b4baf)" (:path %))
          support/live-records-read))

(defn- live-pins []
  (mapv (fn [{:keys [path sha256]}]
          {:sha256 sha256
           :sha-matches? (= sha256 (w/sha256-file path))
           :no-cascade-spec? (not-any? #(and (map? %) (contains? % :cascade-spec))
                                       (tree-seq coll? seq (w/read-record path)))})
        live-records-read))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:target :futon2.report.cascade-decision-test/tick-1-target
            :sources :futon2.report.cascade-decision-test/tick-1-sources
            :assembled :futon3c.diagramprover.wm-wire-construction-support/assembled
            :wire-id wire-id :mutations (into [:none] mutations)}
   :wires {wire-id {:primary (observe :none)
                    :interventions (into {} (map (juxt identity observe) mutations))
                    :second-layer (second-layer)}}
   :live-records {:count (count live-records-read) :pins (live-pins)}
   :left-out {:full-ranked-vector "readers check only that ranked is present and, for refused carriers, its :kind"
              :derived-scoring-spec "readers check only that the derived spec differs from the handed-off spec-in; recorded as :reader-differs-from-derived?"
              :live-record-paths-and-why "readers check only the count, the pinned sha256 match, and the absence of any :cascade-spec key"
              :temporary-paths "this group uses no reader-checked temporary path"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) (str stem "@"))
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str stem "@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest cascade-spec-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable cascade-spec record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (:wires expected))]
            (testing (pr-str path)
              (is (= (get-in expected (into [:wires] path))
                     (get-in actual (into [:wires] path))))))
          (doseq [path (leaf-paths (:live-records expected))]
            (testing (pr-str path)
              (is (= (get-in expected (into [:live-records] path))
                     (get-in actual (into [:live-records] path)))))))))))
