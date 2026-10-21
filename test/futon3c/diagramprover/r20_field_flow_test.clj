(ns futon3c.diagramprover.r20-field-flow-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring])
  (:import [java.security MessageDigest]))

(def map-path "holes/labs/M-wm-wiring/r20-field-flow.edn")
(def map-sha256 "ec6b728f3efd8f9e045e547eb4502a04e8de92f7e0d96296a235dacf53ec565b")
(def repos {"futon2" "270bebc27965b31f1d33012e68b4d1f512eca3f9"})

(def resolved-precision-findings
  #{[:precision-retrospective-f [:f :precision-retrospective] :writes]
    [:precision-carry-advance [:f :precision-retrospective] :reads]})

(defn- sha256 [path]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (java.nio.file.Files/readAllBytes (.toPath (io/file path))))]
    (apply str (map #(format "%02x" %) digest))))

(defn- spec [] (edn/read-string (slurp map-path)))

(defn- materialise [s]
  (let [root (.toFile
              (java.nio.file.Files/createTempDirectory
               "r20-field-flow" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [file (distinct (keep #(get-in % [:site :file]) (:boxes s)))
            :let [[repo path] (str/split file #"/" 2)
                  {:keys [exit out err]}
                  (sh/sh "git" "-C" (str "/home/joe/code/" repo) "show"
                         (str (get (:repos s) repo) ":" path))]]
      (when-not (zero? exit)
        (throw (ex-info "pinned source unavailable"
                        {:repo repo :path path :error err})))
      (io/make-parents (io/file root file))
      (spit (io/file root file) out))
    (str root)))

(defn- comparable [finding]
  (cond-> finding
    (contains? finding :error) (-> (dissoc :error) (assoc :error? true))))

(defn report [root s]
  (let [g (wiring/ingest s)]
    (mapv comparable
          (concat (wiring/written-never-read g)
                  (wiring/read-never-written g)
                  (wiring/multiply-written g)
                  (wiring/conformance root s {:heuristic? true})))))

(deftest the-pins-hold
  (is (= map-sha256 (sha256 map-path)))
  (is (= repos (:repos (spec)))))

(deftest the-report-equals-the-declared-expected-findings
  (let [s (spec)
        got (report (materialise s) s)
        want (remove #(contains? resolved-precision-findings
                                 [(:box/id %) (:field %) (:role %)])
                     (:expected-findings s))]
    (is (= (set want) (set got))
        (pr-str {:unexpected (remove (set want) got)
                 :missing (remove (set got) want)}))
    (is (= (count want) (count got)) "no finding reported twice")))

(deftest precision-retrospective-f-conforms-while-scorer-f-control-still-fails
  (let [s (spec)
        findings (report (materialise s) s)
        true-edge #{[:precision-retrospective-f [:f :precision-retrospective] :writes]
                    [:precision-carry-advance [:f :precision-retrospective] :reads]}
        changed (update s :boxes
                        (fn [boxes]
                          (mapv (fn [box]
                                  (if (= :precision-carry-advance (:box/id box))
                                    (update box :reads conj [:f-consumed {:record :phase8-scorer}])
                                    box))
                                boxes)))
        scorer-findings (report (materialise changed) changed)]
    (is (not-any? #(contains? true-edge [(:box/id %) (:field %) (:role %)]) findings)
        "the true retrospective-F write/read pair has site evidence")
    (is (= 15 (count (filter #(= :declaration-without-occurrence (:finding %)) findings)))
        "the other S4b declarations remain findings")
    (is (some #(and (= :declaration-without-occurrence (:finding %))
                    (= :precision-carry-advance (:box/id %))
                    (= [:f-consumed :phase8-scorer] (:field %)))
              scorer-findings)
        "the planted scorer-F read remains distinguishable from the true F edge")))

(deftest one-authority-per-field
  (is (= [] (wiring/multiply-written (wiring/ingest (spec))))))

(deftest field-metadata-covers-every-wire
  (let [s (spec)
        fields (set (mapcat #(concat (:reads %) (:writes %)) (:boxes s)))]
    (is (= fields (set (keys (:r20/field-types s)))))
    (is (= fields (set (keys (:r20/field-identity s)))))
    (is (every? #{:present :absent :unknown}
                (map :status (vals (:r20/field-identity s)))))))

(deftest precision-carry-cannot-claim-the-scorers-f
  (let [s (spec)
        scorer-f [:f-consumed {:record :phase8-scorer}]
        scorer-vertex (wiring/vertex-key scorer-f)
        changed (update s :boxes
                        (fn [boxes]
                          (mapv (fn [box]
                                  (if (= :precision-carry-advance (:box/id box))
                                    (update box :reads conj scorer-f)
                                    box))
                                boxes)))
        findings (report (materialise changed) changed)]
    (testing "the planted wrong-F edge is named"
      (is (some #(and (= :declaration-without-occurrence (:finding %))
                      (= scorer-vertex (:field %))
                      (= :precision-carry-advance (:box/id %)))
                findings)
          (pr-str findings)))))
