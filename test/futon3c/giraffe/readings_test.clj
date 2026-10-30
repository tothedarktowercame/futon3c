(ns futon3c.giraffe.readings-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.excursions :as excursions]))

(def root "/home/joe/notes/wm-next/giraffe-excursions/")
(def options {:checker (str root "check_edges.py") :members-file (str root "members.json")
              :batches [(str root "batch-00.jsonl")] :repos missions/default-repos})

(deftest real-pilot-certifies-members-not-absent-edges
  (let [result (readings/consume options)
        records (map #(json/parse-string % true)
                     (remove str/blank? (str/split-lines (slurp (str root "batch-00.jsonl")))))
        expected (count records)
        coverage (:coverage result)
        rows (into {} (map (juxt :id identity)) (:members coverage))]
    (is (= 636 (:member-count coverage)))
    (is (= expected (get-in coverage [:counts :read])))
    (is (= (- 636 expected) (get-in coverage [:counts :unread])))
    (is (= :incomplete (:status coverage)))
    (is (every? :passed? (:checked-records result)))
    (doseq [record records]
      (is (= (count (:edges record)) (:edge-count (rows (:id record)))))
      (is (= :read (:status (rows (:id record))))))
    (is (every? #(#{:declared :inferred} (:basis %)) (:edges result)))
    (is (every? #(#{"part-of" "advances" "relates-to" "duplicates" "superseded-by"} (:kind %)) (:links result)))
    (is (= (reduce + (map #(count (:edges %)) records))
           (+ (count (:edges result)) (count (:links result))
              (count (:out-of-scope result)) (count (:rejected result)))))
    (let [adapter (excursions/missions-and-excursions->input
                   {:edges-file "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json"
                    :reading-checker (:checker options) :members-file (:members-file options)
                    :reading-batches (:batches options)})
          pass (excursions/combined-pass (:input adapter))]
      (is (= 636 (count (:order pass))))
      (is (every? #(and (nil? (:cone %)) (nil? (:unblocks %))) (:order pass)))
      (is (every? #(= (:population-sha coverage) (get-in % [:graph-terms :population-sha])) (:order pass))))))

(deftest duplicate-pilot-record-is-ambiguous
  (let [file (java.io.File/createTempFile "giraffe-duplicate-" ".jsonl")
        line (first (str/split-lines (slurp (str root "batch-00.jsonl"))))]
    (try
      (spit file (str line "\n" line "\n"))
      (let [result (readings/consume (assoc options :batches [(.getPath file)]))]
        (is (= 1 (get-in result [:coverage :counts :ambiguous])))
        (is (empty? (:edges result)))
        (is (empty? (:links result))))
      (finally (io/delete-file file)))))

(deftest prerequisite-direction-and-basis-control
  ;; Pilot 00 contains only non-readiness links. These are conversion controls,
  ;; not a claim that its quoted relation is a real prerequisite.
  (let [records (map #(json/parse-string % true)
                     (remove str/blank? (str/split-lines (slurp (str root "batch-00.jsonl")))))
        ids (set (map (comp keyword :id) (json/parse-string (slurp (:members-file options)) true)))
        [record edge] (first (for [r records e (:edges r)
                                  :when (contains? ids (keyword (:to e)))] [r e]))
        convert (ns-resolve 'futon3c.giraffe.readings 'convert-edge)
        forward (:edge (convert missions/default-repos ids (:id record)
                                (assoc edge :kind "depends-on" :basis "declared")))
        backward (:edge (convert missions/default-repos ids (:id record)
                                 (assoc edge :kind "unblocks" :basis "inferred")))]
    (is (some? edge))
    (is (= [(keyword (:id record)) (keyword (:to edge))]
           [(:from forward) (:to forward)]))
    (is (= [(:to forward) (:from forward)] [(:from backward) (:to backward)]))
    (is (= [:declared :inferred] [(:basis forward) (:basis backward)]))
    (is (= :closure (:class forward) (:class backward)))
    (is (= (:evidence edge) (:evidence-pin forward)))))

(deftest scope-defaults-to-missions-excursions-and-flows-into-graph-terms
  (let [result (readings/consume options)
        input {:as-of "scope-test"
               :items [{:id :E-a :state-group :unstarted :source-order 0}]
               :edges []
               :graph-coverage (dissoc (:coverage result) :members)
               :reading-evidence {:edges [] :links [] :out-of-scope []
                                  :rejected [] :unresolved []}}
        pass (excursions/combined-pass input)]
    (is (= :population/missions-excursions (:scope (:coverage result))))
    (is (every? #(= :population/missions-excursions (get-in % [:graph-terms :scope])) (:order pass)))))

(deftest explicit-scope-labels-certificate-and-graph-terms
  (let [result (readings/consume (assoc options :scope :population/missions-excursions-tickets))
        input {:as-of "scope-test"
               :items [{:id :E-a :state-group :unstarted :source-order 0}]
               :edges []
               :graph-coverage (assoc (dissoc (:coverage result) :members)
                                      :status :incomplete)
               :reading-evidence {:edges [] :links [] :out-of-scope []
                                  :rejected [] :unresolved []}}
        pass (excursions/combined-pass input)]
    (is (= :population/missions-excursions-tickets (:scope (:coverage result))))
    (is (every? #(= :population/missions-excursions-tickets (get-in % [:graph-terms :scope])) (:order pass)))))
