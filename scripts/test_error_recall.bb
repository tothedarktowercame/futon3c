#!/usr/bin/env bb

(load-file "scripts/error_recall.bb")

(ns test-error-recall
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is run-tests]]
            [error-recall :as recall]))

(defn memory-result [id tags score]
  {:score score
   :entry {:evidence/id id
           :evidence/type :memory
           :evidence/tags tags
           :evidence/body {:name id :body {:rule (str "rule-" id)}}}})

(deftest term-extraction-preserves-lean-identifiers
  (let [terms (recall/extract-terms
               "error: setIntegral_mono requires a global pointwise inequality;
                unknown identifier MeasureTheory.IntegrableOn.mono'")]
    (is (some #{"setIntegral_mono"} terms))
    (is (some #{"MeasureTheory.IntegrableOn.mono'"} terms))
    (is (not-any? #{"error" "requires" "unknown" "identifier"} terms))
    (is (<= (count terms) 12))))

(deftest ranking-filters-and-caps
  (let [results [(memory-result "ordinary" [:memory] -100)
                 (memory-result "solve" [:memory :solve-lane] -10)
                 (memory-result "arc-b" [:memory :arc-lane] -2)
                 (memory-result "arc-a" [:memory :arc-lane] -20)
                 {:score -999 :entry {:evidence/id "not-memory"
                                      :evidence/type :coordination}}]
        hits (recall/choose-hits results)]
    (is (= ["arc-a" "arc-b" "solve"]
           (mapv #(get-in % [:entry :evidence/id]) hits)))
    (is (= 3 (count hits)))))

(deftest empty-result-is-silent-and-logged
  (let [directory (.toString
                   (java.nio.file.Files/createTempDirectory
                    "error-recall-test" (make-array
                                         java.nio.file.attribute.FileAttribute 0)))
        output (with-redefs [recall/query-store (fn [_ _] [])]
                 (with-out-str
                   (is (= 0 (recall/recall!
                             ["--row" "row-empty" "unknown goal"]
                             {:state-dir directory})))))]
    (is (= "" output))
    (let [record (json/parse-string
                  (slurp (recall/log-path directory "row-empty")) true)]
      (is (= "row-empty" (:row-id record)))
      (is (= [] (:surfaced-memory-ids record)))
      (is (vector? (:terms record)))
      (is (<= (count (:error-excerpt record)) 300)))))

(deftest store-down-process-is-quiet-exit-zero-and-logs
  (let [directory (.toString
                   (java.nio.file.Files/createTempDirectory
                    "error-recall-down" (make-array
                                         java.nio.file.attribute.FileAttribute 0)))
        result @(process/process
                 ["bb" "scripts/error_recall.bb" "--row" "row-down"
                  "setIntegral_mono requires a global pointwise inequality"]
                 {:env (assoc (into {} (System/getenv))
                              "ERROR_RECALL_BASE" "http://127.0.0.1:1"
                              "ERROR_RECALL_STATE_DIR" directory)
                  :out :string :err :string})]
    (is (= 0 (:exit result)))
    (is (= "" (:out result)))
    (is (= "" (:err result)))
    (let [record (json/parse-string
                  (slurp (recall/log-path directory "row-down")) true)]
      (is (= "row-down" (:row-id record)))
      (is (= [] (:surfaced-memory-ids record)))
      (is (some #{"setIntegral_mono"} (:terms record))))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
