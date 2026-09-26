(ns futon3c.diagramprover.causal.guard-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.guard :as guard]
            [futon3c.diagramprover.causal.receipts :as receipts]))

(def memory (dag/load-spec receipts/memory-spec-path))

(deftest star-forest-licenses-mechanism-b
  (let [result (guard/mechanism-verdict :star-forest memory)]
    (is (true? (:licensed? result)))
    (is (= [] (get-in result [:verdict :paths])))
    (is (= result (guard/guard! :star-forest memory)))))

(deftest populated-graph-refuses-with-named-path
  (let [result (guard/mechanism-verdict :populated-graph memory)]
    (is (false? (:licensed? result)))
    (is (= [[:M-in-store :shared-patterns :V12-minus-M]]
           (get-in result [:verdict :paths])))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"NOT licensed"
         (guard/guard! :populated-graph memory)))
    (is (= result (try (guard/guard! :populated-graph memory)
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(deftest unknown-topology-throws
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"Unknown corpus topology"
       (guard/mechanism-verdict :ring-lattice memory))))

(deftest verdicts-are-deterministic
  (is (= (guard/mechanism-verdict :star-forest memory)
         (guard/mechanism-verdict :star-forest memory)))
  (is (= (guard/mechanism-verdict :populated-graph memory)
         (guard/mechanism-verdict :populated-graph memory))))
