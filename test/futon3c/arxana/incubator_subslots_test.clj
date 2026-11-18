(ns futon3c.arxana.incubator-subslots-test
  (:require [clojure.test :refer [deftest is]] [clojure.java.io :as io]
            [futon3c.arxana.incubator-subslots :as sub]
            [futon3c.arxana.incubator-slots :as slots]))
(deftest schema-patterns-and-empty-assignments
  (let [prose (set (map (comp keyword first) (filter #(= :prose (nth % 2)) slots/fields)))]
    (is (= prose (set (keys sub/schemas))))
    (doseq [[_ schema] sub/schemas]
      (is (= "thesis" (:id (first schema))))
      (doseq [s schema]
        (let [path (io/file (System/getProperty "user.home") "code/futon3/library" (str (:pattern s) ".flexiarg"))]
          (is (.isFile path) (str path))
          (is (.contains (slurp path) (str "@flexiarg " (:pattern s)))))))
    (is (empty? (:subslots (sub/annotate {:id "links" :kind :url})))))
  (is (empty? (:acts (slots/session [] [])))))
(deftest attributed-stable-order-and-clear
  (let [rows [{:id "p1" :slot "summary" :text "Mechanism."}
              {:id "edit/1/sentence/0/7" :slot "summary" :text "Thesis."}]
        input {:acts []}
        a {:subject "edit/1/sentence/0/7" :slot "summary" :subslot "thesis" :actor "agent-fixture" :time "2026-11-18T00:00:00Z"}
        assigned (sub/assign input rows a)]
    (is (= rows (sub/ordered [] rows "summary")))
    (is (= (reverse rows) (sub/ordered (:acts assigned) rows "summary")))
    (is (= "agent-fixture" (:actor (first (:acts assigned)))))
    (is (= rows (sub/ordered (:acts (sub/assign assigned rows (assoc a :subslot nil))) rows "summary")))
    (is (thrown? clojure.lang.ExceptionInfo (sub/assign input rows (assoc a :actor nil))))
    (is (thrown? clojure.lang.ExceptionInfo (sub/assign input rows (assoc a :subslot "unknown"))))
    (is (nil? (sub/assignment (:acts assigned) {:id "edit/2/sentence/0/7" :slot "summary"})))))
