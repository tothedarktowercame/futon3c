(ns futon3c.wm.needs-you-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.needs-you :as needs-you])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-file []
  (let [dir (Files/createTempDirectory "wm-needs-you-test"
                                       (into-array FileAttribute []))]
    (.getAbsolutePath (io/file (str dir) "needs-you.edn"))))

(defn- delete-tree! [path]
  (let [root (io/file path)]
    (when (.exists root)
      (doseq [^File f (reverse (file-seq root))]
        (.delete f)))))

(def ^:private learn-entry
  {:action {:type :learn-action-class :target :open-mission}
   :g-total -3.0
   :rank 1})

(deftest action->needs-you-item-shape-test
  (let [item (needs-you/action->needs-you-item learn-entry "run-1")]
    (is (= "wm-needs-learn-action-class-open-mission" (:id item)))
    (is (= "nag" (:lane item)))
    (is (= "wm-needs-you" (:source item)))
    (is (= :learn-action-class (:wm-action-class item)))
    (is (= -3.0 (:g-total item)))
    ;; display salience = negated EFE (higher = more salient for the NAG lane)
    (is (= 3.0 (:salience item)))
    (is (= "run-1" (:run-id item)))
    (is (:unblock-action item))
    (is (:why item))))

(deftest emit-needs-you-dedupes-last-wins-and-writes-vector-test
  (let [path (temp-file)]
    (try
      (let [old (assoc (needs-you/action->needs-you-item learn-entry "old")
                       :title "old title"
                       :salience 1.0)
            new (assoc (needs-you/action->needs-you-item learn-entry "new")
                       :title "new title"
                       :salience -1.0)
            result (needs-you/emit-needs-you! [old new] {:path path})
            written (edn/read-string (slurp path))]
        (is (= 1 (:emitted-count result)))
        (is (= 1 (count written)))
        (is (= "new title" (:title (first written)))))
      (finally
        (delete-tree! (.getParent (io/file path)))))))

(deftest emit-needs-you-caps-with-advisory-item-test
  (let [path (temp-file)
        items (for [i (range 4)]
                {:id (str "item-" i)
                 :title (str "Item " i)
                 :why "blocked"
                 :unblock-action "clear it"
                 :lane "nag"
                 :source "wm-needs-you"
                 :target (str "target-" i)
                 :path nil
                 :salience i
                 :repo nil
                 :wm-action-class :learn-action-class
                 :g-total i
                 :emitted-at "2026-06-06T00:00:00Z"
                 :run-id "run"})]
    (try
      (let [result (needs-you/emit-needs-you! items {:path path :top-k 3})
            written (edn/read-string (slurp path))]
        (is (true? (:capped? result)))
        (is (= 3 (count written)))
        (is (= ["item-0" "item-1" "wm-needs-overflow"] (mapv :id written)))
        (is (= "Review the WM ranked-actions or raise the needs-you cap for this run."
               (:unblock-action (last written)))))
      (finally
        (delete-tree! (.getParent (io/file path)))))))
