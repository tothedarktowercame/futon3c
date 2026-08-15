(ns futon3c.wm.needs-you-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.needs-you :as needs-you]
            [futon3c.wm.operator-bulletin :as bulletin])
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

(deftest action->needs-you-item-attaches-pattern-warrant-test
  (let [entry (assoc learn-entry
                     :guardrails/pattern-warrant
                     {:pattern-id :orchestration/pattern-warranted-choice-point
                      :warrant "operator-only by the consent posture"
                      :gap "confirm"
                      :unblock "Confirm."})
        item (needs-you/action->needs-you-item entry "run-1")]
    (is (= :orchestration/pattern-warranted-choice-point
           (get-in item [:pattern-warrant :pattern-id])))
    (is (= "Confirm." (:unblock-action item)))
    (is (= "Sorry Joe, because of orchestration/pattern-warranted-choice-point: confirm"
           (needs-you/sorry-joe-line item)))))

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

(defn- proctor-finding [id summary compromised?]
  {:finding/id id
   :finding/cycle-id "cycle/t94J02/1"
   :finding/summary summary
   :finding/compromised? compromised?})

(deftest compromised-proctor-finding-survives-bulletin-as-nag
  (let [item (needs-you/proctor-finding->needs-you-item
              (proctor-finding :seat-contamination
                               "solver seat was not exclusive" true))
        projected (bulletin/build-bulletin [item])]
    (is (= :nag (:lane item)))
    (is (= [(:id item)] (mapv :id (:nag projected))))
    (is (empty? (:brief projected)))
    (is (zero? (:silent-count projected)))))

(deftest ordinary-and-nothing-to-report-findings-are-recorded-as-brief
  (let [ordinary (needs-you/proctor-finding->needs-you-item
                  (proctor-finding :classification
                                   "one guidance row was not helpful" false))
        nothing (needs-you/proctor-finding->needs-you-item
                 (proctor-finding :close-witness "nothing to report" false))
        projected (bulletin/build-bulletin [ordinary nothing])]
    (is (= [:brief :brief] (mapv :lane [ordinary nothing])))
    (is (= #{(:id ordinary) (:id nothing)}
           (set (map :id (:brief projected)))))
    (is (= 2 (:total projected)))
    (is (zero? (:silent-count projected)))))

(deftest emit-proctor-finding-preserves-current-items
  (let [path (temp-file)
        existing {:id "existing" :lane :brief :g-total 0.0}]
    (try
      (needs-you/emit-needs-you! [existing] {:path path})
      (needs-you/emit-proctor-finding!
       (proctor-finding :close-witness "nothing to report" false)
       {:path path})
      (let [written (edn/read-string (slurp path))]
        (is (= 2 (count written)))
        (is (some #(= "existing" (:id %)) written))
        (is (some #(and (= :apm-proctor (:source %))
                        (= :brief (:lane %)))
                  written))
        (is (not-any? #(and (= :apm-proctor (:source %))
                            (= :silent (:lane %)))
                      written)))
      (finally
        (delete-tree! (.getParent (io/file path)))))))

(deftest compromise-finding-outranks-a-full-board-of-routine-work
  ;; At -1.0 this lost to ten ordinary items at -5.0 and fell into the overflow,
  ;; so the operator would have seen a count instead of the compromise.
  (let [p (str (java.io.File/createTempFile "needs" ".edn"))
        board (vec (for [i (range 10)]
                     {:id (str "wm-" i) :lane :nag :g-total -5.0 :salience 5.0}))]
    (spit p (pr-str board))
    (needs-you/emit-proctor-finding!
     #:finding{:id "f1" :cycle-id "c1" :summary "seats not exclusive"
               :compromised? true}
     {:path p})
    (let [out (edn/read-string (slurp p))]
      (is (some #(re-find #"apm-proctor" (str (:id %))) out)
          "a compromised cycle must never be displaced into the overflow"))))

(deftest unreadable-board-is-not-an-empty-board
  ;; Defaulting to [] would rewrite the file with only this finding, deleting
  ;; every unrelated operator item with no signal.
  (let [p (str (java.io.File/createTempFile "needs" ".edn"))]
    (spit p "[{:id \"truncated-mid-write")
    (is (thrown? clojure.lang.ExceptionInfo
                 (needs-you/emit-proctor-finding!
                  #:finding{:id "f1" :cycle-id "c1" :summary "x"
                            :compromised? true}
                  {:path p})))))
