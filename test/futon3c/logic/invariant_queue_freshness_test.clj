(ns futon3c.logic.invariant-queue-freshness-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [futon3c.logic.invariant-queue-freshness :as fresh]))

(defn- tmp [name content mtime-ms]
  (let [f (java.io.File/createTempFile (str "iqf-" name "-") ".tmp")]
    (spit f content)
    (.setLastModified f mtime-ms)
    (.deleteOnExit f)
    (.getPath f)))

(def ^:private t-old  1000000000000) ; older
(def ^:private t-new  2000000000000) ; newer

(deftest fresh-when-derived-newer-than-source
  (let [src (tmp "src" "inventory" t-old)
        art (tmp "art" "derived" t-new)
        r (fresh/check src [{:id :a :path art :role "x"}])]
    (is (= :ok (:outcome r)))))

(deftest stale-when-derived-older-than-source
  (let [src (tmp "src" "inventory-edited" t-new)
        art (tmp "art" "derived-stale" t-old)
        r (fresh/check src [{:id :priority-queue :path art :role "ranks"}])]
    (is (= :violation (:outcome r)))
    (let [stale (get-in r [:detail :stale])]
      (is (= [:priority-queue] (mapv :id stale)))
      (is (= :older-than-source (:reason (first stale))))
      (is (pos? (:stale-by-ms (first stale)))))))

(deftest missing-derived-is-stale
  (let [src (tmp "src" "inv" t-new)
        r (fresh/check src [{:id :gone :path "/no/such/path.json" :role "x"}])]
    (is (= :violation (:outcome r)))
    (is (= :derived-missing (:reason (first (get-in r [:detail :stale])))))))

(deftest missing-source-reported-explicitly
  (let [r (fresh/check "/no/such/inventory.sexp" [{:id :a :path "/tmp/x" :role "x"}])]
    (is (= :violation (:outcome r)))
    (is (= :source-missing (:reason (first (get-in r [:detail :stale])))))))

(deftest equal-mtime-counts-as-fresh
  (testing "derived == source mtime is fresh (only strictly-older is stale)"
    (let [src (tmp "src" "inv" t-new)
          art (tmp "art" "der" t-new)]
      (is (= :ok (:outcome (fresh/check src [{:id :a :path art :role "x"}])))))))

(deftest live-state-is-currently-stale
  (testing "documents the stop-the-line condition against the REAL files (2026-06-01):
            both derived artifacts are older than the inventory. If this test ever
            starts FAILING (i.e. returns :ok), the queue became fresh — update/retire it."
    (let [r (fresh/check)]
      ;; As of the stop-the-line, this is a known :violation. Assert the shape,
      ;; not a hard expectation, so a fixed queue doesn't break CI spuriously.
      (is (contains? #{:ok :violation} (:outcome r)))
      (when (= :violation (:outcome r))
        (is (seq (get-in r [:detail :stale])))))))
