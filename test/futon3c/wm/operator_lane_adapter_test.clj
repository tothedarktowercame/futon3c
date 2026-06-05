(ns futon3c.wm.operator-lane-adapter-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.operator-bulletin :as bulletin]
            [futon3c.wm.operator-lane :as lane]
            [futon3c.wm.operator-lane-adapter :as adapter]))

(def fixture-semilattice
  {:backlog
   [{:name "M-1.md" :c-joint 0.80 :days-since 45 :declared "IDENTIFY draft" :repo "futon"}
    {:name "M-2.md" :c-joint 0.70 :days-since 5  :declared "HEAD proposal" :repo "futon"}
    {:name "M-3.md" :c-joint 0.60 :days-since 60 :declared "INSTANTIATE proof" :repo "futon"}
    {:name "M-4.md" :c-joint 0.50 :days-since 60 :declared "DERIVE result" :repo "futon"}
    {:name "M-5.md" :c-joint 0.40 :days-since 60 :declared "MAP facts" :repo "futon"}
    {:name "M-6.md" :c-joint 0.30 :days-since 60 :declared "ARGUE claim" :repo "futon"}
    {:name "M-7.md" :c-joint 0.20 :days-since 60 :declared "VERIFY result" :repo "futon"}
    {:name "M-8.md" :c-joint 0.10 :days-since 60 :declared "draft note" :repo "futon"}]})

(def fixture-mint
  {:priced-sorries
   [{:sorry :S-invoice-4
     :note "invoice four"
     :discharge "issue Invoice #4, send"
     :blocked? false
     :p 0.9
     :expected-lift 641.25}
    {:sorry :S-scenario-c
     :note "scenario continuation"
     :discharge "deliver continuation"
     :blocked? true
     :p 0.7
     :expected-lift 3150.0}]
   :unpriced-sorries
   [{:sorry :S-cold-conversion
     :note "the crux"
     :discharge "one SENT cold cycle"
     :p :unsampled}]})

(defn- temp-edn-file [x]
  (let [f (java.io.File/createTempFile "operator-lane-adapter" ".edn")]
    (.deleteOnExit f)
    (spit f (pr-str x))
    (.getAbsolutePath f)))

(defn fixture-items []
  (adapter/forward-model-items
   {:semilattice-path (temp-edn-file fixture-semilattice)
    :mint-path (temp-edn-file fixture-mint)}))

(defn by-id [items id]
  (first (filter #(= id (:id %)) items)))

(deftest mission-top-quartile-and-declared-parsing
  (let [items (fixture-items)]
    (testing "8 fixture missions make the top 2 central"
      (is (= #{"M-1.md" "M-2.md"}
             (set (map :id (filter #(and (= :mission (:source %))
                                         (:futon-important? %)) items))))))
    (testing "declared phases parse framing-blocked vs eligible phases"
      (is (:framing-blocked? (by-id items "M-1.md")))
      (is (:framing-blocked? (by-id items "M-2.md")))
      (is (not (:framing-blocked? (by-id items "M-3.md"))))
      (is (not (:framing-blocked? (by-id items "M-4.md")))))))

(deftest mission-risk-mode-is-stale-and-central
  (let [items (fixture-items)]
    (is (:risk-mode? (by-id items "M-1.md")))
    (is (not (:risk-mode? (by-id items "M-2.md"))))
    (is (not (:risk-mode? (by-id items "M-3.md"))))))

(deftest business-stream-mapping
  (let [items (fixture-items)]
    (is (= #{:business-sorry}
           (set (map :source (filter #(keyword? (:id %)) items)))))
    (is (:risk-mode? (by-id items :S-scenario-c)))
    (is (:risk-mode? (by-id items :S-cold-conversion)))
    (is (:operator-dependent? (by-id items :S-invoice-4)))
    (is (= 641.25 (:salience (by-id items :S-invoice-4))))))

(deftest end-to-end-classifier-and-bulletin
  (let [items (fixture-items)
        lanes (frequencies (map lane/classify-item items))
        b     (bulletin/build-bulletin items :date "fixture")]
    (is (= :mission (:source (by-id items "M-1.md"))))
    (is (pos? (:brief lanes)))
    (is (pos? (:silent lanes)))
    (is (empty? (:nag b)))
    (is (seq (:brief b)))
    (is (pos? (:silent-count b)))
    (is (= (count items) (:total b)))))
