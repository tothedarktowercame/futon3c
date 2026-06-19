(ns futon3c.watcher.projections.flight-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.projections.flight :as flight]))

(def settled-window
  {:begin "2026-06-12T12:00:00Z" :commit "2026-06-12T12:00:18Z"
   :threshold "2026-06-12T12:01:43Z"
   :scans [{:as-of "2026-06-12T12:03:00Z" :g {:g -4.06 :g-grain :one-step-action}}
           {:as-of "2026-06-12T12:05:00Z" :g {:g -4.0601 :g-grain :one-step-action}}]
   :epsilon 0.005 :agreement 0.0001})

(def conforming-record
  {:flight/id "fixture-flight"
   :flight/derivation :full
   :organs {:field-read {:judgment {:class :chosen :target "M-demo"}
                         :ground "chosen from fixture neighbourhood"}
            :measurement {:judgment {:class :null :window :window}
                          :ground :window}
            :window {:judgment settled-window
                     :ground "fixture settle protocol"}
            :warrant {:sorry {:kind :typed-gap
                              :why "fixture intentionally omits warrant"}}}})

(deftest project-conforming-flight
  (let [projection (flight/project-record "fixture.flight.edn" conforming-record)]
    (is (= "arxana/essay" (get-in projection [:essay :type])))
    (is (= "arxana/essay/flight/fixture-flight" (get-in projection [:essay :id])))
    (is (= 4 (count (:sections projection))))
    (is (= 4 (count (:annotations projection))))
    (is (= "arxana/flight-organ-annotation" (:hx-type (first (:annotations projection)))))
    (is (= #{"annotated" "flight"}
           (set (map :role (:endpoints (first (:annotations projection)))))))
    (is (every? :id (:endpoints (first (:annotations projection)))))
    (is (= "typed-sorry"
           (some (fn [section]
                   (when (= "warrant" (get-in section [:props :organ/key]))
                     (get-in section [:props :organ/state])))
                 (:sections projection))))))

(deftest merge-entity-for-upsert-preserves-existing-props
  (let [existing {:id "entity-1"
                  :name "Existing"
                  :type "arxana/essay"
                  :props {:other-lane "keep" :flight/id "old"}}
        projected {:id "entity-1"
                   :name "Projected"
                   :type "arxana/essay"
                   :props {:flight/id "new" :section-count 13}}
        merged (flight/merge-entity-for-upsert existing projected)]
    (is (= "Projected" (:name merged)))
    (is (= {:other-lane "keep" :flight/id "new" :section-count 13}
           (:props merged)))))

(deftest refuses-derivation-thin-record
  (testing "thin records are fallback-only, not canonical substrate writes"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not canonical-ingestable"
                          (flight/project-record "thin.edn"
                                                 (assoc conforming-record
                                                        :flight/id "thin"
                                                        :flight/derivation :thin))))))

(deftest refuses-transient-measurement
  (testing "transient measurements are not mask-IN canonical records"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not canonical-ingestable"
                          (flight/project-record "transient.edn"
                                                 (assoc-in conforming-record
                                                           [:organs :measurement :judgment :class]
                                                           :transient))))))

(deftest refuses-unsettled-window
  (testing "a clean/null claim without a settled two-scan witness must not become substrate (F2 at the boundary)"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not canonical-ingestable"
                          (flight/project-record "unsettled.edn"
                                                 (assoc-in conforming-record
                                                           [:organs :window :judgment :scans 0 :as-of]
                                                           "2026-06-12T12:00:20Z"))))))
