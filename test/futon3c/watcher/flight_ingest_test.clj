(ns futon3c.watcher.flight-ingest-test
  "The test claude-3's root-cause flag asked for: the --write path
   (ingest-projection!) had NO coverage, which is exactly where the
   post-entity! arity bug hid. Covers: the operator gate refuses; the call
   shapes (entities carry no :labels — the 8d5d094 regression); footgun-1
   merge idempotency (re-ingest does not clobber another lane's props); and
   annotation docs carry labels + base-props."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.file-ingest :as file-ingest]
            [futon3c.watcher.flight-ingest :as fi]
            [futon3c.watcher.projections.flight :as flight]))

(def settled-window
  {:begin "2026-06-12T12:00:00Z" :commit "2026-06-12T12:00:18Z"
   :threshold "2026-06-12T12:01:43Z"
   :scans [{:as-of "2026-06-12T12:03:00Z" :g {:g -4.06 :g-grain :one-step-action}}
           {:as-of "2026-06-12T12:05:00Z" :g {:g -4.0601 :g-grain :one-step-action}}]
   :epsilon 0.005 :agreement 0.0001})

(def fixture-record
  {:flight/id "fixture-ingest"
   :flight/derivation :full
   :organs
   {:field-read {:judgment {:gauge {:ref "fixture" :count 2}}
                 :ground "fixture begin artifact"}
    :measurement {:judgment {:predicted {:g -4.1 :g-grain :one-step-action}
                             :realised {:g -4.0601 :g-grain :one-step-action}
                             :class :clean
                             :window :window}
                  :ground :window}
    :window {:judgment settled-window
             :ground "fixture settle protocol"}
    :act {:judgment {:state :executed
                     :witness {:ref "futon7 fffffff" :verified-by "fixture"
                               :verification "fixture gates"}}
          :ground "fixture commit"}}})

(deftest write-gate-refuses-without-operator-env
  (if (System/getenv "FUTON3C_FLIGHT_INGEST_WRITE")
    (println "SKIP write-gate-refuses test: FUTON3C_FLIGHT_INGEST_WRITE is set in this environment")
    (with-redefs [fi/write-sentinel-path
                  (str (System/getProperty "java.io.tmpdir") "/no-such-sentinel.edn")]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"operator gate"
                            (fi/assert-write-gate!))))))

(deftest write-gate-sentinel-form
  (let [tmp (str (System/getProperty "java.io.tmpdir")
                 "/flight-ingest-sentinel-test-" (System/nanoTime) ".edn")]
    (with-redefs [fi/write-sentinel-path tmp]
      (testing "unexpired sentinel arms the gate"
        (spit tmp (pr-str {:until (str (.plusSeconds (java.time.Instant/now) 600))
                           :by "test-operator"}))
        (is (nil? (fi/assert-write-gate!))))
      (testing "expired sentinel refuses"
        (spit tmp (pr-str {:until (str (.minusSeconds (java.time.Instant/now) 1))
                           :by "test-operator"}))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"operator gate"
                              (fi/assert-write-gate!))))
      (testing "malformed sentinel refuses"
        (spit tmp "{:until :not-a-timestamp")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"operator gate"
                              (fi/assert-write-gate!)))))
    (io/delete-file tmp true)))

(deftest write-path-call-shapes-and-idempotency
  (let [posted-entities (atom [])
        posted-docs (atom [])
        projection (flight/project-record "fixture.flight.edn" fixture-record)
        essay-id (get-in projection [:essay :id])]
    (with-redefs [fi/assert-write-gate! (fn [] nil)
                  ;; the essay already exists in substrate with another lane's
                  ;; prop — re-ingest must keep it (footgun 1)
                  fi/fetch-existing-entity (fn [id]
                                             (when (= id essay-id)
                                               {:id id :props {:other-lane "kept"}}))
                  file-ingest/post-entity! (fn [e] (swap! posted-entities conj e) {:ok true})
                  file-ingest/post-hyperedge-doc! (fn [d] (swap! posted-docs conj d) {:ok true})]
      (let [result (fi/ingest-projection! projection "futon3c")]
        (testing "counts: essay + sections as entities; one annotation per organ"
          (is (= (+ 1 (count (:sections projection)))
                 (:entities result)
                 (count @posted-entities)))
          (is (= (count (:annotations projection))
                 (:annotations result)
                 (count @posted-docs))))
        (testing "the 8d5d094 regression: entities carry NO :labels and have the post-entity! shape"
          (is (every? #(not (contains? % :labels)) @posted-entities))
          (is (every? #(and (:id %) (:name %) (:type %) (map? (:props %))) @posted-entities)))
        (testing "footgun 1: re-ingest merges, never clobbers another lane's props"
          (let [essay (first (filter #(= essay-id (:id %)) @posted-entities))]
            (is (= "kept" (get-in essay [:props :other-lane])))
            (is (= "wm-flight" (get-in essay [:props :record-kind]))
                "projected props still present alongside the preserved ones")))
        (testing "annotation docs carry the labels and merged base-props"
          (is (every? #(some #{"wm-flight"} (:labels %)) @posted-docs))
          (is (every? #(= "futon3c" (get-in % [:props "repo"])) @posted-docs))
          (is (every? #(= "arxana/flight-organ-annotation" (:hx-type %)) @posted-docs)))))))
