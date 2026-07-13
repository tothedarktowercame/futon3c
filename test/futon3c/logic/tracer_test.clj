(ns futon3c.logic.tracer-test
  "Tests for futon3c.logic.tracer — pipeline-tracer items.

   Mission: M-invariant-queue-extend (futon3c/holes/missions/), tracker
   reframe 2026-04-29."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.logic.tracer :as tracer]))

(def ^:dynamic *xtdb-backend* nil)

(def prototype-path "holes/excursions/pipeline-prototype.edn")

(defn prototype []
  (edn/read-string (slurp prototype-path)))

(def test-tracers
  [{:track-id :test-track-a
    :title "test a"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-06"
    :expected-outcome "test outcome a"
    :owner nil}
   {:track-id :test-track-b
    :title "test b"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-07"
    :expected-outcome "test outcome b"
    :owner nil}])

(use-fixtures
  :each
  (fn [f]
    (let [node (xtdb/start-node {})]
      (try
        (binding [*xtdb-backend* (xb/make-xtdb-backend node)]
          (f))
        (finally
          (.close node))))))

(deftest emit-tracer-emits-pipeline-tracer-item
  (testing "emit-tracer! emits one entry tagged :pipeline-tracer + :open"
    (let [tracer-data {:track-id :test-track
                       :title "test"
                       :mission :M-invariant-queue-extend
                       :target-date "2026-05-06"
                       :expected-outcome "test outcome"
                       :owner nil}
          receipt (tracer/emit-tracer! *xtdb-backend* tracer-data)]
      (is (:ok receipt) (str "expected emit success, got " (pr-str receipt))))))

(deftest emit-pipeline-tracers-emits-one-per-tracer
  (testing "emit-pipeline-tracers! returns one receipt per explicit tracer"
    (let [receipts (tracer/emit-pipeline-tracers! *xtdb-backend* test-tracers)]
      (is (= (count test-tracers) (count receipts))
          "one receipt per supplied tracer")
      (is (every? :ok receipts) "all emits succeeded"))))

(deftest open-tracers-queryable-by-tag
  (testing "after emission, open tracers are queryable via :pipeline-tracer + :open"
    (tracer/emit-pipeline-tracers! *xtdb-backend* test-tracers)
    (let [open (store/query* *xtdb-backend*
                             {:query/type :coordination
                              :query/tags [:pipeline-tracer :open]})]
      (is (= (count test-tracers) (count open))
          "tag query returns one entry per tracer"))))

(deftest emit-tracer-closed-emits-resolution
  (testing "emit-tracer-closed! emits a resolution entry for a track-id"
    (let [_ (tracer/emit-tracer! *xtdb-backend*
                                 {:track-id :soon-to-close
                                  :title "x"
                                  :mission :M-invariant-queue-extend
                                  :target-date "2026-05-06"
                                  :expected-outcome "x"
                                  :owner nil})
          receipt (tracer/emit-tracer-closed!
                   *xtdb-backend*
                   {:track-id :soon-to-close
                    :resolution "test resolution"
                    :closed-by "tracer-test"})]
      (is (:ok receipt) (str "expected close emit success, got " (pr-str receipt))))))

(deftest canonical-statement-grep-verifiable
  (is (string? tracer/I-pipeline-tracer))
  (is (re-find #"I-pipeline-tracer" tracer/I-pipeline-tracer)))

;; -----------------------------------------------------------------------------
;; ensure-default-tracers! is idempotent (M-reachable-from-boot)
;; -----------------------------------------------------------------------------

(deftest ensure-default-tracers-emits-when-empty
  (testing "an empty runtime projection returns without touching storage"
    (with-redefs [store/query* (fn [& _]
                                 (throw (ex-info "storage must stay dark" {})))]
      (is (= {:already-present 0
              :emitted 0
              :attempted 0
              :failed []}
             (tracer/ensure-default-tracers! *xtdb-backend*))))))

(deftest ensure-default-tracers-is-idempotent
  (testing "second call against the same store emits nothing"
    (tracer/ensure-default-tracers! *xtdb-backend*)
    (let [r (tracer/ensure-default-tracers! *xtdb-backend*)]
      (is (= 0 (:already-present r)))
      (is (= 0 (:emitted r)))
      (is (= 0 (:attempted r))))))

(deftest default-tracers-are-unhooked-from-runtime
  (testing "historical prototype data is not boot-time tracer data"
    (is (empty? tracer/default-tracers))
    (is (= "holes/excursions/pipeline-prototype.edn"
           tracer/pipeline-prototype-path))))

(deftest pipeline-prototype-carries-unhooked-historical-tracks
  (testing "pipeline-prototype.edn preserves the historical six tracks for scans"
    (let [proto (prototype)
          items (:prototype/items proto)
          track-ids (set (map :track-id items))]
      (is (= :unhooked (:prototype/status proto)))
      (is (= 6 (count items)))
      (is (contains? track-ids :track-4-2-snapshot-as-evidence))
      (is (contains? track-ids :track-4-3-arxana-view-columns))
      (is (contains? track-ids :track-3-write-class-scoping))
      (is (contains? track-ids :track-1-substrate-2-lift))
      (is (contains? track-ids :track-2-war-machine-aif-lift))
      (is (contains? track-ids :track-5-vsatarcs))
      (is (every? :target-date items)))))
