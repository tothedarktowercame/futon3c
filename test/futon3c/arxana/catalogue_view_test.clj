(ns futon3c.arxana.catalogue-view-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.catalogue-view :as view]
            [futon3c.arxana.design-catalogue-test :as fixtures]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store]))

(def at fixtures/at)
(defn fixture-records []
  (let [{:keys [records choices]} (fixtures/built)
        coverage (:id (first (filter #(= :coverage (get-in % [:asserted-content :choice/key])) choices)))]
    (vec (remove #(= coverage (:subject-clusion-id %)) records))))
(defn fake-store [rs calls override]
  (reify store/ArxanaGraphStore
    (bounded-query! [_ query]
      (swap! calls conj query)
      (or (override query)
          {:ok (case (:query/type query)
                 :records-by-type {:status :ok :records (filterv #(= (:type-revision query) (:type-revision %)) rs)}
                 :attestations {:status :ok :total (count (filter #(= (:subject-clusion-id query) (:subject-clusion-id %)) rs))
                                :truncated? false :attestation-ids (mapv :id (filter #(= (:subject-clusion-id query) (:subject-clusion-id %)) rs))}
                 :record-by-id (if-let [r (first (filter #(= (:record-id query) (:id %)) rs))]
                                 {:status :ok :record r} {:status :missing}))}))
    (conditional-write! [_ _] (throw (ex-info "view wrote to store" {})))))

(deftest pure-and-queried-view
  (let [rs (fixture-records)
        v (view/catalogue-view rs at)
        choices (into {} (map (juxt :choice/key identity) (:choices v)))
        calls (atom [])
        queried (view/read-catalogue (fake-store rs calls (constantly nil)) at)]
    (is (= :ok (:status v)))
    (is (= (sort (keys choices)) (map :choice/key (:choices v))))
    (is (= {:status :unknown :reason :no-check} (:enforcement-status (choices :fibres))))
    (is (= {:status :unknown :reasons [:no-run]} (:enforcement-status (choices :coverage))))
    (doseq [key [:scope :functions :links]]
      (is (= {:status :enforced} (:enforcement-status (choices key))))
      (is (= :pass (get-in (choices key) [:checks 0 :latest-run :result]))))
    (is (= v (dissoc queried :queries)))
    (is (seq (:constrains (choices :scope))))
    (is (= (:record/id (fixtures/reference)) (get-in (choices :scope) [:constrains 0 :coordinate :record/id])))
    (is (= #{:records-by-type :attestations :record-by-id} (set (map :query/type @calls))))))

(deftest incomplete-never-partial
  (let [rs (fixture-records)]
    (doseq [response [{:ok {:status :ok}}
                      {:ok {:status :ok :truncated? true :records []}}
                      {:ok {:status :stale-index}}
                      {:error {:error/code :unavailable}}]]
      (let [v (view/read-catalogue (fake-store rs (atom []) (constantly response)) at)]
        (is (= :inconclusive (:status v)))
        (is (not (contains? v :choices)))))
    (let [v (view/read-catalogue
             (fake-store rs (atom []) #(when (= :attestations (:query/type %))
                                        {:ok {:status :ok :attestation-ids [] :truncated? true}})) at)]
      (is (= :inconclusive (:status v)))
      (is (= :truncated (:reason v))))
    (is (= :reference-unavailable
           (:reason (view/read-catalogue
                     (fake-store rs (atom []) #(when (= (:id (fixtures/reference)) (:record-id %))
                                                {:ok {:status :missing}})) at))))))

(deftest dispatch-and-validation
  (let [calls (atom []) cfg {:graph-store (fake-store (fixture-records) calls (constantly nil))}
        request {:protocol/version "arxana.v1" :message/id "catalogue-test"
                 :principal/id "catalogue-reader" :operation :catalogue/view :body {:valid-time at}}
        result (service/dispatch cfg request)
        before (count @calls)]
    (is (= :ok (get-in result [:ok :status])))
    (is (string? (get-in result [:ok :semantic/digest])))
    (is (:error (service/dispatch cfg (assoc-in request [:body :unknown] true))))
    (is (:error (service/dispatch cfg (assoc request :protocol/version "wrong"))))
    (is (:error (service/dispatch cfg (assoc request :body {}))))
    (is (:error (service/dispatch cfg (assoc-in request [:body :valid-time] "not-a-time"))))
    (is (= before (count @calls)))))

(deftest stale-index-is-retried-then-reported
  (let [calls (atom 0)
        stale (reify futon3c.arxana.store/ArxanaGraphStore
                (bounded-query! [_ _] (swap! calls inc) {:ok {:status :stale-index}}))
        r (futon3c.arxana.catalogue-view/read-catalogue stale "2026-11-18T00:00:00.000Z")]
      (is (= :inconclusive (:status r)))
      (is (= :stale-index (:reason r)))
      (is (= 3 (:attempts r)))))

(deftest debt-and-linked-visible-runs
  (let [rs (fixture-records)
        debt (:debt (view/catalogue-view rs at))
        groups (into {} (map (juxt :reason identity) (:by-reason debt)))
        bad (mapv #(if (= :pass (get-in % [:content :result]))
                     (assoc-in % [:content :result] :violation) %) rs)]
    (is (= [:fibres] (:choice-keys (groups :no-check))))
    (is (= [:coverage] (:choice-keys (groups :no-run))))
    (is (= [1 1 0] (mapv :count (:by-reason debt))))
    (is (= :not-assessed (get-in debt [:coverage :status])))
    (is (= 3 (get-in (view/catalogue-view bad at) [:debt :by-reason 2 :count])))
    (let [run (first (filter :subject-clusion-id rs))
          unrelated (assoc run :id "unrelated-run" :method {:check/id "unlinked"})
          future (assoc run :id "future-run")
          calls (atom [])
          result (view/read-catalogue
                  (fake-store (conj rs unrelated future) calls
                              #(when (= "future-run" (:record-id %)) {:ok {:status :missing}})) at)]
      (is (= (view/catalogue-view rs at) (dissoc result :queries)))
      (is (not-any? #(= :records-by-field (:query/type %)) @calls)))))
