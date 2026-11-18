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
                 :records-by-field {:status :ok :records (filterv #(= (:value query) (get-in % [:method :check/id])) rs)}
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
    (is (= #{:records-by-type :records-by-field :record-by-id} (set (map :query/type @calls))))))

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
             (fake-store rs (atom []) #(when (= :records-by-field (:query/type %))
                                        {:ok {:status :ok :records [] :truncated? true}})) at)]
      (is (= :inconclusive (:status v)))
      (is (= :truncated (:reason v))))
    (is (= :reference-unavailable
           (:reason (view/read-catalogue
                     (fake-store rs (atom []) #(when (= :record-by-id (:query/type %))
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
