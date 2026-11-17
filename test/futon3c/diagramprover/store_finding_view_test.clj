(ns futon3c.diagramprover.store-finding-view-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.diagramprover.store-finding-view :as sut]
            [futon3c.diagramprover.finding-view-test :as fixture]
            [futon3c.diagramprover.arxana-records :as ar]))
(defn fake-store [records override calls skip-time?]
  (let [by-id (into {} (map (juxt :id identity) records))]
    (reify store/ArxanaGraphStore
      (bounded-query! [_ q]
        (swap! calls conj q)
        {:ok
         (if (= :record-by-id (:query/type q))
           (let [r (get by-id (:record-id q))]
             (if (and r (or skip-time? (not (pos? (compare (:event-time r fixture/t0) (:valid-time q))))))
               {:status :ok :record r} {:status :missing}))
           (or override
               (let [ids (vec (for [r records :when (= (:edge-type q) (:type-revision r))
                                   :when (some #(and (= (:endpoint-role q) (:role %))
                                                    (= (:endpoint-id q) (get-in % [:target :clusion/id]))) (:endpoints r))]
                               (:id r)))]
                 {:status :ok :edge-ids ids :total (count ids) :truncated? false :watermark 42})))}))))

(deftest future-supersession-uses-as-of
  (let [calls (atom []) records [fixture/f12 fixture/supersession]
        r (sut/store-finding-view (fake-store records nil calls false) (:id fixture/f12) fixture/t0)]
    (is (= :failing (:display r)))
    (is (false? (:stale? r)))
    (is (some #(and (= (:id fixture/supersession) (:record-id %)) (= fixture/t0 (:valid-time %))) @calls))
    (is (= [42 42 42] (mapv :watermark (:queries r))))
    (let [mutant-calls (atom [])]
      (with-redefs [http/get-entity-as-of (fn [_ id _] {:ok (some #(when (= id (:id %)) %) records)})]
        (sut/store-finding-view (fake-store records nil mutant-calls false) (:id fixture/f12) fixture/t0))
      (is (not= (filter #(= :record-by-id (:query/type %)) @calls)
                (filter #(= :record-by-id (:query/type %)) @mutant-calls))))
    ;; Skipping temporal reads admits a record the store says is missing. The
    ;; pure view ALSO filters event-time, so display alone cannot catch this bug.
    (is (= :ok (get-in (store/bounded-query! (fake-store records nil (atom []) true)
                                            {:query/type :record-by-id :record-id (:id fixture/supersession)
                                             :valid-time fixture/t0}) [:ok :status])))
    (is (= :missing (get-in (store/bounded-query! (fake-store records nil (atom []) false)
                                                 {:query/type :record-by-id :record-id (:id fixture/supersession)
                                                  :valid-time fixture/t0}) [:ok :status])))))

(deftest incomplete-index-is-not-an-empty-view
  (doseq [[override reason] [[{:status :stale-index} :stale-index]
                            [{:status :ok :total 100 :truncated? true :edge-ids []} :truncated]]]
    (let [r (sut/store-finding-view (fake-store [fixture/f12] override (atom []) false) (:id fixture/f12) fixture/t0)
          mutant (sut/store-finding-view (fake-store [fixture/f12] {:status :ok :total 0 :truncated? false :edge-ids []}
                                                     (atom []) false) (:id fixture/f12) fixture/t0)]
      (is (= :inconclusive (:status r)))
      (is (= reason (:reason r)))
      (is (not= (:status r) (:status mutant))))))

(deftest residual-and-missing
  (let [residual (assoc (ar/residual-record (:id fixture/f12) {:scope "fixture" :release "v1" :reason "known"})
                        :event-time fixture/t1)
        r (sut/store-finding-view (fake-store [fixture/f12 residual] nil (atom []) false) (:id fixture/f12) fixture/t2)
        no-residual (sut/store-finding-view (fake-store [fixture/f12] nil (atom []) false) (:id fixture/f12) fixture/t2)
        missing (sut/store-finding-view (fake-store [] nil (atom []) false) (:id fixture/f12) fixture/t0)]
    (is (= :failing-accepted (:display r)))
    (is (not= (:display r) (:display no-residual)))
    (is (= :missing (:status missing)))
    (is (not= {} missing))))

(deftest query-error-is-preserved
  (let [error {:error/code :arxana.store/http-unavailable :error/category :failed}
        broken (reify store/ArxanaGraphStore
                 (bounded-query! [_ q]
                   (if (= :record-by-id (:query/type q))
                     {:ok {:status :ok :record fixture/f12}} {:error error})))]
    (is (= error (:error (sut/store-finding-view broken (:id fixture/f12) fixture/t0))))))
