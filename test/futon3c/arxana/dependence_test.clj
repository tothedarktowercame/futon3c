(ns futon3c.arxana.dependence-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.dependence :as d]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.type-registry :as types]))

(def t0 "2026-01-01T00:00:00.000Z")
(def t1 "2026-01-02T00:00:00.000Z")
(defn endpoint [role id] {:role role :kind :clusion :target {:clusion/id id}})
(def item (records/make-clusion {:type-revision [:workplan/item 1] :endpoints []
                                :asserted-content {:item/id "M-invented"}}))
(def successor (records/make-clusion {:type-revision [:workplan/item 1] :endpoints []
                                     :asserted-content {:item/id "M-invented-successor"}}))
(def benefit (records/make-clusion {:type-revision [:benefit-line 1] :endpoints []
                                   :asserted-content {:benefit/line "An invented benefit"}}))
(def serves (records/make-clusion {:type-revision [:workplan/serves 1]
                                  :endpoints [(endpoint :item (:id item)) (endpoint :benefit-line (:id benefit))]
                                  :asserted-content {}}))
(def supersession (assoc (records/make-clusion
                          {:type-revision [:supersedes 1]
                           :endpoints [(endpoint :successor (:id successor)) (endpoint :predecessor (:id item))]
                           :asserted-content {}}) :event-time t1))
(defn fake-store [rs override skip-time?]
  (let [by-id (into {} (map (juxt :id identity) rs))]
    (reify store/ArxanaGraphStore
      (bounded-query! [_ q]
        (if (= :record-by-id (:query/type q))
          (let [r (get by-id (:record-id q))]
            {:ok (if (and r (or skip-time? (not (pos? (compare (:event-time r t0) (:valid-time q))))))
                   {:status :ok :record r} {:status :missing})})
          (or override
              (let [ids (mapv :id (filter #(and (= (:type-revision %) (:edge-type q))
                                               (= (d/target-id % (:endpoint-role q)) (:endpoint-id q))) rs))]
                {:ok {:status :ok :edge-ids ids :total (count ids) :truncated? false}})))))))

(deftest workplan-dependence-without-diagramprover
  ;; Revision 1 is deliberate: :supersedes endpoints require clusions, whereas
  ;; revision 2's item endpoint is an entity.
  (is (= [:item :benefit-line] (mapv :role (:roles (types/declaration [:workplan/serves 1])))))
  (let [rs [item successor benefit serves supersession] s (fake-store rs nil false)]
    (is (= {:status :ok :ids [(:id item)]}
           (d/fibre s [:workplan/serves 1] :benefit-line :item (:id benefit) t0)))
    (is (= {:status :ok :stale? false :stale-because []}
           (d/staleness s [:workplan/serves 1] :benefit-line :item (:id benefit) t0)))
    (is (= {:status :ok :stale? true :stale-because [(:id supersession)]}
           (d/staleness s [:workplan/serves 1] :benefit-line :item (:id benefit) t1)))
    ;; Mutation: bypassing temporal filtering admits a future supersession.
    (is (:stale? (d/staleness (fake-store rs nil true) [:workplan/serves 1] :benefit-line :item (:id benefit) t0)))
    (doseq [[response reason] [[{:ok {:status :stale-index}} :stale-index]
                              [{:ok {:status :ok :truncated? true :edge-ids [(:id serves)]}} :truncated]]]
      (is (= {:status :inconclusive :reason reason}
             (d/fibre (fake-store rs response false) [:workplan/serves 1] :benefit-line :item (:id benefit) t0))))
    ;; Mutation: an incomplete index treated as empty gives a false complete result.
    (is (= {:status :ok :ids []}
           (d/fibre (fake-store rs {:ok {:status :ok :edge-ids []}} false)
                    [:workplan/serves 1] :benefit-line :item (:id benefit) t0)))
    (is (= {:status :ok :ids []}
           (d/fibre (fake-store [benefit serves] nil false) [:workplan/serves 1] :benefit-line :item (:id benefit) t0)))
    (let [error {:error {:error/code :invented/transport}}]
      (is (= error (d/reverse-records (fake-store rs error false) [:workplan/serves 1] :item (:id item) t0))))))
