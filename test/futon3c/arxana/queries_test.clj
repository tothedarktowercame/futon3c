(ns futon3c.arxana.queries-test
  (:require [clojure.test :refer [deftest is]]
            [futon1b-arxana :as a]
            [futon1b-server :as server]
            [futon3c.arxana.queries :as q]
            [futon3c.arxana.dependence :as dep]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [xtdb.node :as xtn]))

(deftest completeness-and-errors
  (is (= 200 (:limit (dep/reverse-query [:invented/link 1] :item "item"))))
  (doseq [response [{:ok {:status :stale-index :source/watermark 9 :index/watermark 8}}
                    {:ok {:status :ok :truncated? true :records []}}
                    {:error {:error/code :transport-failed}}]]
    (let [s (reify store/ArxanaGraphStore (bounded-query! [_ _] response))]
      (is (= (or (:ok response) response) (q/records-by-type s {}))))))

(deftest real-handler-wrappers
  (with-open [node (xtn/start-node)]
    (let [f (java.io.File/createTempFile "query-wrapper" ".db")
          srv (server/start-server! {:node node :port 0 :bind-host "127.0.0.1" :arxana-index-path (.getPath f)})
          s (http/http-store (str "http://127.0.0.1:" (.getPort (.getAddress srv))) a/service-principal)
          at "2026-09-01T00:00:00.000Z"
          base {:type-revision [:invented/span 1] :valid-time at}
          records (mapv (fn [n] {:id (str "arxana:clusion:sha256:wrapper-" n)
                                  :type-revision [:invented/span 1] :endpoints []
                                  :asserted-content {:start n :end (+ n 10) :tag :example}
                                  :event-time "2026-08-27T12:00:00.000Z"}) [1 2])]
      (try
        (is (:ok (http/append-batch! s "wrapper-batch"
                                   (mapv #(hash-map :operation :clusion/assert :record %) records))))
        (is (= records (:records (q/records-by-type s base))))
        (is (:truncated? (q/records-by-type s (assoc base :limit 1))))
        (is (= records (:records (q/records-by-field s (assoc base :field :tag :value :example)))))
        (is (:truncated? (q/records-by-field-range s (assoc base :field :start :limit 1
                              :lower {:value 0 :inclusive? true} :upper {:value 3 :inclusive? false}))))
        (is (= records (:records (q/records-by-span-overlap s (assoc base :start-field :start :end-field :end
                                                                      :start 5 :end 6)))))
        (is (empty? (:records (q/records-by-span-overlap s (assoc base :start-field :start :end-field :end
                                                :start 5 :end 6 :valid-time "2026-08-01T00:00:00.000Z")))))
        (is (:truncated? (q/type-census s {:valid-time at :limit 1})))
        (is (= 2 (reduce + (map :count (:counts (q/type-census s {:valid-time at}))))))
        (is (:ok (http/append-batch! s "attestations"
                     (mapv (fn [n] {:operation :evidence/append
                                    :record {:id (str "arxana:attestation:sha256:wrapper-" n)
                                             :subject-clusion-id (:id (first records))
                                             :event-time "2026-08-27T12:00:00.000Z"}}) [1 2]))))
        (is (= {:total 2 :truncated? true :limit 1}
               (select-keys (q/attestations s {:subject-id (:id (first records)) :limit 1})
                            [:total :truncated? :limit])))
        (is (= records (mapv :record (:results (q/records-by-ids s {:record-ids (mapv :id records) :valid-time at})))))
        (is (= 2 (:total (q/count-by-field s (assoc base :field :tag :value :example)))))
        (is (= 2 (get-in (q/attestations-by-subjects s {:subject-ids [(:id (first records))] :limit 1}) [:results 0 :total])))
        (is (true? (get-in (q/attestations-by-subjects s {:subject-ids [(:id (first records))] :limit 1}) [:results 0 :truncated?])))
        (is (= 0 (get-in (q/role-reverse-many s {:endpoint-ids ["absent"] :edge-type [:invented/span 1] :endpoint-role :item}) [:results 0 :total])))
        (finally (server/stop-server! srv) (.delete f))))))

(deftest stale-index-settles-then-persistent-stale-is-returned
  (let [calls (atom 0)
        flaky (reify futon3c.arxana.store/ArxanaGraphStore
                (bounded-query! [_ _] (if (< (swap! calls inc) 3) {:ok {:status :stale-index}} {:ok {:status :ok :attestation-ids []}})))
        stuck (reify futon3c.arxana.store/ArxanaGraphStore
                (bounded-query! [_ _] {:ok {:status :stale-index}}))]
    (is (= :ok (:status (futon3c.arxana.queries/attestations flaky {:subject-clusion-id "x"}))))
    (is (= 3 @calls))
    (is (= :stale-index (:status (futon3c.arxana.queries/attestations stuck {:subject-clusion-id "x"}))))))
