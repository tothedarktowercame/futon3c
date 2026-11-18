(ns futon3c.arxana.read-cache-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.arxana.read-cache :as c]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.queries :as q]
            [futon3c.arxana.dependence :as dep]))
(use-fixtures :each (fn [f] (c/bump-epoch!) (binding [c/*enabled* true]
                                                     (with-redefs [store/bounded-query-settled! store/bounded-query!] (f)))))
(def t0 "2026-01-01T00:00:00.000Z")
(def t1 "2026-01-02T00:00:00.000Z")
(def t2 "2026-01-03T00:00:00.000Z")
(defn fixture []
  (let [records (atom {"a" {:id "a" :event-time t0} "b" {:id "b"} "c" {:id "c"}})
        from (atom t0) ids (atom ["a" "b"]) calls (atom []) bad (atom nil)
        s (reify store/ArxanaGraphStore
            (bounded-query! [_ request]
              (swap! calls conj request)
              (if @bad @bad
                {:ok (if (= :record-by-id (:query/type request))
                       (if (and (get @records (:record-id request))
                                (not (neg? (compare (:valid-time request) @from))))
                         {:status :ok :record (get @records (:record-id request))}
                         {:status :missing})
                       (let [limit (:limit request)]
                         {:status :ok :total (count @ids) :truncated? (> (count @ids) limit)
                          :watermark 2
                          (if (= :attestations (:query/type request)) :attestation-ids :edge-ids)
                          (vec (take limit @ids))}))})))]
    {:s s :records records :from from :ids ids :calls calls :bad bad}))
(defn page [s at]
  [(q/attestations s {:subject-clusion-id "subject" :limit 200})
   (dep/reverse-records s [:supersedes 1] :predecessor "rule" at)])
(deftest repeated-page-and-append
  (let [{:keys [s ids calls]} (fixture) first-page (page s t1)]
    (reset! calls [])
    (let [{:keys [value metrics]} (c/measure #(page s t1))]
      (is (= first-page value))
      (is (= [:attestations :role-reverse] (mapv :query/type @calls)))
      (is (every? #(= 1 (:limit %)) @calls))
      (is (= {:hits 4 :misses 0 :revalidations 2} metrics)))
    (swap! ids conj "c")
    (is (= ["a" "b" "c"] (:attestation-ids (q/attestations s {:subject-clusion-id "subject" :limit 200}))))))
(deftest bounds-epoch-and-ttl
  (let [{:keys [s calls from]} (fixture) now (atom 0)]
    (binding [c/*clock* #(deref now) c/*ttl-ms* 10]
      (is (:ok (http/get-entity-as-of s "a" t1)))
      (is (dep/missing? (http/get-entity-as-of s "a" "2000-01-01T00:00:00.000Z")))
      (http/get-entity-as-of s "b" t1)
      (reset! calls [])
      (is (:ok (http/get-entity-as-of s "b" t0)))
      (is (= 1 (count @calls)) "before undated witness reaches store")
      (page s t1)
      (c/bump-epoch!)
      (reset! from t2)
      (is (dep/missing? (http/get-entity-as-of s "b" t1)) "deleted/reloaded same undated id has new bound")
      (reset! calls [])
      (q/attestations s {:subject-clusion-id "subject" :limit 200})
      (is (= [200] (mapv :limit @calls)) "epoch discards sets")
      (http/get-entity-as-of s "b" t2)
      (reset! calls []) (reset! now 11)
      (q/attestations s {:subject-clusion-id "subject" :limit 200})
      (http/get-entity-as-of s "b" t2)
      (is (= [:attestations :record-by-id] (mapv :query/type @calls)) "TTL expires sets and presence"))))
(deftest refusals-cap-and-switch
  (let [{:keys [s bad calls]} (fixture)]
    (doseq [r [{:ok {:status :stale-index}} {:error {:why :transport}}
               {:ok {:status :ok :total 2 :truncated? true :attestation-ids ["a"]}}
               {:ok {:status :inconclusive}}]]
      (reset! bad r) (reset! calls [])
      (dotimes [_ 2] (q/attestations s {:subject-clusion-id "subject" :limit 200}))
      (is (= [200 200] (mapv :limit @calls))))
    (reset! bad nil)
    (binding [c/*capacity* 1]
      (http/get-entity-as-of s "a" t1) (http/get-entity-as-of s "b" t1)
      (reset! calls []) (http/get-entity-as-of s "a" t1)
      (is (= 1 (count @calls))))
    (binding [c/*enabled* false]
      (reset! calls [])
      (dotimes [_ 2] (http/get-entity-as-of s "a" t1))
      (is (= 2 (count @calls))))))
(deftest explicit-identity
  (is (= (c/identity-key (http/http-store "http://localhost:7073/" "reader"))
         (c/identity-key (http/http-store "http://localhost:7073" "reader"))))
  (is (not= (c/identity-key (http/http-store "http://localhost:7073" "other"))
            (c/identity-key (http/http-store "http://localhost:7073" "reader")))))
(deftest revalidation-cannot-cross-invalidation
  (doseq [invalidate [:epoch :ttl]]
    (c/bump-epoch!)
    (let [clock (atom 0) calls (atom []) s (Object.)
          query {:query/type :attestations :subject-clusion-id "x" :limit 200}
          fetch (fn [q]
                  (swap! calls conj (:limit q))
                  (when (= 1 (:limit q))
                    (if (= :epoch invalidate) (c/bump-epoch!) (reset! clock 11)))
                  {:ok {:status :ok :total 0 :truncated? false :attestation-ids []}})]
      (binding [c/*clock* #(deref clock) c/*ttl-ms* 10]
        (c/set-query! s query fetch)
        (reset! calls [])
        (c/set-query! s query fetch)
        (is (= [1 200] @calls) "a bump or TTL expiry during the probe forces a full read")))))
