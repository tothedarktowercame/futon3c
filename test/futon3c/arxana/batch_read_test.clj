(ns futon3c.arxana.batch-read-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.batch-read :as batch]
            [futon3c.arxana.benefit-filings :as view]
            [futon3c.arxana.benefit-filings-test :as fixture]
            [futon3c.arxana.read-cache :as cache]
            [futon3c.arxana.store :as store]))
(deftest warm-page-coalesces-probes
  (cache/bump-epoch!)
  (let [inner (fixture/fake (atom []) (constantly nil)) calls (atom [])
        s (reify store/ArxanaGraphStore (bounded-query! [_ q] (swap! calls conj q) (store/bounded-query! inner q)))
        cold (view/read-filings s fixture/request)]
    (is (= :ok (:status cold)))
    (reset! calls [])
    (let [warm (view/read-filings s fixture/request)]
      (is (= (dissoc cold :queries) (dissoc warm :queries)))
      (is (<= (count @calls) 4))
      (is (not-any? #(= :records-by-ids (:query/type %)) @calls))
      (is (every? #(= 1 (:limit %)) @calls)))))
(deftest missing-batch-member-is-not-complete
  (let [s (reify store/ArxanaGraphStore (bounded-query! [_ _] {:ok {:status :ok :results []}}))]
    (is (thrown? clojure.lang.ExceptionInfo (batch/records s ["absent"] fixture/at true)))
    (is (= [] (batch/records s [] fixture/at true)))
    (is (= [] (batch/sets s {:query/type :attestations :limit 200} [])))))

(deftest appended-member-and-incomplete-sets
  (cache/bump-epoch!)
  (let [members (atom ["a"]) calls (atom []) bad (atom nil)
        s (reify store/ArxanaGraphStore
            (bounded-query! [_ q]
              (swap! calls conj q)
              {:ok (or @bad
                       {:status :ok :watermark 1
                        :results (mapv (fn [id] {:id id :status :ok :total (count @members)
                                                :truncated? (> (count @members) (:limit q))
                                                :attestation-ids (vec (take (:limit q) @members))})
                                       (:subject-ids q))})}))
        query {:query/type :attestations :limit 200}]
    (is (= ["a"] (:attestation-ids (first (batch/sets s query ["subject"])))))
    (reset! members ["a" "b"])
    (reset! calls [])
    (is (= ["a" "b"] (:attestation-ids (first (batch/sets s query ["subject"])))))
    (is (= [1 200] (mapv :limit @calls)))
    (cache/bump-epoch!)
    (reset! bad {:status :stale-index})
    (is (thrown? clojure.lang.ExceptionInfo (batch/sets s query ["subject"])))
    (reset! bad nil)
    (reset! calls [])
    (is (= ["a" "b"] (:attestation-ids (first (batch/sets s query ["subject"])))))
    (is (= [200] (mapv :limit @calls)))))
