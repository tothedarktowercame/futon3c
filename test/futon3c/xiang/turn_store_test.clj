(ns futon3c.xiang.turn-store-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.turn-record :as tr]
            [futon3c.xiang.turn-store :as ts]))

(defn- temp-store []
  (let [dir (java.nio.file.Files/createTempDirectory "xiang-store-" (make-array java.nio.file.attribute.FileAttribute 0))]
    (ts/store (str dir))))

(defn- record [session text]
  (:record (tr/make-record {:text text :agent-id "claude-1" :session-id session :turn-id "t" :now-ms 0})))

(deftest write-read-update-round-trip
  (let [s (temp-store)
        {:keys [id path]} (ts/write-record! s (record "sess" "I agree 象."))]
    (is (ts/record-id? id))
    (is (.exists (io/file path)))
    (is (= "I agree 象." (:source_text (ts/read-record s id))))
    (ts/update-record! s id #(assoc % :happened_summary "did"))
    (is (= "did" (:happened_summary (ts/read-record s id))))
    (is (= "I agree 象." (:source_text (ts/read-record s id))) "update keeps the rest")
    (is (nil? (ts/read-record s "turn-nope")))
    (is (thrown? clojure.lang.ExceptionInfo (ts/update-record! s "turn-nope" identity)))))

(deftest ids-are-validated-before-they-touch-a-path
  (let [s (temp-store)]
    (is (thrown? clojure.lang.ExceptionInfo (ts/record-path s "../etc/passwd")))
    (is (thrown? clojure.lang.ExceptionInfo (ts/read-record s "turn-a/b")))
    (is (= (str (:dir s) "/turn-abc.json.analysis.json") (ts/analysis-path s "turn-abc")))))

(deftest analysis-is-published-once
  (let [s (temp-store)
        {:keys [id]} (ts/write-record! s (record "sess" "hello"))]
    (is (not (ts/analysis-published? s id)))
    (ts/publish-analysis! s id {:status "analyzed" :labeller "x"})
    (is (ts/analysis-published? s id))
    (is (= "x" (:labeller (ts/read-analysis s id))))
    (let [rec (ts/read-record s id)]
      (is (= "analyzed" (:analysis_status rec)))
      (is (= (ts/analysis-path s id) (:analysis_file rec))))
    (is (= :analysis-exists
           (try (ts/publish-analysis! s id {:status "again"}) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    (is (= "x" (:labeller (ts/read-analysis s id))) "the first interpretation stands")))

(deftest listing-filters-and-orders-newest-first
  (let [s (temp-store)
        a (:id (ts/write-record! s (assoc (record "s1" "one") :created_at "2026-10-01T00:00:00Z")))
        b (:id (ts/write-record! s (assoc (record "s1" "two") :created_at "2026-10-02T00:00:00Z")))
        c (:id (ts/write-record! s (assoc (record "s2" "three") :created_at "2026-10-03T00:00:00Z")))]
    (spit (str (:dir s) "/not-a-record.txt") "x")
    (ts/publish-analysis! s a {:status "analyzed"})
    (is (= [c b a] (map :id (ts/list-records s))))
    (is (= [b a] (map :id (ts/list-records s :session-id "s1"))))
    (is (= [c] (map :id (ts/list-records s :agent-id "claude-1" :limit 1))))
    (is (= [] (ts/list-records (ts/store (str (:dir s) "/missing")))))
    (testing "the analysis sibling is not listed as a record"
      (is (= 3 (count (ts/list-records s)))))))
