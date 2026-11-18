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
        c (:id (ts/write-record! s (assoc (record "s2" "three") :created_at "2026-10-03T00:00:00Z"
                                          :surface "matrix (!room:example.org)")))]
    (spit (str (:dir s) "/not-a-record.txt") "x")
    (ts/publish-analysis! s a {:status "analyzed"})
    (is (= [c b a] (map :id (ts/list-records s))))
    (is (= [b a] (map :id (ts/list-records s :session-id "s1"))))
    (is (= [c] (map :id (ts/list-records s :agent-id "claude-1" :limit 1))))
    (is (= [c] (map :id (ts/list-records s :surface "matrix (!room:example.org)"))))
    (is (= [] (ts/list-records s :surface "matrix (!other:example.org)")))
    (is (= [] (ts/list-records (ts/store (str (:dir s) "/missing")))))
    (testing "the analysis sibling is not listed as a record"
      (is (= 3 (count (ts/list-records s)))))))

(deftest listing-sees-changes-made-on-disk
  ;; list-records keeps an index of parsed records by file mtime and length.
  ;; A record rewritten by another writer, a record removed, and a record
  ;; added after the first listing must all show on the next one.
  (let [s (temp-store)
        a (:id (ts/write-record! s (assoc (record "s1" "one") :created_at "2026-10-01T00:00:00Z")))
        b (:id (ts/write-record! s (assoc (record "s1" "two") :created_at "2026-10-02T00:00:00Z")))]
    (is (= [b a] (map :id (ts/list-records s :session-id "s1"))))
    (ts/update-record! s a #(assoc % :session_id "s2" :happened_summary "moved to another session"))
    (io/delete-file (ts/record-path s b))
    (let [c (:id (ts/write-record! s (assoc (record "s1" "three") :created_at "2026-10-03T00:00:00Z")))]
      (is (= [c] (map :id (ts/list-records s :session-id "s1"))) "a rewritten record leaves s1, a deleted one is gone, a new one arrives")
      (is (= [a] (map :id (ts/list-records s :session-id "s2"))))
      (is (= "moved to another session"
             (:happened_summary (:record (first (ts/list-records s :session-id "s2")))))
          "the listed record is the rewritten one, not the indexed copy"))))

(deftest concurrent-updates-to-one-record-lose-no-field
  (let [s (temp-store)
        {:keys [id]} (ts/write-record! s (record "s1" "an invented turn"))
        fields (mapv #(keyword (str "field_" %)) (range 24))
        start (java.util.concurrent.CountDownLatch. 1)
        workers (mapv (fn [k]
                        (future (.await start)
                                (ts/update-record! s id #(assoc % k true))))
                      fields)]
    (.countDown start)
    (run! deref workers)
    (is (every? true? (map (ts/read-record s id) fields))
        "each concurrent update's field survives the others")))

(deftest operator-turns-form-a-linked-list-per-session
  ;; Joe 2026-11-18: sessions keep backpointers; finding N-1 must not list
  ;; the store (51,703 files).
  (let [s (temp-store)
        a (:id (ts/write-record! s (record "sess" "one")))
        other (:id (ts/write-record! s (record "sess-2" "elsewhere")))
        b (:id (ts/write-record! s (record "sess" "two")))
        bot (:id (ts/write-record! s (assoc (record "sess" "bell") :origin "agent")))
        c (:id (ts/write-record! s (record "sess" "three")))]
    (is (contains? (ts/read-record s a) :prev_operator_turn_id))
    (is (nil? (:prev_operator_turn_id (ts/read-record s a))) "first in session")
    (is (= a (:prev_operator_turn_id (ts/read-record s b))))
    (is (= b (:next_operator_turn_id (ts/read-record s a))))
    (is (= b (:prev_operator_turn_id (ts/read-record s c))) "agent turns are not in the operator strand")
    (is (not (contains? (ts/read-record s bot) :prev_operator_turn_id)))
    (is (nil? (:prev_operator_turn_id (ts/read-record s other))) "sessions never cross")
    (testing "without a list of the store"
      (with-redefs [ts/list-records (fn [& _] (throw (ex-info "scanned" {})))]
        (let [d (:id (ts/write-record! s (record "sess" "four")))]
          (is (= c (:prev_operator_turn_id (ts/read-record s d)))))))
    (testing "a head file left behind by a crash is advanced along :next"
      (spit (str (io/file (:dir s) "session-heads" "sess.json")) (str "{\"head\":\"" a "\"}"))
      (let [e (:id (ts/write-record! s (record "sess" "five")))]
        (is (not= a (:prev_operator_turn_id (ts/read-record s e))))
        (is (= (:next_operator_turn_id (ts/read-record s c))
               (:prev_operator_turn_id (ts/read-record s e))))))))

(deftest backfill-links-legacy-turns-once-and-keeps-existing-links
  (let [s (temp-store)
        legacy (fn [id at] (ts/write-record-id! s id (assoc (record "sess" id) :created_at at)))]
    (legacy "turn-old002" "2026-10-01T00:00:02Z")
    (legacy "turn-old001" "2026-10-01T00:00:01Z")
    (ts/write-record-id! s "turn-elsewh" (assoc (record "sess-2" "x") :created_at "2026-10-01T00:00:00Z"))
    (is (= {:sessions 2 :prev 3 :next 1 :heads 2 :mode :dry-run} (ts/backfill-strand! s)))
    (is (not (contains? (ts/read-record s "turn-old001") :prev_operator_turn_id)) "dry run writes nothing")
    (ts/backfill-strand! s :write? true)
    (is (nil? (:prev_operator_turn_id (ts/read-record s "turn-old001"))))
    (is (= "turn-old001" (:prev_operator_turn_id (ts/read-record s "turn-old002"))))
    (is (= "turn-old002" (:next_operator_turn_id (ts/read-record s "turn-old001"))))
    (let [new (:id (ts/write-record! s (assoc (record "sess" "new") :created_at "2026-10-01T00:00:03Z")))]
      (is (= "turn-old002" (:prev_operator_turn_id (ts/read-record s new))) "head file names the tail"))
    (is (= {:sessions 2 :prev 0 :next 0 :heads 0 :mode :dry-run} (ts/backfill-strand! s))
        "idempotent")))
