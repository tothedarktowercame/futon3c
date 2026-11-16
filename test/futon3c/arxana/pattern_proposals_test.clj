(ns futon3c.arxana.pattern-proposals-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.pattern-proposals :as sut]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns]))

(def turn {:id "turn-p" :origin "operator" :evidence_id "ev" :source_text "operator"
           :created_at "2026-11-16T19:00:00Z"})
(def turn-ref (records/make-external-reference
               {:ref/kind :evidence-text :store/id "futon1b/evidence" :store/endpoint "x"
                :record/id "ev/body/text" :schema/authority :futon1b/evidence-text
                :valid-time "now" :revision/digest "turn-sha"}))

(defn pattern-ref [id]
  (records/make-external-reference
   {:ref/kind :pattern-file :store/id id :store/endpoint "lib" :record/id (str id ".flexiarg")
    :schema/authority :futon3/pattern-library :valid-time "content-addressed"
    :revision/digest (str id "-sha")}))

(deftest writer-is-one-batch-with-endpoints-and-model-attestations
  (let [calls (atom []) lu {:version 1 :at "now" :source "驴"
                            :ranked [{:id "p/a" :rank 1 :quote "a"}
                                     {:id "p/b" :rank 2 :quote "b"}]}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                  sut/pattern-reference (fn [_ id] (pattern-ref (str id)))
                  http/append-batch! (fn [_ _ rows] (swap! calls conj rows) {:ok {}})]
      (is (= {:written 2 :skipped []} (sut/write! {:graph-store :stub} turn lu)))
      (is (= 1 (count @calls)))
      (let [rows (first @calls) proposals (map :record (filter #(= :clusion/assert (:operation %)) rows))
            attestations (map :record (filter #(= :evidence/append (:operation %)) rows))]
        (is (= [(:id turn-ref)] (distinct (map #(get-in % [:endpoints 0 :target :ref/id]) proposals))))
        (is (= #{(:id (pattern-ref "p/a")) (:id (pattern-ref "p/b"))}
               (set (map #(get-in % [:endpoints 1 :target :ref/id]) proposals))))
        (is (every? #(and (= "驴" (:producer %)) (= :model (:basis %))) attestations)))))
  (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                sut/pattern-reference (fn [_ id] (when (= "p/a" (str id)) (pattern-ref "p/a")))
                http/append-batch! (fn [& _] {:ok {}})]
    (is (= {:written 1 :skipped ["p/b"]}
           (sut/write! {:graph-store :stub} turn
                       {:ranked [{:id "p/a" :rank 1} {:id "p/b" :rank 2}]}))))
  (let [calls (atom [])]
    (with-redefs [mentions/field-evidence (constantly nil)
                  http/append-batch! (fn [& _] (swap! calls conj :write))]
      (is (= :operator-evidence-unavailable (:failed (sut/write! {:graph-store :stub} turn {:ranked []}))))
      (is (empty? @calls)))))

(deftest reader-sorts-by-attested-rank
  (let [p1 {:id "proposal-1" :endpoints [nil {:target {:ref/id "ref-a"}}]}
        p2 {:id "proposal-2" :endpoints [nil {:target {:ref/id "ref-b"}}]}
        entities {"proposal-1" p1 "proposal-2" p2 "att-1" {:content {:rank 2}}
                  "att-2" {:content {:rank 1}} "ref-a" {:store/id "p/a"} "ref-b" {:store/id "p/b"}}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                  store/bounded-query! (fn [_ q]
                                         (if (= :role-reverse (:query/type q))
                                           {:ok {:edge-ids ["proposal-1" "proposal-2"]}}
                                           ;; futon1b's live :attestations route: reads
                                           ;; :subject-id, answers :attestation-ids.
                                           {:ok {:attestation-ids
                                                 (case (:subject-id q)
                                                   "proposal-1" ["att-1"] "proposal-2" ["att-2"] [])}}))
                  store/get-entity (fn [_ id] {:ok (get entities id)})]
      (is (= ["p/b" "p/a"] (:ids (sut/read-ranked {:graph-store :stub} turn))))
      (with-redefs [store/get-entity (fn [_ id] {:ok (get (assoc entities "att-2" {:content {}}) id)})]
        (is (= {:pending :lu-proposal-rank-unreadable}
               (sut/read-ranked {:graph-store :stub} turn))
            "a missing rank is pending, never a guessed order")))))

(deftest backfill-dry-run-never-writes
  (with-redefs [turns/list-records (fn [& _] [{:id "turn-p" :record (assoc turn :created_at (str (java.time.Instant/now))
                                                                            :lu_ranked {:ranked []})}])
                sut/read-ranked (constantly {:pending :lu-proposals-pending})
                sut/plan (constantly {:rows []})
                http/append-batch! (fn [& _] (throw (AssertionError. "dry run wrote")))]
    (is (= :dry-run (:mode (sut/backfill! {:turn-store :stub} {:write? false}))))))

(deftest a-changed-batch-for-a-turn-is-a-new-message
  (let [ids (atom [])]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                  sut/pattern-reference (fn [_ id] (pattern-ref (str id)))
                  http/append-batch! (fn [_ mid _] (swap! ids conj mid) {:ok {}})]
      (sut/write! {:graph-store :stub} turn {:ranked [{:id "p/a" :rank 1}]})
      (sut/write! {:graph-store :stub} turn {:ranked [{:id "p/a" :rank 1}]})
      (sut/write! {:graph-store :stub} turn {:ranked [{:id "p/b" :rank 1}]})
      (is (= (first @ids) (second @ids)) "identical batch, same message")
      (is (not= (first @ids) (nth @ids 2)) "changed batch, new message"))))
