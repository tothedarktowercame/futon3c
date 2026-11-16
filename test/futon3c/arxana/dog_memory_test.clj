(ns futon3c.arxana.dog-memory-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.dog-memory :as sut]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns]))

(def turn-ref
  (records/make-external-reference
   {:ref/kind :evidence-text :store/id "e" :store/endpoint "x"
    :record/id "ev/body/text" :schema/authority :futon1b/evidence-text
    :valid-time "now" :revision/digest "sha"}))
(def turn {:id "turn-record-A" :turn_id "claude-3-turn-9" :origin "operator"
           :source_text "operator" :evidence_id "ev"})

(defn memory-results [_base handle]
  [{:entry {:evidence/id (str "version-" handle) :evidence/at "2026-11-16T20:00:00Z"
            :evidence/subject {:ref/type :memory :ref/id (str "cc:" handle)}}}])

(def recall {:at "2026-11-16T20:00:01Z" :arm "shown" :query-source "mixed"
             :query-ms 42
             :items [{:memory "p/one" :score 0.8 :why "one"}
                     {:memory "p/two" :score 0.7 :why "two"}]})

(deftest writer-one-content-keyed-batch-and-typed-skips
  (let [calls (atom [])]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator"})
                  http/append-batch! (fn [_ id rows] (swap! calls conj [id rows]) {:ok {}})]
      (is (= {:written 2 :skipped []}
             (sut/write! {:graph-store :stub :memory-lookup memory-results} turn recall)))
      (is (= 1 (count @calls)))
      (let [rows (second (first @calls))
            claims (map :record (filter #(= :clusion/assert (:operation %)) rows))]
        (is (= 2 (count claims)))
        (is (= "shown" (get-in (first claims) [:asserted-content :arm])))
        (is (= (:id turn-ref) (get-in (first claims) [:endpoints 0 :target :ref/id])))))
    (reset! calls [])
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator"})
                  http/append-batch! (fn [& xs] (swap! calls conj xs) {:ok {}})]
      (is (= {:written 1 :skipped ["p/missing"]}
             (sut/write! {:graph-store :stub
                          :memory-lookup (fn [base handle]
                                           (when-not (= "p/missing" handle)
                                             (memory-results base handle)))}
                         turn (assoc recall :items [{:memory "p/one"}
                                                    {:memory "p/missing"}]))))
      (is (= 1 (count @calls))))
    (reset! calls [])
    (with-redefs [http/append-batch! (fn [& xs] (swap! calls conj xs) {:ok {}})]
      (is (= 0 (:written (sut/write! {} turn {:status "not-ready" :items []}))))
      (is (= 0 (:written (sut/write! {} turn {:status "none" :items []}))))
      (is (empty? @calls)))))

(deftest changed-render-gives-a-new-message-id
  (let [ids (atom [])]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator"})
                  http/append-batch! (fn [_ id _] (swap! ids conj id) {:ok {}})]
      (sut/write! {:graph-store :stub :memory-lookup memory-results} turn recall)
      (sut/write! {:graph-store :stub :memory-lookup memory-results}
                  turn (assoc recall :at "2026-11-16T20:00:02Z" :query-ms 43))
      (is (= 2 (count (distinct @ids)))))))

(deftest reader-latest-render-and-attestation-win-in-both-index-orders
  (let [mk-claim (fn [id memory arm]
                   {:id id :asserted-content {:arm arm}
                    :endpoints [{:target {:ref/id (:id turn-ref)}}
                                {:target {:ref/id memory}}]})
        claims {"c-old" (mk-claim "c-old" "m-old" "shown")
                "c-one" (mk-claim "c-one" "m-one" "shown")
                "c-two" (mk-claim "c-two" "m-two" "shown")}
        atts {"a-old" {:content {:memory "p/old" :rank 1 :written-at "2026-11-16T20:00:00Z"}}
              "a-one-old" {:content {:memory "p/one" :rank 2 :written-at "2026-11-16T20:00:00Z"}}
              "a-one-new" {:content {:memory "p/one" :rank 1 :written-at "2026-11-16T20:00:01Z"}}
              "a-two" {:content {:memory "p/two" :rank 2 :written-at "2026-11-16T20:00:01Z"}}}
        run (fn [reverse?]
              (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator"})
                            store/bounded-query!
                            (fn [_ q]
                              (if (= :role-reverse (:query/type q))
                                {:ok {:edge-ids ["c-old" "c-one" "c-two"]}}
                                (let [ids (case (:subject-id q)
                                            "c-old" ["a-old"]
                                            "c-one" ["a-one-old" "a-one-new"]
                                            "c-two" ["a-two"])]
                                  (is (contains? q :limit))
                                  {:ok {:attestation-ids (if reverse? (vec (reverse ids)) ids)}})))
                            store/get-entity
                            (fn [_ id] {:ok (or (claims id) (atts id))})]
                (sut/read-recall {:graph-store :stub} turn)))]
    (doseq [reverse? [false true]]
      (is (= ["p/one" "p/two"] (mapv :memory (:items (run reverse?))))
          "old re-render items are ignored; two newest items retain rank order"))))

(deftest backfill-dry-run-never-writes
  (with-redefs [turns/list-records
                (fn [& _] [{:id "turn-new" :record
                            (assoc turn :created_at "2026-11-16T20:00:00Z"
                                   :dog_recall recall)}])
                sut/read-recall (constantly {:status "no-recall" :items []})
                http/append-batch! (fn [& _] (throw (AssertionError. "dry run wrote")))]
    (is (= {:would-write 1}
           (:outcomes (sut/backfill! {:turn-store :stub} {:write? false}))))))

(deftest a-later-render-with-items-replaces-a-not-ready-outcome
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "dog-gcc-marker-" (make-array java.nio.file.attribute.FileAttribute 0)))
        st (turns/store dir) id "turn-dogMARK"
        cfg {:graph-store :stub :memory-lookup memory-results}]
    (turns/write-record-id! st id (assoc turn :dog_recall {:status "not-ready" :items []}))
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator"})
                  http/append-batch! (fn [& _] {:ok {}})]
      (sut/write-turn! cfg st id)
      (is (= 0 (get-in (turns/read-record st id) [:dog_recall_gcc :written])))
      (turns/update-record! st id #(assoc % :dog_recall recall))
      (sut/write-turn! cfg st id)
      (is (= 2 (get-in (turns/read-record st id) [:dog_recall_gcc :written])))
      (turns/update-record! st id #(assoc % :dog_recall {:status "failed" :items []}))
      (with-redefs [http/append-batch! (fn [& _] {:ok {}})]
        (sut/write-turn! cfg st id))
      (is (= 2 (get-in (turns/read-record st id) [:dog_recall_gcc :written]))
          "a later empty render never replaces a real write"))))
