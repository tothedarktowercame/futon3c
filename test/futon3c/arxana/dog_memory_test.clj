(ns futon3c.arxana.dog-memory-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
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

(deftest use-writer-includes-used-and-unused-and-skips-typed-absences
  (let [calls (atom [])
        use {:status "ok" :at "2026-11-16T21:00:00Z"
             :items [{:memory "p/one" :used true :kind "named" :evidence "label-a"}
                     {:memory "p/two" :used false :kind "unused" :evidence "label-b"}]}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                  http/append-batch! (fn [_ id rows] (swap! calls conj [id rows]) {:ok {}})]
      (is (= 2 (:written (sut/write-use! {:graph-store :stub :memory-lookup memory-results}
                                         turn use))))
      (is (= 1 (count @calls)))
      (let [atts (map :record (filter #(= :evidence/append (:operation %))
                                      (second (first @calls))))]
        (is (= #{[true "named"] [false "unused"]}
               (set (map #(vector (get-in % [:content :used])
                                  (get-in % [:content :kind])) atts)))))
      (reset! calls [])
      (is (zero? (:written (sut/write-use! {} turn {:status "pending"}))))
      (is (zero? (:written (sut/write-use! {} turn {:status "no-recall"}))))
      (is (empty? @calls)))))

(deftest use-recheck-has-new-message-id
  (let [ids (atom []) use {:status "ok" :at "2026-11-16T21:00:00Z"
                           :items [{:memory "p/one" :used false :kind "unused"}]}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                  http/append-batch! (fn [_ id _] (swap! ids conj id) {:ok {}})]
      (sut/write-use! {:graph-store :stub :memory-lookup memory-results} turn use)
      (sut/write-use! {:graph-store :stub :memory-lookup memory-results}
                      turn (-> use (assoc :at "2026-11-16T21:01:00Z")
                               (assoc-in [:items 0 :used] true)))
      (is (= 2 (count (distinct @ids)))))))

(deftest burial-and-two-repetitions-share-one-safe-batch
  (let [calls (atom []) secret "PRIVATE OPERATOR SENTENCE"
        buried {:status "ok" :at "2026-11-16T21:02:00Z"
                :items [{:slug "new" :status "buried"
                         :basis {:act-id "a1" :range [3 9]}}
                        {:slug "failed" :status "failed"}
                        {:slug "exists" :status "exists"}]
                :said-again [{:act "a2" :range [10 20] :text secret
                              :memory-ids ["p/one"] :shared [["private" "words"]]}
                             {:act "a3" :range [21 30] :text secret
                              :memory-ids ["p/one"] :shared [["other" "words"]]}]}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                  http/append-batch! (fn [_ _ rows] (swap! calls conj rows) {:ok {}})]
      (is (= {:written 3 :burials 1 :repetitions 2 :skipped []}
             (sut/write-buried! {:graph-store :stub :memory-lookup memory-results
                                 :memory-resolution-retry-ms 0} turn buried)))
      (is (= 1 (count @calls)))
      (is (= 1 (count (filter #(= [:memory/repetition 1]
                                  (get-in % [:record :type-revision])) (first @calls))))
          "two events share one repetition clusion")
      (is (= 2 (count (filter #(and (= :evidence/append (:operation %))
                                    (= "dog/repetition" (get-in % [:record :method :id])))
                              (first @calls)))))
      (is (not (str/includes? (pr-str (first @calls)) secret)))
      (is (not (str/includes? (pr-str (first @calls)) "private words"))))))

(deftest readers-ignore-index-order-and-repetitions-preserve-events
  (let [claims {"cu" {:id "cu"} "cr" {:id "cr"}}
        atts {"u0" {:content {:memory "p/one" :used false :kind "unused"
                              :written-at "2026-11-16T21:00:00Z"}}
              "u1" {:content {:memory "p/one" :used true :kind "named"
                              :written-at "2026-11-16T21:01:00Z"}}
              "r1" {:content {:memory "p/one" :event-index 0 :source-act-id "a1"
                              :written-at "2026-11-16T21:02:00Z"}}
              "r2" {:content {:memory "p/one" :event-index 1 :source-act-id "a2"
                              :written-at "2026-11-16T21:02:00Z"}}}
        run (fn [reverse? reader]
              (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref})
                            store/bounded-query!
                            (fn [_ q]
                              (if (= :role-reverse (:query/type q))
                                {:ok {:edge-ids [(if (= [:memory/use 1] (:edge-type q)) "cu" "cr")]}}
                                (let [ids (if (= "cu" (:subject-id q)) ["u0" "u1"] ["r1" "r2"])]
                                  {:ok {:attestation-ids (if reverse? (vec (reverse ids)) ids)}})))
                            store/get-entity (fn [_ id] {:ok (or (claims id) (atts id))})]
                (reader {:graph-store :stub} turn)))]
    (doseq [reverse? [false true]]
      (is (true? (get-in (run reverse? sut/read-use) [:items 0 :used])))
      (is (= ["a1" "a2"]
             (mapv :source-act-id (:items (run reverse? sut/read-repetitions))))))))

(deftest remaining-backfill-dry-run-never-writes
  (with-redefs [turns/list-records
                (fn [& _] [{:id "turn-new" :record
                            (assoc turn :created_at "2026-11-16T21:00:00Z"
                                   :dog_use {:status "ok" :items [{:memory "p/one"}]}
                                   :dog_buried {:status "ok" :items [{:status "buried"}]
                                                :said-again [{:memory-ids ["p/one"]}]})}])
                sut/read-use (constantly {:items []})
                sut/read-burials (constantly {:items []})
                sut/read-repetitions (constantly {:items []})
                http/append-batch! (fn [& _] (throw (AssertionError. "dry run wrote")))]
    (let [r (sut/backfill-remaining! {:turn-store :stub} {:write? false})]
      (is (= {:would-write 1} (get-in r [:use :outcomes])))
      (is (= {:would-write 1} (get-in r [:burial :outcomes])))
      (is (= {:would-write 1} (get-in r [:repetition :outcomes]))))))
