(ns futon3c.arxana.xiang-interpretations-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.arxana.xiang-interpretations :as sut]
            [futon3c.xiang.turn-store :as turns]))

(def turn-ref (records/make-external-reference
          {:ref/kind :evidence-text :store/id "futon1b/evidence" :store/endpoint "x"
           :record/id "ev/body/text" :schema/authority :futon1b/evidence-text
           :valid-time "now" :revision/digest "sha"}))
(def turn {:id "turn-x" :turn_id "claude-3-turn-9" :origin "operator" :evidence_id "ev"
           :source_text "report this reply" :interpretation_version 2 :vocabulary_version 3
           :acts [{:id "claude-3-turn-9-f-s1-0" :author "operator" :kind "report-problem"
                   :start 0 :end 6 :text "report" :about "problem" :to "claude-13"}
                  {:id "claude-3-turn-9-r-0" :author "agent" :kind "report" :start 7 :end 11 :text "this"}]})

(deftest writer-single-batch-has-scopes-intents-ranges-and-attestations
  (let [calls (atom [])]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text (:source_text turn)})
                  http/append-batch! (fn [_ _ rows] (swap! calls conj rows) {:ok {}})]
      (is (= {:written 1} (sut/write! {:graph-store :stub} turn)))
      (is (= 1 (count @calls)))
      (let [rows (first @calls) claim (:record (first (filter #(= :clusion/assert (:operation %)) rows)))
            att (:record (first (filter #(= :evidence/append (:operation %)) rows)))]
        (is (= [0 6] (get-in claim [:endpoints 0 :target :locator :codepoint-range])))
        (is (= :report-problem (get-in claim [:endpoints 1 :target :intent/id])))
        (is (not= (:id turn-ref) (get-in claim [:endpoints 2 :target :ref/id])))
        (is (= "象" (:producer att)))
        (is (= "claude-3-turn-9-f-s1-0" (get-in att [:content :act/id]))
            "act ids live under the agent turn id, not the record id")
        (is (= 1 (:acts (sut/plan {:graph-store :stub} turn))) "agent/reply act excluded"))))
  (let [calls (atom [])]
    (with-redefs [mentions/field-evidence (constantly nil)
                  http/append-batch! (fn [& _] (swap! calls conj :write))]
      (is (= :operator-evidence-unavailable (:failed (sut/write! {:graph-store :stub} turn))))
      (is (empty? @calls)))))

(deftest reader-uses-live-route-keys-and-orders-by-start
  (let [c1 {:id "c1" :endpoints [{:target {:locator {:codepoint-range [7 11]}}}
                                   {:target {:intent/id :explain}}]}
        c2 {:id "c2" :endpoints [{:target {:locator {:codepoint-range [0 6]}}}
                                   {:target {:intent/id :report-problem}}]}
        ents {"c1" c1 "c2" c2 "a1" {:content {:act/id "later"}}
              "a2" {:content {:act/id "first"}}}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text (:source_text turn)})
                  store/bounded-query!
                  (fn [_ q]
                    (if (= :role-reverse (:query/type q))
                      {:ok {:edge-ids ["c1" "c2"]}}
                      (do (is (contains? q :subject-id)) (is (contains? q :limit))
                          {:ok {:attestation-ids [(if (= "c1" (:subject-id q)) "a1" "a2")]}})))
                  store/get-entity (fn [_ id] {:ok (get ents id)})]
      (is (= ["first" "later"] (mapv :id (:acts (sut/read-acts {:graph-store :stub} turn))))))))

(deftest backfill-dry-run-never-writes
  (with-redefs [turns/list-records (fn [& _] [{:id "turn-x" :record
                                                (assoc turn :analysis_status "analyzed"
                                                       :created_at (str (java.time.Instant/now)))}])
                sut/read-acts (constantly {:pending :interpretations-pending})
                sut/plan (constantly {:rows [{}]})
                http/append-batch! (fn [& _] (throw (AssertionError. "dry run wrote")))]
    (is (= {:would-write 1} (:outcomes (sut/backfill! {:turn-store :stub} {:write? false})))))
  (with-redefs [turns/list-records (fn [& _] [{:id "turn-x" :record
                                                (assoc turn :analysis_status "analyzed"
                                                       :created_at (str (java.time.Instant/now)))}])
                sut/read-acts (constantly {:pending :interpretations-pending})
                sut/plan (constantly {:rows [] :acts 0})]
    (is (= {:no-operator-acts 1} (:outcomes (sut/backfill! {:turn-store :stub} {:write? false})))
        "an empty plan is not counted as a write")))
