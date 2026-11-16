(ns futon3c.arxana.pattern-occurrences-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.pattern-occurrences :as sut]
            [futon3c.arxana.pattern-proposals :as proposals]
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

(def turn {:id "turn-record-A" :turn_id "claude-3-turn-99" :origin "operator"
           :evidence_id "ev" :source_text "operator evidence"})
(def verdict {:version 1 :at "2026-11-16T20:00:00Z" :reader "馬-sonnet"
              :window ["turn-old" "turn-record-A"]
              :patterns [{:pattern "p/one" :sha256 "one" :status "occurrence"
                          :sources ["驴"] :shown true :turn_mapped 4 :chain_mapped 4
                          :turn {:I1 {:quote "one"}} :chain {:B {:quote "two"}}}
                         {:pattern "p/void" :sha256 "void" :status "void"
                          :sources ["embedding"] :shown false :turn_mapped 0
                          :chain_mapped 0 :turn {} :chain {}}]})

(defn- pattern-ref [id]
  (records/make-external-reference
   {:ref/kind :pattern-file :store/id id :store/endpoint "/lib"
    :record/id (str id ".flexiarg") :schema/authority :futon3/pattern-library
    :valid-time "content-addressed" :revision/digest (last (str/split id #"/"))}))

(deftest writer-batches-all-verdicts-including-void
  (let [calls (atom [])]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator evidence"})
                  proposals/pattern-reference (fn [_ id] (pattern-ref id))
                  http/append-batch! (fn [_ message rows]
                                       (swap! calls conj [message rows]) {:ok {}})]
      (is (= {:written 2 :skipped []} (sut/write! {:graph-store :stub} turn verdict)))
      (is (= 1 (count @calls)))
      (let [rows (second (first @calls))
            claims (map :record (filter #(= :clusion/assert (:operation %)) rows))
            atts (map :record (filter #(= :evidence/append (:operation %)) rows))]
        (is (= 2 (count claims)))
        (is (= #{"occurrence" "void"} (set (map #(get-in % [:content :status]) atts))))
        (is (= ["turn-old" "turn-record-A"]
               (get-in (first claims) [:asserted-content :window])))
        (is (= ["驴"] (get-in (first claims) [:asserted-content :proposal])))
        (is (= "馬-sonnet" (:producer (first atts)))))))
  (with-redefs [http/append-batch! (fn [& _] (throw (AssertionError. "pending wrote")))]
    (is (= :verdict-pending
           (:failed (sut/write! {} turn {:state :lu-proposals-pending}))))))

(deftest changed-verdicts-produce-new-message-ids
  (let [ids (atom [])]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator evidence"})
                  proposals/pattern-reference (fn [_ id] (pattern-ref id))
                  http/append-batch! (fn [_ id _] (swap! ids conj id) {:ok {}})]
      (sut/write! {:graph-store :stub} turn verdict)
      (sut/write! {:graph-store :stub} turn (assoc-in verdict [:patterns 0 :status] "partial"))
      (is (= 2 (count (distinct @ids)))))))

(deftest reader-uses-live-keys-and-latest-stamped-attestation
  (let [claim {:id "c" :endpoints [{:target {:ref/id "p"}} {:target {:ref/id "t"}}]}
        entities {"c" claim "p" {:store/id "p/one"}
                  "new" {:content {:status "partial" :written-at "2026-11-16T20:02:00Z"}}
                  "old" {:content {:status "void" :written-at "2026-11-16T20:01:00Z"}}}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference turn-ref :text "operator evidence"})
                  store/bounded-query!
                  (fn [_ q]
                    (case (:query/type q)
                      :role-reverse (do (is (= :turn (:endpoint-role q))) {:ok {:edge-ids ["c"]}})
                      :attestations (do (is (= "c" (:subject-id q)))
                                        (is (= 50 (:limit q)))
                                        {:ok {:attestation-ids ["new" "old"]}})))
                  store/get-entity (fn [_ id] {:ok (get entities id)})]
      (is (= [{:pattern "p/one" :status "partial"
               :written-at "2026-11-16T20:02:00Z"}]
             (:rows (sut/read-turn {:graph-store :stub} turn)))))))

(deftest backfill-dry-run-does-not-write
  (with-redefs [turns/list-records
                (fn [& _] [{:id (:id turn) :record
                            (assoc turn :created_at (str (java.time.Instant/now))
                                   :pattern_verdicts verdict)}])
                sut/read-turn (constantly {:rows []})
                sut/plan (constantly {:rows [{}]})
                http/append-batch! (fn [& _] (throw (AssertionError. "dry run wrote")))]
    (is (= {:would-write 1}
           (:outcomes (sut/backfill! {:turn-store :stub} {:write? false}))))))
