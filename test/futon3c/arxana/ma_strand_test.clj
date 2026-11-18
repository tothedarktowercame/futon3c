(ns futon3c.arxana.ma-strand-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.ma-strand :as sut]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns]))

(defn- temp-store []
  (turns/store (str (java.nio.file.Files/createTempDirectory
                     "ma-strand-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- fixture-ref [kind id]
  (records/make-external-reference
   {:ref/kind kind :store/id id :store/endpoint "fixture"
    :record/id id :schema/authority :fixture :valid-time "now"
    :revision/digest (str "sha-" id)}))

(defn- record! [store id over]
  (turns/write-record-id!
   store id (merge {:origin "operator" :session_id "s" :created_at "2026-01-01T00:00:00Z"
                    :source_text id :evidence_id id}
                   over)))

(defn- seams [f]
  (with-redefs [mentions/field-evidence (fn [_ turn _] {:reference (fixture-ref :evidence-text (:id turn))})
                proposals/pattern-reference (fn [_ id] (fixture-ref :pattern-file id))]
    (f)))

(deftest connection-and-cross-turn-quote-shapes
  (let [store (temp-store)]
    (record! store "turn-t0" {})
    (record! store "turn-t1"
             {:pattern_verdicts
              {:window ["turn-t0" "turn-t1"]
               :connection {:status :linked :turn/id "turn-t0" :sha256 "digest-0"}
               :patterns [{:pattern "p/one"
                           :turn {:I1 {:element "x" :turn "turn-t1"}}
                           :chain {:I2 {:element "y" :turn "turn-t0"}}}]}})
    (seams
     #(let [p (sut/plan {:turn-store store :pattern-library "fixture"}
                        (assoc (turns/read-record store "turn-t1") :id "turn-t1"))
            claims (map :record (filter (comp #{:clusion/assert} :operation) (:rows p)))]
        (is (= 1 (:quotes p)))
        (is (= [:turn :prev] (mapv :role (:endpoints (first claims)))))
        (is (= [:turn :quoted-turn :pattern]
               (mapv :role (:endpoints (second claims)))))))
    (doseq [[id connection] [["turn-start" {:status :session-start}]
                             ["turn-absent" {:status :prev-absent :turn/id "turn-t0" :reason :no-reply}]]]
      (record! store id {:pattern_verdicts {:window [id] :connection connection :patterns []}})
      (seams #(is (= [:turn]
                     (mapv :role (:endpoints (:strand (sut/plan {:turn-store store}
                                                                 (assoc (turns/read-record store id) :id id)))))))))))

(deftest no-ma-record-uses-the-durable-turn-link-with-a-typed-session-start
  (let [store (temp-store)]
    (record! store "turn-old" {:prev_operator_turn_id nil})
    (record! store "turn-new" {:prev_operator_turn_id "turn-old"})
    (seams
     #(let [cfg {:turn-store store}
            start (sut/plan cfg (assoc (turns/read-record store "turn-old") :id "turn-old"))
            linked (sut/plan cfg (assoc (turns/read-record store "turn-new") :id "turn-new"))]
        (is (= :no-ma-record-session-start
               (get-in start [:strand :asserted-content :connection/status])))
        (is (= [:turn] (mapv :role (get-in start [:strand :endpoints]))))
        (is (= :no-ma-record
               (get-in linked [:strand :asserted-content :connection/status])))
        (is (= [:turn :prev] (mapv :role (get-in linked [:strand :endpoints]))))))))

(deftest no-ma-record-republish-is-idempotent
  (let [store (temp-store) messages (atom [])]
    (record! store "turn-start" {:prev_operator_turn_id nil})
    (seams
     #(with-redefs [http/append-batch! (fn [_ id _] (swap! messages conj id) {:ok {}})]
        (let [turn (assoc (turns/read-record store "turn-start") :id "turn-start")]
          (sut/write! {:turn-store store :graph-store :fixture} turn)
          (sut/write! {:turn-store store :graph-store :fixture} turn))))
    (is (= 1 (count (distinct @messages))))))

(deftest idempotent-replay-and-repair-have-stable-distinct-message-ids
  (let [store (temp-store) messages (atom [])]
    (record! store "turn-t0" {})
    (record! store "turn-t1" {:pattern_verdicts {:window ["turn-t0" "turn-t1"] :patterns []
                                             :connection {:status :prev-pending :turn/id "turn-t0"}}})
    (seams
     (fn [] (with-redefs [http/append-batch! (fn [_ id _] (swap! messages conj id) {:ok {}})]
        (let [cfg {:turn-store store :graph-store :fixture}
              turn #(assoc (turns/read-record store "turn-t1") :id "turn-t1")]
          (sut/write! cfg (turn))
          (sut/write! cfg (turn))
          (turns/update-record! store "turn-t1"
                                #(assoc-in % [:pattern_verdicts :connection]
                                           {:status :linked :turn/id "turn-t0" :sha256 "d"}))
          (sut/write! cfg (turn))))))
    (is (= (first @messages) (second @messages)))
    (is (not= (second @messages) (nth @messages 2)))))

(deftest predecessor-reader-prefers-repaired-linked-edge
  (let [claims {"no-ma" {:asserted-content {:connection/status :no-ma-record}
                            :endpoints [{:role :turn :target {:ref/id "t1"}}]}
                "linked" {:asserted-content {:connection/status :linked}
                           :endpoints [{:role :turn :target {:ref/id "t1"}}
                                       {:role :prev :target {:ref/id "t0"}}]}}]
    (with-redefs [mentions/field-evidence (fn [& _] {:reference (fixture-ref :evidence-text "t1")})
                  store/bounded-query! (fn [_ _] {:ok {:edge-ids ["linked" "no-ma"]}})
                  store/get-entity (fn [_ id] {:ok (claims id)})]
      (is (= {:status :linked :predecessor-reference-id "t0"}
             (sut/read-predecessor {:graph-store :fixture} {:id "t1"}))))))

(deftest backfill-dry-and-write-count-the-same-candidates
  (let [store (temp-store) writes (atom 0)]
    (record! store "turn-t" {:pattern_verdicts {:window ["turn-t"] :patterns []
                                            :connection {:status :session-start}}})
    (record! store "turn-legacy" {:prev_operator_turn_id "turn-t"})
    (seams
     #(with-redefs [sut/write! (fn [& _] (swap! writes inc) {:written 1})]
        (let [dry (sut/backfill! {:turn-store store} {})
              write (sut/backfill! {:turn-store store} {:write? true :pause-ms 0})]
          (is (= 2 (:turns dry) (:turns write)))
          (is (= {:session-start 1 :no-ma-record 1} (:by-status dry) (:by-status write)))
          (is (= 2 (get-in dry [:outcomes :would-write])))
          (is (= 2 (get-in write [:outcomes :written]))))
        (is (= 2 @writes))))))
