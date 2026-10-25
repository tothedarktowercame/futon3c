(ns futon3c.agency.pattern-card-provider-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.rule-record :as hx-store]
            [futon3c.agency.pattern-card-provider :as pattern]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.xiang.turn-store :as turn-store]))

(defn entry [id agent session at pattern-id]
  {:evidence/id id :evidence/author agent :evidence/session-id session
   :evidence/at at
   :evidence/body {"event" "context-retrieval"
                   "results" [{"id" pattern-id "score" 0.7 "rank" 1}
                               {"id" "other/two" "score" 0.6 "rank" 2}]}})

(use-fixtures :each (fn [f] (pattern/reset-cache!) (f)))

(deftest refresh-and-provider-use-exact-session
  (let [records [(entry "e-wrong" "claude-17" "other"
                       "2026-09-27T19:59:30Z" "wrong/pattern")
                 (entry "e-right" "claude-17" "target"
                        "2026-09-27T19:59:20Z" "right/pattern")]]
    (pattern/refresh! "claude-17" "target" (constantly records))
    (let [segment (pattern/provider {:agent-id "claude-17" :session-id "target"
                                     :render-at "2026-09-27T20:00:00Z"})]
      (is (= "~right/pattern" (:segment/value segment)))
      (is (= "e-right" (get-in segment [:segment/basis :evidence-ref])))
      (is (= :persisted (get-in segment [:segment/basis :scope :basis-status])))
      (is (= "retrieved right/pattern 0.7; also other/two"
             (:segment/header segment))))))

(deftest ranked-list-carries-all-proposed-patterns-with-scores
  (pattern/observe-results!
   "claude-17" "target"
   [{:id "top/pattern" :score 0.42 :rank 1}
    {:id "second/pattern" :score 0.31 :rank 2}
    {:id "third/pattern" :score 0.2 :rank 3}]
   "2026-09-27T19:59:59Z" "e-ranked" :provisional)
  (let [segment (pattern/provider {:agent-id "claude-17" :session-id "target"
                                   :render-at "2026-09-27T20:00:00Z"})]
    (is (= [{:id "top/pattern" :score 0.42 :rank 1 :source "embedding"}
            {:id "second/pattern" :score 0.31 :rank 2 :source "embedding"}
            {:id "third/pattern" :score 0.2 :rank 3 :source "embedding"}]
           (:segment/ranked segment))
        "the top and both also-patterns travel in rank order with scores, stamped embedding")
    (is (= "~top/pattern" (:segment/value segment))
        "value and header are unchanged by the new field")
    (is (= "retrieved top/pattern 0.42; also second/pattern, third/pattern"
           (:segment/header segment)))))

(deftest stale-retrieval-is-omitted
  (pattern/observe-entry! (entry "e-stale" "claude-17" "target"
                                 "2026-09-27T18:00:00Z" "old/pattern"))
  (is (nil? (pattern/provider {:agent-id "claude-17" :session-id "target"
                               :render-at "2026-09-27T20:00:00Z"}))))

(deftest persisted-edn-string-body-is-readable
  (pattern/observe-entry!
   (assoc (entry "e-wire" "claude-17" "target"
                 "2026-09-27T19:59:50Z" "wire/pattern")
          :evidence/body
          (pr-str {"event" "context-retrieval"
                   "results" [{:id "wire/pattern" :score 0.8 :rank 1}]})))
  (is (= "~wire/pattern"
         (:segment/value
          (pattern/provider {:agent-id "claude-17" :session-id "target"
                             :render-at "2026-09-27T20:00:00Z"})))))

(deftest provisional-results-are-visible-before-evidence-append
  (pattern/observe-results!
   "claude-17" "target" [{:id "fast/pattern" :score 0.9 :rank 1}]
   "2026-09-27T19:59:59Z" "e-pending" :provisional)
  (let [segment (pattern/provider {:agent-id "claude-17" :session-id "target"
                                   :render-at "2026-09-27T20:00:00Z"})]
    (is (= "~fast/pattern" (:segment/value segment)))
    (is (= "e-pending" (get-in segment [:segment/basis :evidence-ref])))
    (is (= :provisional (get-in segment [:segment/basis :scope :basis-status])))))

(deftest background-reads-are-rate-limited-per-seat
  (let [calls (atom [])]
    (with-redefs [pattern/refresh-async! (fn [a s] (swap! calls conj [:retrieval a s]))
                  pattern/refresh-cards-async! (fn [a s] (swap! calls conj [:card a s]))]
      (dotimes [_ 5]
        (pattern/provider {:agent-id "claude-17" :session-id "none"
                           :render-at "2026-09-27T20:00:00Z"}))
      (pattern/observe-entry! (entry "e-fresh" "claude-17" "target"
                                     "2026-09-27T19:59:50Z" "fresh/pattern"))
      (dotimes [_ 3]
        (pattern/provider {:agent-id "claude-17" :session-id "target"
                           :render-at "2026-09-27T20:00:00Z"})))
    (is (= [[:retrieval "claude-17" "none"] [:card "claude-17" "none"]
            [:retrieval "claude-17" "target"] [:card "claude-17" "target"]]
           @calls))))

(def card
  {:id "act:card" :kind :pattern-card/selection :author "claude-17"
   :agent "claude-17" :session "target" :at "2026-09-27T19:58:00Z"
   :pattern-id "card/chosen"})

(deftest active-card-wins-over-retrieval-without-render-path-http
  (pattern/observe-entry! (entry "e-retrieval" "claude-17" "target"
                                 "2026-09-27T19:59:50Z" "retrieved/pattern"))
  (pattern/publish-card-result! "claude-17" "target"
                                {:active card :provisional [] :ignored []}
                                "2026-09-27T19:59:55Z")
  (with-redefs [hx-store/request! (fn [& _] (throw (ex-info "HTTP on render" {})))
                pattern/refresh-async! (fn [& _])
                pattern/refresh-cards-async! (fn [& _])]
    (let [segment (pattern/provider {:agent-id "claude-17" :session-id "target"
                                     :render-at "2026-09-27T20:00:00Z"})]
      (is (= "~card/chosen" (:segment/value segment)))
      (is (= "card card/chosen (act:card)" (:segment/header segment)))
      (is (= "act:card" (get-in segment [:segment/basis :evidence-ref]))))))

(deftest withdrawn-card-falls-back-and-another-session-is-unaffected
  (pattern/observe-entry! (entry "e-target" "claude-17" "target"
                                 "2026-09-27T19:59:50Z" "retrieved/target"))
  (pattern/observe-entry! (entry "e-other" "claude-17" "other"
                                 "2026-09-27T19:59:50Z" "retrieved/other"))
  (pattern/publish-card-result! "claude-17" "target"
                                {:active nil :provisional [] :ignored []}
                                "2026-09-27T19:59:55Z")
  (pattern/publish-card-result! "claude-17" "other"
                                {:active (assoc card :session "other")
                                 :provisional [] :ignored []}
                                "2026-09-27T19:59:55Z")
  (with-redefs [pattern/refresh-async! (fn [& _])
                pattern/refresh-cards-async! (fn [& _])]
    (is (= "~retrieved/target"
           (:segment/value (pattern/provider {:agent-id "claude-17" :session-id "target"
                                              :render-at "2026-09-27T20:00:00Z"}))))
    (is (= "~card/chosen"
           (:segment/value (pattern/provider {:agent-id "claude-17" :session-id "other"
                                              :render-at "2026-09-27T20:00:00Z"}))))))

(deftest provisional-withdrawal-marker-precedes-retrieval-without-http
  (let [effect {:id "act:provisional" :kind :act/withdrawal :author "joe"
                :target "act:card" :status :provisional
                :basis {:kind :provisional-interpretation}
                :at "2026-09-27T19:59:00Z"}]
    (pattern/observe-entry! (entry "e-target" "claude-17" "target"
                                   "2026-09-27T19:59:50Z" "retrieved/target"))
    (pattern/publish-card-result! "claude-17" "target"
                                  {:candidate card :active nil
                                   :provisional [effect] :ignored []}
                                  "2026-09-27T19:59:55Z")
    (with-redefs [hx-store/request! (fn [& _] (throw (ex-info "HTTP on render" {})))
                  pattern/refresh-async! (fn [& _])
                  pattern/refresh-cards-async! (fn [& _])]
      (let [segment (pattern/provider {:agent-id "claude-17" :session-id "target"
                                       :render-at "2026-09-27T20:00:00Z"})]
        (is (= "~card/chosen?" (:segment/value segment)))
        (is (= "withdrawn? card/chosen (act:provisional)"
               (:segment/header segment))))
      ;; Through prompt-line itself, which validates segment shape.
      (let [result (prompt-line/render {:agent-id "claude-17" :session-id "target"
                                        :render-at "2026-09-27T20:00:00Z"}
                                       [{:segment/id :pattern
                                         :provider "futon3c.agency.pattern-card-provider/provider"
                                         :fn pattern/provider :budget-ms 100}])]
        (is (= "$~card/chosen?> " (:prompt result)))
        (is (empty? (:omitted result)))))))

(deftest unreadable-card-document-does-not-break-refresh
  (let [valid {:hx/id "act:card" :hx/type :pattern-card/selection
               :hx/props {:author "claude-17" :agent "claude-17" :session "target"
                          :at "2026-09-27T19:58:00Z" :pattern-id "card/chosen"}}
        missing-at {:hx/id "act:old" :hx/type :pattern-card/selection
                    :hx/props {:author "claude-17" :agent "claude-17"
                               :session "target" :pattern-id "card/old"}}
        result (pattern/refresh-cards!
                "claude-17" "target"
                (fn [type _endpoint _system-as-of]
                  (if (= :pattern-card/selection type)
                    [missing-at valid] [])))]
    (is (= "act:card" (get-in result [:result :active :id])))
    (is (= [{:hx/id "act:old" :reason :missing-at}] (:unreadable result)))))

(deftest refresh-filters-selection-by-session-and-withdrawal-by-target
  (let [calls (atom [])
        selection {:hx/id "act:card" :hx/type :pattern-card/selection
                   :hx/props {:author "claude-17" :agent "claude-17"
                              :session "target" :at "2026-09-27T19:58:00Z"
                              :pattern-id "card/chosen"}}
        other-agent {:hx/id "act:other" :hx/type :pattern-card/selection
                     :hx/props {:author "agent-b" :agent "agent-b"
                                :session "target" :at "2026-09-27T19:58:00Z"
                                :pattern-id "card/other"}}
        withdrawal {:hx/id "act:withdraw" :hx/type :act/withdrawal
                    :hx/props {:author "claude-17" :at "2026-09-27T19:59:00Z"
                               :target "act:card" :status :effective
                               :basis {:kind :self}}}
        result (pattern/refresh-cards!
                "claude-17" "target"
                (fn [type endpoint _system-as-of]
                  (swap! calls conj [type endpoint])
                  (cond
                    (= [type endpoint]
                       [:pattern-card/selection "session:target"])
                    [selection other-agent]
                    (= [type endpoint] [:act/withdrawal "act:card"])
                    [withdrawal]
                    :else (throw (ex-info "unscoped query" {:type type
                                                             :endpoint endpoint})))))]
    (is (= [[:pattern-card/selection "session:target"]
            [:act/withdrawal "act:card"]]
           @calls))
    (is (nil? (get-in result [:result :active])))))

(deftest older-background-result-cannot-overwrite-immediate-publication
  (pattern/publish-card-result! "claude-17" "target"
                                {:active card :provisional [] :ignored []}
                                "2026-09-27T20:00:00Z")
  (pattern/publish-card-result! "claude-17" "target"
                                {:active nil :provisional [] :ignored []}
                                "2026-09-27T19:59:59Z")
  (is (= "act:card"
         (get-in (pattern/cached-card "claude-17" "target")
                 [:result :active :id]))))

(deftest active-card-survives-a-long-turn
  ;; Refreshed at turn start, rendered at turn end ten minutes later.
  (pattern/publish-card-result! "claude-17" "target"
                                {:active card :provisional [] :ignored []}
                                "2026-09-27T19:50:00Z")
  (with-redefs [pattern/refresh-async! (fn [& _])
                pattern/refresh-cards-async! (fn [& _])]
    (is (= "~card/chosen"
           (:segment/value (pattern/active-pattern-card
                            {:agent-id "claude-17" :session-id "target"
                             :render-at "2026-09-27T20:00:00Z"}))))
    (is (nil? (pattern/active-pattern-card
               {:agent-id "claude-17" :session-id "target"
                :render-at "2026-09-27T20:31:00Z"})))))

;; 驴's picks on the turn record (Packet B): used only when already there,
;; never waited for — the provider has no whistle path at all.

(defn- temp-turn-record [id lu-ranked]
  (let [store (turn-store/store
               (str (java.nio.file.Files/createTempDirectory
                     "lu-provider-" (make-array java.nio.file.attribute.FileAttribute 0))))]
    (turn-store/write-record-id!
     store id (merge {:origin "operator" :agent_id "claude-17" :session_id "target"
                      :turn_id id :created_at "2026-10-25T00:00:00Z"
                      :source_text "operator text"}
                     (when lu-ranked {:lu_ranked lu-ranked})))
    store))

(deftest lu-ranked-picks-are-used-when-already-on-the-record
  (let [store (temp-turn-record
               "turn-lu"
               {:version 1 :at "2026-10-25T00:00:05Z" :ms 6500 :source "驴"
                :reason nil
                :ranked [{:id "lu/one" :rank 1 :quote "q1 q2 q3 q4 q5"}
                         {:id "lu/two" :rank 2 :quote "q2"}
                         {:id "lu/three" :rank 3 :quote "q3"}]})]
    (pattern/observe-results!
     "claude-17" "target" [{:id "emb/top" :score 0.9 :rank 1}]
     "2026-10-25T00:00:10Z" "e-lu" :provisional)
    (binding [prompt-line/*turn-record-store* store]
      (let [segment (pattern/provider
                     {:agent-id "claude-17" :session-id "target"
                      :render-at "2026-10-25T00:00:11Z"
                      :turn-record-id "turn-lu"})]
        (is (= "~lu/one" (:segment/value segment)))
        (is (= [{:id "lu/one" :rank 1 :source "驴"}
                {:id "lu/two" :rank 2 :source "驴"}
                {:id "lu/three" :rank 3 :source "驴"}]
               (:segment/ranked segment)))
        (is (= "驴 lu/one; also lu/two, lu/three" (:segment/header segment)))
        (is (re-find #"lu-rerank|provider" (:segment/provider segment)))))))

(deftest without-lu-ranked-the-embedding-segment-is-unchanged-apart-from-source
  ;; no :lu_ranked at all, and one whose source is the embedding fallback:
  ;; both must render the embedding segment, without waiting for anything.
  ;; The 驴/m40 read is stubbed to a typed fallback so no resident starts.
  (doseq [store [(temp-turn-record "turn-no-lu" nil)
                 (temp-turn-record
                  "turn-fallback"
                  {:version 1 :at "2026-10-25T00:00:05Z" :ms 3500
                   :source "embedding-fallback" :reason "timeout-or-empty-reply"
                   :ranked [{:id "fb/one" :rank 1}]})]]
    (pattern/observe-results!
     "claude-17" "target" [{:id "emb/top" :score 0.9 :rank 1}
                           {:id "emb/two" :score 0.8 :rank 2}]
     "2026-10-25T00:00:10Z" "e-emb" :provisional)
    (binding [prompt-line/*turn-record-store* store
              pattern/*read-near* (fn [_text _opts]
                                    {:ranked [] :neighbours 0 :ms 3
                                     :source nil :reason "no-positive-pattern"})]
      (let [segment (pattern/provider
                     {:agent-id "claude-17" :session-id "target"
                      :render-at "2026-10-25T00:00:11Z"
                      :turn-record-id "turn-no-lu"})]
        (is (= "~emb/top" (:segment/value segment)))
        (is (= "embedding" (:source (first (:segment/ranked segment)))))))))

(deftest a-render-context-without-a-turn-record-id-is-the-embedding-path
  (pattern/observe-results!
   "claude-17" "target" [{:id "emb/only" :score 0.7 :rank 1}]
   "2026-10-25T00:00:10Z" "e-no-id" :provisional)
  (is (= "~emb/only"
         (:segment/value
          (pattern/provider {:agent-id "claude-17" :session-id "target"
                             :render-at "2026-10-25T00:00:11Z"})))))

;; Packet 2b: the 驴/m40 read shown by record-id alternation, budgeted,
;; recorded, and fully disabled by the off-switch. "turn-even-5"'s first
;; sha256 byte is 100 (even), "turn-odd-1"'s is 165 (odd); both are
;; asserted as preconditions.

(def m40-ranked
  [{:id "m40/one" :score 1.5 :rank 1 :evidence ["r9" "r8"]}
   {:id "m40/two" :score 1.0 :rank 2 :evidence ["r8"]}
   {:id "m40/three" :score 0.6 :rank 3 :evidence ["r1"]}])

(defn- m40-read [ranked source]
  (fn [_text _opts]
    {:ranked ranked :neighbours (if (seq ranked) 6 0) :ms 7
     :source source :reason nil}))

(deftest the-sha-parity-of-the-test-record-ids
  (is (true? (pattern/m40-parity-even? "turn-even-5")))
  (is (false? (pattern/m40-parity-even? "turn-odd-1"))))

(deftest an-even-record-id-shows-the-m40-read
  (let [store (temp-turn-record "turn-even-5" nil)]
    (pattern/observe-results!
     "claude-17" "target" [{:id "emb/top" :score 0.9 :rank 1}] 
     "2026-10-25T00:00:10Z" "e-emb" :provisional)
    (binding [prompt-line/*turn-record-store* store
              pattern/*read-near* (m40-read m40-ranked "驴/m40")]
      (let [segment (pattern/provider
                     {:agent-id "claude-17" :session-id "target"
                      :render-at "2026-10-25T00:00:11Z"
                      :turn-record-id "turn-even-5"})]
        (is (= "~m40/one" (:segment/value segment)))
        (is (= "驴/m40" (:segment/shown-source segment)))
        (is (= "驴/m40" (:source (first (:segment/ranked segment)))))
        (is (= "驴/m40 m40/one (score 1.5); also m40/two, m40/three"
               (:segment/header segment)))
        (is (= "驴/m40" (:source (:segment/m40 segment))))
        (is (= ["emb/top"] (mapv :id (:segment/embedding-ranked segment)))
            "the unshown embedding top 3 is still carried for the record")))))

(deftest an-odd-record-id-shows-the-embedding-but-records-both
  (let [store (temp-turn-record "turn-odd-1" nil)]
    (pattern/observe-results!
     "claude-17" "target" [{:id "emb/top" :score 0.9 :rank 1}
                           {:id "emb/two" :score 0.8 :rank 2}]
     "2026-10-25T00:00:10Z" "e-emb" :provisional)
    (binding [prompt-line/*turn-record-store* store
              pattern/*read-near* (m40-read m40-ranked "驴/m40")]
      (let [segment (pattern/provider
                     {:agent-id "claude-17" :session-id "target"
                      :render-at "2026-10-25T00:00:11Z"
                      :turn-record-id "turn-odd-1"})]
        (is (= "~emb/top" (:segment/value segment)))
        (is (= "embedding" (:segment/shown-source segment)))
        (is (= "驴/m40" (:source (:segment/m40 segment)))
            "the m40 read is recorded even though it was not shown")))))

(deftest a-read-slower-than-the-budget-is-typed-and-shows-the-embedding
  (let [store (temp-turn-record "turn-even-5" nil)]
    (pattern/observe-results!
     "claude-17" "target" [{:id "emb/top" :score 0.9 :rank 1}]
     "2026-10-25T00:00:10Z" "e-emb" :provisional)
    (binding [prompt-line/*turn-record-store* store
              pattern/*read-near* (fn [_ _]
                                    (Thread/sleep 400)
                                    {:ranked m40-ranked :neighbours 6 :ms 400
                                     :source "驴/m40" :reason nil})]
      (let [segment (pattern/provider
                     {:agent-id "claude-17" :session-id "target"
                      :render-at "2026-10-25T00:00:11Z"
                      :turn-record-id "turn-even-5"})]
        (is (= "~emb/top" (:segment/value segment))
            "even parity but no read in budget: the embedding is shown")
        (is (= "embedding" (:segment/shown-source segment)))
        (is (= "budget" (:reason (:segment/m40 segment))))))))

(deftest the-off-switch-keeps-the-pre-2b-behaviour
  (let [store (temp-turn-record "turn-even-5" nil)
        off (doto (java.io.File/createTempFile "lu-off" "") (.deleteOnExit))]
    (pattern/observe-results!
     "claude-17" "target" [{:id "emb/top" :score 0.9 :rank 1}]
     "2026-10-25T00:00:10Z" "e-emb" :provisional)
    (binding [prompt-line/*turn-record-store* store
              pattern/*m40-off-switch* (.getPath off)
              pattern/*read-near* (fn [_ _] (throw (ex-info "must not read" {})))]
      (let [segment (pattern/provider
                     {:agent-id "claude-17" :session-id "target"
                      :render-at "2026-10-25T00:00:11Z"
                      :turn-record-id "turn-even-5"})]
        (is (= "~emb/top" (:segment/value segment)))
        (is (nil? (:segment/m40 segment)))
        (is (nil? (:segment/shown-source segment)))))))

(deftest a-render-records-m40-embedding-ranked-and-shown-source
  (let [store (temp-turn-record "turn-even-5" nil)]
    (pattern/observe-results!
     "claude-17" "target" [{:id "emb/top" :score 0.9 :rank 1}]
     "2026-10-25T00:00:10Z" "e-emb" :provisional)
    (binding [prompt-line/*turn-record-store* store
              pattern/*read-near* (m40-read m40-ranked "驴/m40")]
      (prompt-line/render! {:agent-id "claude-17" :session-id "target"
                            :render-at "2026-10-25T00:00:11Z"
                            :turn-record-id "turn-even-5" :job-id "j-1"})
      (let [entry (peek (:proposed_patterns
                         (turn-store/read-record store "turn-even-5")))]
        (is (= ["~m40/one"] (:patterns entry)))
        (is (= "驴/m40" (:shown_source entry)))
        (is (= "驴/m40" (:source (:m40_ranked entry))))
        (is (= 7 (:ms (:m40_ranked entry))))
        (is (= ["emb/top"] (mapv :id (:embedding_ranked entry))))
        (is (= "j-1" (:job_id entry)))))))
