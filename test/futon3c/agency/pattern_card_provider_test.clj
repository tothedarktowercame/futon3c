(ns futon3c.agency.pattern-card-provider-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.rule-record :as hx-store]
            [futon3c.agency.pattern-card-provider :as pattern]
            [futon3c.agency.prompt-line :as prompt-line]))

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
