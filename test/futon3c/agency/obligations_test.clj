(ns futon3c.agency.obligations-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.agreement-record :as agreement]
            [futon3c.agency.obligations :as obligations]))

(def t "2026-09-28T12:00:00Z")
(def stamp {:executor "joe" :signer "joe" :authority {:operator true}
            :executor-basis :session-bound})
(def harness {:kind :none :basis :producer-context :source-ref "test"})

(defn history-row
  ([id type pid n at rec] (history-row id type pid n at rec nil {}))
  ([id type pid n at rec previous details]
   {:evidence/id id :evidence/type type :evidence/at at
    :evidence/body (merge rec details
                          {:history/format 3 :history/promise-id pid
                           :history/promise-sequence n
                           :history/predecessor previous
                           :history/payload-edn
                           (pr-str {:record rec :changes [] :predecessor previous})})}))

(defn chain [pid rec & events]
  (loop [n 1 previous nil specs (cons [:promise/park-made "2026-09-28T10:00:00Z" {}] events) out []]
    (if-let [[type at details] (first specs)]
      (let [id (str pid "-" n)
            row (history-row id type pid n at rec previous details)]
        (recur (inc n) {:sequence n :id id :type type} (next specs) (conj out row)))
      out)))

(defn outcome [id type pid at]
  {:evidence/id id :evidence/type type :evidence/at at
   :evidence/body {:promise-id pid}})

(defn check
  ([id pid at verdict] (check id pid at verdict nil))
  ([id pid at verdict unable-reason]
   {:evidence/id id :evidence/type :promise/fulfilment-check :evidence/at at
    :evidence/body (cond-> {:promise-id pid :verdict verdict
                            :due-at "2026-09-28T11:00:00Z"}
                     unable-reason (assoc :unable-reason unable-reason))}))

(defn project [history outcomes agent]
  (obligations/obligations-as-of
   {:promise-history history :promise-outcomes outcomes :agreements [] :offers []}
   agent t))

(deftest overdue-and-explicitly-released
  (let [a {:id "a" :agent "agent-a" :beneficiary "agent-b"
           :deadline "2026-09-28T11:00:00Z"
           :fulfilment-criterion {:kind :job-terminal-ok :job-id "job-a"
                                  :machine-evaluable? true}}
        b (assoc a :id "b")
        h (concat (chain "a" a)
                  (chain "b" b [:promise/released "2026-09-28T11:30:00Z"
                                 {:release/basis :explicit :release/role :creditor
                                  :release/reason "Cannot proceed"}]))
        r (project h [(outcome "lapsed-a" :promise/lapsed "a" "2026-09-28T11:01:00Z")]
                   "agent-a")]
    (is (= ["a"] (mapv :obligation/id (:owes r))))
    (is (= :overdue (get-in r [:owes 0 :status])))
    (is (= [[:voided-by-creditor "b"]]
           (mapv (juxt :status :obligation/id) (:ignored r))))))

(deftest wake-and-plain-release-do-not-discharge-debt
  (let [rec {:id "wake" :agent "agent-a" :beneficiary "agent-b"
             :fulfilment-criterion {:kind :job-terminal-ok :job-id "job"
                                    :machine-evaluable? true}}
        rows (chain "wake" rec
                    [:promise/woken "2026-09-28T11:00:00Z" {}]
                    [:promise/released "2026-09-28T11:00:00Z" {}])
        r (project rows [] "agent-a")]
    (is (= ["wake"] (mapv :obligation/id (:owes r))))
    (is (= :open (get-in r [:owes 0 :status])))))

(deftest wake-and-plain-release-with-failed-check-stays-overdue
  (let [rec {:id "wake-failed" :agent "agent-a" :beneficiary "agent-b"
             :deadline "2026-09-28T11:00:00Z"
             :fulfilment-criterion {:kind :job-terminal-ok :job-id "job"
                                    :machine-evaluable? true}}
        rows (chain "wake-failed" rec
                    [:promise/woken "2026-09-28T11:00:00Z" {}]
                    [:promise/released "2026-09-28T11:00:00Z" {}])
        r (project rows [(check "check-failed" "wake-failed" "2026-09-28T11:01:00Z"
                                :unfulfilled)] "agent-a")]
    (is (= ["wake-failed"] (mapv :obligation/id (:owes r))))
    (is (= :overdue (get-in r [:owes 0 :status])))
    (is (= "check-failed" (get-in r [:owes 0 :check :id])))
    (is (some #{"check-failed"} (get-in r [:owes 0 :facts])))
    (is (not-any? #{:outcome-unknown} (get-in r [:owes 0 :facts])))))

(deftest fulfilled-and-completed-late-are-auditable-closed-rows
  (let [rec {:id "done" :agent "a" :beneficiary "b"
             :deadline "2026-09-28T11:00:00Z"
             :fulfilment-criterion {:kind :job-terminal-ok :job-id "j"
                                    :machine-evaluable? true}}
        rows (chain "done" rec)
        completed (project rows [(outcome "f" :promise/fulfilled "done" "2026-09-28T11:00:00Z")] "a")
        late (project rows [(outcome "l" :promise/lapsed "done" "2026-09-28T11:00:00Z")
                            (outcome "f" :promise/fulfilled "done" "2026-09-28T11:30:00Z")] "a")]
    (is (empty? (:owes completed)))
    (is (= :completed (get-in completed [:ignored 0 :status])))
    (is (= :completed-late (get-in late [:ignored 0 :status])))))

(deftest checks-close-or-mark-incomplete-and-later-fulfilment-wins
  (let [rec {:id "checked" :agent "a" :beneficiary "b"
             :deadline "2026-09-28T11:00:00Z"
             :fulfilment-criterion {:kind :job-terminal-ok :job-id "j"
                                    :machine-evaluable? true}}
        rows (chain "checked" rec)
        completed (project rows [(check "check-ok" "checked" "2026-09-28T11:00:00Z"
                                             :fulfilled)] "a")
        unable (project rows [(check "check-unknown" "checked" "2026-09-28T11:00:00Z"
                                           :unable-to-determine :job-not-found)] "a")
        later (project rows [(check "check-first" "checked" "2026-09-28T11:00:00Z"
                                          :unfulfilled)
                             (outcome "fulfilled-later" :promise/fulfilled "checked"
                                      "2026-09-28T11:30:00Z")] "a")]
    (is (= :completed (get-in completed [:ignored 0 :status])))
    (is (= :overdue (get-in unable [:owes 0 :status])))
    (is (= {:obligation/id "checked" :reason :unable-to-determine
            :record-id "check-unknown" :unable-reason :job-not-found}
           (first (:incomplete unable))))
    (is (= :completed-late (get-in later [:ignored 0 :status])))
    (is (every? (set (get-in later [:ignored 0 :facts]))
                ["check-first" "fulfilled-later"]))))

(deftest pending-check-rules-and-check-time-boundary
  (let [deadline-rec {:id "due" :agent "a" :beneficiary "b"
                      :deadline "2026-09-28T11:00:00Z"
                      :fulfilment-criterion {:kind :job-terminal-ok :job-id "j"
                                             :machine-evaluable? true}}
        no-deadline (dissoc (assoc deadline-rec :id "natural") :deadline)
        no-criterion {:id "wait" :agent "a" :beneficiary "b"}
        due (project (chain "due" deadline-rec) [] "a")
        natural (project (chain "natural" no-deadline) [] "a")
        wait (project (chain "wait" no-criterion) [] "a")
        after (project (chain "due" deadline-rec)
                       [(check "check-after" "due" "2026-09-28T12:00:00.001Z" :fulfilled)] "a")]
    (is (= :check-pending (get-in due [:incomplete 0 :reason])))
    (is (= :check-pending (get-in natural [:incomplete 0 :reason])))
    (is (= ["wait"] (mapv :obligation/id (:unchecked wait))))
    (is (= :check-pending (get-in after [:incomplete 0 :reason])))
    (is (nil? (get-in after [:owes 0 :check])))))

(deftest explicit-release-precedes-a-failed-check
  (let [rec {:id "released-check" :agent "a" :beneficiary "b"
             :deadline "2026-09-28T11:00:00Z"
             :fulfilment-criterion {:kind :job-terminal-ok :job-id "j"
                                    :machine-evaluable? true}}
        rows (chain "released-check" rec
                    [:promise/released "2026-09-28T11:30:00Z"
                     {:release/basis :explicit :release/role :creditor
                      :release/reason "Cannot proceed"}])
        r (project rows [(check "check-no" "released-check" "2026-09-28T11:01:00Z"
                                :unfulfilled)] "a")]
    (is (empty? (:owes r)))
    (is (= :voided-by-creditor (get-in r [:ignored 0 :status])))))

(deftest orphan-check-is-incomplete-without-inventing-a-row
  (let [r (project [] [(check "orphan" "missing" "2026-09-28T11:00:00Z"
                             :unfulfilled)] "a")]
    (is (empty? (:owes r)))
    (is (empty? (:owed r)))
    (is (= {:obligation/id "missing" :reason :orphan-check :record-id "orphan"}
           (first (:incomplete r))))))

(deftest unchecked-wait-ends-on-plain-release
  (let [rec {:id "wait" :agent "a" :beneficiary "b"}
        open (project (chain "wait" rec) [] "a")
        ended (project (chain "wait" rec [:promise/released "2026-09-28T11:00:00Z" {}]) [] "a")]
    (is (= ["wait"] (mapv :obligation/id (:unchecked open))))
    (is (empty? (:unchecked ended)))))

(deftest agreement-is-undated-and-grant-until-is-not-due-at
  (let [scope {:description "do x" :act-kinds [:x]
               :grant-until "2026-10-01T00:00:00Z"}
        offer {:id "act:offer" :kind :offer/record :author "agent-a" :addressee "joe"
               :seat {:agent "agent-a" :session "s"} :at "2026-09-28T10:00:00Z"
               :options [{:option/id "1" :option/label "x" :option/scope scope}]
               :act/stamp {:executor "agent-a" :signer "agent-a"
                           :authority {:grant "act:g"} :executor-basis :declared}}
        agree {:id "act:agreement" :kind :agreement/record :agreement/offer "act:offer"
               :agreement/acceptance-evidence "e-joe" :agreement/option-id "1"
               :agreement/scope scope :agreement/offeror "agent-a"
               :agreement/acceptor "joe" :agreement/at "2026-09-28T11:00:00Z"
               :act/stamp agreement/operator-stamp :act/harness harness}
        input {:promise-history [] :promise-outcomes [] :agreements [agree] :offers [offer]}
        a (obligations/obligations-as-of input "agent-a" t)
        joe (obligations/obligations-as-of input "joe" t)
        none (obligations/obligations-as-of (assoc input :agreements []) "agent-a" t)]
    (is (= scope (get-in a [:owes 0 :deliverable])))
    (is (nil? (get-in a [:owes 0 :due-at])))
    (is (= :no-due-at (get-in a [:incomplete 0 :reason])))
    (is (= "act:agreement" (get-in joe [:owed 0 :obligation/id])))
    (is (empty? (:owes none)))))

(deftest temporal-boundary-and-broken-chain
  (let [rec {:id "edge" :agent "a" :beneficiary "b"
             :deadline "2026-09-28T13:00:00Z"}
        exact (project (chain "edge" rec [:promise/released t
                                          {:release/basis :explicit :release/role :creditor
                                           :release/reason "Cannot proceed"}]) [] "a")
        later (project (chain "edge" rec [:promise/released "2026-09-28T12:00:00.001Z"
                                          {:release/basis :explicit :release/role :creditor
                                           :release/reason "Cannot proceed"}]) [] "a")
        broken (let [rows (chain "edge" rec [:promise/woken "2026-09-28T11:00:00Z" {}])
                     row (-> (second rows)
                             (assoc-in [:evidence/body :history/promise-sequence] 3)
                             (assoc-in [:evidence/body :history/predecessor]
                                       {:sequence 2 :id "missing-row" :type :promise/dependency-terminated}))]
                 (project [(first rows) row] [] "a"))]
    (is (empty? (:owes exact)))
    (is (= ["edge"] (mapv :obligation/id (:owes later))))
    (is (empty? (:owes broken)))
    (is (= :missing-transition (get-in broken [:incomplete 0 :reason])))))

(deftest debtor-void-and-invalid-explicit-release
  (let [rec {:id "p" :agent "a" :beneficiary "b" :deadline "2026-09-29T00:00:00Z"}
        abandoned (project (chain "p" rec [:promise/released "2026-09-28T11:00:00Z"
                                           {:release/basis :explicit :release/role :debtor
                                            :release/reason "Cannot proceed"}]) [] "a")
        invalid (project (chain "p" rec [:promise/released "2026-09-28T11:00:00Z"
                                         {:release/basis :explicit :release/role :observer
                                          :release/reason "Cannot proceed"}]) [] "a")]
    (is (= :voided-by-debtor (get-in abandoned [:ignored 0 :status])))
    (is (empty? (:owes abandoned)))
    (is (empty? (:owes invalid)))
    (is (= :invalid-release (get-in invalid [:incomplete 0 :reason])))))

(deftest opposite-party-countersignature-settles
  (let [rec {:id "settle" :agent "a" :beneficiary "b"
             :deadline "2026-09-29T00:00:00Z"}
        rows (chain "settle" rec
                    [:promise/released "2026-09-28T11:00:00Z"
                     {:release/basis :explicit :release/role :creditor
                      :release/reason "Cannot proceed"}]
                    [:promise/released "2026-09-28T11:30:00Z"
                     {:release/basis :explicit :release/role :debtor
                      :release/reason "Agreed" :release/countersigns "settle-2"}])
        result (project rows [] "a")]
    (is (empty? (:owes result)))
    (is (= :settled (get-in result [:ignored 0 :status])))
    (is (every? (set (get-in result [:ignored 0 :facts])) ["settle-2" "settle-3"]))))

(deftest explicit-release-needs-a-reason-and-countersignature-needs-its-first-row
  (let [rec {:id "bad-release" :agent "a" :beneficiary "b"
             :deadline "2026-09-29T00:00:00Z"}
        no-reason (project (chain "bad-release" rec
                                  [:promise/released "2026-09-28T11:00:00Z"
                                   {:release/basis :explicit :release/role :creditor}]) [] "a")
        no-first (project (chain "bad-release" rec
                                 [:promise/released "2026-09-28T11:00:00Z"
                                  {:release/basis :explicit :release/role :debtor
                                   :release/reason "Agreed"
                                   :release/countersigns "missing"}]) [] "a")]
    (is (= :invalid-release (get-in no-reason [:incomplete 0 :reason])))
    (is (= :nothing-to-countersign (get-in no-first [:incomplete 0 :reason])))
    (is (empty? (:owes no-reason)))
    (is (empty? (:owes no-first)))))

(deftest owes-and-owed-partition-and-missing-beneficiary
  (let [rec {:id "p" :agent "agent-a" :beneficiary "agent-b"
             :fulfilment-criterion {:kind :prose :text "do it"
                                    :machine-evaluable? false}}
        input {:promise-history (chain "p" rec) :promise-outcomes []
               :agreements [] :offers []}
        debtor (obligations/obligations-as-of input "agent-a" t)
        creditor (obligations/obligations-as-of input "agent-b" t)
        missing (project (chain "missing" (dissoc (assoc rec :id "missing") :beneficiary)) [] "agent-a")]
    (is (= ["p"] (mapv :obligation/id (:owes debtor))))
    (is (= ["p"] (mapv :obligation/id (:owed creditor))))
    (is (= ["missing"] (mapv :obligation/id (:owes missing))))
    (is (some #(= :no-beneficiary (:reason %)) (:incomplete missing)))))

(deftest other-agents-waits-and-closed-rows-are-not-listed
  (let [wait (chain "wait-c" {:id "wait-c" :agent "agent-c" :beneficiary "agent-d"})
        done-rec {:id "done-c" :agent "agent-c" :beneficiary "agent-d"
                  :deadline "2026-09-28T11:30:00Z"}
        done (chain "done-c" done-rec)
        result (project (into wait done)
                        [(outcome "o-c" :promise/fulfilled "done-c" "2026-09-28T11:00:00Z")]
                        "agent-a")]
    (is (empty? (:unchecked result)))
    (is (empty? (:ignored result)))
    (is (= ["wait-c"] (mapv :obligation/id
                            (:unchecked (project wait [] "agent-d")))))))
