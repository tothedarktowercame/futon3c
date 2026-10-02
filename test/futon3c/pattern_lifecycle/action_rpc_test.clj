(ns futon3c.pattern-lifecycle.action-rpc-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.boundary :as evidence]
            [futon3c.pattern-lifecycle.action-rpc :as rpc]))

(defn- entries [store]
  (mapv #(get-in @store [:entries %]) (:order @store)))

(def base
  {:pattern-id "coordination/mandatory-pur"
   :agent-id "agent-gate-a"
   :session-id "session-gate-a"
   :task-id "task-gate-a"
   :rationale "exercise automatic lifecycle"})

(deftest automatic-action-persists-matching-psr-and-pur
  (let [store (atom {:entries {} :order []})
        action-ran? (atom false)
        result (rpc/execute! (assoc base
                                    :evidence-store store
                                    :action #(do (reset! action-ran? true)
                                                 {:delivered :increment})))
        [psr pur] (entries store)]
    (is (:ok result))
    (is @action-ran?)
    (is (= [:pattern-selection :pattern-outcome]
           (mapv :evidence/type [psr pur])))
    (is (= ["agent-gate-a" "agent-gate-a"]
           (mapv :evidence/author [psr pur])))
    (is (= ["session-gate-a" "session-gate-a"]
           (mapv :evidence/session-id [psr pur])))
    (is (= (mapv :evidence/pattern-id [psr pur])
           [:coordination/mandatory-pur :coordination/mandatory-pur]))
    (is (= (:evidence/id psr) (:evidence/in-reply-to pur)))
    (is (every? true? (map #(get-in % [:evidence/body :automatic?]) [psr pur])))))

(deftest thrown-action-still-persists-failure-pur-with-continuity
  (testing "the exact bad case: failure after selection cannot orphan the PSR"
    (let [store (atom {:entries {} :order []})
          failure (try
                    (rpc/execute! (assoc base
                                         :evidence-store store
                                         :action #(throw (ex-info "boom" {}))))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))
          [psr pur] (entries store)]
      (is (= :pattern-action-failed (:failure-kind (ex-data failure))))
      (is (= 2 (count (entries store))))
      (is (= :pattern-action/failed (get-in pur [:evidence/body :event])))
      (is (= (:evidence/id psr) (:evidence/in-reply-to pur)))
      (is (= (select-keys psr [:evidence/author :evidence/session-id
                               :evidence/pattern-id])
             (select-keys pur [:evidence/author :evidence/session-id
                               :evidence/pattern-id]))))))

(deftest incomplete-identity-refuses-before-action-or-evidence
  (let [store (atom {:entries {} :order []})
        ran? (atom false)
        failure (try
                  (rpc/execute! (assoc base :session-id ""
                                       :evidence-store store
                                       :action #(reset! ran? true)))
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
    (is (= :pattern-action-identity-incomplete
           (:failure-kind (ex-data failure))))
    (is (false? @ran?))
    (is (empty? (:order @store)))))

(deftest completed-pur-persistence-failure-is-not-an-action-failure
  (testing "completed-PUR persistence failure must not emit a false failure PUR"
    (let [append-calls (atom [])
          action-runs (atom 0)
          failure (with-redefs [evidence/append!
                                (fn [_ entry]
                                  (swap! append-calls conj entry)
                                  (if (= :pattern-action/completed
                                         (get-in entry [:body :event]))
                                    {:ok false :error/code :store-unavailable}
                                    {:ok true :evidence/id (:evidence-id entry)}))]
                    (try
                      (rpc/execute! (assoc base
                                           :evidence-store ::store
                                           :action #(swap! action-runs inc)))
                      nil
                      (catch clojure.lang.ExceptionInfo e e)))]
      (is (= 1 @action-runs))
      (is (= :pattern-action-evidence-not-persisted
             (:failure-kind (ex-data failure))))
      (is (= :pur (:stage (ex-data failure))))
      (is (= [:pattern-action/selected :pattern-action/completed]
             (mapv #(get-in % [:body :event]) @append-calls)))
      (is (not-any? #(= :pattern-action/failed (get-in % [:body :event]))
                    @append-calls)))))
