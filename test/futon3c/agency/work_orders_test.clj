(ns futon3c.agency.work-orders-test
  "W1 ledger tests for E-agency-work-orders. The transition fns are pure;
   the impure wrappers are exercised with a rebound *append-act!* so no
   evidence store is touched."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.work-orders :as wo]))

(defn- fresh-ledger [f]
  (wo/reset-ledger!)
  (f))

(use-fixtures :each fresh-ledger)

(defn- open-pure
  "Open an order on a pure ledger map."
  [orders m]
  (wo/open-order orders m))

(deftest open-root-and-children
  (testing "a caller with no open order gets a chain root, then the child under it"
    (let [opened (atom [])
          registered? #{"codex-19"}]
      (binding [wo/*append-act!* (fn [entry] (swap! opened conj entry))]
        (let [result (wo/maybe-open-order!
                      {:caller "codex-19" :debtor "codex-20" :job-id "job-1"
                       :prompt "implement the thing" :mode "work"
                       :registered? registered?
                       :running-job-id-fn (constantly nil)})]
          (is (= 2 (count result)) "root + child opened")
          (let [[root child] result]
            (is (= "joe" (:requester root)))
            (is (= "codex-19" (:debtor root)))
            (is (nil? (:parent root)))
            (is (= "chain root" (:text root)))
            (is (= :open (:state root)))
            (is (= "codex-19" (:requester child)))
            (is (= "codex-20" (:debtor child)))
            (is (= (:id root) (:parent child)))
            (is (= "job-1" (:job-id child)))
            (is (= :open (:state child)))
            (is (= [] (:nudges child))))
          (is (= 2 (count @opened)) "one :promise act per opened order")
          (is (every? #(= :promise (get-in % [:evidence/body :kind])) @opened))
          (is (every? #(= "work-orders" (:evidence/author %)) @opened)))))))

(deftest parent-found-from-running-order
  (testing "a second work bell hangs under the order the caller is running"
    (binding [wo/*append-act!* (fn [_] nil)]
      (wo/maybe-open-order!
       {:caller "codex-19" :debtor "codex-20" :job-id "job-1"
        :prompt "implement" :mode "work"
        :registered? #{"codex-19"} :running-job-id-fn (constantly nil)})
      (let [root (first (wo/list-orders {:agent "codex-19" :state :open}))
            opened (wo/maybe-open-order!
                    {:caller "codex-19" :debtor "codex-18" :job-id "job-2"
                     :prompt "replay" :mode "work"
                     :registered? #{"codex-19"}
                     :running-job-id-fn (constantly "job-root-job" )})
            ;; The caller is running the ROOT's job: simulate by pointing the
            ;; running job at the root's :job-id. Root opened with :job-id nil,
            ;; so fall back: parent must be the open root.
            child (last opened)]
        (is (= 1 (count opened)) "root already exists; only the child opens")
        (is (= (:id root) (:parent child)))))))

(deftest parent-prefers-the-running-job-order
  (testing "when the caller's running job matches a held order, that order is the parent"
    (let [[orders' root] (open-pure {} {:requester "joe" :debtor "codex-19"
                                        :parent nil :job-id nil :text "chain root"})
          ;; An order codex-19 OWES (debtor) under the root, whose job is running.
          [orders'' owed] (open-pure orders'
                                     {:requester "codex-18" :debtor "codex-19"
                                      :parent (:id root) :job-id "job-19" :text "fix"})
          parent (wo/parent-order orders'' "codex-19" "job-19")]
      (is (= (:id owed) (:id parent))
          "the order whose job the caller is running beats the root")
      (is (= (:id root) (:id (wo/parent-order orders'' "codex-19" "job-unrelated")))
          "no running-job match falls back to the open root")
      (is (nil? (wo/parent-order orders'' "codex-99" nil))
          "a caller holding no open order has no parent"))))

(deftest deliver-then-close-on-bellback
  (testing "debtor's job terminal delivers; the bellback job's terminal closes"
    (let [acts (atom [])]
      (binding [wo/*append-act!* (fn [e] (swap! acts conj e))]
        (wo/maybe-open-order!
         {:caller "codex-19" :debtor "codex-20" :job-id "job-impl"
          :prompt "implement" :mode "work"
          :registered? #{"codex-19"} :running-job-id-fn (constantly nil)})
        (let [child (last (wo/list-orders {:agent "codex-20"}))]
          (is (= :open (:state child)))
          (is (= "codex-20" (:holder child))))
        (let [closed (wo/job-terminal! {:job-id "job-impl"
                                        :bellback-job-id "job-bellback-1"})]
          (is (empty? closed) "delivery alone does not close when a bellback is sent"))
        (let [child (last (wo/list-orders {:agent "codex-20"}))]
          (is (= :delivered (:state child)))
          (is (= "job-bellback-1" (:bellback-job-id child)))
          (is (= "codex-19" (:holder child))
              "the token returns to the requester on delivery"))
        (let [closed (wo/job-terminal! {:job-id "job-bellback-1"
                                        :bellback-job-id nil})]
          (is (= 1 (count closed)))
          (is (= :fulfil (:closed-by (first closed)))))
        (let [child (last (wo/list-orders {:agent "codex-20"}))]
          (is (= :closed (:state child)))
          (is (nil? (:holder child))))
        (is (= :fulfil (get-in (last @acts) [:evidence/body :kind]))
            "closing appends a :fulfil act")))))

(deftest close-at-delivery-when-no-bellback
  (testing "no bellback (disabled/suppressed) closes the order at delivery"
    (binding [wo/*append-act!* (fn [_] nil)]
      (wo/maybe-open-order!
       {:caller "codex-19" :debtor "codex-18" :job-id "job-replay"
        :prompt "replay" :mode "work"
        :registered? #{"codex-19"} :running-job-id-fn (constantly nil)})
      (let [closed (wo/job-terminal! {:job-id "job-replay" :bellback-job-id nil})]
        (is (= 1 (count closed)))
        (is (= :fulfil (:closed-by (first closed))))
        (is (= :closed (:state (first (wo/list-orders {:agent "codex-18"
                                                       :state :closed})))))))))

(deftest non-work-bell-opens-nothing
  (testing "a mode=brief bell opens no order"
    (binding [wo/*append-act!* (fn [_] nil)]
      (is (nil? (wo/maybe-open-order!
                 {:caller "codex-19" :debtor "codex-20" :job-id "job-x"
                  :prompt "quick question" :mode "brief"
                  :registered? #{"codex-19"} :running-job-id-fn (constantly nil)})))
      (is (empty? (wo/list-orders {}))))))

(deftest excluded-callers-open-nothing
  (testing "auto-bellback / http-caller / joe and unregistered callers open nothing"
    (binding [wo/*append-act!* (fn [_] nil)]
      (doseq [caller ["auto-bellback" "http-caller" "joe" "unregistered-agent"]]
        (is (nil? (wo/maybe-open-order!
                   {:caller caller :debtor "codex-20" :job-id "job-y"
                    :prompt "work" :mode "work"
                    :registered? #{"codex-19"} :running-job-id-fn (constantly nil)}))))
      (is (empty? (wo/list-orders {}))))))

(deftest holder-is-computed-per-state
  (testing "holder-of: debtor while open, requester while delivered, nil closed"
    (let [[orders o] (wo/open-order {} {:requester "a" :debtor "b" :parent nil
                                        :job-id "j" :text "t"})]
      (is (= "b" (wo/holder-of (get orders (:id o)))))
      (let [orders' (wo/deliver orders (:id o) "bb")]
        (is (= "a" (wo/holder-of (get orders' (:id o))))))
      (let [orders'' (wo/close (wo/deliver orders (:id o) "bb") (:id o) :fulfil)]
        (is (nil? (wo/holder-of (get orders'' (:id o)))))))))

(deftest text-is-truncated-to-400
  (testing ":text holds the first 400 chars of the prompt"
    (let [long-prompt (apply str (repeat 500 "x"))
          [_ o] (wo/open-order {} {:requester "a" :debtor "b" :parent nil
                                   :job-id "j" :text long-prompt})]
      (is (= 400 (count (:text o)))))))

(deftest transitions-stamp-movement-and-explicit-close-releases
  (let [[orders order] (wo/open-order {} {:requester "requester" :debtor "debtor"
                                          :job-id "job" :text "work"})
        opened-move (:moved-at order)
        delivered (wo/deliver orders (:id order) "bb")]
    (is (integer? opened-move))
    (is (>= (get-in delivered [(:id order) :moved-at]) opened-move)))
  (binding [wo/*append-act!* (fn [_] nil)]
    (let [opened (wo/maybe-open-order!
                  {:caller "requester" :debtor "debtor" :job-id "child-job"
                   :prompt "work" :mode "work" :registered? #{"requester"}
                   :running-job-id-fn (constantly nil)})
          root (first opened)
          child (second opened)]
      (is (= 409 (:status (wo/close-order! {:id (:id root) :by "joe" :reason "stop"}))))
      (is (= 403 (:status (wo/close-order! {:id (:id child) :by "stranger" :reason "no"}))))
      (is (:ok (wo/close-order! {:id (:id root) :by "joe" :reason "stop" :force true})))
      (is (= :closed (:state (first (wo/list-orders {:agent "requester" :state :closed}))))))))
