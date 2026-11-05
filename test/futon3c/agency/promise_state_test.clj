(ns futon3c.agency.promise-state-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.promise-log :as promise-log]
            [futon3c.agency.promise-state :as promise-state]))

(def promise-id "park:state-1")
(def record {:id promise-id :agent "debtor-1" :beneficiary "creditor-1"
             :fulfilment-criterion {:kind :job-terminal-ok :job-id "job-1"}})
(def transitions
  [:promise/park-made :promise/ready-enqueued :promise/ready-leased
   :promise/ready-acked :promise/dependency-terminated :promise/woken
   :promise/released])

(defn- row [n transition]
  {:evidence/id (str "promise-history:state-" n)
   :evidence/type transition
   :evidence/author "debtor-1"
   :evidence/at (format "2026-11-05T18:00:0%dZ" n)
   :evidence/body (cond-> {:history/format 3
                           :history/payload-edn (pr-str {:record record :changes []})
                           :history/promise-id promise-id
                           :history/promise-sequence n}
                    (> n 1) (assoc :history/predecessor
                                   {:sequence (dec n)
                                    :id (str "promise-history:state-" (dec n))
                                    :type (nth transitions (- n 2))}))})

(def rows (mapv row (range 1 8) transitions))

(deftest fold-distinguishes-open-and-terminal-closed-chain
  (let [open (promise-state/fold-state (pop rows))
        closed (promise-state/fold-state rows)]
    (is (= :open (:status open)))
    (is (nil? (:terminal-transition open)))
    (is (= :closed (:status closed)))
    (is (= :promise/released (:terminal-transition closed)))
    (is (= "debtor-1" (:debtor closed)))
    (is (= "creditor-1" (:creditor closed)))
    (is (= (:fulfilment-criterion record) (:fulfilment-criterion closed)))))

(deftest missing-middle-is-incomplete-not-a-guessed-state
  (let [state (promise-state/fold-state [(first rows) (nth rows 2)])]
    (is (= :incomplete (:promise/state state)))
    (is (= :missing-transition (:reason state)))))

(deftest incremental-update-equals-full-fold-at-every-step
  (loop [prior nil remaining rows prefix []]
    (when-let [next-row (first remaining)]
      (let [incremental (promise-state/advance-state prior next-row)
            prefix' (conj prefix next-row)]
        (is (= (promise-state/fold-state prefix') incremental))
        (recur incremental (next remaining) prefix')))))

(deftest status-endpoint-flips-on-terminal-row
  (let [open-edge (promise-state/state->hyperedge (promise-state/fold-state (pop rows)))
        closed-edge (promise-state/state->hyperedge (promise-state/fold-state rows))]
    (is (some #{"promise-status:open"} (:hx/endpoints open-edge)))
    (is (not-any? #{"promise-status:closed"} (:hx/endpoints open-edge)))
    (is (some #{"promise-status:closed"} (:hx/endpoints closed-edge)))
    (is (not-any? #{"promise-status:open"} (:hx/endpoints closed-edge)))
    (is (= (:last-event-time (promise-state/fold-state rows))
           (:hx/valid-time closed-edge)))
    (is (not-any? nil? (vals (:hx/props open-edge))))))

(deftest writer-replay-does-not-rewind-an-advanced-state
  (let [edges (atom {})]
    (binding [promise-log/*get-fn* #(get @edges %)
              promise-log/*post-fn*
              (fn [edge]
                (swap! edges assoc (:hx/id edge) edge)
                {:ok true :hx/id (:hx/id edge)})]
      (doseq [history-row rows]
        (is (:ok (promise-state/write! history-row))))
      (let [closed (get @edges (promise-state/state-id promise-id))]
        (is (:replayed? (promise-state/write! (nth rows 3))))
        (is (= closed (get @edges (promise-state/state-id promise-id))))))))
