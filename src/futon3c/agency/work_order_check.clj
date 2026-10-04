(ns futon3c.agency.work-order-check
  "Pure E3 decision for the work-order token design
  (holes/excursions/E-agency-work-orders.md, \"Token design for the live
  case\", event E3).

  At the end of an agent's job, decide whether that agent is holding a
  work-order token still, and what to do about it: nothing, nudge it, or
  escalate to the order's requester.

  Depends only on plain data:

    order: {:id :requester :debtor :parent :job-id :text :opened-at
            :state (:open|:delivered|:closed) :closed-by
            :nudges [{:at ms :to agent :kind (:nudge|:escalate)}]}
    agent-state: {:running-jobs n :queued-jobs n :parked? bool}

  (check agent orders agent-state now-ms) =>
    nil                                   ; nothing to do
    {:action :nudge|:escalate
     :order <id> :to <agent-id|\"joe\"> :text <bell text>}")

(defn- holding?
  "Rule 1: agent holds the token when it is the debtor of an :open order,
  or the requester of a :delivered one (token returned; it owes the next
  move)."
  [agent order]
  (or (and (= agent (:debtor order)) (= :open (:state order)))
      (and (= agent (:requester order)) (= :delivered (:state order)))))

(defn- has-open-child?
  "Rule 3: an order with a child in :open or :delivered state has passed
  the token down; its holder is not stalled."
  [orders order]
  (some (fn [child]
          (and (= (:id order) (:parent child))
               (contains? #{:open :delivered} (:state child))))
        orders))

(defn- last-move-ms
  "The most recent observable movement on an order: its own opening, or
  the opening of any child (dispatching a child is the debtor moving)."
  [orders order]
  (reduce max
          (or (:opened-at order) 0)
          (keep (fn [child]
                  (when (= (:id order) (:parent child))
                    (:opened-at child)))
                orders)))

(defn- nudge-since-move?
  "Rule 5 antecedent: a :nudge to this agent recorded after the order's
  last observable movement (no newer child opened since)."
  [agent orders order]
  (let [moved (last-move-ms orders order)]
    (some (fn [nudge]
            (and (= :nudge (:kind nudge))
                 (= agent (:to nudge))
                 (> (or (:at nudge) 0) moved)))
          (:nudges order))))

(defn- escalated?
  "Rule 5 coda: after one escalation, nil forever for that order."
  [order]
  (some #(= :escalate (:kind %)) (:nudges order)))

(defn- waiting-on
  "Who is waiting on this order: its requester, or joe for a root."
  [order]
  (if (:parent order)
    (:requester order)
    (or (:requester order) "joe")))

(defn- nudge-text
  [agent order]
  (str "Work order " (:id order) " is still open and you hold the token.\n"
       "Order: " (:text order) "\n"
       (waiting-on order) " is waiting on it. You owe one of:\n"
       "  1. dispatch the next step: python3 /home/joe/code/futon3c/scripts/agency_send.py"
       " --from " agent " --to <agent> --kind bell --mode work  (message on stdin)\n"
       "  2. close the order: curl -X POST http://localhost:7070/api/alpha/work-orders/"
       (:id order) "/close -d '{\"by\":\"" agent "\",\"reason\":\"...\"}'"))

(defn- escalate-text
  [order]
  (str "Work order " (:id order) " is stalled: " (:debtor order)
       " was nudged and its next job ended with nothing dispatched.\n"
       "Order: " (:text order)))

(defn check
  "E3 stall decision for `agent` at the end of its job. Returns nil when
  there is nothing to do, otherwise a single action map for the oldest
  stalled order."
  [agent orders agent-state _now-ms]
  (when-not (or (:parked? agent-state)
                (pos? (or (:running-jobs agent-state) 0))
                (pos? (or (:queued-jobs agent-state) 0)))
    (let [orders (seq orders)]
      (when orders
        (->> orders
             (filter #(holding? agent %))
             (remove #(has-open-child? orders %))
             (sort-by (fn [order] [(or (:opened-at order) 0) (:id order)]))
             (keep (fn [order]
                     (cond
                       (escalated? order) nil

                       (nudge-since-move? agent orders order)
                       {:action :escalate
                        :order (:id order)
                        :to (waiting-on order)
                        :text (escalate-text order)}

                       :else
                       {:action :nudge
                        :order (:id order)
                        :to agent
                        :text (nudge-text agent order)})))
             (first))))))
