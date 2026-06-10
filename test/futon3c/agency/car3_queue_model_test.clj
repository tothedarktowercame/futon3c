(ns futon3c.agency.car3-queue-model-test
  "Phase-1 logic model for Car-3 same-agent queue routing.

   Abstract trace shape:
     {:turns    [{:id :from :to :surface :msg-id :seq :accepted-at}]
      :outcomes [{:turn-id :status :delivered-surface :processed-order}]}

   Status values are abstract terminal states: :processed, :failed, :stale,
   :deduped, :reconciled. This model intentionally does not implement the queue;
   it ratifies the invariants that Phase 2 must satisfy."
  (:require [clojure.test :refer [deftest is]]))

(def terminal-statuses #{:processed :failed :stale :deduped :reconciled})
(defn- by-id [xs k]
  (into {} (map (juxt k identity) xs)))

(defn- indexed [xs]
  (map-indexed (fn [i x] (assoc x ::idx i)) xs))

(defn- outcome-by-turn [trace]
  (by-id (:outcomes trace) :turn-id))

(defn- violation [invariant ref detail]
  {:invariant invariant :ref ref :detail detail})

(defn check-q1-ordered-drain
  "C3-Q1: for each agent, work-bearing turns are processed in accepted order."
  [trace]
  (let [turns-by-id (by-id (:turns trace) :id)
        accepted-index (into {} (map (juxt :id ::idx) (indexed (:turns trace))))
        processed (->> (:outcomes trace)
                       (filter #(and (contains? terminal-statuses (:status %))
                                     (not= :deduped (:status %))))
                       (sort-by :processed-order)
                       (map #(merge % (select-keys (turns-by-id (:turn-id %)) [:to]))))]
    (vec
     (mapcat
      (fn [[to outs]]
        (let [accepted-seq (mapv #(accepted-index (:turn-id %)) outs)]
          (keep-indexed
           (fn [i [a b]]
             (when (> a b)
               (violation :C3-Q1 (:turn-id (nth outs (inc i)))
                          (str "processed out of accepted order for " to))))
           (partition 2 1 accepted-seq))))
      (group-by :to processed)))))

(defn check-q2-no-drop
  "C3-Q2: every accepted turn reaches a visible terminal state."
  [trace]
  (let [outs (outcome-by-turn trace)]
    (vec (for [{:keys [id]} (:turns trace)
               :let [status (:status (get outs id))]
               :when (not (contains? terminal-statuses status))]
           (violation :C3-Q2 id "accepted turn has no terminal outcome")))))

(defn check-r1-reply-routing
  "C3-R1: replies return to the turn's origin surface."
  [trace]
  (let [turns (by-id (:turns trace) :id)]
    (vec (for [{:keys [turn-id delivered-surface status]} (:outcomes trace)
               :let [turn (get turns turn-id)]
               :when (and (= :processed status)
                          turn
                          (not= (:surface turn) delivered-surface))]
           (violation :C3-R1 turn-id
                      (str "reply delivered to " delivered-surface
                           " instead of " (:surface turn)))))))

(defn check-d1-idempotency
  "C3-D1: one msg-id may create multiple accepted records, but only one may do work."
  [trace]
  (let [turns (by-id (:turns trace) :id)
        work-outs (filter #(= :processed (:status %)) (:outcomes trace))]
    (vec
     (mapcat
      (fn [[msg-id outs]]
        (when (> (count outs) 1)
          (map #(violation :C3-D1 (:turn-id %)
                           (str "msg-id processed more than once: " msg-id))
               outs)))
      (group-by (fn [out] (:msg-id (turns (:turn-id out)))) work-outs)))))

(defn check-c1-causal-markers
  "C3-C1: every non-duplicate turn has a sequence marker; per recipient, sequence
   markers are unique for distinct msg-ids; work whose seq is below the
   recipient's already-drained frontier at processing time must be marked stale
   or reconciled rather than delivered as normal work."
  [trace]
  (let [missing (for [{:keys [id seq]} (:turns trace)
                      :when (nil? seq)]
                  (violation :C3-C1 id "turn has no causal sequence marker"))
        duplicate-seq (mapcat
                       (fn [[[to seq] turns]]
                         (let [msg-ids (set (map :msg-id turns))]
                           (when (and seq (> (count msg-ids) 1))
                             (map #(violation :C3-C1 (:id %)
                                              (str "distinct turns share seq " seq
                                                   " for " to))
                                  turns))))
                       (group-by (juxt :to :seq) (:turns trace)))
        stale-normal (let [turns (by-id (:turns trace) :id)]
                       (second
                        (reduce
                         (fn [[frontiers violations] {:keys [turn-id status]}]
                           (let [{:keys [to seq]} (turns turn-id)
                                 frontier (get frontiers to 0)
                                 stale? (and seq (< seq frontier))
                                 frontiers* (if (and seq (not= :deduped status))
                                              (update frontiers to (fnil max 0) seq)
                                              frontiers)]
                             [frontiers*
                              (if (and stale? (= :processed status))
                                (conj violations
                                      (violation :C3-C1 turn-id
                                                 "seq below drained frontier was delivered as normal work instead of stale/reconciled"))
                                violations)]))
                         [{} []]
                         (sort-by :processed-order (:outcomes trace)))))]
    (vec (concat missing duplicate-seq stale-normal))))

(defn queue-violations [trace]
  (vec (concat (check-q1-ordered-drain trace)
               (check-q2-no-drop trace)
               (check-r1-reply-routing trace)
               (check-d1-idempotency trace)
               (check-c1-causal-markers trace))))

(def conforming-trace
  {:turns [{:id "t1" :from "joe" :to "claude-4" :surface "emacs-repl"
            :msg-id "m1" :seq 1 :accepted-at 1}
           {:id "t2" :from "claude-3" :to "claude-4" :surface "bell"
            :msg-id "m2" :seq 2 :accepted-at 2}
           {:id "t3" :from "claude-3" :to "claude-4" :surface "bell"
            :msg-id "m3" :seq 3 :accepted-at 3}]
   :outcomes [{:turn-id "t1" :status :processed :delivered-surface "emacs-repl" :processed-order 1}
              {:turn-id "t2" :status :processed :delivered-surface "bell" :processed-order 2}
              {:turn-id "t3" :status :processed :delivered-surface "bell" :processed-order 3}]})

(deftest conforming-trace-satisfies-all-invariants
  (is (empty? (queue-violations conforming-trace))))

(deftest adversarial-out-of-order-caught-by-q1
  (let [trace {:turns [{:id "a" :from "x" :to "agent" :surface "bell" :msg-id "m1" :seq 1 :accepted-at 1}
                       {:id "b" :from "x" :to "agent" :surface "bell" :msg-id "m2" :seq 2 :accepted-at 2}]
               :outcomes [{:turn-id "b" :status :processed :delivered-surface "bell" :processed-order 1}
                          {:turn-id "a" :status :stale :delivered-surface "bell" :processed-order 2}]}]
    (is (= [:C3-Q1] (mapv :invariant (queue-violations trace))))))

(deftest adversarial-drop-caught-by-q2
  (let [trace {:turns [{:id "a" :from "x" :to "agent" :surface "bell" :msg-id "m1" :seq 1 :accepted-at 1}]
               :outcomes []}]
    (is (= [:C3-Q2] (mapv :invariant (queue-violations trace))))))

(deftest adversarial-wrong-surface-caught-by-r1
  (let [trace {:turns [{:id "a" :from "joe" :to "agent" :surface "emacs-repl" :msg-id "m1" :seq 1 :accepted-at 1}]
               :outcomes [{:turn-id "a" :status :processed :delivered-surface "bell" :processed-order 1}]}]
    (is (= [:C3-R1] (mapv :invariant (queue-violations trace))))))

(deftest adversarial-duplicate-work-caught-by-d1
  (let [trace {:turns [{:id "a" :from "joe" :to "agent" :surface "emacs-repl" :msg-id "m1" :seq 1 :accepted-at 1}
                       {:id "a-replay" :from "joe" :to "agent" :surface "emacs-repl" :msg-id "m1" :seq 1 :accepted-at 2}]
               :outcomes [{:turn-id "a" :status :processed :delivered-surface "emacs-repl" :processed-order 1}
                          {:turn-id "a-replay" :status :processed :delivered-surface "emacs-repl" :processed-order 2}]}]
    (is (= [:C3-D1 :C3-D1] (mapv :invariant (queue-violations trace))))))

(deftest adversarial-crossed-bells-caught-by-c1
  (let [trace {:turns [{:id "a" :from "c3" :to "agent" :surface "bell" :msg-id "m1" :seq 7 :accepted-at 1}
                       {:id "b" :from "c4" :to "agent" :surface "bell" :msg-id "m2" :seq 7 :accepted-at 2}]
               :outcomes [{:turn-id "a" :status :stale :delivered-surface "bell" :processed-order 1}
                          {:turn-id "b" :status :processed :delivered-surface "bell" :processed-order 2}]}]
    (is (= [:C3-C1 :C3-C1] (mapv :invariant (queue-violations trace))))))

(deftest adversarial-stale-normal-work-caught-by-c1
  (let [trace {:turns [{:id "new" :from "c4" :to "agent" :surface "bell" :msg-id "m2" :seq 2 :accepted-at 1}
                       {:id "old" :from "c3" :to "agent" :surface "bell" :msg-id "m1" :seq 1 :accepted-at 2}]
               :outcomes [{:turn-id "new" :status :processed :delivered-surface "bell" :processed-order 1}
                          {:turn-id "old" :status :processed :delivered-surface "bell" :processed-order 2}]}]
    (is (= [:C3-C1] (mapv :invariant (queue-violations trace))))))

(deftest adversarial-missing-seq-caught-by-c1
  (let [trace {:turns [{:id "a" :from "c3" :to "agent" :surface "bell" :msg-id "m1" :seq nil :accepted-at 1}]
               :outcomes [{:turn-id "a" :status :processed :delivered-surface "bell" :processed-order 1}]}]
    (is (= [:C3-C1] (mapv :invariant (queue-violations trace))))))
