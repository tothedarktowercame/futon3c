(ns futon3c.agency.mesh-qa
  "Unified coordination mesh reader and misrouting invariant checks.

   This reads edge metadata only: invoke-job ledger rows and social-layer
   :mesh-edge evidence. It intentionally does not inspect turn content."
  (:require [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.social.coordination-ledger :as coordination])
  (:import [java.time Duration Instant]))

(def terminal-states
  #{"succeeded" "failed" "error" "done" "complete" "completed" "timeout" "cancelled"})

(def checkability
  {:MQ-1 {:status :checkable
          :detail "terminal invoke-jobs expose :delivery status"}
   :MQ-2 {:status :capture-gap
          :detail "invoke-jobs do not capture the requested timeout/deadline; nonterminal timeout-window checks are gap findings"}
   :MQ-3 {:status :partial
          :detail "bellback intent is not explicit; only edges with :bellback-of can be checked"}
   :MQ-4 {:status :partial
          :detail "checks served :session-id against current registry status; historical session owner is not persisted"}
   :MQ-5 {:status :checkable
          :detail "origin :surface and delivery :surface are both in invoke-jobs"}
   :MQ-6 {:status :deferred
          :detail "ordering belongs to the Car-3 queue authority; timestamps alone are not the invariant source"}
   :MQ-7 {:status :checkable
          :detail "terminal codex invoke-jobs with http-caller/blank/unregistered callers cannot auto-bell back"}})

(defn- blankish? [x]
  (or (nil? x) (str/blank? (str x))))

(defn- parse-instant [x]
  (try
    (when-not (blankish? x)
      (Instant/parse (str x)))
    (catch Throwable _ nil)))

(defn- terminal? [state]
  (contains? terminal-states (str/lower-case (str state))))

(defn- delivered? [delivery]
  (= "delivered" (str/lower-case (str (:status delivery)))))

(defn- edge-time [edge k]
  (parse-instant (get edge k)))

(defn- seconds-between [a b]
  (when (and a b)
    (Math/abs (.getSeconds (Duration/between a b)))))

(defn job->edge
  "Project one invoke-job public row to the unified edge schema."
  [job]
  (let [delivery (:delivery job)
        state (str (:state job))]
    {:edge-id (:job-id job)
     :source :invoke-job
     :from (str (or (:caller job) "unknown"))
     :to (str (or (:agent-id job) "unknown"))
     :surface (str (or (:surface job) "unknown"))
     :kind :invoke
     :accepted-at (:created-at job)
     :terminal-at (:finished-at job)
     :terminal-state state
     :delivered? (delivered? delivery)
     :delivery-surface (some-> (:surface delivery) str)
     :session-id (some-> (:session-id job) str)
     :ok? (= "succeeded" (str/lower-case state))
     ;; bell-router: carry the EXPLICIT correlation (the bell this job answers).
     ;; edge-id == job-id, so this references the parent edge directly.
     :bellback-of (some-> (:bellback-of job) str str/trim not-empty)
     :type (:bell-type job)
     :ref (some-> (:ref job) str str/trim not-empty)
     :raw job}))

(defn- mesh-event-id [event]
  (or (:edge-id event) (:session-id event) (:id event)))

(defn coordination-events->edges
  "Coalesce Car-1 :mesh-edge evidence public views into direct-invoke edges."
  [events]
  (->> events
       (remove #(blankish? (:from %)))
       (group-by mesh-event-id)
       (mapv (fn [[eid evs]]
               (let [invoke (or (some #(when (= :invoke (:kind %)) %) evs)
                                (some #(when (= "invoke" (str (:kind %))) %) evs)
                                (first evs))
                     result (or (some #(when (= :invoke-result (:kind %)) %) evs)
                                (some #(when (= "invoke-result" (str (:kind %))) %) evs))
                     ok? (:ok? result)]
                 {:edge-id (str eid)
                  :source :mesh-evidence
                  :from (str (or (:from invoke) (:from result) "unknown"))
                  :to (str (or (:to invoke) (:to result) "unknown"))
                  :surface (str (or (:surface invoke) (:surface result) "unknown"))
                  :kind :direct-invoke
                  :accepted-at (:at invoke)
                  :terminal-at (:at result)
                  :terminal-state (cond
                                    (true? ok?) "succeeded"
                                    (false? ok?) "failed"
                                    :else nil)
                  :delivered? nil
                  :delivery-surface nil
                  :session-id (str eid)
                  :ok? ok?
                  :bellback-of nil
                  :raw evs})))))

(defn- duplicate-http-edge? [job-edge evidence-edge]
  (and (= (:from job-edge) (:from evidence-edge))
       (= (:to job-edge) (:to evidence-edge))
       (= (:surface job-edge) (:surface evidence-edge))
       (let [dt (seconds-between (edge-time job-edge :accepted-at)
                                 (edge-time evidence-edge :accepted-at))]
         (and dt (<= dt 2)))))

(defn dedupe-evidence-twins
  "Drop social evidence twins when the invoke-jobs ledger already has the edge."
  [job-edges evidence-edges]
  (vec (remove (fn [e]
                 (some #(duplicate-http-edge? % e) job-edges))
               evidence-edges)))

(defn derive-bellbacks
  "Explicit `:bellback-of` (set by the bell-router) WINS; for edges without it, fall
   back to the best-effort heuristic (a later reverse-direction edge is a bellback).
   Misrouted bellbacks without an explicit parent still can't be inferred reliably —
   the bell-router is the fix that makes them explicit."
  [edges]
  (let [ordered (sort-by #(or (:accepted-at %) "") edges)
        awaiting (atom {})]
    (mapv (fn [edge]
            (let [explicit? (some? (:bellback-of edge))
                  back-key [(:to edge) (:from edge)]
                  prior (when-not explicit? (first (get @awaiting back-key)))
                  edge* (if (and (not explicit?) prior) (assoc edge :bellback-of prior) edge)]
              (when (and (not explicit?) prior)
                (swap! awaiting update back-key #(vec (rest %))))
              (swap! awaiting update [(:from edge) (:to edge)] (fnil conj []) (:edge-id edge))
              edge*))
          ordered)))

(defn unify-edges
  "Merge invoke-jobs and social mesh-edge evidence into one edge set."
  [{:keys [jobs coordination-events]}]
  (let [job-edges (mapv job->edge (or jobs []))
        evidence-edges (coordination-events->edges (or coordination-events []))]
    (->> (into job-edges (dedupe-evidence-twins job-edges evidence-edges))
         derive-bellbacks
         (sort-by #(or (:accepted-at %) "") #(compare %2 %1))
         vec)))

(defn registry-session-map
  ([] (registry-session-map (reg/registry-status)))
  ([status]
   (into {}
         (keep (fn [[aid info]]
                 (when-let [sid (some-> (:session-id info) str str/trim not-empty)]
                   [(str aid) sid])))
         (:agents status))))

(defn registry-context
  ([] (registry-context (reg/registry-status)))
  ([status]
   (let [agents (:agents status)]
     {:sessions (registry-session-map status)
      :types (into {} (map (fn [[aid info]] [(str aid) (:type info)]) agents))
      :registered (set (map (comp str key) agents))})))

(defn- normalize-registry-context [registry]
  (if (and (map? registry) (contains? registry :sessions))
    registry
    {:sessions (or registry {})
     :types {}
     :registered (set (keys (or registry {})))}))

(defn- violation [invariant edge detail & {:as more}]
  (merge {:invariant invariant
          :ref (:edge-id edge)
          :detail detail}
         more))

(defn check-mq-1-delivery-integrity
  [edges]
  (vec (for [e edges
             :when (and (= :invoke-job (:source e))
                        (terminal? (:terminal-state e))
                        (not (:delivered? e)))]
         (violation :MQ-1 e "terminal invoke-job has no successful delivery record"))))

(defn check-mq-2-orphaned-bells
  [edges]
  (vec (for [e edges
             :when (and (= :invoke-job (:source e))
                        (not (terminal? (:terminal-state e))))]
         (violation :MQ-2 e
                    "accepted invoke-job is nonterminal, but timeout/deadline is not captured"
                    :capture-gap? true))))

(defn check-mq-3-caller-attribution
  [edges]
  (let [by-id (into {} (map (juxt :edge-id identity) edges))]
    (vec (for [e edges
               :let [orig (get by-id (:bellback-of e))]
               :when (and (:bellback-of e)
                          orig
                          (not= (:to e) (:from orig)))]
           (violation :MQ-3 e "bellback edge is not addressed to original caller"
                      :original (:edge-id orig)
                      :expected-to (:from orig)
                      :actual-to (:to e))))))

(defn check-mq-4-recipient-fidelity
  [edges registry-sessions]
  (vec (for [e edges
             :let [expected (get registry-sessions (:to e))]
             :when (and (= :invoke-job (:source e))
                        (:session-id e)
                        expected
                        (not= (:session-id e) expected))]
         (violation :MQ-4 e "invoke-job session-id does not match recipient registry session"
                    :expected-session-id expected
                    :actual-session-id (:session-id e)))))

(defn- compatible-surface? [surface delivery-surface]
  (let [s (some-> surface str str/lower-case)
        d (some-> delivery-surface str str/lower-case)]
    (or (= s d)
        (and (= s "http") (= d "http"))
        (and (= s "bell") (= d "job-status"))
        (and (= s "whistle") (= d "whistle"))
        (and (= s "whistle-stream") (= d "whistle-stream"))
        (and (= s "irc") (= d "irc")))))

(defn check-mq-5-surface-return
  [edges]
  (vec (for [e edges
             :when (and (= :invoke-job (:source e))
                        (terminal? (:terminal-state e))
                        (:delivered? e)
                        (:delivery-surface e)
                        (not (compatible-surface? (:surface e) (:delivery-surface e))))]
         (violation :MQ-5 e "delivery surface does not match originating surface"
                    :origin-surface (:surface e)
                    :delivery-surface (:delivery-surface e)))))


(defn- codex-recipient? [registry-context edge]
  (= :codex (get-in registry-context [:types (:to edge)])))

(defn- unaddressable-caller? [registry-context caller]
  (let [caller* (some-> caller str str/trim)]
    (and (not (str/blank? (or caller* "")))
         (not (#{"http-caller" "joe"} caller*))
         (not (contains? (:registered registry-context) caller*)))))

(defn check-mq-7-unaddressable-caller
  [edges registry-context]
  (vec (for [e edges
             :when (and (= :invoke-job (:source e))
                        (terminal? (:terminal-state e))
                        (codex-recipient? registry-context e)
                        (unaddressable-caller? registry-context (:from e)))]
         (violation :MQ-7 e
                    (str "codex completion has no addressable caller (" (:from e) ")")
                    :caller (:from e)))))

(defn check-mesh
  ([edges] (check-mesh edges (registry-context)))
  ([edges registry]
   (let [registry* (normalize-registry-context registry)
         violations (vec (concat (check-mq-1-delivery-integrity edges)
                                 (check-mq-2-orphaned-bells edges)
                                 (check-mq-3-caller-attribution edges)
                                 (check-mq-4-recipient-fidelity edges (:sessions registry*))
                                 (check-mq-5-surface-return edges)
                                 (check-mq-7-unaddressable-caller edges registry*)))
         counts (merge (zipmap (keys checkability) (repeat 0))
                       (frequencies (map :invariant violations)))]
     {:ok (empty? violations)
      :edge-count (count edges)
      :violation-count (count violations)
      :counts counts
      :violations violations
      :checkability checkability})))

(defn- current-invoke-jobs [limit]
  (if-let [v (ns-resolve 'futon3c.transport.http 'recent-invoke-jobs)]
    (@v limit)
    []))

(defn current-edges
  ([] (current-edges 100))
  ([limit]
   (unify-edges {:jobs (current-invoke-jobs limit)
                 :coordination-events (coordination/recent-mesh-edges limit)})))

(defn current-report
  ([] (current-report 100))
  ([limit]
   (let [edges (current-edges limit)]
     (assoc (check-mesh edges)
            :checked-at (str (Instant/now))))))
