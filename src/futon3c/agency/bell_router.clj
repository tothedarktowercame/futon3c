(ns futon3c.agency.bell-router
  "Conversation graph over the invoke-jobs ledger (E-crossed-bells).

   A *thread* is a bell and its reply, correlated by the explicit `:bellback-of`
   field (set by the bell-router slice in transport/http.clj). This namespace is
   pure read-only analysis — open threads, who-owes-whom, and A<->B crossings — the
   coordination view that lets agents (and Joe) see *who is waiting on whom* instead
   of re-deriving it from interleaved turns. The crossing it surfaces is the
   actionable signal: when A and B both have an open bell to each other, one of them
   should whistle to reconcile (see README-bells-and-whistles.md)."
  (:require [clojure.string :as str]))

(defn- norm [x] (some-> x str str/trim not-empty))
(defn- bell? [job] (= "bell" (some-> (:surface job) str str/trim)))

(defn graph
  "Build the conversation graph from a seq of invoke-job maps. Each job has
   :job-id, :caller (from), :agent-id (to), :surface, :state, and — for replies —
   :bellback-of (the job-id it answers).

   Returns:
     :open       [{:id :from :to :state}]        bells with no reply yet
     :answered   [{:id :from :to :reply-id}]      bells whose reply has been recorded
     :crossings  [{:a :b}]                         unordered pairs with mutual OPEN bells
     :by-agent   {agent {:awaiting [{:id :to}]     bells this agent sent, still open
                         :owes     [{:id :from}]}}  bells to this agent it hasn't answered"
  [jobs]
  (let [bells (filter bell? jobs)
        bell-ids (into #{} (keep (comp norm :job-id)) bells)
        reply-of (->> jobs
                      (keep (fn [j]
                              (let [jid (norm (:job-id j))
                                    bo (norm (:bellback-of j))
                                    ref (norm (:ref j))
                                    bell-type (some-> (:bell-type j) name)]
                                (cond
                                  bo [bo jid]
                                  (and (= "answer" bell-type)
                                       (contains? bell-ids ref))
                                  [ref jid]))))
                      (into {}))
        rows (for [b bells
                   :let [id (norm (:job-id b))
                         from (norm (:caller b))
                         to (norm (:agent-id b))
                         bell-type (some-> (:bell-type b) name keyword)
                         ref (norm (:ref b))]
                   :when (and id from to)]
               {:id id :from from :to to
                :state (some-> (:state b) str)
                :type bell-type
                :ref ref
                :reply-id (get reply-of id)
                :open? (nil? (get reply-of id))})
        open (filter :open? rows)
        open-dir (into #{} (map (juxt :from :to) open))
        crossings (->> open-dir
                       (keep (fn [[a b]]
                               (when (and (contains? open-dir [b a])
                                          (neg? (compare a b)))
                                 {:a a :b b})))
                       vec)
        agents (into #{} (concat (map :from open) (map :to open)))
        by-agent (into {}
                       (for [ag agents]
                         [ag {:awaiting (->> open (filter #(= ag (:from %)))
                                             (mapv #(select-keys % [:id :to])))
                              :owes (->> open (filter #(= ag (:to %)))
                                         (mapv #(select-keys % [:id :from])))}]))]
    {:open (mapv #(select-keys % [:id :from :to :state :type :ref]) open)
     :answered (->> rows (remove :open?) (mapv #(select-keys % [:id :from :to :reply-id :type :ref])))
     :crossings crossings
     :by-agent by-agent}))
