(ns futon3c.diagramprover.skeleton.ledger
  "The invoke-jobs ledger and the 象 operator turns, mapped onto the Agency
  skeleton's event shapes (`futon3c.diagramprover.skeleton.agency`).

  The mapping the skeleton's docstring left unwritten is this: one job from
  caller A to agent B becomes a :ring (a :request bell, rung when the job
  was accepted), a :deliver (the running event — accept+drain collapsed to
  the protocol step), and a :reply when the job reaches a terminal state
  (done, failed, cancelled, timed-out — a failure is still a reply in
  protocol terms; :state rides on the event and on the bell summary so the
  drawing can colour it). An operator turn is the same with :from \"joe\".
  Each agent registers the first time it appears in the window.

  Callers that are not agents (turn-capture, http-caller, parked-resume,
  auto-bellback, nil) become lanes named \"system:<caller>\" (nil caller →
  \"system:unknown\"). The skeleton's signature parametrises agents by
  string, so these remain ordinary lanes and the laws apply unchanged; no
  ops are invented for them. Events whose own timestamps invert (a reply
  recorded before its ring) are emitted in time order anyway — the
  skeleton reads the reply's bell as a dangling input — and the inversion
  itself is reported under :ledger in the findings, so a corrupt ledger is
  a finding, not a crash."
  (:require [futon3c.diagramprover.skeleton.agency :as agency]))

(def ^:private terminal-states #{"done" "failed" "error" "cancelled" "timeout" "timed-out"})

(defn- instant-string
  "Best-effort ISO instant string for ordering; nil when absent/unparseable."
  [x]
  (try (some-> (java.time.Instant/parse (str x)) str)
       (catch Exception _ nil)))

(defn- lane-of
  "Caller lane name: agents keep their id; non-agent callers (and nil) get
  a system: lane so they stay ordinary wires rather than unreadable rows."
  [caller]
  (cond (nil? caller) "system:unknown"
        (re-matches #"(turn-capture|http-caller|parked-resume|auto-bellback).*" (str caller))
        (str "system:" caller)
        :else (str caller)))

(defn- job-events
  "One job -> [ring/deliver/reply events with :at for ordering]."
  [job]
  (let [id (str (or (:job-id job) (get job :job_id)))
        from (lane-of (:caller job))
        to (some-> (:agent-id job) str)
        ring-at (or (instant-string (:created-at job))
                    (some->> (:events job)
                             (filter #(= "accepted" (:type %)))
                             first :at instant-string))
        deliver-at (or (instant-string (:started-at job))
                       (some->> (:events job)
                                (filter #(= "running" (:type %)))
                                first :at instant-string))
        state (some-> (:state job) str)
        reply-at (when (terminal-states state)
                   (or (instant-string (:finished-at job))
                       (some->> (:events job)
                                (filter terminal-states)
                                last :at instant-string)))]
    (when (and (seq id) to ring-at)
      ;; a terminal job was certainly delivered even when the ledger lacks
      ;; the running event; a reply without a deliver would read :ill-typed.
      (let [deliver-at (or deliver-at (when reply-at reply-at))]
        (keep identity
              [{:op :ring :id id :from from :to to :type :request :at ring-at
                :kind (if (.startsWith from "system:") :system :bell)}
               (when deliver-at {:op :deliver :agent to :bell id :at deliver-at})
               (when reply-at {:op :reply :agent to :ref id :at reply-at
                               :state state})])))))

(defn- turn-events
  "One 象 operator turn -> ring (+deliver) and, when replied, reply events."
  [{:keys [record id]}]
  (let [agent (some-> (:agent_id record) str)
        turn-id (str (or (:turn_id record) id))
        created (instant-string (:created_at record))
        replied (instant-string (:reply_at record))]
    (when (and agent created)
      (keep identity
            [{:op :ring :id turn-id :from "joe" :to agent :type :request
              :at created :kind :operator}
             {:op :deliver :agent agent :bell turn-id :at created}
             (when replied {:op :reply :agent agent :ref turn-id :at replied
                            :state (if (:reply_at record) "done" nil)})]))))

(defn- registrations
  "[:register agent] for every lane in first-appearance order, so every wire
  the window touches has a beginning. Duplicated registers would be
  :concurrent-identity findings, hence first appearance only."
  [events]
  (let [seen (volatile! #{})]
    (keep (fn [e]
            (let [a (or (:agent e) (:from e))]
              (when (and a (not (@seen a)))
                (vswap! seen conj a)
                {:op :register :agent a})))
          events)))

(defn- rank
  "Stable ordering: time first, then ring < deliver < reply within a bell."
  [e]
  [(or (:at e) "")
   (case (:op e) :register 0 :ring 1 :deliver 2 (:drain :accept) 3 (:answer :reply) 4 5)
   (str (:id e) (:bell e) (:ref e) (:agent e))])

(defn ledger->events
  "JOBS (invoke ledger maps) and TURNS ([{:id :record}] from the 象 store)
  -> the skeleton's event shapes, in time order with a :register per lane.
  Jobs/turns missing the fields a bell needs (no id, no target, no ring
  time) are dropped, not guessed at."
  [jobs turns]
  (let [;; Only operator turns are rings from joe. The 象 store also records
        ;; agent bells (origin "agent", turn id = the job id) and session
        ;; ends (origin "system"); the job ledger already carries the bells,
        ;; so counting them again here duplicated every bell (claude-3's
        ;; review, 2026-10-19: 33 linearity and 18 signature findings in a
        ;; live hour, all from that). A record with no origin is kept.
        turns (filter #(contains? #{nil "operator"} (get-in % [:record :origin])) turns)
        raw (concat (mapcat job-events jobs) (mapcat turn-events turns))
        ordered (sort-by rank raw)]
    (vec (concat (registrations ordered) ordered))))

(defn- bell-table
  "Fold the ring/deliver/reply events back into one summary per bell id."
  [events]
  (reduce (fn [m e]
            (case (:op e)
              :ring (let [id (str (:id e))]
                      (update m id merge
                              {:id id :from (:from e) :to (:to e)
                               :ring-at (:at e)
                               :kind (case (:kind e) :operator :operator :system :system :bell)}))
              :deliver (update m (str (:bell e)) assoc :deliver-at (:at e))
              (:reply :answer) (update m (str (:ref e)) merge
                                       {:reply-at (:at e) :state (:state e)})
              m))
          {} events))

(defn- bell-summary
  "One bell for the drawing: what happened, when, and how it ended."
  [{:keys [id from to kind ring-at deliver-at reply-at state]}]
  (cond-> {"id" (str id) "from" (str from) "to" (str to)
           "kind" (case kind :operator "operator" :system "system" "bell")}
    ring-at (assoc "rung_at" ring-at)
    deliver-at (assoc "delivered_at" deliver-at)
    reply-at (assoc "replied_at" reply-at)
    state (assoc "state" state)))

(defn dispatch-view
  "The checked data the Diagram tab draws: lanes, bell summaries, open
  bells, crossings, and the law report over the same window. Pure — the
  HTTP layer only supplies JOBS, TURNS and the window bounds."
  [jobs turns]
  (let [events (ledger->events jobs turns)
        report (agency/check events)
        bells (bell-table events)
        lanes (into [] (distinct)
                    (concat ["joe"]
                            (into [] (comp (filter #(= :register (:op %))) (map :agent))
                                  events)))
        inverted (keep (fn [[id {:keys [ring-at reply-at]}]]
                         (when (and ring-at reply-at (pos? (compare ring-at reply-at)))
                           {:finding :reply-before-ring :ref (str id)}))
                       bells)
        findings (-> (select-keys report [:signature :linearity :identity
                                          :concurrency :routing :unreadable
                                          :open-bells :crossings])
                     (assoc :ledger inverted))]
    {:lanes lanes
     :bells (mapv (comp bell-summary val) (sort-by key bells))
     :open (mapv #(str (:ref %)) (:open-bells report))
     :crossings (mapv #(mapv str %) (:crossings report))
     :lawful? (and (agency/lawful? report) (empty? inverted))
     :findings (into {} (filter (comp seq val)) findings)}))

(defn dispatch-response
  "The JSON body of GET /api/alpha/diagram/dispatch for a window of JOBS
  and TURNS (already filtered to the window by the caller)."
  [jobs turns from-instant to-instant]
  (let [view (dispatch-view jobs turns)]
    {:ok true
     :window {"from" (str from-instant) "to" (str to-instant)
              "hours" (when (and from-instant to-instant)
                        (Math/round (double (/ (- (.toEpochMilli to-instant)
                                                  (.toEpochMilli from-instant))
                                               3600000.0))))}
     :lanes (:lanes view)
     :bells (:bells view)
     :open (:open view)
     :crossings (:crossings view)
     :lawful (:lawful? view)
     :findings (into {} (for [[k v] (:findings view)] [(name k) v]))}))
