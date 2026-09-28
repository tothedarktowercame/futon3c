(ns futon3c.agency.promise-outcome
  "P5 criterion observations, separate from wake/release and snapshot-edit history.
   Evaluated asynchronously after recorded wake/release (also creation, dependency,
   and followup transitions), and by the 30-second parked-on deadline sweep.
   Sweep reads retained history, including promises removed from /tmp. Prose is
   never automatically fulfilled or lapsed. Missing job/read failure is unknown.
   Evidence time is observation time, not the historical transition's clock.
   Deterministic per-promise/per-outcome ids are checked in the durable backend;
   no process-local flag can suppress retry after a failed write or restart."
  (:require [clojure.edn :as edn]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.origin :as origin]
            [futon3c.evidence.store :as store])
  (:import [java.time Instant]
           [java.util UUID]))

(def types #{:promise/fulfilled :promise/lapsed})
(def check-type :promise/fulfilment-check)
(def live-job-states
  #{"announced" "accepted" "queued" "activating" "running" "overrun" "invoking" "parked"})
(def terminal-job-states #{"done" "failed" "error" "cancelled" "timeout" "deduped" "rejected"})
(def known-job-states (into live-job-states terminal-job-states))
(def unable-reasons
  #{:unsupported-criterion :job-not-found :job-state-unknown :job-deduped
    :job-read-failed :invalid-promise-record})
(def ^:dynamic *lookup-job*
  (fn [id]
    (when-let [v (some-> (find-ns 'futon3c.transport.http)
                        (ns-resolve 'promise-job-lookup))]
      (v id))))
(defonce ^:private !stats (atom {:evaluated 0 :written 0 :existing 0 :failed 0}))
(defn stats [] @!stats)
(defn failed! [e]
  (swap! !stats update :failed inc)
  (binding [*out* *err*] (println "promise-outcome failed:" (.getMessage ^Throwable e))))

(defn outcome-id [pid type]
  (str "promise-outcome:" (UUID/nameUUIDFromBytes (.getBytes (pr-str [pid type]) "UTF-8"))))

(defn check-id [pid]
  (str "promise-check:" (UUID/nameUUIDFromBytes (.getBytes (str pid) "UTF-8"))))

(defn validate-check!
  "Validate the closed P8 verdict/reason relation before any append."
  [entry]
  (let [{:keys [promise-id source-evidence-id criterion due-at checked-at verdict unable-reason refs]}
        (:evidence/body entry)]
    (when-not (= check-type (:evidence/type entry))
      (throw (ex-info "Not a fulfilment check" {:reason :invalid-check-type})))
    (when-not (and (string? promise-id) (seq promise-id) (string? source-evidence-id)
                   (map? criterion) (string? due-at) (string? checked-at) (vector? refs)
                   (contains? #{:fulfilled :unfulfilled :unable-to-determine} verdict))
      (throw (ex-info "Invalid fulfilment check shape" {:reason :invalid-check-shape})))
    (if (= :unable-to-determine verdict)
      (when-not (contains? unable-reasons unable-reason)
        (throw (ex-info "Unknown unable reason" {:reason :invalid-unable-reason
                                                  :unable-reason unable-reason})))
      (when (contains? (:evidence/body entry) :unable-reason)
        (throw (ex-info "Reason only belongs to unable verdict"
                        {:reason :unexpected-unable-reason}))))
    entry))

(defn- millis [x]
  (cond (integer? x) x
        (string? x) (.toEpochMilli (Instant/parse x))
        :else nil))

(defn- creation-ms [rec source-at now-ms]
  (or (:parked-at-ms rec) (:created-at-ms rec) (millis source-at) now-ms))

(defn- check-decision
  [rec job now-ms source-at read-failed? existing]
  (let [criterion (:fulfilment-criterion rec)
        job-kind? (= :job-terminal-ok (:kind criterion))
        deadline-ms (millis (:deadline rec))
        state (some-> (:state job) name)
        deadline-due? (and deadline-ms (>= now-ms deadline-ms))
        terminal? (contains? terminal-job-states state)
        due-ms (cond deadline-ms deadline-ms
                     (not job-kind?) (creation-ms rec source-at now-ms)
                     terminal? (or (millis (:finished-at job))
                                   (some-> existing :evidence/body :due-at millis)
                                   now-ms))]
    (when (and due-ms (>= now-ms due-ms))
      (cond
        (not job-kind?) {:verdict :unable-to-determine :unable-reason :unsupported-criterion
                         :due-ms due-ms}
        read-failed? (when (>= now-ms (+ due-ms (* 60 60 1000)))
                       {:verdict :unable-to-determine :unable-reason :job-read-failed
                        :due-ms due-ms})
        (nil? job) {:verdict :unable-to-determine :unable-reason :job-not-found :due-ms due-ms}
        (not (contains? known-job-states state))
        {:verdict :unable-to-determine :unable-reason :job-state-unknown :due-ms due-ms}
        (= "deduped" state)
        {:verdict :unable-to-determine :unable-reason :job-deduped :due-ms due-ms}
        (= "done" state)
        {:verdict (if (and deadline-ms (millis (:finished-at job))
                           (> (millis (:finished-at job)) deadline-ms))
                    :unfulfilled :fulfilled)
         :due-ms due-ms}
        (or terminal? deadline-due?) {:verdict :unfulfilled :due-ms due-ms}
        :else nil))))

(defn- write-check!
  [backend source-id rec now-ms source-at job read-failed? & [forced]]
  (let [pid (or (:id rec) (:followup-id rec))
        eid (when (string? pid) (check-id pid))
        existing (when eid (store/get-entry* backend eid))
        decision (or forced (check-decision rec job now-ms source-at read-failed? existing))]
    (when decision
      (when-not eid (throw (ex-info "Missing promise id" {:record rec})))
      (let [criterion (:fulfilment-criterion rec)
            due-at (str (Instant/ofEpochMilli (:due-ms decision)))
            same? (fn [entry]
                    (and (= check-type (:evidence/type entry))
                         (= pid (get-in entry [:evidence/body :promise-id]))
                         (= source-id (get-in entry [:evidence/body :source-evidence-id]))
                         (= criterion (get-in entry [:evidence/body :criterion]))
                         (= due-at (get-in entry [:evidence/body :due-at]))))
            observation (cond-> {:job-id (get-in rec [:fulfilment-criterion :job-id])
                                 :observed-at (str (Instant/ofEpochMilli now-ms))}
                          job (merge (select-keys job [:state :finished-at :terminal-code])))
            refs (if (= :job-terminal-ok (:kind criterion)) [observation] [])]
        (if existing
          (do (when-not (same? existing)
                (throw (ex-info "Conflicting promise fulfilment check" {:id eid})))
              (swap! !stats update :existing inc)
              {:status :existing :id eid})
          (let [body (cond-> {:promise-id pid :source-evidence-id source-id
                              :criterion criterion :due-at due-at
                              :checked-at (str (Instant/ofEpochMilli now-ms))
                              :verdict (:verdict decision) :refs refs}
                       (:unable-reason decision) (assoc :unable-reason (:unable-reason decision)))
                entry (validate-check!
                       (cond-> {:evidence/id eid :evidence/type check-type
                                :evidence/claim-type :observation
                                :evidence/subject {:ref/type :evidence :ref/id source-id}
                                :evidence/author (str (:agent rec))
                                :evidence/at (:checked-at body)
                                :evidence/tags [:promise-outcome :fulfilment-check]
                                :evidence/body body}
                         (:session rec) (assoc :evidence/session-id (:session rec))))
                receipt (boundary/append! backend
                                          (origin/stamp entry
                                                        (origin/harness "promise-outcome" pid)
                                                        "futon3c.agency.promise-outcome"))]
            (cond
              (:ok receipt) (do (swap! !stats update :written inc) {:status :written :id eid})
              (and (= :duplicate-id (:error/code receipt)) (same? (store/get-entry* backend eid)))
              (do (swap! !stats update :existing inc) {:status :existing :id eid})
              :else (throw (ex-info "Promise check write failed" receipt)))))))))

(defn decide
  "Pure criterion check. A late successful job can have both lapsed and fulfilled
   observations: its immutable finished-at proves it was not done by the deadline.
   Missing terminal timestamps cannot prove retrospective lateness."
  [rec job now-ms]
  (let [criterion (:fulfilment-criterion rec)
        kind (:kind criterion)
        state (some-> (:state job) name)
        known? (contains? #{"announced" "accepted" "queued" "activating" "running"
                            "overrun" "invoking" "parked" "done" "failed" "error"
                            "cancelled" "timeout" "deduped" "rejected"} state)
        satisfied? (= "done" state)
        deadline (some-> (:deadline rec) Instant/parse .toEpochMilli)
        finished (some-> (:finished-at job) Instant/parse .toEpochMilli)]
    (if (and (= :job-terminal-ok kind) (true? (:machine-evaluable? criterion)) known?)
      (cond-> []
        (and deadline (> now-ms deadline)
             (or (not satisfied?) (and finished (> finished deadline)))) (conj :promise/lapsed)
        satisfied? (conj :promise/fulfilled))
      [])))

(defn- evaluate-observation!
  "Check one retained promise record against a job-ledger observation. Outcome
   records carry all the job fields the evaluator read and the source record id.
   No outcome is inferred from a dependency payload or from the fact of waking."
  [backend source-id rec now-ms source-at write-check?]
  (let [criterion (:fulfilment-criterion rec)]
    (when criterion
      (swap! !stats update :evaluated inc)
      (let [pid (or (:id rec) (:followup-id rec))
            job-kind? (= :job-terminal-ok (:kind criterion))
            lookup (when job-kind?
                     (try {:job (*lookup-job* (:job-id criterion))}
                          (catch Throwable e {:read-failed? true :error e})))
            job (:job lookup)
            observation (assoc (select-keys job [:state :finished-at :terminal-code])
                               :job-id (:job-id criterion) :observed-at (str (Instant/ofEpochMilli now-ms)))]
        (when-not (string? pid) (throw (ex-info "Missing promise id" {:record rec})))
        (let [check (when write-check?
                      (write-check! backend source-id rec now-ms source-at job (:read-failed? lookup)))
              outcomes (when (and job-kind? (not (:read-failed? lookup))) (decide rec job now-ms))]
         (cond-> (mapv
         (fn [type]
           (let [eid (outcome-id pid type)
                 same? (fn [entry] (and (= type (:evidence/type entry))
                                       (= pid (get-in entry [:evidence/body :promise-id]))
                                       (= criterion (get-in entry [:evidence/body :criterion]))))]
             (if-let [existing (store/get-entry* backend eid)]
               (do (when-not (same? existing) (throw (ex-info "Conflicting promise outcome" {:id eid})))
                   (swap! !stats update :existing inc)
                   {:status :existing :id eid})
               (let [entry {:evidence/id eid :evidence/type type :evidence/claim-type :observation
                            :evidence/subject {:ref/type :evidence :ref/id source-id}
                            :evidence/author (str (:agent rec))
                            :evidence/at (:observed-at observation)
                            :evidence/tags [:promise-outcome]
                            :evidence/body (merge (select-keys rec [:beneficiary :deadline])
                                                  {:promise-id pid :source-evidence-id source-id
                                                   :criterion criterion :job-observation observation
                                                   :outcome-basis (if (= type :promise/fulfilled)
                                                                    :criterion-true
                                                                    (if (= "done" (some-> (:state job) name))
                                                                      :finished-after-deadline :deadline-passed))
                                                   :criterion-true? (= "done" (some-> (:state job) name))})}
                     entry (cond-> entry (:session rec) (assoc :evidence/session-id (:session rec)))
                     receipt (boundary/append! backend (origin/stamp entry
                                                         (origin/harness "promise-outcome" pid)
                                                         "futon3c.agency.promise-outcome"))]
                 (cond
                   (:ok receipt) (do (swap! !stats update :written inc) {:status :written :id eid})
                   (and (= :duplicate-id (:error/code receipt))
                        (same? (store/get-entry* backend eid)))
                   (do (swap! !stats update :existing inc) {:status :existing :id eid})
                   :else (throw (ex-info "Promise outcome write failed" receipt)))))))
         outcomes)
           check (conj check)))))))

(defn evaluate!
  "Preserve P5's transition-triggered fulfilled/lapsed observations. Due-time
   checks are emitted only by evaluate-due! from the retained-history sweep."
  ([backend source-id rec now-ms]
   (evaluate! backend source-id rec now-ms nil))
  ([backend source-id rec now-ms source-at]
   (when (and (= :job-terminal-ok (get-in rec [:fulfilment-criterion :kind]))
              (true? (get-in rec [:fulfilment-criterion :machine-evaluable?])))
     (evaluate-observation! backend source-id rec now-ms source-at false))))

(defn evaluate-due!
  "Evaluate P5 outcomes and the single P8 due-time check from a creation row."
  ([backend source-id rec now-ms]
   (evaluate-due! backend source-id rec now-ms nil))
  ([backend source-id rec now-ms source-at]
   (evaluate-observation! backend source-id rec now-ms source-at true)))

(defn sweep!
  "Complete retained-history read, not a /tmp snapshot. Reject partial reads and
   pre-repair payloads rather than guessing missing promise fields. Type namespace
   pushdown is avoided (the evidence client currently drops type namespaces)."
  [backend now-ms]
  (let [entries (store/query* backend {:query/tags [:promise-history]})
        seen (volatile! #{})]
    (when (:partial? (meta entries)) (throw (ex-info "Incomplete promise outcome sweep" (meta entries))))
    (doseq [entry (sort-by (juxt :evidence/at :evidence/id) entries)
            :when (contains? #{:promise/park-made :promise/followup-enqueued} (:evidence/type entry))
            :let [b (:evidence/body entry)]
            :when (:fulfilment-criterion b)
            :let [pid (or (:id b) (:followup-id b) (get-in b [:history/promise-id]))]
            :when (or (nil? pid) (not (contains? @seen pid)))]
      (when pid (vswap! seen conj pid))
      (try
        (if-not (= 3 (:history/format b))
          (let [due-ms (or (millis (:deadline b)) (millis (:evidence/at entry)) now-ms)]
            (if (and (string? pid) (>= now-ms due-ms))
              (write-check! backend (:evidence/id entry)
                            (assoc b :id pid) now-ms (:evidence/at entry) nil false
                            {:verdict :unable-to-determine
                             :unable-reason :invalid-promise-record :due-ms due-ms})
              (throw (ex-info "Incomplete pre-repair promise" {:id (:evidence/id entry)}))))
          (let [rec (:record (edn/read-string (:history/payload-edn b)))]
            (evaluate-due! backend (:evidence/id entry) rec now-ms (:evidence/at entry))))
        (catch Throwable e (failed! e))))))
