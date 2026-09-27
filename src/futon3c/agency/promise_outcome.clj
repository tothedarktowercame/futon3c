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

(defn evaluate!
  "Check one retained promise record against a job-ledger observation. Outcome
   records carry all the job fields the evaluator read and the source record id.
   No outcome is inferred from a dependency payload or from the fact of waking."
  [backend source-id rec now-ms]
  (let [criterion (:fulfilment-criterion rec)]
    (when (and (= :job-terminal-ok (:kind criterion))
               (true? (:machine-evaluable? criterion)))
      (swap! !stats update :evaluated inc)
      (let [pid (or (:id rec) (:followup-id rec))
            job (*lookup-job* (:job-id criterion))
            observation (assoc (select-keys job [:state :finished-at :terminal-code])
                               :job-id (:job-id criterion) :observed-at (str (Instant/ofEpochMilli now-ms)))]
        (when-not (string? pid) (throw (ex-info "Missing promise id" {:record rec})))
        (mapv
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
         (decide rec job now-ms))))))

(defn sweep!
  "Complete retained-history read, not a /tmp snapshot. Reject partial reads and
   pre-repair payloads rather than guessing missing promise fields. Type namespace
   pushdown is avoided (the evidence client currently drops type namespaces)."
  [backend now-ms]
  (let [entries (store/query* backend {:query/tags [:promise-history]})]
    (when (:partial? (meta entries)) (throw (ex-info "Incomplete promise outcome sweep" (meta entries))))
    (doseq [entry entries
            :when (contains? #{:promise/park-made :promise/followup-enqueued} (:evidence/type entry))
            :let [b (:evidence/body entry)]
            :when (:fulfilment-criterion b)]
      (try
        (when-not (= 3 (:history/format b)) (throw (ex-info "Incomplete pre-repair promise" {:id (:evidence/id entry)})))
        (let [rec (:record (edn/read-string (:history/payload-edn b)))]
          (evaluate! backend (:evidence/id entry) rec now-ms))
        (catch Throwable e (failed! e))))))
