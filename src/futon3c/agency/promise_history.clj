(ns futon3c.agency.promise-history
  "Best-effort transition history; /tmp park/followup stores remain authoritative.
   :evidence/at is the transition's event clock, NOT XTDB valid time: futon1b
   currently assigns valid/system time at insertion (P6). An ordered background
   writer prevents evidence outages from blocking parking or waking. Failed or
   process-lost writes leave history incomplete; authority/recovery is P2c's task."
  (:require [futon3c.agency.promise-record :as promise]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend])
  (:import [futon3c.evidence.futon1b_backend Futon1bBackend]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent ThreadPoolExecutor TimeUnit LinkedBlockingQueue ThreadFactory]))

(def ^:dynamic *backend* nil)
(defonce ^:private writer-id (str (UUID/randomUUID)))
(defonce ^:private !counts (atom {:submitted 0 :written 0 :failed 0}))
(defonce ^:private writer
  (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (LinkedBlockingQueue. 1024)
                       (reify ThreadFactory
                         (newThread [_ task]
                           (doto (Thread. task "promise-history") (.setDaemon true))))))

(defn stats "Observable process-local write/failure counts (not durable)." [] @!counts)

(defn- failed! [type detail]
  (swap! !counts update :failed inc)
  (binding [*out* *err*]
    (println "promise-history write failed:" type (str detail))))

(defn- backend []
  (or *backend*
      (when-let [v (some-> (find-ns 'futon3c.dev) (ns-resolve '!evidence-store))]
        (let [candidate @(var-get v)]
          (when (instance? Futon1bBackend candidate) candidate)))
      (throw (ex-info "Evidence backend unavailable" {}))))

(defn record!
  "Submit exactly one uniquely identified transition. NOW-MS belongs to the
   transition, even if the writer runs much later. Writer id + sequence preserve
   submission order when event timestamps tie. No fulfilment is inferred."
  ([type rec now-ms] (record! type rec now-ms {}))
  ([type rec now-ms details]
   (let [sequence-number (:submitted (swap! !counts update :submitted inc))]
    (try
     (let [entry (cond->
                  {:evidence/id (str "e-" (UUID/randomUUID))
                   :evidence/type type :evidence/claim-type :step
                   :evidence/subject {:ref/type :agent :ref/id (str (:agent rec))}
                   :evidence/author (str (:agent rec))
                   :evidence/at (str (Instant/ofEpochMilli now-ms))
                   :evidence/tags [:promise-history]
                   :evidence/body (merge (select-keys rec (into [:id :followup-id :agent :session]
                                                               promise/field-keys))
                                         {:history/writer-id writer-id :history/sequence sequence-number
                                          :awaiting (vec (sort (into (set (:awaiting rec))
                                                                    (keys (:arrived rec)))))}
                                         details)}
                   (:session rec) (assoc :evidence/session-id (:session rec)))]
       (.execute writer
                 ^Runnable
                 (bound-fn []
                   (try
                     (let [result (boundary/append! (backend) entry)]
                       (if (:ok result)
                         (swap! !counts update :written inc)
                         (failed! type (:error/code result))))
                     (catch Throwable e (failed! type (.getMessage e)))))))
     (catch Throwable e (failed! type (.getMessage e)))))))

(defn await-writes!
  "Wait up to TIMEOUT-MS for previously submitted writes; diagnostics/tests only."
  [timeout-ms]
  (let [done (promise)]
    (.execute writer ^Runnable #(deliver done true))
    (= true (deref done timeout-ms false))))
