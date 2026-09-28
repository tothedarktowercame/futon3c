(ns futon3c.agency.promise-history
  "Best-effort transition history; /tmp park/followup stores remain authoritative.
   :evidence/at is the transition's event clock, NOT XTDB valid time: futon1b
   currently assigns valid/system time at insertion (P6). An ordered background
   writer prevents evidence outages from blocking parking or waking. Failed or
   process-lost writes leave history incomplete; authority/recovery is P2c's task."
  (:require [futon3c.evidence.origin :as origin] [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.agency.promise-capture :as capture]
            [futon3c.agency.promise-outcome :as outcome]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend])
  (:import [futon3c.evidence.futon1b_backend Futon1bBackend]
           [java.time Instant]
           [java.nio.file Files StandardCopyOption CopyOption]
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

(defn stats "Observable process-local write/failure counts (not durable)." []
  (assoc @!counts :outcomes (outcome/stats)))

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

(def ^:dynamic *heads* nil)
(defonce ^:private !chain-cache (atom nil))

(defn- chain-path []
  (or (System/getenv "FUTON3C_PROMISE_HISTORY_CHAINS_PATH")
      "/tmp/futon3c-promise-history-chains.edn"))

(defn- next-link! [promise-id eid type]
  ;; This sidecar is sequence allocation metadata only, never park authority.
  ;; Persist before enqueue. Failed writes therefore leave detectable gaps. It is
  ;; not an atomic commit with either /tmp store or XTDB; that protocol is P2c.
  (locking !chain-cache
    (let [path (chain-path)
          heads (or *heads*
                    (do (when (nil? @!chain-cache)
                          (reset! !chain-cache
                                  (if (.exists (io/file path))
                                    (edn/read-string (slurp path)) {})))
                        !chain-cache))
          previous (get @heads promise-id)
          head {:sequence (inc (or (:sequence previous) 0)) :id eid :type type}]
      (swap! heads assoc promise-id head)
      (when-not *heads*
        (let [target (.toAbsolutePath (.toPath (io/file path)))
              tmp (Files/createTempFile (.getParent target) "promise-chains-" ".edn"
                                        (make-array java.nio.file.attribute.FileAttribute 0))]
          (try
            (spit (.toFile tmp) (pr-str @heads))
            (Files/move tmp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                          StandardCopyOption/REPLACE_EXISTING]))
            (finally (Files/deleteIfExists tmp)))))
      {:history/promise-id promise-id :history/promise-sequence (:sequence head)
       :history/predecessor previous})))

(defn record!
  "Submit exactly one uniquely identified transition. NOW-MS belongs to the
   transition, even if the writer runs much later. Writer id + sequence preserve
   submission order when event timestamps tie. No fulfilment is inferred."
  ([type rec now-ms] (record! type rec now-ms {}))
  ([type rec now-ms details]
   (let [sequence-number (:submitted (swap! !counts update :submitted inc))]
    (try
     (let [eid (str "e-" (UUID/randomUUID))
           promise-id (or (:id rec) (:followup-id rec) (:park-id rec))
           changes (capture/drain!)
           link (next-link! promise-id eid type)
           entry (cond->
                  {:evidence/id eid
                   :evidence/type type :evidence/claim-type :step
                   :evidence/subject {:ref/type :agent :ref/id (str (:agent rec))}
                   :evidence/author (str (:agent rec))
                   :evidence/at (str (Instant/ofEpochMilli now-ms))
                   :evidence/tags [:promise-history]
                   :evidence/body (merge rec
                                         {:history/format 3
                                          ;; XTDB's document representation elides nil map
                                          ;; values. The EDN wire payload preserves absence
                                          ;; versus explicit nil and arbitrary snapshot keys.
                                          :history/payload-edn (pr-str {:record rec :changes changes
                                                                       :predecessor (:history/predecessor link)})
                                          :history/writer-id writer-id :history/sequence sequence-number
                                          :awaiting (vec (sort (into (set (:awaiting rec))
                                                                    (keys (:arrived rec)))))}
                                         details link)}
                   (:session rec) (assoc :evidence/session-id (:session rec)))]
       (.execute writer
                 ^Runnable
                 (bound-fn []
                   (try
                     (let [result (boundary/append! (backend)
                                                     (origin/stamp entry
                                                       (origin/harness "promise-history" promise-id)
                                                       "futon3c.agency.promise-history"))]
                       (if (:ok result)
                         (do (swap! !counts update :written inc)
                             ;; Criterion observations do not consume snapshot edits or
                             ;; promise-chain sequence numbers. Failure stays best-effort.
                             (try (outcome/evaluate! (backend) eid rec (System/currentTimeMillis))
                                  (catch Throwable e (outcome/failed! e))))
                         (failed! type (:error/code result))))
                     (catch Throwable e (failed! type (.getMessage e))))))
       eid)
     (catch Throwable e (failed! type (.getMessage e)))))))

(defn await-writes!
  "Wait up to TIMEOUT-MS for previously submitted writes; diagnostics/tests only."
  [timeout-ms]
  (let [done (promise)]
    (.execute writer ^Runnable #(deliver done true))
    (= true (deref done timeout-ms false))))


(defn capture!
  "Observe authoritative mutations; nested same-store calls share an ordered edit
   buffer. A semantic record consumes edits since its predecessor. Remaining edits
   get an explicit maintenance record (load/clear), never silently disappear."
  [store f]
  (if (= store (:store capture/*capture*))
    (f)
    (binding [capture/*capture* {:store store :changes (atom [])}]
      (try (f)
           (finally
             (when (seq @(:changes capture/*capture*))
               (record! (case store :parked :promise/park-store-changed
                                   :followup :promise/followup-store-changed)
                        {:id (str "store:" (name store)) :agent "promise-history"}
                        (System/currentTimeMillis))))))))

(defn payload
  "Decode the lossless replay payload. Top-level body fields are a query projection,
   not replay input: XTDB elides nil map entries there, even over the EDN API.
   Formats before 3 cannot prove exact snapshot coverage and are refused."
  [entry]
  (let [b (:evidence/body entry)]
    (when-not (and (= 3 (:history/format b)) (string? (:history/payload-edn b)))
      (throw (ex-info "Incomplete pre-repair history" {:reason :incomplete-pre-repair-history
                                                       :id (:evidence/id entry)})))
    (edn/read-string (:history/payload-edn b))))

(defn check-chains
  "Pure reader gate. Pre-repair rows are incomplete, never upgraded by inference.
   Predecessors name missing transitions even when timestamps are identical."
  [entries]
  (vec
   (mapcat
    (fn [[pid rows]]
      (let [by-id (into {} (map (juxt :evidence/id identity)) rows)]
        (mapcat
         (fn [row]
           (let [b (:evidence/body row)
                 n (:history/promise-sequence b)
                 prev (:history/predecessor b)]
             (cond
               (or (not= 3 (:history/format b)) (not (pos-int? n)))
               [{:promise-id pid :reason :incomplete-pre-repair-history :id (:evidence/id row)}]
               (and (= n 1) (not (contains? #{:promise/park-made :promise/followup-enqueued
                                                          :promise/park-store-changed :promise/followup-store-changed}
                                                        (:evidence/type row))))
               [{:promise-id pid :reason :missing-origin :sequence 1}]
               (and (= n 1) prev)
               [{:promise-id pid :reason :invalid-predecessor :sequence n}]
               (> n 1)
               (let [prior (get by-id (:id prev))]
                 (cond
                   (not= (dec n) (:sequence prev))
                   [{:promise-id pid :reason :invalid-predecessor :sequence n}]
                   (nil? prior)
                   [{:promise-id pid :reason :missing-transition :sequence (:sequence prev)
                     :predecessor-id (:id prev) :predecessor-type (:type prev)}]
                   (or (not= (:type prev) (:evidence/type prior))
                       (not= (:sequence prev) (get-in prior [:evidence/body :history/promise-sequence])))
                   [{:promise-id pid :reason :predecessor-mismatch :sequence n}]
                   :else []))
               :else []))) rows)))
    (group-by #(or (get-in % [:evidence/body :history/promise-id])
                   (get-in % [:evidence/body :id])
                   (get-in % [:evidence/body :followup-id])) entries))))

(defonce ^:private !outcome-sweep-pending (atom false))
(defonce ^:private !outcome-sweep-last-ms (atom 0))
(def outcome-sweep-min-interval-ms
  "A sweep reads the whole promise history (~20 s per read on 2026-09-27). Queued
   from the 30 s park timer it held one of futon1b's four query permits most of the
   time, and appends elsewhere hit 504 permit timeouts. Lapse is judged against the
   deadline itself, so a slower sweep only delays when it is noticed."
  (* 5 60 1000))
(defn sweep-outcomes!
  "Queue at most one deadline scan on the ordered background writer, and at most
   one per outcome-sweep-min-interval-ms. Never block the park timer on evidence IO;
   history remains the source after cache deletion."
  []
  (when (and (>= (- (System/currentTimeMillis) @!outcome-sweep-last-ms)
                 outcome-sweep-min-interval-ms)
             (compare-and-set! !outcome-sweep-pending false true))
    (reset! !outcome-sweep-last-ms (System/currentTimeMillis))
    (try
      (.execute writer ^Runnable
                (bound-fn []
                  (try (outcome/sweep! (backend) (System/currentTimeMillis))
                       (catch Throwable e (outcome/failed! e))
                       (finally (reset! !outcome-sweep-pending false)))))
      (catch Throwable e
        (reset! !outcome-sweep-pending false)
        (outcome/failed! e)))))
