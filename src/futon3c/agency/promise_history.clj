(ns futon3c.agency.promise-history
  "Best-effort transition history; /tmp park/followup stores remain authoritative."
  (:require [futon3c.evidence.origin :as origin] [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.agency.promise-capture :as capture]
            [futon3c.agency.promise-outcome :as outcome]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as estore]
            [futon3c.evidence.futon1b-backend])
  (:import [futon3c.evidence.futon1b_backend Futon1bBackend]
           [java.time Instant]
           [java.nio.file Files StandardCopyOption CopyOption]
           [java.util UUID]
           [java.util.concurrent ThreadPoolExecutor TimeUnit LinkedBlockingQueue ThreadFactory]))

(def ^:dynamic *backend* nil)
(defonce ^:private writer-id (str (UUID/randomUUID)))
(defonce ^:private !counts (atom {:submitted 0 :written 0 :failed 0
                                  :pending 0 :drained 0 :conflict 0}))
(defonce ^:private !failed-outbox-ids (atom #{}))
(defonce ^:private writer
  (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (LinkedBlockingQueue. 1024)
                       (reify ThreadFactory
                         (newThread [_ task]
                           (doto (Thread. task "promise-history") (.setDaemon true))))))

(defn stats [] (assoc @!counts :outcomes (outcome/stats)))
(defn- failed! [type detail]
  (swap! !counts update :failed inc)
  (binding [*out* *err*] (println "promise-history write failed:" type (str detail))))
(defn- outbox-failed! [eid type detail]
  (when-not (contains? (first (swap-vals! !failed-outbox-ids conj eid)) eid)
    (failed! type detail)))
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
(defn- heads! []
  (or *heads*
      (do (when (nil? @!chain-cache)
            (reset! !chain-cache
                    (if (.exists (io/file (chain-path)))
                      (edn/read-string (slurp (chain-path))) {})))
          !chain-cache)))
(defn- persist-heads! [heads]
  (when-not *heads*
    (let [target (.toAbsolutePath (.toPath (io/file (chain-path))))
          tmp (Files/createTempFile (.getParent target) "promise-chains-" ".edn"
                                    (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (spit (.toFile tmp) (pr-str @heads))
        (Files/move tmp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                       StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists tmp))))))
(defn- history-id [promise-id sequence]
  (str "promise-history:"
       (UUID/nameUUIDFromBytes (.getBytes (pr-str [promise-id sequence]) "UTF-8"))))
(defn- next-link! [promise-id type persist?]
  (locking !chain-cache
    (let [heads (heads!) previous (get @heads promise-id)
          sequence (inc (or (:sequence previous) 0))
          eid (history-id promise-id sequence)
          head {:sequence sequence :id eid :type type}]
      (swap! heads assoc promise-id head)
      (when persist? (persist-heads! heads))
      {:eid eid :history/promise-id promise-id :history/promise-sequence sequence
       :history/predecessor previous})))
(defn- prepare-entry! [type rec now-ms details persist-head?]
  (let [submission (:submitted (swap! !counts update :submitted inc))
        promise-id (or (:id rec) (:followup-id rec) (:park-id rec))
        changes (capture/drain!)
        {:keys [eid] :as allocated} (next-link! promise-id type persist-head?)
        link (dissoc allocated :eid)]
    (origin/stamp
     (cond->
      {:evidence/id eid :evidence/type type :evidence/claim-type :step
       :evidence/subject {:ref/type :agent :ref/id (str (:agent rec))}
       :evidence/author (str (:agent rec))
       :evidence/at (str (Instant/ofEpochMilli now-ms))
       :evidence/tags [:promise-history]
       :evidence/body
       (merge rec
              {:history/format 3
               :history/payload-edn
               (pr-str {:record rec :changes changes
                        :predecessor (:history/predecessor link)})
               :history/writer-id writer-id :history/sequence submission
               :awaiting (vec (sort (into (set (:awaiting rec))
                                         (keys (:arrived rec)))))}
              details link)}
      (:session rec) (assoc :evidence/session-id (:session rec)))
     (origin/harness "promise-history" promise-id)
     "futon3c.agency.promise-history")))

(defn record!
  "Submit one old-path transition through the ordered background writer."
  ([type rec now-ms] (record! type rec now-ms {}))
  ([type rec now-ms details]
   (try
     (let [entry (prepare-entry! type rec now-ms details true)
           eid (:evidence/id entry)
           ]
       (.execute writer
                 ^Runnable
                 (bound-fn []
                   (try
                     (let [result (boundary/append! (backend) entry)]
                       (if (:ok result)
                         (do (swap! !counts update :written inc)
                             (try (outcome/evaluate! (backend) eid rec (System/currentTimeMillis))
                                  (catch Throwable e (outcome/failed! e))))
                         (failed! type (:error/code result))))
                     (catch Throwable e (failed! type (.getMessage e))))))
       eid)
     (catch Throwable e (failed! type (.getMessage e))))))

(def ^:dynamic *after-outbox-persist* nil)
(def ^:dynamic *after-outbox-append* nil)
(defn stage!
  "Prepare one fixed row in STATE. Caller persists STATE before drain!."
  ([state type rec now-ms] (stage! state type rec now-ms {}))
  ([state type rec now-ms details]
   (let [entry (prepare-entry! type rec now-ms details false)
         eid (:evidence/id entry)]
     (swap! state assoc-in [:history-outbox eid] entry)
     (swap! !counts update :pending inc)
     eid)))
(defn register-pending! [entries]
  (locking !chain-cache
    (let [heads (heads!)]
      (swap! !counts update :pending max (count entries))
      (doseq [entry (sort-by #(get-in % [:evidence/body :history/promise-sequence]) entries)
              :let [pid (get-in entry [:evidence/body :history/promise-id])
                    head {:sequence (get-in entry [:evidence/body :history/promise-sequence])
                          :id (:evidence/id entry) :type (:evidence/type entry)}]
              :when (> (:sequence head) (or (:sequence (get @heads pid)) 0))]
        (swap! heads assoc pid head)))))
(defn allocator-snapshot [] @(heads!))
(defn restore-allocator! [snapshot]
  (locking !chain-cache (reset! (heads!) snapshot)))
(defn- same-history-row? [a b]
  (and (= (select-keys a [:evidence/id :evidence/type :evidence/at :evidence/author
                          :evidence/session-id])
          (select-keys b [:evidence/id :evidence/type :evidence/at :evidence/author
                          :evidence/session-id]))
       (= (select-keys (:evidence/body a)
                       [:history/promise-id :history/promise-sequence
                        :history/predecessor :history/payload-edn])
          (select-keys (:evidence/body b)
                       [:history/promise-id :history/promise-sequence
                        :history/predecessor :history/payload-edn]))))
(defn drain-now! [state persist!]
  (register-pending! (vals (:history-outbox @state)))
  (doseq [[eid entry] (sort-by #(get-in (val %) [:evidence/body :history/promise-sequence])
                               (:history-outbox @state))]
    (let [result (boundary/append! (backend) entry)
          existing (when (= :duplicate-id (:error/code result))
                     (estore/get-entry* (backend) eid))]
      (cond
        (or (:ok result) (and existing (same-history-row? entry existing)))
        (do (swap! !counts update :written inc)
            (when *after-outbox-append* (*after-outbox-append* entry))
            (locking !chain-cache (persist-heads! (heads!)))
            (swap! state update :history-outbox dissoc eid)
            (persist! @state)
            (swap! !counts #(-> % (update :drained inc)
                                  (update :pending (fn [n] (max 0 (dec n))))))
            (try
              (let [rec (:record (edn/read-string
                                  (get-in entry [:evidence/body :history/payload-edn])))]
                (outcome/evaluate! (backend) eid rec (System/currentTimeMillis)))
              (catch Throwable e (outcome/failed! e))))
        (= :duplicate-id (:error/code result)) (swap! !counts update :conflict inc)
        :else (outbox-failed! eid (:evidence/type entry) (:error/code result))))))
(defn drain! [state persist!]
  (.execute writer ^Runnable (bound-fn [] (try (drain-now! state persist!)
                                               (catch Throwable e
                                                 (failed! :outbox (.getMessage e)))))))

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

(defn- check-chains*
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

(defn check-chains
  "Check persisted chains. With PENDING outbox rows, a predecessor awaiting
   drain is reported as :pending rather than as a missing transition."
  ([entries] (check-chains* entries))
  ([entries pending]
   (let [pending-ids (set (map :evidence/id pending))]
     (mapv (fn [issue]
             (if (and (= :missing-transition (:reason issue))
                      (contains? pending-ids (:predecessor-id issue)))
               (assoc issue :reason :pending)
               issue))
           (check-chains* entries)))))

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
