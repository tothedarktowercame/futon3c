(ns futon3c.xiang.frames-cache
  "Read-on-change cache for the browser's assembled 象 turn frames."
  (:refer-clojure :exclude [reset!]))

(defonce !cache (atom {}))

(defn- now-ms [opts]
  ((or (:now-ms opts) #(System/currentTimeMillis))))

(defn- iso [ms]
  (str (java.time.Instant/ofEpochMilli (long ms))))

(defn- schedule! [opts delay-ms thunk]
  ((or (:schedule! opts)
       (fn [delay f] (future (when (pos? delay) (Thread/sleep delay)) (f))))
   delay-ms thunk))

(defn reset!
  "Clear all cached sessions. Intended for tests and operator maintenance."
  []
  (clojure.core/reset! !cache {})
  nil)

(defn evict-idle!
  "Drop sessions not read within MAX-IDLE-MS (24 hours by default). A session
   whose first build or rebuild is in flight is kept."
  ([] (evict-idle! (* 24 60 60 1000)))
  ([max-idle-ms]
   (let [cutoff (- (System/currentTimeMillis) max-idle-ms)]
     (swap! !cache (fn [entries]
                     (into {} (remove (fn [[_ entry]]
                                        (and (not (:building entry))
                                             (not (:rebuilding? entry))
                                             (< (or (:last-read-at entry) 0) cutoff))))
                           entries))))
   nil))

(defn- run-build
  "Run the runner once for SESSION and fold the result into PREVIOUS. The
   in-flight marks (:building, :rebuilding?, :pending?, :dirty?) belong to the
   atom, not to the result, so they are never carried through here."
  [session opts previous]
  (let [previous (dissoc previous :building :rebuilding? :pending? :dirty?)
        started (now-ms opts)
        result (try ((:runner opts) session)
                    (catch Throwable t {:ok false :reason :exception
                                        :err (.getMessage t)}))
        finished (now-ms opts)]
    (if (:ok result)
      (-> previous
          (assoc :frames (vec (:frames result))
                 :count (count (:frames result))
                 :version (inc (or (:version previous) 0))
                 :built-at (iso finished)
                 :build-ms (max 0 (- finished started))
                 :warnings (:warnings result)
                 :last-error nil))
      (assoc previous :last-error
             (assoc (select-keys result [:reason :err :exit :timeout-s :stderr])
                    :at (iso finished))))))

(declare queue-rebuild!)

(defn- settle-build!
  "Under the lock, replace SESSION's entry with BUILT while keeping whatever
   landed in the atom during the build: the read time, a first-build promise
   another thread installed, and the dirty mark. Returns true when an
   invalidation arrived during the build, so the caller queues one more."
  [session built]
  (locking !cache
    (let [current (get @!cache session)
          merged (cond-> (assoc built
                                :last-read-at (max (or (:last-read-at current) 0)
                                                   (or (:last-read-at built) 0))
                                :rebuilding? false :dirty? false :pending? false)
                   (:building current) (assoc :building (:building current)))]
      (swap! !cache assoc session merged)
      (boolean (:dirty? current)))))

(defn- rebuild! [session opts]
  (let [before (locking !cache
                 (when-let [entry (get @!cache session)]
                   (swap! !cache assoc session
                          (assoc entry :pending? false :rebuilding? true :dirty? false))
                   entry))]
    (when before
      (let [built (run-build session opts before)]
        (when (settle-build! session built)
          (queue-rebuild! session opts))))))

(defn- queue-rebuild! [session opts]
  (let [queue? (locking !cache
                 (when-let [entry (get @!cache session)]
                   (cond
                     ;; A build is running (first build or rebuild): mark it
                     ;; dirty and let that build queue the follow-up when it
                     ;; settles. Scheduling now would run two scripts at once
                     ;; and the later result could be the staler one.
                     (or (:building entry) (:rebuilding? entry))
                     (do (swap! !cache assoc-in [session :dirty?] true) false)

                     (:pending? entry) false

                     :else
                     (do (swap! !cache assoc session (assoc entry :pending? true)) true))))]
    (when queue?
      (schedule! opts (or (:debounce-ms opts) 2000) #(rebuild! session opts)))
    queue?))

(defn frames!
  "Return SESSION's cache entry, synchronously building it when absent.
   Concurrent first readers share the promise installed under :building.
   :cached is true only when the answer came from memory; a reader that
   waited for a build reports false, like the reader that ran it."
  [session opts]
  (let [at (now-ms opts)
        [owner? building cached]
        (locking !cache
          (if-let [entry (get @!cache session)]
            (if-let [p (:building entry)]
              [false p nil]
              (if (contains? entry :frames)
                (do (swap! !cache assoc-in [session :last-read-at] at)
                    [false nil entry])
                (let [p (promise)]
                  (swap! !cache assoc session (assoc entry :building p :last-read-at at))
                  [true p nil])))
            (let [p (promise)]
              (swap! !cache assoc session {:building p :last-read-at at
                                            :pending? false :rebuilding? false})
              [true p nil])))]
    (cond
      cached (assoc cached :ok true :cached true :last-read-at at)

      owner?
      (let [previous (merge {:version 0 :last-read-at at} (get @!cache session))
            built (run-build session opts previous)
            success? (contains? built :frames)
            again? (locking !cache
                     (let [current (get @!cache session)
                           entry (-> built
                                     (assoc :last-read-at (max at (or (:last-read-at current) 0))
                                            :pending? false :rebuilding? false :dirty? false))]
                       (swap! !cache assoc session entry)
                       (boolean (:dirty? current))))
            entry (get @!cache session)]
        (deliver building entry)
        (when again? (queue-rebuild! session opts))
        (assoc entry :ok success? :cached false))

      :else
      (let [entry @building]
        (swap! !cache assoc-in [session :last-read-at] at)
        (assoc entry :ok (contains? entry :frames) :cached false :last-read-at at)))))

(defn invalidate!
  "Schedule a coalesced rebuild for an observed SESSION; unseen sessions cost nothing."
  [session opts]
  (evict-idle! (or (:max-idle-ms opts) (* 24 60 60 1000)))
  (boolean (queue-rebuild! session opts)))
