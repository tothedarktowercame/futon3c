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
  "Drop sessions not read within MAX-IDLE-MS (24 hours by default)."
  ([] (evict-idle! (* 24 60 60 1000)))
  ([max-idle-ms]
   (let [cutoff (- (System/currentTimeMillis) max-idle-ms)]
     (swap! !cache (fn [entries]
                     (into {} (remove (fn [[_ entry]]
                                        (and (not (:building entry))
                                             (< (or (:last-read-at entry) 0) cutoff))))
                           entries))))
   nil))

(defn- run-build [session opts previous]
  (let [started (now-ms opts)
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

(defn- rebuild! [session opts]
  (let [before (locking !cache
                 (when-let [entry (get @!cache session)]
                   (swap! !cache assoc session
                          (assoc entry :pending? false :rebuilding? true :dirty? false))
                   entry))]
    (when before
      (let [built (run-build session opts before)
            again? (locking !cache
                     (let [current (get @!cache session)
                           dirty? (:dirty? current)
                           ;; Preserve read time and invalidations that landed while
                           ;; the runner was outside the lock.
                           merged (-> built
                                      (assoc :last-read-at (:last-read-at current)
                                             :rebuilding? false :dirty? false
                                             :pending? false))]
                       (swap! !cache assoc session merged)
                       dirty?))]
        (when again? (queue-rebuild! session opts))))))

(defn- queue-rebuild! [session opts]
  (let [queue? (locking !cache
                 (when-let [entry (get @!cache session)]
                   (cond
                     (:rebuilding? entry)
                     (do (swap! !cache assoc-in [session :dirty?] true) false)

                     (:pending? entry) false

                     :else
                     (do (swap! !cache assoc session (assoc entry :pending? true)) true))))]
    (when queue?
      (schedule! opts (or (:debounce-ms opts) 2000) #(rebuild! session opts)))
    queue?))

(defn frames!
  "Return SESSION's cache entry, synchronously building it when absent.
   Concurrent first readers share the promise installed under :building."
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
      (let [previous (merge {:version 0 :last-read-at at :pending? false :rebuilding? false}
                            (dissoc (get @!cache session) :building))
            built (run-build session opts previous)
            success? (contains? built :frames)
            entry (dissoc built :building)]
        (locking !cache (swap! !cache assoc session entry))
        (deliver building entry)
        (assoc entry :ok success? :cached false))

      :else
      (let [entry @building]
        (swap! !cache assoc-in [session :last-read-at] at)
        (assoc entry :ok (contains? entry :frames) :cached true :last-read-at at)))))

(defn invalidate!
  "Schedule a coalesced rebuild for an observed SESSION; unseen sessions cost nothing."
  [session opts]
  (evict-idle! (or (:max-idle-ms opts) (* 24 60 60 1000)))
  (boolean (queue-rebuild! session opts)))
