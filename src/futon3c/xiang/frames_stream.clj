(ns futon3c.xiang.frames-stream
  "SSE framing and cache watches for pushed 象 frame sets. No transport code."
  (:require [cheshire.core :as json]
            [futon3c.xiang.frames-cache :as frames-cache]))

(defn frames-payload
  "The public cached-frame response for SESSION, limited to its newest LIMIT."
  [session limit entry]
  (let [all (:frames entry)
        frames (if limit (vec (take-last limit all)) all)]
    (cond-> {:ok true :session session :limit limit
             :count (count frames) :frames frames
             :version (:version entry) :built-at (:built-at entry)
             :warnings (:warnings entry)}
      (:last-error entry) (assoc :last-error (:last-error entry)))))

(defn sse-event
  "Encode one named SSE event. JSON stays on exactly one data line."
  [event-name id data]
  (str "event: " event-name "\n"
       (when (some? id) (str "id: " id "\n"))
       "data: " (json/generate-string data) "\n\n"))

(defn keepalive [] ": keepalive\n\n")

(defn subscribe!
  "Watch SESSION in the frames cache and push frames/status SSE strings.
   Returns a zero-argument function that removes this subscription."
  [session {:keys [limit last-event-id send! on-missing]}]
  (let [key [::stream (str (java.util.UUID/randomUUID))]
        stopped? (atom false)
        unsubscribe (fn []
                      (when (compare-and-set! stopped? false true)
                        (remove-watch frames-cache/!cache key))
                      nil)
        safe-send! (fn [message]
                     (when-not @stopped?
                       (try (send! message)
                            true
                            (catch Throwable _
                              (unsubscribe)
                              false))))
        emit-entry! (fn [entry]
                      (safe-send! (sse-event "frames" (:version entry)
                                             (frames-payload session limit entry))))]
    (add-watch
     frames-cache/!cache key
     (fn [_ _ old new]
       (let [before (get old session)
             after (get new session)]
         (cond
           (and (:frames after) (not= (:version before) (:version after)))
           (emit-entry! after)

           (and (:last-error after)
                (not= (get-in before [:last-error :at])
                      (get-in after [:last-error :at])))
           (safe-send! (sse-event "status" nil
                                  {:state "rebuild-failed"
                                   :last-error (:last-error after)}))))))
    (let [entry (get @frames-cache/!cache session)]
      (if (:frames entry)
        (when (not= (:version entry) last-event-id)
          (emit-entry! entry))
        (when (safe-send! (sse-event "status" nil
                                     {:state "building" :session session}))
          (when on-missing (on-missing)))))
    unsubscribe))
