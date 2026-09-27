(ns futon3c.agency.prompt-line
  "Bounded, inspectable composition of prompt-line segments."
  (:require [clojure.string :as str])
  (:import [java.time Instant]
           [java.util.concurrent Callable Executors TimeUnit TimeoutException ExecutionException]))

(def default-provider-budget-ms 100)
(def whole-render-budget-ms 250)

(defonce ^:private !providers (atom []))
(defonce ^:private !last-renders (atom {}))

(defn reset-registry!
  "Testing seam. Clears providers and remembered renders."
  []
  (reset! !providers [])
  (reset! !last-renders {}))

(defn providers [] @!providers)

(defn register-provider!
  [{:keys [segment/id provider fn budget-ms] :as registration}]
  (when-not (and (keyword? id) (not (str/blank? provider)) (ifn? fn))
    (throw (ex-info "Invalid prompt-line provider registration"
                    {:error/code :invalid-provider :registration registration})))
  (let [registration {:segment/id id :provider provider :fn fn
                      :budget-ms (long (or budget-ms default-provider-budget-ms))}]
    (loop []
      (let [before @!providers
            existing (some #(when (= id (:segment/id %)) %) before)]
        (cond
          (and existing (= provider (:provider existing)))
          (let [updated (mapv #(if (= id (:segment/id %)) registration %) before)]
            (if (compare-and-set! !providers before updated) registration (recur)))
          existing (throw (ex-info "Prompt-line segment already has a provider"
                                   {:error/code :duplicate-segment-provider
                                    :segment/id id
                                    :existing (:provider existing)
                                    :attempted provider}))
          (compare-and-set! !providers before (conj before registration)) registration
          :else (recur))))))

(defn- valid-segment?
  [registration segment]
  (let [id (:segment/id registration)
        marker (:segment/marker segment)
        pattern? (= :pattern id)]
    (and (map? segment)
         (= id (:segment/id segment))
         (= (:provider registration) (:segment/provider segment))
         (string? (:segment/observed-at segment))
         (map? (:segment/basis segment))
         (if pattern?
           (and (string? (:segment/value segment))
                (not (str/blank? (:segment/value segment)))
                (nil? marker))
           (and (nil? (:segment/value segment))
                (or (nil? marker)
                    (and (string? marker) (= 1 (count marker)))))))))

(defn- submit-provider
  [pool ctx registration]
  (let [submitted-at (System/nanoTime)]
    [submitted-at
     (.submit pool
              ^Callable
              (fn []
                (try
                  {:value ((:fn registration)
                           (assoc ctx :budget-ms (:budget-ms registration)))
                   :completed-at (System/nanoTime)}
                  (catch Throwable t
                    {:error t :completed-at (System/nanoTime)}))))]))

(defn render
  "Render with the supplied ordered provider registrations. No registry state is read
   or written. Provider work runs concurrently and is bounded by both deadlines."
  [ctx provider-registrations]
  (let [rendered-at (str (or (:render-at ctx) (Instant/now)))
        ctx (assoc ctx :render-at rendered-at)
        started (System/nanoTime)
        deadline (+ started (* whole-render-budget-ms 1000000))
        pool (Executors/newFixedThreadPool (max 1 (count provider-registrations)))
        jobs (mapv (fn [p]
                     (let [[submitted-at job] (submit-provider pool ctx p)]
                       [p submitted-at job]))
                   provider-registrations)]
    (try
      (let [{:keys [segments omitted]}
            (reduce
             (fn [acc [registration submitted-at job]]
               (let [remaining-ms (max 0 (quot (- deadline (System/nanoTime)) 1000000))
                     wait-ms (min (long (:budget-ms registration)) remaining-ms)
                     id (:segment/id registration)]
                 (if (zero? wait-ms)
                   (do (.cancel job true)
                       (update acc :omitted conj {:segment/id id :reason :timeout}))
                   (try
                     (let [{:keys [value error completed-at]}
                           (.get job wait-ms TimeUnit/MILLISECONDS)
                           provider-elapsed-ms (quot (- completed-at submitted-at) 1000000)]
                       (cond
                         (> provider-elapsed-ms (:budget-ms registration))
                         (update acc :omitted conj {:segment/id id :reason :timeout})
                         error (update acc :omitted conj {:segment/id id :reason :error})
                         (nil? value) (update acc :omitted conj {:segment/id id :reason :nil})
                         (not (valid-segment? registration value))
                         (update acc :omitted conj {:segment/id id :reason :invalid})
                         :else (update acc :segments conj
                                       (assoc value :segment/rendered-at rendered-at))))
                     (catch TimeoutException _
                       (.cancel job true)
                       (update acc :omitted conj {:segment/id id :reason :timeout}))
                     (catch ExecutionException _
                       (update acc :omitted conj {:segment/id id :reason :error}))
                     (catch InterruptedException _
                       (.interrupt (Thread/currentThread))
                       (update acc :omitted conj {:segment/id id :reason :error}))))))
             {:segments [] :omitted []}
             jobs)
            pattern-value (some #(when (= :pattern (:segment/id %))
                                   (:segment/value %)) segments)
            markers (apply str (keep #(when-not (= :pattern (:segment/id %))
                                        (:segment/marker %)) segments))
            prompt (if (and (nil? pattern-value) (str/blank? markers))
                     "> "
                     (str "$" (or pattern-value "") markers "> "))]
        {:prompt prompt :segments segments :omitted omitted :rendered-at rendered-at})
      (finally
        (doseq [[_ _ job] jobs] (when-not (.isDone job) (.cancel job true)))
        (.shutdownNow pool)))))

(defn render!
  [ctx]
  (let [agent (some-> (:agent-id ctx) str)
        session (some-> (:session-id ctx) str)]
    (when (or (str/blank? agent) (str/blank? session))
      (throw (ex-info "Prompt-line render needs exact agent and session"
                      {:error/code :missing-identity})))
    (let [result (render (assoc ctx :agent-id agent :session-id session) @!providers)]
      (swap! !last-renders assoc [agent session] result)
      result)))

(defn last-render [agent-id session-id]
  (get @!last-renders [(str agent-id) (str session-id)]))
