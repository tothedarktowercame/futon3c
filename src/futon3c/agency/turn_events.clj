(ns futon3c.agency.turn-events
  "Turn finalisation as a JVM event (Joe, 2026-10-17: everything event
   driven; Emacs already has a turn finalisation, so the JVM gets the hook).

   A turn is final when its finalising evidence row is appended:
     :reply         an agent's final reply segment (role assistant,
                    segment-final true; Emacs claude-repl writes it)
     :harness-turn  a Zai/Kimi turn's final turn-round row (zai_api writes
                    exactly one per turn, with :stop on the interrupted,
                    wall-clock and max-rounds exits)

   futon3c.evidence.boundary/append! raises :turn-final in two phases:
     :pre   synchronously, just before the row is stored. Observe only, and
            fast: a slow :pre listener delays the append. Errors are logged
            and swallowed; the append always proceeds.
     :post  asynchronously, on one daemon thread, after the row is stored.
            For real work (馬's observation, FUTON's view).

   The event: {:event :turn-final :phase :pre|:post :kind :reply|:harness-turn
               :session-id .. :author .. :evidence-id .. :turn-id .. :entry ..}"
  (:import [java.util.concurrent Executors ExecutorService ThreadFactory]))

(defonce ^:private !listeners (atom {}))

(defonce ^:private ^ExecutorService post-executor
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "turn-events-post") (.setDaemon true))))))

(defn add-listener!
  "Register keyed F for PHASE (:pre or :post). Re-registering replaces it."
  [key phase f]
  {:pre [(#{:pre :post} phase)]}
  (swap! !listeners assoc key {:phase phase :f f})
  nil)

(defn remove-listener! [key]
  (swap! !listeners dissoc key)
  nil)

(defn- field [m k]
  (or (get m (keyword "evidence" (name k))) (get m k) (get m (name k))))

(defn- body-field [body k]
  (when (map? body) (or (get body k) (get body (name k)))))

(defn final-turn
  "The :turn-final event for ENTRY (namespaced or unqualified keys), or nil
   when the row does not finalise a turn."
  [entry]
  (when (map? entry)
    (let [body (field entry :body)
          kind (cond
                 (and (= "assistant" (some-> (body-field body :role) name))
                      (#{true "true"} (body-field body :segment-final)))
                 :reply
                 (and (= "turn-round" (some-> (body-field body :event) name))
                      (#{true "true"} (body-field body :final)))
                 :harness-turn)]
      (when kind
        {:event :turn-final :kind kind
         :session-id (field entry :session-id)
         :author (field entry :author)
         :evidence-id (field entry :id)
         :turn-id (or (body-field body :turn-id) (body-field body :unified-turn-id))
         :entry entry}))))

(defn- run [phase event]
  (doseq [[k {p :phase f :f}] @!listeners
          :when (= p phase)]
    (try (f (assoc event :phase phase))
         (catch Throwable e
           (binding [*out* *err*]
             (println "[turn-events]" k phase "listener failed:" (.getMessage e)))))))

(defn pre!
  "Run :pre listeners for ENTRY if it finalises a turn. Never throws."
  [entry]
  (when-let [ev (final-turn entry)] (run :pre ev)))

(defn post!
  "Queue :post listeners for stored ENTRY if it finalises a turn."
  [entry]
  (when-let [ev (final-turn entry)]
    (.submit post-executor ^Runnable (fn [] (run :post ev)))
    true))
