(ns futon3c.agency.turn-prompt-delivery
  "Bound the cache-publication wait at the REPL terminal-event boundary.")

(defn await-ready!
  "Run WORKER asynchronously. WORKER receives an idempotent publish function.
   Return its first published prompt within TIMEOUT-MS, otherwise nil. Analysis
   seats do not start a worker because their terminal events carry no prompt."
  [{:keys [analysis-seat? timeout-ms worker]}]
  (when-not analysis-seat?
    (let [ready (promise)
          publish! #(deliver ready %)]
      (future
        (try
          (worker publish!)
          (catch Throwable _)
          (finally (publish! nil))))
      (let [value (deref ready (long timeout-ms) ::timeout)]
        (when (string? value) value)))))
