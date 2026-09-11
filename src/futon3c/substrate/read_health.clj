(ns futon3c.substrate.read-health
  "Read latency observations. Evidence acceptance remains the caller's job.")

(def warning-ms 5000)
(def hard-limit-ms 30000)
(def ^:dynamic *context* nil)
(def ^:dynamic *record-warning!* nil)
(def ^:dynamic *nano-time* #(System/nanoTime))

(defn timeout-ms [requested]
  (if *context* hard-limit-ms requested))

(defn observe!
  "Measure one actual HTTP read, including response decoding. Preserve its
  value or exception. A failed durable warning write fails closed. Never log
  response bodies or exception messages, which can contain private evidence."
  [identity read!]
  (let [start (*nano-time*)
        outcome (volatile! :failed)]
    (try
      (let [result (read!)]
        (vreset! outcome :returned)
        result)
      (finally
        (let [elapsed (quot (- (*nano-time*) start) 1000000)]
          (when (> elapsed warning-ms)
            (let [warning (merge *context* identity
                                 {:warning/type :slow-store-read
                                  :warning/at (str (java.time.Instant/now))
                                  :warning/threshold-ms warning-ms
                                  :elapsed-ms elapsed :read/outcome @outcome})]
              (when *record-warning!* (*record-warning!* warning))
              (println (str "[apm-store-read-warning] " (pr-str warning))))))))))
