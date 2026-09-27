(ns futon3c.watcher.write-pace
  "Process-wide minimum interval between watcher substrate writes.

  XTDB 2.1's embedded node opens a fresh loopback pgwire connection per query,
  and a single futon1b write runs several. On Windows (~16k dynamic ports,
  ~2-minute TIME_WAIT) an unpaced reindex or full-history commit backfill
  exhausts the port range and writes begin failing with
  `PSQLException: The connection attempt failed`. Pacing the write rate keeps
  the concurrent socket count bounded.

  FUTON3C_SUBSTRATE_WRITE_MIN_INTERVAL_MS sets the floor (ms) between writes;
  0 or unset = off (Joe's Linux behaviour, where TIME_WAIT reuse is not this
  constraint). This host sets 60 (~14 writes/s), which held TIME_WAIT at ~10k
  with zero write failures during the futon1a->futon1b reindex."
  (:require [clojure.string :as str]))

(def ^:private min-interval-ms
  (or (some-> (System/getenv "FUTON3C_SUBSTRATE_WRITE_MIN_INTERVAL_MS")
              str/trim
              not-empty
              (Long/parseLong))
      0))

(defonce ^:private !next-ok (atom 0))

(defn enabled? [] (pos? min-interval-ms))

(defn pace!
  "Block until at least min-interval-ms has elapsed since the previous paced
   write. No-op when the interval is 0. Thread-safe: the gate is serialised so
   concurrent watcher writers still honour the one global floor; the sleep
   happens OUTSIDE the lock so writers reserve a slot and wait without blocking
   each other's slot computation."
  []
  (when (pos? min-interval-ms)
    (let [wait (locking !next-ok
                 (let [now (System/currentTimeMillis)
                       t (max now @!next-ok)]
                   (reset! !next-ok (+ t min-interval-ms))
                   (- t now)))]
      (when (pos? wait)
        (Thread/sleep wait)))))
