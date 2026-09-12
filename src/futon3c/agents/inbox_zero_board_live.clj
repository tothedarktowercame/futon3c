(ns futon3c.agents.inbox-zero-board-live
  "The live wiring for the inbox-zero board (SPEC-chip-boards-v0.md §4
  task 0). Pure aggregation from the futon3.inbox-zero state records to the
  board's observation packet, plus a runner that loads real state, runs one
  board cycle, and prints the runtime certificate.

  Channels (NOTE-inbox-zero-aif.md):
  - SMELL: file-observation records from storage/inbox-zero/state.edn
    (typed records, watcher-sourced);
  - the in-flight channel (prediction 3): recent watcher activity. A repo
    with any file-observation inside the liveness window (1h default) is
    in-flight — someone is editing now — and is never ZAPped (U59). This is
    derived from the same typed records, so it is auditable and needs no
    second service. A session-seat channel can refine it later.

  The aggregation is pure and unit-tested; only run-live!/-main do I/O."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agents.inbox-zero-board :as board])
  (:import [java.time Instant Duration]))

(def ignored-by-design?
  "Paths the clean definition already excludes via .gitignore: build
  output and runtime state. Explicit here so the board's smell channel
  matches the sweeper's, not a silent filter."
  (fn [path]
    (or (str/starts-with? path "target/")
        (str/starts-with? path "data/")
        (str/ends-with? path ".class")
        (str/starts-with? path ".#"))))

(defn- age-hours
  [observed-at ^Instant now]
  (let [^Instant t (cond
                     (instance? java.util.Date observed-at)
                     (.toInstant ^java.util.Date observed-at)
                     (instance? Instant observed-at)
                     observed-at
                     :else (throw (ex-info "untyped observed-at"
                                           {:observed-at observed-at})))]
    (/ (.toMillis (Duration/between t now)) 3600000.0)))

(defn file-observations
  [records]
  (filter #(= (:record/type %) :inbox-zero/file-observation) (vals records)))

(defn sweep-from-records
  "Aggregate parsed state records into per-repo sweep rows:
  {:repo <root basename> :clean? bool :clauses-failed [...]}. Clause (one
  channel, README definition): :dirty-older-than-24h — a non-ignored
  modified/untracked observation older than 24h at NOW."
  ([records] (sweep-from-records records (Instant/now)))
  ([records ^Instant now]
   (let [by-repo (group-by :repo/root (file-observations records))]
     (mapv (fn [[root rows]]
             (let [dirty (->> rows
                              (filter #(contains? #{:modified :untracked}
                                                  (:git/status %)))
                              (filter #(not (ignored-by-design? (:path %))))
                              (filter #(> (age-hours ^Instant (:observed-at %) now)
                                          24.0)))
                   clauses (cond-> []
                             (seq dirty) (conj :dirty-older-than-24h))]
               {:repo (.getName (io/file root))
                :clean? (empty? clauses)
                :clauses-failed clauses}))
           by-repo))))

(defn in-flight-from-records
  "Repos with watcher activity inside the liveness window — the explicit
  in-flight-turn channel (NOTE-inbox-zero-aif prediction 3). Conservative
  by design: a false in-flight costs one deferred sweep; a false idle
  costs the U59 incident."
  ([records] (in-flight-from-records records (Instant/now) 1.0))
  ([records ^Instant now] (in-flight-from-records records now 1.0))
  ([records ^Instant now window-hours]
   (->> (file-observations records)
        (filter #(<= (age-hours ^Instant (:observed-at %) now) window-hours))
        (map :repo/root)
        (map #(.getName (io/file %)))
        set)))

(def effect-handler
  "Print effect handler for a live cycle. Commits print as proposals —
  board v0 never shells git itself; the established sweeper machinery owns
  the act (R9: the board proposes, the existing pipeline witnesses)."
  (fn [effect]
    (prn {:effect (first effect) :payload (second effect)})
    nil))

(defn run-live!
  "Load state.edn, build the observation packet from typed records, run
  one board cycle. Returns the run with :certificate."
  ([state-path] (run-live! state-path (Instant/now)))
  ([state-path ^Instant now]
   (let [records (:records (edn/read-string (slurp state-path)))
         packet (board/observation-packet
                 (sweep-from-records records now)
                 (in-flight-from-records records now)
                 false)]
     (board/run packet effect-handler))))

(defn -main
  [& [state-path]]
  (let [run (run-live! (or state-path
                           (str (System/getProperty "user.home")
                                "/code/storage/inbox-zero/state.edn")))]
    (prn {:end-reason (:end-reason run)
          :certificate (:certificate run)})))
