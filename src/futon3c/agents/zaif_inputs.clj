(ns futon3c.agents.zaif-inputs
  "D-1 (Z3a prerequisite): the ZAIF input hydrator.

   Supplies the controller's `decide()` with real beliefs instead of empty
   maps, so live decisions are non-degenerate and Z3a-scoreable. Three
   channels:

   1. **γ(mission)** — read from the B1 fold artifact (the source of truth),
      cached per-JVM. Configurable via FUTON3C_ZAIF_GAMMA_EDN.
   2. **c-belief :operator-c-uncertainty** — derived from per-mission
      correction rate (marks + PZ1 final truth, proxied via the B1 γ table's
      per-cell perf-history). Higher correction rate → higher C-uncertainty.
   3. **posting-stats** — derived from the turn's context text (IDF-ish proxy,
      matching the z2_calibrate replay harness's estimate-posting-stats).

   Cadence: per-turn hydration (the γ table is cached once; c-uncertainty and
   posting-stats are cheap derivations from cached data). Per-round store
   queries are avoided (turn latency).

   **Failure discipline:** any error in any channel degrades to the
   empty-map default for that channel — hydration NEVER hurts a turn. The
   caller (maybe-zaif-decision!) wraps the whole call in a try/catch as a
   second layer of defense."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private default-gamma-edn-path
  "The B1 γ(mission) artifact. Resolved relative to the workspace root
   (/home/joe/code), not futon3c/, so it works from any cwd. The workspace
   root is derived from the user.dir system property's parent when running
   from futon3c/, or directly when running from the workspace root."
  (let [cwd (System/getProperty "user.dir")
        ;; Handle both /home/joe/code and /home/joe/code/futon3c as cwd
        root (if (str/ends-with? cwd "futon3c")
               (subs cwd 0 (- (count cwd) (count "futon3c")))
               (str cwd "/"))]
    (str root "futon2/holes/labs/M-zaif-harness/b1-gamma-mission.edn")))

(def ^:private default-gamma-fn
  (fn [path]
    (with-open [r (java.io.PushbackReader. (io/reader path))]
      (edn/read r))))

(defonce ^:private !gamma-cache
  (atom nil))

(defn gamma-edn-path
  "Resolve the γ table EDN path from FUTON3C_ZAIF_GAMMA_EDN or the default
   B1 artifact path."
  []
  (or (some-> (System/getenv "FUTON3C_ZAIF_GAMMA_EDN")
              str/trim not-empty)
      default-gamma-edn-path))

(defn load-gamma-table
  "Read and cache the B1 γ(mission) table. Returns the full EDN structure
   (the B1 artifact with :cells, :events, :summary). Cached per-JVM;
   subsequent calls return the cache without re-reading the file.

   Returns nil if the file is missing or unreadable (failure discipline:
   caller degrades to empty γ)."
  ([]
   (load-gamma-table (gamma-edn-path) default-gamma-fn))
  ([path]
   (load-gamma-table path default-gamma-fn))
  ([path read-fn]
   (if-let [cached @!gamma-cache]
     cached
     (try
       (let [data (read-fn path)]
         (reset! !gamma-cache data)
         data)
       (catch Throwable _ nil)))))

(defn reset-gamma-cache!
  "Clear the γ table cache (for testing)."
  []
 (reset! !gamma-cache nil))

(defn gamma-cells
  "Extract the γ(mission) cells as a flat map {mission-id policy-precision}.
   Returns {} if the table is unavailable."
  ([]
   (gamma-cells (load-gamma-table)))
  ([gamma-data]
   (if-let [cells (:cells gamma-data)]
     (into {} (for [[k v] cells]
                [(name k) (:policy-precision v)]))
     {})))

(defn gamma-for
  "Look up γ for a mission from the flat cells map. Returns 1.0 (uniform
   prior) if the mission is missing or unburned — matching the controller's
   gamma-for-mission semantics."
  [cells mission]
  (if mission
    (let [m (name mission)]
      (or (get cells m)
          (get cells (keyword mission))
          1.0))
    1.0))

(defn- correction-rate-from-cell
  "Derive per-mission correction rate from a γ table cell's perf-history.
   Each -0.5 entry is a correction event; each +0.5 is an approval. Rate =
   corrections / total events. Returns nil if no events."
  [cell]
  (when-let [history (seq (:perf-history cell))]
    (let [total (count history)
          corrections (count (filter neg? history))]
      (if (pos? total)
        (/ (double corrections) (double total))
        nil))))

(defn correction-rate-table
  "Build a map of {mission-id correction-rate} from the γ table's cells.
   Uses the same artifact as γ (self-contained: no separate marks query).
   Returns {} if unavailable."
  ([]
   (correction-rate-table (load-gamma-table)))
  ([gamma-data]
   (if-let [cells (:cells gamma-data)]
     (into {} (for [[k v] cells
                    :let [rate (correction-rate-from-cell v)]
                    :when (some? rate)]
                [(name k) rate]))
     {})))

(defn c-uncertainty-for
  "Derive :operator-c-uncertainty from per-mission correction rate.

   Formula: the correction rate IS the c-uncertainty proxy. A mission with
   100% corrections → c-uncertainty 1.0 (operator intent maximally unclear);
   0% corrections → 0.0 (intent clear). For missions with no data, returns
   a mild prior of 0.3 (the aggregate correction base rate is ~24-30% per
   PZ1/PZ2, so a new mission starts slightly below the average to avoid
   over-hedging on first contact).

   This is the simplest faithful mapping: correction rate directly measures
   how often the agent's C-belief was wrong, which is exactly what
   c-uncertainty quantifies."
  ([mission]
   (c-uncertainty-for mission (correction-rate-table)))
  ([mission rate-table]
   (if mission
     (let [m (name mission)]
       (or (get rate-table m)
           (get rate-table (keyword mission))
           0.3))
     0.3)))

(defn estimate-posting-stats
  "Derive posting statistics from context text for the retrieve-EIG proxy.
   Matches the z2_calibrate replay harness's estimate-posting-stats so live
   and replay decisions are comparable. Returns {} if context is empty."
  [context]
  (let [text (str context)]
    (if (str/blank? text)
      {}
      (let [words (filter seq (str/split text #"\s+"))
            total (max 1 (count words))
            unique (set (map str/lower-case words))
            dfs (map (fn [w] (count (filter #(= w %) (map str/lower-case words)))) unique)]
        {:total-docs total
         :dfs (vec (take 10 (sort dfs)))
         :estimated-tokens (min 2000 (* 2 (count words)))}))))

(defn- extract-mission-from-ctx
  "Try to extract a mission id from the ctx. Checks :mission key first,
   then searches :context or :prompt text for M-/E- patterns."
  [ctx]
  (or (:mission ctx)
      (when-let [text (or (:context ctx) (:prompt ctx))]
        (first (re-seq #"[ME]-[a-z0-9-]+" (str text))))))

(defn hydrate-inputs
  "Build controller inputs from ctx. Pure: reads cached γ table and derives
   beliefs. Returns the full inputs map for `decide`. Any channel that
   fails degrades to its empty default (failure discipline)."
  [ctx]
  (let [gamma-data (or (load-gamma-table) {:cells {}})
        cells (gamma-cells gamma-data)
        rate-table (correction-rate-table gamma-data)
        mission (extract-mission-from-ctx ctx)
        context-text (or (:context ctx) (:prompt ctx) "")
        gamma-val (gamma-for cells mission)
        c-uncertainty (c-uncertainty-for mission rate-table)
        posting-stats (estimate-posting-stats context-text)]
    {:mission mission
     :gamma {mission {:policy-precision gamma-val}}
     :task-belief {}
     :c-belief {:operator-c-uncertainty c-uncertainty}
     :observations {:posting-stats posting-stats}}))

(defn make-hydrator
  "Return a zaif-inputs-fn suitable for the :zaif-inputs-fn option in
   make-invoke-fn. The returned function takes ctx and returns hydrated
   inputs, or empty maps on any error."
  []
  (fn [ctx]
    (try
      (hydrate-inputs ctx)
      (catch Throwable _
        {:task-belief {}
         :c-belief {}
         :gamma {}
         :mission nil
         :observations {}}))))
