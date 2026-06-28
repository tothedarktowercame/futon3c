(ns futon3c.agency.clock-lineage
  "Durable agent↔session↔mission lineage for the auto-clock — C-cascade-real D1/O3.

   `futon3c.agency.clock-store` holds the *live* (in-RAM) clock; this ns makes a
   clock transition DURABLE by writing it to substrate-2 (futon1a hyperedge
   store, :7071) so the reconstitution query — \"the N most recent missions, and
   who/which sessions are on each\" — survives a JVM teardown (CHARTER standard 3,
   the \"no sheet of paper\" test). The in-RAM atom alone dies on restart; this is
   the persist + query half of D1.

   Single-active + time-travel ride on the D3 valid-time work: a clock SWITCH
   retracts (end-valid-time) the agent's prior clock edge and puts the new one,
   so a `db-as-of`-now read shows exactly one active target per agent while the
   bitemporal history stays queryable.

   The write is fire-and-forget off the caller's thread (a `future`) so the hot
   invoke path never blocks on the substrate (blackboard-backpressure
   discipline). Reads/writes go to futon1a over HTTP, respecting the I-5 boundary."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.agency.clock-store :as clock-store])
  (:import [java.net URLEncoder]))

(def ^:private FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def ^:private PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def clock-type "clock/clocked-on")

(defn target-endpoint
  "A clock's single-active target as a substrate-2 endpoint id (excursion >
   mission > campaign — the most specific wins, matching clock-store's label
   precedence). nil when the clock has no target (\"no mission\")."
  [{:keys [campaign-id mission-id excursion-id]}]
  (cond
    excursion-id (str "excursion:" excursion-id)
    mission-id   (str "mission:" mission-id)
    campaign-id  (str "campaign:" campaign-id)
    :else        nil))

;; ---------------------------------------------------------------------------
;; persist — durable write on a clock transition (retract-old + put-new)
;; ---------------------------------------------------------------------------

(defn- post-hx!
  "Single hyperedge write to futon1a. ENDPOINTS final; honours VALID-TIME-MS and
   an optional OP (\"retract\" → end-valid-time). Returns {:ok? :status}."
  [{:keys [endpoints props valid-time-ms op]}]
  (let [payload (cond-> {"hx/type" clock-type "hx/endpoints" endpoints}
                  props         (assoc "hx/props" props)
                  valid-time-ms (assoc "hx/valid-time" valid-time-ms)
                  op            (assoc "hx/op" op))
        resp (try
               (http/post (str FUTON1A "/api/alpha/hyperedge")
                          {:headers {"Content-Type" "application/json"
                                     "X-Penholder" PENHOLDER}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))]
    {:ok? (= 200 (:status resp)) :status (:status resp)}))

(defn persist-clock!
  "Durably record a clock transition for AGENT-ID/SESSION-ID. Single-active: when
   the target changed, retract the agent's prior target edge (end-valid-time),
   then put the new `[agent → target]` edge at valid-time NOW with session +
   witness in props. Fire-and-forget — returns a future (deref for {:ok?}); a
   no-op returning nil when NEW-CLOCK has no target. Never throws into the caller."
  [{:keys [agent-id session-id old-clock new-clock witness now-ms]}]
  (when-let [new-ep (target-endpoint new-clock)]
    (let [now      (long (or now-ms (System/currentTimeMillis)))
          agent-ep (str "agent:" agent-id)
          old-ep   (target-endpoint old-clock)
          props    (cond-> {"agent-id" (str agent-id) "clocked-at-ms" now}
                     (some-> session-id str not-empty) (assoc "session-id" (str session-id))
                     (:mission-id new-clock)   (assoc "mission-id" (:mission-id new-clock))
                     (:campaign-id new-clock)  (assoc "campaign-id" (:campaign-id new-clock))
                     (:excursion-id new-clock) (assoc "excursion-id" (:excursion-id new-clock))
                     witness                   (assoc "witness" witness))]
      (future
        (try
          (when (and old-ep (not= old-ep new-ep))
            (post-hx! {:endpoints [agent-ep old-ep] :valid-time-ms now :op "retract"}))
          (post-hx! {:endpoints [agent-ep new-ep] :props props :valid-time-ms now})
          (catch Exception _ {:ok? false}))))))

(defn clock-dispatch!
  "Invoke-receipt wire point: clock AGENT-ID/SESSION-ID to MISSION-ID in the live
   store (clock-store/set-dispatch-mission!) AND durably persist the transition.
   Reads old-clock first so the persist can retract it. Returns the new live
   session state (or nil for a blank mission-id). Safe on the hot path — the
   durable write is async inside persist-clock!."
  [agent-id session-id mission-id]
  (when (some-> mission-id str str/trim not-empty)
    (let [old-clock (clock-store/current-clock agent-id session-id)
          state     (clock-store/set-dispatch-mission! agent-id session-id mission-id)
          new-clock (:clock state)]
      (persist-clock! {:agent-id agent-id :session-id session-id
                       :old-clock old-clock :new-clock new-clock
                       :witness (:last-auto-clock-witness state)})
      state)))

(defn clock-edit!
  "Edit-activity wire point (the warm-pouch per-tool feed, `dev.clj`'s
   `record-agent-tool-use!`): record a tool-use against clock-store and, if it
   SWITCHED the clock (the edit-activity threshold over a `C-/M-/E-.md` doc), make
   that transition DURABLE. Reads old-clock BEFORE `record-tool-use!` mutates, so
   the retract targets the true prior edge. Unlike the dispatch path, this resolves
   campaign / mission / excursion targets (via `resolve-clock-target-file`), so
   editing `C-cascade-real.md` clocks onto `campaign:C-cascade-real`. Returns
   clock-store's result (nil when the file is not a witnessed doc)."
  [agent-id session-id tool-detail]
  (let [old-clock (clock-store/current-clock agent-id session-id)
        result    (clock-store/record-tool-use! agent-id session-id tool-detail)
        new-clock (get-in result [:state :clock])]
    (when (and new-clock (not= old-clock new-clock))
      (persist-clock! {:agent-id agent-id :session-id session-id
                       :old-clock old-clock :new-clock new-clock
                       :witness (get-in result [:state :last-auto-clock-witness])}))
    result))

;; ---------------------------------------------------------------------------
;; reconstitute — the s3 read (survives teardown; from the durable store)
;; ---------------------------------------------------------------------------

(defn- query-by-type
  "GET futon1a for all currently-valid hyperedges of CLOCK-TYPE (db-as-of now —
   retracted edges are excluded). Returns a seq of hyperedge maps, or []."
  []
  (let [url  (str FUTON1A "/api/alpha/hyperedges?type="
                  (URLEncoder/encode clock-type "UTF-8"))
        resp (try (http/get url {:headers {"Accept" "application/edn"} :throw false})
                  (catch Exception _ nil))]
    (or (when (and resp (= 200 (:status resp)) (string? (:body resp)))
          (try (:hyperedges (edn/read-string (:body resp)))
               (catch Exception _ nil)))
        [])))

(defn- prop [props k]
  (or (get props k) (get props (name k))))

(defn summarize-edges
  "Pure: a seq of `clock/clocked-on` hyperedge maps → the reconstitution missions
   list, one entry per target (who/which sessions are on each), most-recent-first.
   Pulled out of `reconstitute` so it is unit-testable without the substrate."
  [edges]
  (let [rows (for [e    edges
                   :let [props   (:hx/props e)
                         target  (or (prop props :mission-id)
                                     (prop props :campaign-id)
                                     (prop props :excursion-id))
                         agent   (prop props :agent-id)
                         session (prop props :session-id)
                         at      (or (prop props :clocked-at-ms) 0)]
                   :when target]
               {:target target :agent agent :session session :at at})]
    (->> (group-by :target rows)
         (map (fn [[target rs]]
                {:target        target
                 :agents        (vec (distinct (keep :agent rs)))
                 :sessions      (vec (distinct (keep :session rs)))
                 :last-clock-ms (apply max (map :at rs))}))
         (sort-by :last-clock-ms >)
         vec)))

(defn reconstitute
  "The s3 reconstitution read, from the DURABLE store (survives a teardown):
   the most-recently-clocked targets, and who/which sessions are on each.
   Returns {:as-of-ms now :count N :missions [{:target :agents [...] :sessions
   [...] :last-clock-ms ...} ...]} ordered most-recent-first. LIMIT optionally
   caps the list."
  ([] (reconstitute {}))
  ([{:keys [limit]}]
   (let [missions (summarize-edges (query-by-type))]
     {:as-of-ms (System/currentTimeMillis)
      :count    (count missions)
      :missions (cond-> missions limit (->> (take limit) vec))})))
