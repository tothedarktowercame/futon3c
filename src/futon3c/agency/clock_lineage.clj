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
            [clojure.java.io :as io]
            [clojure.string :as str]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3c.agency.clock-store :as clock-store])
  (:import [java.net URLEncoder]))

(def ^:private FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def ^:private PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def clock-type "clock/clocked-on")

(def ^:private code-root (or (System/getenv "FUTON_CODE_ROOT") "/home/joe/code"))
(def ^:private kind->prefix {:mission "M" :campaign "C" :excursion "E"})
(def ^:private kind->subdir {:mission "missions" :campaign "campaigns" :excursion "excursions"})

(defn target-kind+id
  "The clock's single-active [kind id] (excursion > mission > campaign — most
   specific wins, matching clock-store's label precedence), or nil for no target."
  [{:keys [campaign-id mission-id excursion-id]}]
  (cond
    excursion-id [:excursion excursion-id]
    mission-id   [:mission mission-id]
    campaign-id  [:campaign campaign-id]
    :else        nil))

(defn- doc-repo
  "The repo dir under code-root that holds the KIND doc for STEM (e.g. mission
   stem \"autoclock-in\" → \"futon3c\"). Excludes `*-desktop-save` backup checkouts
   (the drift claude-2 flagged). nil when no doc is found."
  [kind stem]
  (let [fname (str (kind->prefix kind) "-" stem ".md")
        sub   (kind->subdir kind)]
    (some (fn [^java.io.File d]
            (when (and (.isDirectory d)
                       (not (str/includes? (.getName d) "desktop-save"))
                       (or (.exists (io/file d "holes" sub fname))
                           (.exists (io/file d "holes" fname))))
              (.getName d)))
          (sort (or (seq (.listFiles (io/file code-root))) [])))))

(defn- endpoint-exists?
  "True when substrate-2 has any current-valid edge on endpoint EP (so we only
   attach to nodes that really exist). Never throws."
  [ep]
  (let [url  (str FUTON1A "/api/alpha/hyperedges?end=" (URLEncoder/encode ep "UTF-8"))
        resp (try (http/get url {:headers {"Accept" "application/edn"} :throw false})
                  (catch Exception _ nil))]
    (boolean (and resp (= 200 (:status resp)) (string? (:body resp))
                  (re-find #"hx/id" (:body resp))))))

(defn canonical-endpoint
  "Resolve a clock target [KIND ID] (e.g. [:mission \"M-autoclock-in\"]) to its
   CANONICAL substrate-2 node id `<repo>-d/<kind>/<stem>` (claude-2's contract,
   2026-06-30), VERIFIED to exist — so O3 lineage ATTACHES to the real node and
   never mints a non-canonical island (C-cascade-real Clause 3). Returns nil when
   no doc/canonical node is found; the caller then SKIPS the durable write rather
   than write non-canonical."
  [kind id]
  (when-let [prefix (kind->prefix kind)]
    (let [stem (str/replace (str id) (re-pattern (str "^" prefix "-")) "")]
      (when (and (seq stem) (not= stem (str id)))      ; require the K- prefix was present
        (when-let [repo (doc-repo kind stem)]
          (let [ep (str repo "-d/" (name kind) "/" stem)]
            (when (endpoint-exists? ep) ep)))))))

(defn canonical-target-endpoint
  "The clock's CANONICAL substrate-2 endpoint (verified), or nil if no canonical
   node exists — never an island. Replaces the old non-canonical `target-endpoint`."
  [clock]
  (when-let [[kind id] (target-kind+id clock)]
    (canonical-endpoint kind id)))

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

(declare query-by-type)

(defn- agent-current-targets
  "The agent's currently-valid clock target endpoints in the DURABLE store (0 or 1
   in steady state; >1 only if a prior multi-feed left duplicates — which this then
   cleans up). Read from substrate-2, not RAM, so it is the authoritative prior
   state regardless of how the in-RAM clock was mutated."
  [agent-ep]
  (->> (query-by-type)
       (filter (fn [e] (some #{agent-ep} (:hx/endpoints e))))
       (mapcat (fn [e] (remove #{agent-ep} (:hx/endpoints e))))
       distinct
       vec))

(defn persist-clock!
  "Durably record a clock transition for AGENT-ID/SESSION-ID. Single-active by
   retracting (end-valid-time) the agent's prior DURABLE target edge(s) — read
   from substrate-2, NOT the passed RAM old-clock — then putting the new
   `[agent → target]` edge at valid-time NOW with session + witness in props.
   Reading the retract target from the durable store makes this correct and
   idempotent no matter how many feed loci mutated the RAM clock or in what order
   (kills the dispatch double-feed retract hazard). Fire-and-forget — returns a
   future (deref for {:ok?}); a no-op returning nil when NEW-CLOCK has no target.
   Never throws into the caller. (OLD-CLOCK is accepted for caller symmetry but no
   longer drives the retract.)"
  [{:keys [agent-id session-id new-clock witness now-ms]}]
  (when-let [new-ep (canonical-target-endpoint new-clock)]
    (let [now      (long (or now-ms (System/currentTimeMillis)))
          agent-ep (str "agent:" agent-id)
          props    (cond-> {"agent-id" (str agent-id) "clocked-at-ms" now}
                     (some-> session-id str not-empty) (assoc "session-id" (str session-id))
                     (:mission-id new-clock)   (assoc "mission-id" (:mission-id new-clock))
                     (:campaign-id new-clock)  (assoc "campaign-id" (:campaign-id new-clock))
                     (:excursion-id new-clock) (assoc "excursion-id" (:excursion-id new-clock))
                     witness                   (assoc "witness" witness))]
      (future
        (try
          (doseq [t (agent-current-targets agent-ep) :when (not= t new-ep)]
            (post-hx! {:endpoints [agent-ep t] :valid-time-ms now :op "retract"}))
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

(defn- query-edges-of-type
  "GET futon1a for all currently-valid hyperedges of HX-TYPE (db-as-of now —
   retracted edges are excluded). Returns a seq of hyperedge maps, or []."
  [hx-type]
  (let [url  (str FUTON1A "/api/alpha/hyperedges?type="
                  (URLEncoder/encode hx-type "UTF-8"))
        resp (try (http/get url {:headers {"Accept" "application/edn"} :throw false})
                  (catch Exception _ nil))]
    (or (when (and resp (= 200 (:status resp)) (string? (:body resp)))
          (try (:hyperedges (edn/read-string (:body resp)))
               (catch Exception _ nil)))
        [])))

(defn- query-by-type [] (query-edges-of-type clock-type))

(defn- prop [props k]
  (or (get props k) (get props (name k))))

(defn summarize-edges
  "Pure: a seq of `clock/clocked-on` hyperedge maps → the reconstitution missions
   list, one entry per target (who/which sessions are on each), most-recent-first.
   Pulled out of `reconstitute` so it is unit-testable without the substrate.
   Each row also carries `:canonical` — the non-agent endpoint, the canonical
   `<repo>-d/<kind>/<id>` node — which is the join key onto the held-work ledger
   (`held/on-mission` edges key the same canonical node)."
  [edges]
  (let [rows (for [e    edges
                   :let [props   (:hx/props e)
                         target  (or (prop props :mission-id)
                                     (prop props :campaign-id)
                                     (prop props :excursion-id))
                         canon   (first (remove #(str/starts-with? (str %) "agent:")
                                                (:hx/endpoints e)))
                         agent   (prop props :agent-id)
                         session (prop props :session-id)
                         at      (or (prop props :clocked-at-ms) 0)]
                   :when target]
               {:target target :canonical (some-> canon str) :agent agent
                :session session :at at})]
    (->> (group-by :target rows)
         (map (fn [[target rs]]
                {:target        target
                 :canonical     (->> rs (sort-by :at >) (keep :canonical) first)
                 :agents        (vec (distinct (keep :agent rs)))
                 :sessions      (vec (distinct (keep :session rs)))
                 :last-clock-ms (apply max (map :at rs))}))
         (sort-by :last-clock-ms >)
         vec)))

(defn held-by-mission
  "Pure: a seq of `held/on-mission` hyperedge maps → {canonical-mission-id →
   [{:item id :disposition d :reason r} ...]}. The held-work ledger keys its
   second endpoint on the canonical mission node, so this map joins onto
   `summarize-edges`' `:canonical` — wiring Exit-criterion-2 ('what held work is
   pending') onto the reconstitution read without a second registry."
  [held-edges]
  (->> (for [e   held-edges
             :let [eps   (:hx/endpoints e)
                   item  (first (filter #(str/starts-with? (str %) "held/item/") eps))
                   miss  (first (remove #(str/starts-with? (str %) "held/item/") eps))
                   props (:hx/props e)]
             :when (and item miss)]
         {:mission (str miss)
          :item    (str item)
          :disposition (prop props :held/disposition)
          :reason  (prop props :held/reason)})
       (group-by :mission)
       (into {} (map (fn [[m rs]] [m (mapv #(dissoc % :mission) rs)])))))

(defn reconstitute
  "The s3 reconstitution read, from the DURABLE store (survives a teardown):
   the most-recently-clocked targets, and who/which sessions are on each.
   Returns {:as-of-ms now :count N :missions [{:target :agents [...] :sessions
   [...] :last-clock-ms ... :held-count N :held [...]} ...]} ordered
   most-recent-first. Each mission is joined against the held-work ledger
   (`held/on-mission`) on its `:canonical` node, so the read answers Exit
   criterion 2 — 'N recent missions, who/which sessions on each, AND what held
   work is pending' — in one durable query. LIMIT optionally caps the list."
  ([] (reconstitute {}))
  ([{:keys [limit]}]
   (let [missions (summarize-edges (query-by-type))
         held     (held-by-mission (query-edges-of-type "held/on-mission"))
         missions (mapv (fn [m]
                          (let [hs (get held (:canonical m) [])]
                            (assoc m :held-count (count hs) :held hs)))
                        missions)]
     {:as-of-ms     (System/currentTimeMillis)
      :count        (count missions)
      :held-total   (reduce + (map :held-count missions))
      :missions     (cond-> missions limit (->> (take limit) vec))})))
