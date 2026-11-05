(ns futon3c.logic.cascade-real-live
  "Live-data gate for C-cascade-real STANDARD-VERIFY (RUN/DELIVER).

   `futon3c.logic.cascade-real` verifies the cascade-real CONTRACT (the design).
   This ns maps REAL substrate-2 rows onto the SAME relations so `cr/verify` runs
   over LIVE data — the acceptance bar each RUN/DELIVER car must pass for its
   dimension (a runnable gate, not a judgment call).

   Per dimension, an EXTRACTOR turns that dimension's substrate-2 hyperedges into
   `claims-typeo` facts (which node-id it references AS which type). The shared
   ontology (CHARTER standard 5) is checked by `cr/composition-violations`: a
   node-id referenced by two dimensions must carry one type. That check is
   **vacuous until ≥2 dimensions reference a shared real node** — today only O3/D1
   (the durable lineage) is landed, so the gate RUNS over live rows but the
   cross-dimension bite arrives as cars land. As each car lands, add its extractor
   to `extractors`; `verify-live` then checks its live rows against the rest.

   The dimension/owner/held-on/coverage SCAFFOLD stays from the contract (it is the
   campaign's own structure, not landed data); only `claims-typeo` becomes live."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.agency.registry :as registry]
            [futon3c.logic.cascade-real :as cr]
            [futon3c.substrate.client :as substrate])
  (:import (java.util.concurrent Callable Executors TimeUnit TimeoutException)))

(def ^:private code-root (or (System/getenv "FUTON_CODE_ROOT") "/home/joe/code"))
;; Keep each request below Futon1b's five-second query deadline.  A 1,000-row
;; request used to work when the server deadline was looser, but now receives a
;; typed :query-deadline-exceeded before this client's 30-second transport
;; timeout.  Pagination is slower overall, but it preserves the authoritative
;; complete walk instead of making the outer selector lose an entire channel.
(def ^:private fetch-timeout-ms 30000)
(def ^:private fetch-page-size 100)
(def ^:private fetch-page-size-by-type
  ;; The mission-pattern binder is now the only multi-thousand-row section.
  ;; At 100 rows its complete cursor walk repeatedly exceeds the graph's
  ;; 90-second deadline under live load; 200 remains below Futon1b's per-query
  ;; deadline and measured a complete 1,878-row walk in about 30 seconds.
  {"mission-scope/pattern" 200})

(def ^:private fetch-fields-by-type
  {"mission-scope/pattern"
   ["hx/id" "hx/type" "hx/endpoints"
    "hx/props.pattern/ref" "hx/props.pattern/ident" "hx/props.pattern/state"]})

#_{:clj-kondo/ignore [:unresolved-var]}
(def ^:private claims-typeo-rel cr/claims-typeo)

(defn fetch-edges
  "GET currently-valid authoritative substrate hyperedges of HX-TYPE. A
  transport or incomplete pagination failure is loud; [] means the
  authoritative query was exhausted and empty."
  [hx-type]
  (let [page-size (get fetch-page-size-by-type hx-type fetch-page-size)
        rows (substrate/hyperedges-by-type hx-type {:timeout-ms fetch-timeout-ms
                                                     :page-size page-size
                                                     :fields (get fetch-fields-by-type
                                                                  hx-type)})]
    (when (substrate/partial-result? rows)
      (throw (ex-info "authoritative substrate walk incomplete"
                      {:hyperedge-type hx-type
                       :pagination (meta rows)})))
    rows))

(defn- prop [props k] (or (get props k) (get props (name k))))

;; ---------------------------------------------------------------------------
;; per-dimension extractors — edges → [[claims-typeo dim node-id type] ...]
;; Each is pure (takes edges); the *-claims wrapper fetches.
;; ---------------------------------------------------------------------------

;; A canonical work-object node: `<repo>-d/mission/<id>`, `<repo>-d/excursion/<id>`
;; or `<repo>-d/ticket/<id>`. The trailing slash is part of the match, so
;; `-d/missionary/x` is not a mission node.
(def ^:private node-kind-re #"-d/(mission|excursion|ticket)/")

(defn node-kind
  "The kind of canonical work-object node S names (:mission, :excursion or
   :ticket), or nil."
  [s]
  (some-> (re-find node-kind-re (str s)) second keyword))

(defn o3-claims-from
  "O3/D1 — the durable agent↔session↔mission lineage (`clock/clocked-on` edges).
   Keys on the EDGE ENDPOINTS (the CANONICAL node-ids the lineage now writes,
   `<repo>-d/mission/<id>`), so O3's mission claim shares its node-id with O1/D4 —
   that's the cross-dimension composition. Claims the canonical mission node
   `:mission` (same type O1 claims it → composes cleanly), the agent node `:agent`.
   Excursion and ticket nodes are claimed as `:excursion` / `:ticket` where a
   clock edge ends on one (the outer cascade's field has all three kinds).
   Pure: EDGES → claims-typeo fact-vectors."
  [edges]
  (for [e   edges
        ep  (:hx/endpoints e)
        :let [s (str ep)
              [nid type] (cond
                           (str/starts-with? s "agent:")    [s :agent]
                           (node-kind s)                     [s (node-kind s)]
                           (str/starts-with? s "campaign:")  [s :campaign]
                           :else                             nil)]
        :when nid]
    [claims-typeo-rel :O3 nid type]))

(defn- o3-lineage-claims [] (o3-claims-from (fetch-edges "clock/clocked-on")))

(defn o2-meme-claims-from
  "O2 — the canonical mine (`mine/meme` edges, claude-1). Each meme is a node
   `meme:ask-<hash>` of TYPE :meme. Pre-written against the stable shape claude-1
   committed to; contributes 0 claims until the rows land (honest non-landing).
   The mine's `meme:` nodes are DISJOINT from O3's mission/agent nodes, so O2
   composes cleanly; the concept-index (`mission/*` keys) is deferred to later
   *reference* edges — NOT type claims — precisely because it would collide with
   O3's `mission:` typing (the conflict the gate catches). Pure: EDGES → claims."
  [edges]
  (for [e   edges
        nid (filter #(str/starts-with? (str %) "meme:") (:hx/endpoints e))]
    [claims-typeo-rel :O2 (str nid) :meme]))

(defn- o2-mine-claims [] (o2-meme-claims-from (fetch-edges "mine/meme")))

(defn o1-mined-move-claims-from
  "O1/D4 — the mined-move arrows (`code/v05/mined-move` edges, claude-2's feeder-(b)).
   Each arrow's HAVE endpoint is a canonical mission node `<repo>-d/mission/<id>` —
   **the shared node with O3** (177/177 direct hits), where cross-dimension
   composition happens. Claims that mission node `:mission` — the SAME type O3's
   lineage claims it, so they compose cleanly (non-vacuous, no conflict). Pure:
   EDGES → claims-typeo. Lights up when feeder-(b) lands; 0 until then."
  [edges]
  (for [e    edges
        :let [have (first (:hx/endpoints e))]
        :when (and have
                   (re-find #"-d/mission/" (str have))
                   (not (re-find #"-head$" (str have))))]
    [claims-typeo-rel :O1 (str have) :mission]))

(defn- o1-arrow-claims [] (o1-mined-move-claims-from (fetch-edges "code/v05/mined-move")))

(defn o4-cluster-claims-from
  "O4 — the upward structure (`cascade/cluster/<slug>` nodes + `cascade/cluster-member`
   edges, claude-10). A cluster-member edge connects a cluster to a canonical mission
   node; O4 claims that mission node `:mission` — **the shared node with O1/O3**
   (composition on the spine) — and the cluster node `:cluster` (its own). Pure:
   EDGES → claims. Lights up when O4 lands; 0 until then."
  [edges]
  (mapcat
   (fn [e]
     (keep (fn [ep]
             (let [s (str ep)]
               (cond
                 (str/starts-with? s "cascade/cluster/") [claims-typeo-rel :O4 s :cluster]
                 (re-find #"-d/mission/" s)               [claims-typeo-rel :O4 s :mission]
                 :else nil)))
           (:hx/endpoints e)))
   edges))

(defn- o4-upward-claims [] (o4-cluster-claims-from (fetch-edges "cascade/cluster-member")))

(defn o5-hole-claims-from
  "O5 — honest holes (`cascade/hole/<slug>` nodes + `cascade/hole-target` edges,
   owner TBD/codex-1). A hole-target edge connects a hole to the canonical node it
   marks as a gap; O5 claims that node — a mission → `:mission`, a capability →
   `:capability` — composing with O1/O3/O4 on the mission spine — and the hole
   node `:hole` (its own). Pure: EDGES → claims. Lights up when O5 lands; 0 until."
  [edges]
  (mapcat
   (fn [e]
     (keep (fn [ep]
             (let [s (str ep)]
               (cond
                 (str/starts-with? s "cascade/hole/")     [claims-typeo-rel :O5 s :hole]
                 (re-find #"-d/mission/" s)                [claims-typeo-rel :O5 s :mission]
                 (str/starts-with? s "scope/capability/") [claims-typeo-rel :O5 s :capability]
                 :else nil)))
           (:hx/endpoints e)))
   edges))

(defn- o5-holes-claims [] (o5-hole-claims-from (fetch-edges "cascade/hole-target")))

(def extractors
  "Registry of LANDED-dimension extractors (dim → 0-arg fn → claims-typeo facts).
   Add an entry as each RUN/DELIVER car lands its substrate-2 rows. O2 is
   pre-wired against `mine/meme`; it lights up automatically when claude-1 lands
   the pinned mine rows."
  {:O3 o3-lineage-claims
   :O2 o2-mine-claims
   :O1 o1-arrow-claims
   :O4 o4-upward-claims
   :O5 o5-holes-claims})

;; ---------------------------------------------------------------------------
;; the live gate
;; ---------------------------------------------------------------------------

(defn live-facts
  "The contract SCAFFOLD (dim/owner/held-on/covers) + LIVE `claims-typeo` from the
   landed dimensions' real substrate-2 rows. Unlanded dimensions contribute no
   claims (honest — their data isn't there yet)."
  []
  (concat
   (remove (fn [[rel]] (= rel claims-typeo-rel)) cr/contract-facts)
   (mapcat (fn [extract] (extract)) (vals extractors))))

(defn live-dimensions
  "Which dimensions currently contribute LIVE claims (coverage transparency) —
   {dim claim-count}. A dimension with 0 here has not landed cascade data yet."
  []
  (into {} (for [[dim extract] extractors] [dim (count (extract))])))

(defn verify-live
  "Run the L1 checks over LIVE substrate-2 rows. The gate each RUN/DELIVER car must
   pass for its dimension. Returns `cr/verify`'s map plus `:live-dimensions` so the
   coverage is explicit (composition only bites once ≥2 dimensions share a node)."
  []
  (assoc (cr/verify (cr/db-from-data (live-facts)))
         :live-dimensions (live-dimensions)))

;; ---------------------------------------------------------------------------
;; live summary — the JSON the pipeline-pattern-cascade view renders
;; ---------------------------------------------------------------------------

(defn- nodes-of-type [type claims]
  (set (map #(nth % 2) (filter #(= type (nth % 3)) claims))))

(defn- mission-nodes [extract]
  (nodes-of-type :mission (extract)))

(defn- claims-consistent?
  "Fast UI consistency check: a shared real node may not be claimed with two
   different types. The full STANDARD-VERIFY gate remains `verify-live`; the
   live HTML summary should not run core.logic on every poll."
  [claims]
  (not-any? (fn [[_ types]] (> (count types) 1))
            (reduce (fn [acc [_rel _dim node type]]
                      (update acc node (fnil conj #{}) type))
                    {}
                    claims)))

(defn- summary-edge-map
  []
  {"clock/clocked-on" (fetch-edges "clock/clocked-on")
   "mine/meme" (fetch-edges "mine/meme")
   "code/v05/mined-move" (fetch-edges "code/v05/mined-move")
   "cascade/cluster-member" (fetch-edges "cascade/cluster-member")
   "cascade/hole-target" (fetch-edges "cascade/hole-target")})

(defn- summary-claims
  [edges]
  {:O3 (o3-claims-from (get edges "clock/clocked-on"))
   :O2 (o2-meme-claims-from (get edges "mine/meme"))
   :O1 (o1-mined-move-claims-from (get edges "code/v05/mined-move"))
   :O4 (o4-cluster-claims-from (get edges "cascade/cluster-member"))
   :O5 (o5-hole-claims-from (get edges "cascade/hole-target"))})

(defn standards-map
  "The one-queryable-self-account standards, each a reading that can be false.
   s1-s4 have no honest check yet: they were hardcoded literals (Joe, 2026-11-05:
   'true by construction cannot continue'), so they now report themselves as
   :unchecked with the reason, and the probe counts :unchecked as not passing.
   s4 in particular must not pass on an empty cascade/hole-target section that
   nothing writes. s5-composed is genuinely computed: the cascade is one graph
   only if some mission node carries both an O1 and an O4 claim."
  [{:keys [o1 o4 o5 hole-edges now-ms]}]
  (let [o5-checked-none? (zero? (count o5))]
    {:s1-regenerates {:state :unchecked
                      :reason "regeneration is not distinguished from answering: a fresh response proves the endpoint answered, not that the map regenerates from the substrate"}
     :s2-evidence {:state :unchecked
                   :reason "claims are not yet inspected for carried evidence; no check exists"}
     :s3-reconstitution {:state :unchecked
                         :reason "no reconstitution exercise exists; nothing is rebuilt from the page alone"}
     :s4-honest-holes (if (and o5-checked-none? (seq hole-edges))
                        {:state :unchecked :reason "hole-target edges present but not yet checked for honesty"}
                        {:state :unchecked
                         :reason "no producer writes cascade/hole-target edges; an empty section is not evidence of zero holes"})
     ;; s4-evidence stays a typed transport fact: fetch-edges is loud on failure,
     ;; so reaching here with an empty o5 means the authoritative query SUCCEEDED
     ;; and found zero hole-target edges — a checked none at transport level
     ;; (workshop/honest-holes-gate-composed-claims). The evidence rides in-band.
     :s4-evidence (if o5-checked-none?
                    {:checked "cascade/hole-target"
                     :holes-found 0
                     :basis "authoritative query succeeded; empty result is checked, not unread (transport-level only: no producer writes this type yet)"
                     :as-of (java.time.Instant/ofEpochMilli now-ms)}
                    {:checked "cascade/hole-target"
                     :holes-found (count o5)
                     :basis "hole-target edges present"})
     :s5-composed (pos? (count (set/intersection o1 o4)))}))

(defn cascade-real-summary
  "A live snapshot of the COMPOSING cascade for the operator view: per-dimension
   claim counts, the cross-dimension shared-mission-node overlaps (the composition
   — non-vacuous overlap means the cascade is one graph, not seven pictures), the
   honest holes, the canonical spine size, and :consistent?. Computed live from the
   substrate-2 extractors — this is the 'real data' the pipeline-pattern-cascade
   sketch is made of."
  []
  (let [edges (summary-edge-map)
        claims-by-dim (summary-claims edges)
        claims (mapcat identity (vals claims-by-dim))
        o1  (mission-nodes #(get claims-by-dim :O1))
        o3  (mission-nodes #(get claims-by-dim :O3))
        o4  (mission-nodes #(get claims-by-dim :O4))
        o5  (mission-nodes #(get claims-by-dim :O5))
        n   (fn [a b] (count (set/intersection a b)))]
    {:as-of-ms     (System/currentTimeMillis)
     :consistent?  (claims-consistent? claims)
     :dimensions   (into {} (map (fn [[dim xs]] [dim (count xs)]) claims-by-dim))
     :spine        {:canonical-mission-nodes (count (set/union o1 o3 o4 o5))
                    ;; zero is a measured zero: the reads above succeeded
                    :canonical-excursion-nodes (count (nodes-of-type :excursion claims))
                    :canonical-ticket-nodes (count (nodes-of-type :ticket claims))
                    :O1-missions (count o1) :O4-missions (count o4)}
     :composition  {:O1xO4 (n o1 o4) :O5xO1 (n o5 o1) :O4xO3 (n o4 o3) :O1xO3 (n o1 o3)}
     :holes        (frequencies (keep #(prop (:hx/props %) :hole-kind)
                                      (get edges "cascade/hole-target")))
     :standards    (standards-map {:o1 o1 :o4 o4 :o5 o5 :hole-edges (get edges "cascade/hole-target")
                                   :now-ms (System/currentTimeMillis)})
     :owners       {:O1 "claude-2" :O3 "claude-4" :O4 "claude-10"
                    :O5 "claude-4" :O7 "claude-10" :O2 "claude-1"}}))

;; ---------------------------------------------------------------------------
;; live graph — the per-section STRUCTURE the cascade BODY renders
;; `cascade-real-summary` gives the header METADATA (counts/overlaps); this gives
;; the BODY (the real nodes+edges each dimension contributes), so the pipeline-
;; pattern-cascade SVG regenerates from live data instead of the hand-built sketch
;; (C-cascade-real §7 DISSOLUTION, Checklist B). Pure section-fns take edges so they
;; are unit-testable without the substrate; the `cascade-real-graph` aggregator fetches.
;; ---------------------------------------------------------------------------

(defn- endpoints-of [e] (mapv str (:hx/endpoints e)))
(defn- mission-ep [eps] (some #(when (re-find #"-d/mission/" %) %) eps))

(defn- doc-files
  []
  (let [root (io/file code-root)]
    (if-not (.exists root)
      []
      (->> (.listFiles root)
           (filter #(.isDirectory ^java.io.File %))
           (filter #(str/starts-with? (.getName ^java.io.File %) "futon"))
           (remove #(= "futon7" (.getName ^java.io.File %)))
           ;; Walk only <repo>/holes — every M-/E-/T- doc lives there, and a full
           ;; repo file-seq costs ~2.2s (traverses .venv/node_modules/data;
           ;; measured 2026-07-05) on an endpoint the page now polls every 60s.
           (map #(io/file ^java.io.File % "holes"))
           (filter #(.exists ^java.io.File %))
           (mapcat file-seq)
           (filter #(.isFile ^java.io.File %))
           (filter #(re-matches #"[MET]-[^/]+\.md" (.getName ^java.io.File %)))
           (remove #(str/includes? (.getPath ^java.io.File %) "/.git/"))
           vec))))

(defn- repo-name
  [^java.io.File file]
  (let [root-path (.toPath (.getCanonicalFile (io/file code-root)))
        path (.toPath (.getCanonicalFile file))]
    (some-> (.relativize root-path path)
            (.getName 0)
            str)))

(defn- doc-row
  [^java.io.File file]
  (let [stem (str/replace (.getName file) #"\.md$" "")]
    {:stem stem
     :kind (cond (str/starts-with? stem "E-") "excursion"
                 (str/starts-with? stem "T-") "ticket"
                 :else "mission")
     :repo (repo-name file)
     :path (.getCanonicalPath file)
     :mtime-ms (.lastModified file)}))

(defn- clock-stem-key
  [x]
  (let [tail (some-> x str (str/split #"/") last)]
    (some-> tail
            (str/replace #"(?i)^[MECT]-" "")
            str/lower-case)))

(defn- durable-clocked-stems
  [edges]
  (set (keep (fn [e]
               (some (fn [ep]
                       (let [s (str ep)]
                         (when (re-find #"-d/(mission|excursion|ticket|campaign)/" s)
                           (clock-stem-key s))))
                     (:hx/endpoints e)))
             edges)))

(defn- live-clocked-stems
  []
  (set (keep (fn [[agent-id info]]
               (let [clock (:clock (clock-store/current-state agent-id (:session-id info)))]
                 (clock-stem-key (or (:mission-id clock)
                                     (:excursion-id clock)
                                     (:campaign-id clock)))))
             (:agents (registry/registry-status)))))

(defn tickets-section
  "Recent unclocked mission/excursion/ticket docs. Read-only; degrades empty if disk,
   lineage, or live-clock state hiccups."
  ([] (tickets-section (fetch-edges "clock/clocked-on")))
  ([clock-edges]
   (try
     (let [clocked (set/union (durable-clocked-stems clock-edges)
                              (live-clocked-stems))
           items (->> (doc-files)
                      (map doc-row)
                      (remove #(contains? clocked (clock-stem-key (:stem %))))
                      (sort-by :mtime-ms >)
                      vec)]
       {:count-total (count items)
        :items items})
     (catch Throwable _
       {:count-total 0
        :items []}))))

(def ^:private self-rules
  "Witness rules that record the agent's own act: its own edits to a doc, or
   the War Machine's own selection."
  #{"agent-edit-activity" "selection-decision"})

(defn dispatched-by
  "Who put the agent on this target, as the clock edge's WITNESS says: the
   named caller (`:evidence :caller-id`, an inherited clock), `:self` when the
   rule records the agent's own act, else `{:absent :caller-not-recorded}` --
   a dispatch receipt without a caller, and a clock decision from a named
   target, which reads the same for a Kimi seat's requisition and an agent's
   own turn. Nothing is looked up elsewhere: the field is what the witness
   says, roster or not."
  [props]
  (let [w (prop props :witness)
        caller (some-> (prop w :evidence) (prop :caller-id) str not-empty)]
    (cond
      caller                          caller
      (self-rules (str (prop w :rule))) :self
      :else                           {:absent :caller-not-recorded})))

(defn lineage-section
  "O3 — agent→target clock edges (who/which session is on each mission/excursion/
   campaign), most-recent-first. Target = the canonical non-agent endpoint;
   `:dispatched-by` as the edge's witness records it (`dispatched-by`)."
  [edges]
  (->> (for [e edges
             :let [eps    (endpoints-of e)
                   p      (:hx/props e)
                   agent  (some #(when (str/starts-with? % "agent:") %) eps)
                   target (some #(when-not (str/starts-with? % "agent:") %) eps)]
             :when (and agent target)]
         {:agent agent :target target
          :session (some-> (prop p :session-id) str)
          :at (or (prop p :clocked-at-ms) 0)
          :dispatched-by (dispatched-by p)})
       (sort-by :at >)
       vec))

(defn cluster-section
  "O4 — cluster→mission member edges (the upward structure: mission clusters)."
  [edges]
  (vec (for [e edges
             :let [eps     (endpoints-of e)
                   cluster (some #(when (str/starts-with? % "cascade/cluster/") %) eps)
                   mission (mission-ep eps)]
             :when (and cluster mission)]
         {:cluster cluster :mission mission})))

(defn hole-section
  "O5 — hole→target edges + hole kind (the honest holes: where the cascade, read
   from its own data, asks for the next work)."
  [edges]
  (vec (for [e edges
             :let [eps    (endpoints-of e)
                   hole   (some #(when (str/starts-with? % "cascade/hole/") %) eps)
                   target (some #(when-not (str/starts-with? % "cascade/hole/") %) eps)]
             :when (and hole target)]
         {:hole hole :target target :kind (prop (:hx/props e) :hole-kind)})))

(defn arrow-section
  "O1 — mined-move have→want arrows on the mission spine (kept `:mined-structural`,
   NOT laundered as proofs). Carries the move-class + ΔG so the render can show the
   self-loop / low-ΔG honesty (the Q-B magnet-quality finding is visible in the data)."
  [edges]
  (vec (for [e    edges
             :let [eps  (endpoints-of e)
                   have (first eps) want (second eps)
                   p    (:hx/props e)]
             :when (and have (re-find #"-d/mission/" have) (not (re-find #"-head$" have)))]
         {:have have :want want
          :move-class (prop p :move-class) :delta-g (prop p :delta-g)})))

(defn held-section
  "D2 — held/on-mission edges (parked work per canonical mission — the deferral
   ledger, the 'what should wake up now' surface). Namespaced props read directly."
  [edges]
  (vec (for [e    edges
             :let [eps     (endpoints-of e)
                   item    (some #(when (str/starts-with? % "held/item/") %) eps)
                   mission (mission-ep eps)
                   p       (:hx/props e)]
             :when (and item mission)]
         {:held item :mission mission
          :registry (get p :held/source-registry)
          :reason   (get p :held/reason)})))

(defn- qprop
  "Like `prop`, but for NAMESPACED prop keys. `prop` falls back to `(name k)`,
   which turns `:pattern/ref` into \"ref\" and misses; try the keyword first, then
   the fully-qualified string form."
  [props k]
  (or (get props k) (get props (subs (str k) 1))))

(defn- unwrap-endpoint
  "Mission-scope endpoints are stored as STRINGIFIED role maps
   (`{:role :entity, :entity-id \"futon3c-d/mission/...\"}`), not bare ids. Pull the
   id out; pass anything already bare straight through. Positional splitting is
   NOT safe here -- a `:role :concept` endpoint can precede the pattern one."
  [ep]
  (let [s (str ep)]
    (or (second (re-find #":entity-id\s+\"([^\"]+)\"" s)) s)))

(defn- library-pattern-id
  "`futon3/library/<ns>/<name>.flexiarg` -> `<ns>/<name>`, the cascade's pattern
   id-space (the same `<ns>/<name>` the library and the `@why` cascade use)."
  [ref]
  (when ref (second (re-find #"/library/(.+)\.flexiarg$" (str ref)))))

(defn mission-pattern-section
  "O4-backlink — mission→pattern crosslinks, read from the LIVE
   `mission-scope/pattern` binder edges. This used to fetch `cascade/mission-pattern`,
   a type nothing writes, so the layer rendered 0 while 115 real edges sat under the
   binder's own name (fixed 2026-08-23).

   Each edge carries the canonical mission node on its `:role :entity` endpoint and
   the library path in `:pattern/ref`; the pattern is keyed `<ns>/<name>` so it
   shares the library's id-space and composes with the mission spine.

   `:state` rides along rather than being filtered on: `:detached` means the pattern
   NODE is absent from substrate-2 (`:pattern-node-not-found`) while the citation
   itself is real — an honest hole in the node layer, not a bad row. Pure: EDGES → rows."
  [edges]
  (vec (for [e    edges
             :let [p       (:hx/props e)
                   eps     (map unwrap-endpoint (:hx/endpoints e))
                   mission (mission-ep eps)
                   pattern (library-pattern-id (qprop p :pattern/ref))]
             :when (and mission pattern)]
         {:mission mission :pattern pattern
          :ident    (qprop p :pattern/ident)
          :state    (qprop p :pattern/state)
          ;; every mission-scope pattern binder IS an applied citation; the
          ;; renderer buckets on this key.
          :relation "applied"})))

(def ^:private graph-fetch-deadline-ms
  "Wall-clock ceiling for the WHOLE graph, per walk. Without it `.get` blocks
   forever: the per-page deadline bounds one page, and the longest cursor chain
   is 10 pages, so a stalled substrate could hold this endpoint open for
   minutes with no upper bound at all. A walk that blows this becomes an honest
   :failed section like any other read failure -- the point of per-section
   degradation is that a slow dimension costs that dimension, not the page."
  90000)

(def ^:dynamic graph-fetch-workers
  "Concurrent typed walks. Dynamic so worker count can be A/B-measured against
   the live store in one window instead of across two edits under different
   load -- which is the only way to tell a real speedup from drift."
  2)

(def ^:private graph-hyperedge-types
  ;; Longest cursor chain first: submitting the 971-row pattern walk after
  ;; the short walks adds their latency directly to the critical path.
  ["mission-scope/pattern"
   "code/v05/mined-move"
   "held/on-mission"
   "cascade/cluster-member"
   "clock/clocked-on"
   "cascade/hole-target"
   "mine/meme"])

(def graph-cache-max-age-ms
  "Backstop re-walk interval: edits that leave a type's COUNT unchanged
  (endpoint/prop changes on existing edges) are invisible to the count
  signal, so no entry is reused forever. 30 minutes."
  (* 30 60 1000))

(def ^:dynamic *graph-count-reader*
  "fn [hx-type] -> exact row count; throws on failure. Injectable so the
  graph cache tests need no futon1b. Default: substrate/hyperedge-count."
  (fn [hx-type]
    (substrate/hyperedge-count hx-type {:timeout-ms 5000})))

;; hx-type -> {:count n :edges [...] :fetched-at-ms t} for the last GOOD walk,
;; or {:failed? true} after a walk failed with no good entry to keep. The edges
;; are reused verbatim while the type's count signal has not moved.
(defonce graph-section-cache (atom {}))

;; stale-while-revalidate (live failure 2026-11-05: an over-age mission-scope/
;; pattern entry forced a ~172 s rewalk in the REQUEST path against the 90 s
;; deadline; every 1-min publish of the WIP page timed out for 24 minutes).
;; A type due only for revalidation is served from cache immediately, marked
;; stale, and refreshed in the background — at most one refresh per type.
(def ^:dynamic graph-refresh-deadline-ms
  "Ceiling for one BACKGROUND refresh walk. No request waits on it; generous
   (10 min) because the point is to survive slow-store windows, not to bound
   the user-visible request. Dynamic so tests can shrink it."
  (* 10 60 1000))

(defonce graph-refresh-in-flight (atom #{}))

(defn- daemon-factory [thread-name]
  (let [default-factory (Executors/defaultThreadFactory)]
    (reify java.util.concurrent.ThreadFactory
      (newThread [_ runnable]
        (doto (.newThread default-factory runnable)
          (.setDaemon true)
          (.setName thread-name))))))

;; Two pools, never one: a refresh task waits on its walk, so if both ran on
;; one bounded pool, two due sections would hold both threads waiting on walks
;; queued behind them (all sections age out together). The supervisor pool is
;; unbounded but holds at most one task per type (graph-refresh-in-flight);
;; the walk pool bounds the load on futon1b.
(defonce graph-refresh-pool
  (Executors/newCachedThreadPool (daemon-factory "graph-section-refresh")))

(defonce graph-refresh-walk-pool
  (Executors/newFixedThreadPool 2 (daemon-factory "graph-section-refresh-walk")))

(defn- start-graph-refresh
  "Start at most one background refresh of HX-TYPE. The refresh walks with no
   request deadline and swaps the cache only on success — a failed refresh
   leaves the stale-but-good entry to be served again on the next call."
  [hx-type]
  (let [before @graph-refresh-in-flight]
    (when (and (not (contains? before hx-type))
               (compare-and-set! graph-refresh-in-flight before (conj before hx-type)))
      (.submit graph-refresh-pool
               ^Callable
               (fn []
                 (try
                   (let [future (.submit graph-refresh-walk-pool ^Callable #(fetch-edges hx-type))
                         edges  (try (vec (.get future graph-refresh-deadline-ms TimeUnit/MILLISECONDS))
                                     (catch TimeoutException _
                                       (.cancel future true)
                                       (throw (ex-info (str "refresh walk exceeded "
                                                            graph-refresh-deadline-ms "ms")
                                                       {:hyperedge-type hx-type}))))]
                     (swap! graph-section-cache assoc hx-type
                            {:count (count edges)
                             :edges edges
                             :fetched-at-ms (System/currentTimeMillis)}))
                   (catch Throwable _          ;; keep the stale-but-good entry
                     )
                   (finally
                     (swap! graph-refresh-in-flight disj hx-type))))))))

(defn- graph-count-reads
  "Parallel exact-count reads. Returns {hx-type count} for the types whose
  count read succeeded; a failed read is simply absent, which makes the
  caller fall back to walking that type (today's behaviour)."
  [types]
  (let [pool (Executors/newFixedThreadPool 4)]
    (try
      (let [tasks (into {}
                        (map (fn [hx-type]
                               [hx-type (.submit pool ^Callable
                                                  (fn []
                                                    (try {:count (*graph-count-reader* hx-type)}
                                                         (catch Throwable _ nil))))]))
                        types)]
        (into {}
              (keep (fn [[hx-type task]]
                      (try
                        (when-let [result (some-> (.get task 10000 TimeUnit/MILLISECONDS)
                                                  :count)]
                          [hx-type (long result)])
                        (catch Throwable _
                          (.cancel task true)
                          nil))))
              tasks))
      (finally
        (.shutdownNow pool)))))

(defn- graph-stale-reason
  "When HX-TYPE's good entry is due only for revalidation, the reason keyword
   (:count-moved, :age past the max-age backstop, :count-read-failed), else
   nil. Only a missing entry or a failed placeholder walks in the request path."
  [entry count-now now-ms]
  (when (and entry (not (:failed? entry)))
    (cond
      (nil? count-now) :count-read-failed
      ;; a moved count with a good entry is also served stale and refreshed in
      ;; the background: the pattern section's walk takes ~172 s against the
      ;; 90 s request deadline, so a request-path rewalk can never succeed
      ;; (2026-11-05, 01:00Z, count 2146 -> 2147)
      (not= (:count entry) count-now) :count-moved
      (> (- now-ms (:fetched-at-ms entry 0)) graph-cache-max-age-ms) :age
      :else nil)))

(defn- graph-walk-needed?
  "True when HX-TYPE must be re-walked IN THE REQUEST PATH: no cache entry, a
   failed placeholder, or a count that MOVED (a count read that merely failed
   is not a move — with a good entry that case is served stale and refreshed in
   the background instead)."
  [entry count-now]
  (boolean (or (nil? entry)
               (:failed? entry)
               (and (not (nil? count-now))
                    (not= (:count entry) count-now)))))

(defn graph-sections*
  "fetch-graph-sections with its cache accounting exposed. Returns
  {:fetches {hx-type {:status ... :edges [...]}} :cache {:reused [...] :rewalked [...]
                                                          :stale [...]}}.
  A type whose count signal has not moved reuses its cached edges; a type due
  only for revalidation (over-age with unmoved count, or a failed count read,
  with a good entry present) is served from cache at once, marked :stale? with
  :stale-reason and :stale-since-ms, and refreshed in the background (at most
  one refresh per type); a failed walk reports :failed exactly as an uncached
  walk would and never overwrites a good cache entry."
  []
  (let [now-ms   (System/currentTimeMillis)
        counts   (graph-count-reads graph-hyperedge-types)
        cached   @graph-section-cache
        stale    (keep (fn [hx-type]
                         (when-let [reason (graph-stale-reason (get cached hx-type)
                                                                (get counts hx-type) now-ms)]
                           [hx-type reason]))
                       graph-hyperedge-types)
        stale-t  (into #{} (map first) stale)
        rewalk   (filter (fn [hx-type]
                           (and (not (contains? stale-t hx-type))
                                (graph-walk-needed? (get cached hx-type) (get counts hx-type))))
                         graph-hyperedge-types)
        reuse    (remove (into #{} (concat stale-t rewalk)) graph-hyperedge-types)
        pool     (Executors/newFixedThreadPool (int graph-fetch-workers))]
    (try
      (let [tasks (into {}
                        (map (fn [hx-type]
                               [hx-type
                                (.submit pool
                                         ^Callable
                                         (fn []
                                           (try
                                             {:status :ok
                                              :hyperedge-type hx-type
                                              :edges (vec (fetch-edges hx-type))}
                                             (catch Throwable t
                                               {:status :failed
                                                :hyperedge-type hx-type
                                                :edges []
                                                :error {:message (or (ex-message t) (str t))
                                                        :class (.getName (class t))
                                                        :data (some-> (ex-data t) pr-str)}}))))]))
                        rewalk)
        results (into {}
                      (map (fn [[hx-type task]]
                             [hx-type
                              (try
                                (.get task graph-fetch-deadline-ms TimeUnit/MILLISECONDS)
                                (catch TimeoutException t
                                  (.cancel task true)
                                  {:status :failed
                                   :hyperedge-type hx-type
                                   :edges []
                                   :error {:message (str "walk exceeded " graph-fetch-deadline-ms
                                                         "ms graph deadline")
                                           :class (.getName (class t))
                                           :data nil}}))])
                           tasks))]
        ;; a successful walk updates the cache; a failed one never overwrites
        ;; a good entry — it only leaves a failed placeholder when there was
        ;; nothing good to keep, so the next call retries it
        (doseq [[hx-type result] results]
          (if (= :ok (:status result))
            (swap! graph-section-cache assoc hx-type
                   {:count (or (get counts hx-type) (count (:edges result)))
                    :edges (:edges result)
                    :fetched-at-ms (System/currentTimeMillis)})
            (when-not (and (contains? @graph-section-cache hx-type)
                           (not (:failed? (get @graph-section-cache hx-type))))
              (swap! graph-section-cache assoc hx-type {:failed? true}))))
        (doseq [hx-type stale-t] (start-graph-refresh hx-type))
        {:fetches (merge (into {} (map (fn [hx-type]
                                         [hx-type {:status :ok
                                                   :hyperedge-type hx-type
                                                   :edges (:edges (get cached hx-type))}]))
                               reuse)
                         (into {} (map (fn [[hx-type reason]]
                                         (let [entry (get cached hx-type)]
                                           [hx-type {:status :ok
                                                     :hyperedge-type hx-type
                                                     :edges (:edges entry)
                                                     :stale? true
                                                     :stale-reason (name reason)
                                                     :stale-since-ms (:fetched-at-ms entry)}])))
                                       stale)
                         results)
         :cache {:reused (vec (sort reuse))
                 :rewalked (vec (sort rewalk))
                 :stale (vec (sort stale-t))}})
      (finally
        (.shutdownNow pool)))))

(defn fetch-graph-sections
  "Fetch the graph's independent typed walks, reusing cached edges for any
  type whose exact-count change signal has not moved (and whose entry is under
  the max-age backstop). A type due only for revalidation is served from cache
  immediately, marked :stale?/:stale-reason, and refreshed in the background.
  Each result is explicit: an authoritative empty read is `:ok` with zero
  rows; a failed read is `:failed` with its type and cause.
  Cursor-linked pages within one `fetch-edges` call remain sequential."
  []
  (:fetches (graph-sections*)))

(defn- fetched-edges [fetches hx-type]
  (:edges (get fetches hx-type)))

(defn- fetch-status
  [fetches]
  (into {}
        (map (fn [[hx-type {:keys [status edges error stale? stale-reason stale-since-ms]}]]
               [hx-type (cond-> {:status status
                                 :hyperedge-type hx-type}
                          (= :ok status) (assoc :row-count (count edges))
                          stale? (assoc :stale? true
                                        :stale-reason stale-reason
                                        :stale-since-ms stale-since-ms)
                          error (assoc :error error))]))
        fetches))

(defn cascade-real-graph
  "The per-section STRUCTURE (nodes/edges) the pipeline-pattern-cascade BODY renders,
   from live substrate-2 rows — the reconstruction of the cascade itself, not just the
   header counts `cascade-real-summary` gives. Each section is one dimension's real data;
   an unlanded dimension is empty (honest). `:patterns` names the still-open PSR
   mission→pattern back-link gap rather than fabricating edges: the pattern NODES are
   landed + queryable (`pattern/library`, `pattern/clause`) but not yet cited into the
   cascade body (O4 enrichment, post-campaign)."
  []
  (let [{fetches :fetches cache :cache} (graph-sections*)
        lineage  (lineage-section (fetched-edges fetches "clock/clocked-on"))
        clusters (cluster-section (fetched-edges fetches "cascade/cluster-member"))
        holes    (hole-section (fetched-edges fetches "cascade/hole-target"))
        arrows   (arrow-section (fetched-edges fetches "code/v05/mined-move"))
        held     (held-section (fetched-edges fetches "held/on-mission"))
        patterns (mission-pattern-section (fetched-edges fetches "mission-scope/pattern"))
        tickets  (tickets-section (fetched-edges fetches "clock/clocked-on"))]
    {:as-of-ms (System/currentTimeMillis)
     :section-status (fetch-status fetches)
     :cache cache
     :lineage  lineage
     :clusters clusters
     :holes    holes
     :arrows   arrows
     :held     held
     :tickets  tickets
     :patterns {:edges patterns
                ;; NO COUNT TYPED BY HAND (WR-8) — the note is derived from the rows.
                :note (let [state-of #(some-> (:state %) name)
                            linked   (count (filter #(= "linked" (state-of %)) patterns))
                            detached (- (count patterns) linked)]
                        (str "mission→pattern crosslinks from the live `mission-scope/pattern` "
                             "binder edges (NOT PSR/PUR, which are rarely used): "
                             (count patterns) " citations across "
                             (count (distinct (map :pattern patterns))) " patterns and "
                             (count (distinct (map :mission patterns))) " missions. "
                             linked " resolve to a live pattern node; " detached
                             " cite a library pattern that is absent from substrate-2 "
                             "(:pattern-node-not-found) — an honest hole in the pattern NODE "
                             "layer, not in the citations themselves."))}
     :counts   {:lineage (count lineage) :clusters (count clusters)
                :holes (count holes) :arrows (count arrows) :held (count held)
                :patterns (count patterns)}}))
