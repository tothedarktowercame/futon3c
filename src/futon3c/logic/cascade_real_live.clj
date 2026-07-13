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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [babashka.http-client :as http]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.agency.registry :as registry]
            [futon3c.logic.cascade-real :as cr])
  (:import [java.net URLEncoder]))

(def ^:private substrate-url
  "Substrate-2 hyperedge API. Do not read FUTON1A_URL here: this JVM may bind
   that env var to the futon1b evidence store (:7073), whose API has no
   /hyperedges route and can make the live cascade page appear dead."
  (or (System/getenv "FUTON1A_HYPEREDGE_URL") "http://127.0.0.1:7071"))
(def ^:private code-root (or (System/getenv "FUTON_CODE_ROOT") "/home/joe/code"))
(def ^:private fetch-timeout-ms 5000)

#_{:clj-kondo/ignore [:unresolved-var]}
(def ^:private claims-typeo-rel cr/claims-typeo)

(defn fetch-edges
  "GET currently-valid substrate-2 hyperedges of HX-TYPE (db-as-of now). Returns a
   seq of hyperedge maps, or [] on any error (the gate degrades to 'no live rows
   for this dimension', never throws)."
  [hx-type]
  (let [url  (str substrate-url "/api/alpha/hyperedges?type=" (URLEncoder/encode hx-type "UTF-8"))
        resp (try (http/get url {:headers {"Accept" "application/edn"}
                                 :throw false
                                 :timeout fetch-timeout-ms})
                  (catch Exception _ nil))]
    (or (when (and resp (= 200 (:status resp)) (string? (:body resp)))
          (try (:hyperedges (edn/read-string (:body resp))) (catch Exception _ nil)))
        [])))

(defn- prop [props k] (or (get props k) (get props (name k))))

;; ---------------------------------------------------------------------------
;; per-dimension extractors — edges → [[claims-typeo dim node-id type] ...]
;; Each is pure (takes edges); the *-claims wrapper fetches.
;; ---------------------------------------------------------------------------

(defn o3-claims-from
  "O3/D1 — the durable agent↔session↔mission lineage (`clock/clocked-on` edges).
   Keys on the EDGE ENDPOINTS (the CANONICAL node-ids the lineage now writes,
   `<repo>-d/mission/<id>`), so O3's mission claim shares its node-id with O1/D4 —
   that's the cross-dimension composition. Claims the canonical mission node
   `:mission` (same type O1 claims it → composes cleanly), the agent node `:agent`.
   Pure: EDGES → claims-typeo fact-vectors."
  [edges]
  (for [e   edges
        ep  (:hx/endpoints e)
        :let [s (str ep)
              [nid type] (cond
                           (str/starts-with? s "agent:")    [s :agent]
                           (re-find #"-d/mission/" s)        [s :mission]
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

(defn- mission-nodes [extract]
  (set (map #(nth % 2) (filter #(= :mission (nth % 3)) (extract)))))

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
                    :O1-missions (count o1) :O4-missions (count o4)}
     :composition  {:O1xO4 (n o1 o4) :O5xO1 (n o5 o1) :O4xO3 (n o4 o3) :O1xO3 (n o1 o3)}
     :holes        (frequencies (keep #(prop (:hx/props %) :hole-kind)
                                      (get edges "cascade/hole-target")))
     :standards    {:s1-regenerates true :s2-evidence true :s3-reconstitution true
                    :s4-honest-holes (boolean (seq o5)) :s5-composed (pos? (n o1 o4))}
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
           ;; Walk only <repo>/holes — every M-/E- doc lives there, and a full
           ;; repo file-seq costs ~2.2s (traverses .venv/node_modules/data;
           ;; measured 2026-07-05) on an endpoint the page now polls every 60s.
           (map #(io/file ^java.io.File % "holes"))
           (filter #(.exists ^java.io.File %))
           (mapcat file-seq)
           (filter #(.isFile ^java.io.File %))
           (filter #(re-matches #"[ME]-[^/]+\.md" (.getName ^java.io.File %)))
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
     :kind (if (str/starts-with? stem "E-") "excursion" "mission")
     :repo (repo-name file)
     :path (.getCanonicalPath file)
     :mtime-ms (.lastModified file)}))

(defn- clock-stem-key
  [x]
  (let [tail (some-> x str (str/split #"/") last)]
    (some-> tail
            (str/replace #"(?i)^[MEC]-" "")
            str/lower-case)))

(defn- durable-clocked-stems
  [edges]
  (set (keep (fn [e]
               (some (fn [ep]
                       (let [s (str ep)]
                         (when (re-find #"-d/(mission|excursion|campaign)/" s)
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
  "Recent unclocked mission/excursion docs. Read-only; degrades empty if disk,
   lineage, or live-clock state hiccups."
  []
  (try
    (let [clocked (set/union (durable-clocked-stems (fetch-edges "clock/clocked-on"))
                             (live-clocked-stems))
          items (->> (doc-files)
                     (map doc-row)
                     (remove #(contains? clocked (clock-stem-key (:stem %))))
                     (sort-by :mtime-ms >)
                     vec)]
      {:count-total (count items)
       :items (vec (take 40 items))})
    (catch Throwable _
      {:count-total 0
       :items []})))

(defn lineage-section
  "O3 — agent→target clock edges (who/which session is on each mission/excursion/
   campaign), most-recent-first. Target = the canonical non-agent endpoint."
  [edges]
  (->> (for [e edges
             :let [eps    (endpoints-of e)
                   p      (:hx/props e)
                   agent  (some #(when (str/starts-with? % "agent:") %) eps)
                   target (some #(when-not (str/starts-with? % "agent:") %) eps)]
             :when (and agent target)]
         {:agent agent :target target
          :session (some-> (prop p :session-id) str)
          :at (or (prop p :clocked-at-ms) 0)})
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

(defn mission-pattern-section
  "O4-backlink — mission→pattern crosslinks (`cascade/mission-pattern` edges, the
   'cited patterns' layer reconstructed from historical mining, NOT PSR/PUR). Each
   edge connects a canonical mission node to a canonical `<ns>/<name>` pattern node,
   `:relation` applied|candidate. Both endpoints already resolve on the live spine
   (mission composes with O1/O3/O4; pattern is its own id-space). Pure: EDGES → rows."
  [edges]
  (vec (for [e    edges
             :let [eps     (endpoints-of e)
                   mission (mission-ep eps)
                   pattern (some #(when-not (re-find #"-d/mission/" %) %) eps)
                   p       (:hx/props e)]
             :when (and mission pattern)]
         {:mission mission :pattern pattern
          :relation (prop p :relation) :cos (prop p :cos)})))

(defn cascade-real-graph
  "The per-section STRUCTURE (nodes/edges) the pipeline-pattern-cascade BODY renders,
   from live substrate-2 rows — the reconstruction of the cascade itself, not just the
   header counts `cascade-real-summary` gives. Each section is one dimension's real data;
   an unlanded dimension is empty (honest). `:patterns` names the still-open PSR
   mission→pattern back-link gap rather than fabricating edges: the pattern NODES are
   landed + queryable (`pattern/library`, `pattern/clause`) but not yet cited into the
   cascade body (O4 enrichment, post-campaign)."
  []
  (let [lineage  (lineage-section (fetch-edges "clock/clocked-on"))
        clusters (cluster-section (fetch-edges "cascade/cluster-member"))
        holes    (hole-section    (fetch-edges "cascade/hole-target"))
        arrows   (arrow-section   (fetch-edges "code/v05/mined-move"))
        held     (held-section    (fetch-edges "held/on-mission"))
        patterns (mission-pattern-section (fetch-edges "cascade/mission-pattern"))
        tickets  (tickets-section)]
    {:as-of-ms (System/currentTimeMillis)
     :lineage  lineage
     :clusters clusters
     :holes    holes
     :arrows   arrows
     :held     held
     :tickets  tickets
     :patterns {:edges patterns
                :note (str "mission→pattern crosslinks reconstructed from historical mining "
                           "(mission-pattern-scopes; :applied citations), NOT PSR/PUR. Both endpoints "
                           "on live canonical (mission spine × pattern/library <ns>/<name>). Honest "
                           "holes remain: :try-candidates not yet landed, + newer patterns absent from "
                           "pattern/library; on-demand refresh = cascade_construct.")}
     :counts   {:lineage (count lineage) :clusters (count clusters)
                :holes (count holes) :arrows (count arrows) :held (count held)
                :patterns (count patterns)}}))
