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
            [clojure.string :as str]
            [babashka.http-client :as http]
            [futon3c.logic.cascade-real :as cr])
  (:import [java.net URLEncoder]))

(def ^:private FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))

(defn fetch-edges
  "GET currently-valid substrate-2 hyperedges of HX-TYPE (db-as-of now). Returns a
   seq of hyperedge maps, or [] on any error (the gate degrades to 'no live rows
   for this dimension', never throws)."
  [hx-type]
  (let [url  (str FUTON1A "/api/alpha/hyperedges?type=" (URLEncoder/encode hx-type "UTF-8"))
        resp (try (http/get url {:headers {"Accept" "application/edn"} :throw false})
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
   Attributes to mission/campaign nodes (shared with O4 when it lands) and agent
   nodes. Pure: EDGES → claims-typeo fact-vectors."
  [edges]
  (for [e    edges
        :let [props (:hx/props e)]
        [nid type] (keep identity
                         [(when-let [m (prop props :mission-id)]   [(str "mission:" m) :mission])
                          (when-let [c (prop props :campaign-id)]  [(str "campaign:" c) :campaign])
                          (when-let [a (prop props :agent-id)]     [(str "agent:" a) :agent])])]
    [cr/claims-typeo :O3 nid type]))

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
    [cr/claims-typeo :O2 (str nid) :meme]))

(defn- o2-mine-claims [] (o2-meme-claims-from (fetch-edges "mine/meme")))

(def extractors
  "Registry of LANDED-dimension extractors (dim → 0-arg fn → claims-typeo facts).
   Add an entry as each RUN/DELIVER car lands its substrate-2 rows. O2 is
   pre-wired against `mine/meme`; it lights up automatically when claude-1 lands
   the pinned mine rows."
  {:O3 o3-lineage-claims
   :O2 o2-mine-claims})

;; ---------------------------------------------------------------------------
;; the live gate
;; ---------------------------------------------------------------------------

(defn live-facts
  "The contract SCAFFOLD (dim/owner/held-on/covers) + LIVE `claims-typeo` from the
   landed dimensions' real substrate-2 rows. Unlanded dimensions contribute no
   claims (honest — their data isn't there yet)."
  []
  (concat
   (remove (fn [[rel]] (= rel cr/claims-typeo)) cr/contract-facts)
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
