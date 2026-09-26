(ns futon3c.logic.cascade-real
  "STANDARD-VERIFY Level-1 logic model for campaign **C-cascade-real**
   (`futon3c/holes/campaigns/C-cascade-real.md`).

   `logic-model-before-code` at the campaign tier (the pattern STANDARD-VERIFY runs,
   per `library/mission-coherence/logic-model-before-code`; same idiom as
   `futon3c.logic.aif2-invariants`): the cascade-real DATA contract (STANDARD-ARGUE
   obligations O1–O7 + the ESCROW ledger E1–E6) is encoded as `core.logic`/`pldb`
   relations so the contract's **cross-dimension composition** is *executable* —
   checked for consistency BEFORE (and, when data lands, OVER) the population code.

   The crux is CHARTER standard 5 (*composed*): the seven data dimensions must write
   to **one shared ontology** — a node minted in one dimension (e.g. an O1 (have,want)
   arrow endpoint) must carry the SAME identity/type every other dimension that
   references it claims for it (O4 cites it, O3 attributes to it). If two dimensions
   claim one node-id with different types, the cascade is seven pictures, not one.

   A design is VERIFIED-AT-L1 iff:
     (a) the contract model yields ZERO composition / self-dependency / coverage
         violations, and
     (b) each adversarial model (one per check) is CAUGHT (see the test ns).
   The `:owner-holes` (O4/O5, owner-TBD) are reported as KNOWN honest holes — not
   failures (per the ratified STANDARD-ARGUE).

   When the first cars land (claude-2's promoted (have,want) arrows; claude-1's
   pinned mine; etc.), `db-from-data` maps the real rows onto the SAME relations and
   `verify` runs unchanged — the model becomes the live gate. L2 (DarkTower vs the
   Lean CT theory) is claude-10's complementary formal check."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations — the fact schema over the cascade-real contract
;; =============================================================================

(pldb/db-rel dimo             dim)               ; a data dimension exists (O1..O7)
(pldb/db-rel dim-ownero       dim owner)         ; its owner (or :TBD = an honest hole)
(pldb/db-rel claims-typeo     dim node-id type)  ; dim references node-id AS this type
(pldb/db-rel held-ono         dependent keystone); ESCROW: dependent slice held on keystone
(pldb/db-rel coverso          dim standard)      ; dim instantiates a CHARTER standard (1..5)

;; =============================================================================
;; Checks (pure relations over a db — "violations" are non-empty = a problem)
;; =============================================================================

(defn composition-violations
  "Node-ids claimed with CONFLICTING types across dimensions — the shared-ontology
   invariant (CHARTER standard 5). Empty ⇒ the dimensions compose into one cascade."
  [db]
  (->> (pldb/with-db db
         (l/run* [nid]
           (l/fresh [t1 t2 d1 d2]
             (claims-typeo d1 nid t1)
             (claims-typeo d2 nid t2)
             (l/!= t1 t2))))
       distinct vec))

(defn self-deps
  "ESCROW held-on self-loops (a slice held on itself) — a malformed dependency."
  [db]
  (vec (pldb/with-db db (l/run* [d] (held-ono d d)))))

(defn held-on-counts
  "keystone → number of dependents held on it."
  [db]
  (frequencies
   (pldb/with-db db (l/run* [ks] (l/fresh [dep] (held-ono dep ks))))))

(defn keystone
  "The node the MOST dependents hold on (the campaign's keystone = D4 = O1)."
  [db]
  (when-let [c (seq (held-on-counts db))]
    (key (apply max-key val c))))

(defn uncovered-standards
  "CHARTER standards (of ALL-STANDARDS) that NO dimension instantiates — a gap in
   the cascade-real property's coverage."
  [db all-standards]
  (let [covered (set (pldb/with-db db (l/run* [s] (l/fresh [d] (coverso d s)))))]
    (vec (remove covered all-standards))))

(defn owner-holes
  "Dimensions with owner :TBD — reported as KNOWN honest holes, not failures."
  [db]
  (vec (pldb/with-db db (l/run* [d] (dim-ownero d :TBD)))))

(def all-standards
  "The five CHARTER standards (1 generated · 2 grounded · 3 queryable · 4 honest ·
   5 composed)."
  [1 2 3 4 5])

(defn verify
  "Run all L1 checks over DB. Consistent (the design passes L1) iff composition,
   self-dep and coverage violations are all empty; owner-holes are reported."
  [db]
  (let [comp (composition-violations db)
        selfd (self-deps db)
        uncov (uncovered-standards db all-standards)]
    {:composition-violations comp
     :self-deps selfd
     :keystone (keystone db)
     :uncovered-standards uncov
     :owner-holes (owner-holes db)
     :consistent? (and (empty? comp) (empty? selfd) (empty? uncov))}))

;; =============================================================================
;; The contract model (conforming) — STANDARD-ARGUE O1–O7 + ESCROW E1–E6
;; =============================================================================

(defn add-facts [db facts]
  (reduce (fn [d [rel & args]] (apply pldb/db-fact d rel args)) db facts))

(def contract-facts
  "The cascade-real contract as facts. Owners per STANDARD-ARGUE; the shared nodes
   (MN mission, PN pattern, WN want, HN have/move, AN (have,want) arrow) carry ONE
   type across every dimension that references them (conforming composition); the
   held-on edges are the ESCROW ledger; coverage spans all five standards."
  [;; O1..O7 + owners (O4/O5 = honest holes)
   [dimo :O1] [dim-ownero :O1 :claude-2]
   [dimo :O2] [dim-ownero :O2 :claude-1]
   [dimo :O3] [dim-ownero :O3 :claude-4]
   [dimo :O4] [dim-ownero :O4 :TBD]
   [dimo :O5] [dim-ownero :O5 :TBD]
   [dimo :O6] [dim-ownero :O6 :claude-8]
   [dimo :O7] [dim-ownero :O7 :claude-10]
   ;; shared-ontology composition points — every claim of a node-id agrees on type
   [claims-typeo :O3 "MN" :mission] [claims-typeo :O4 "MN" :mission]      ; lineage ↔ citation
   [claims-typeo :O4 "PN" :pattern] [claims-typeo :O5 "PN" :pattern]
   [claims-typeo :O7 "PN" :pattern]
   [claims-typeo :O1 "WN" :want]    [claims-typeo :O2 "WN" :want]
   [claims-typeo :O4 "WN" :want]
   [claims-typeo :O1 "HN" :move]    [claims-typeo :O2 "HN" :move]
   [claims-typeo :O7 "HN" :move]
   [claims-typeo :O1 "AN" :arrow]   [claims-typeo :O7 "AN" :arrow]        ; D4 arrow ↔ fold wiring
   ;; ESCROW held-on edges (D4 = O1 is the keystone; the join is held on the mine O2)
   [held-ono :O4 :O1] [held-ono :O7 :O1] [held-ono :O6 :O1]               ; generator/fold/fwd ⟂ D4
   [held-ono :O1 :O2]                                                     ; D4 full join ⟂ canonical mine
   ;; CHARTER standard coverage (1 generated · 2 grounded · 3 queryable · 4 honest · 5 composed)
   [coverso :O4 1] [coverso :O1 2] [coverso :O3 3] [coverso :O5 4] [coverso :O7 5]])

(defn contract-db []
  (add-facts (pldb/db) contract-facts))

;; =============================================================================
;; Live-data entry point (used once the cars land — same relations, same checks)
;; =============================================================================

(defn db-from-data
  "Build a db from REAL cascade data. ROWS is a seq of fact-vectors in the same
   shape as `contract-facts` (a loader off substrate-2 / the mine / the arrows fills
   these). `verify` then runs unchanged — the L1 model becomes the live gate."
  [rows]
  (add-facts (pldb/db) rows))
