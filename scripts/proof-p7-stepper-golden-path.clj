(ns scripts.proof-p7-stepper-golden-path
  "Framing-first golden path for First Proof Problem 7.

   P7: Suppose Gamma is a uniform lattice in a real semisimple Lie group,
   and Gamma contains an element of order 2. Is it possible that Gamma is
   the fundamental group of a closed manifold whose universal cover is
   acyclic over Q?

   Our answer: Conditional YES (discharged CW complex, left manifold open).
   Official answer: NO.

   Error class A (same as P1): YES bias + missed deep obstruction.
   We correctly identified Fowler's criterion for finite CW complexes
   but assumed the manifold upgrade would work. The Novikov conjecture
   (assembly map injectivity for lattices in semisimple Lie groups)
   provides the definitive obstruction.

   The stepper should catch this via the framing-first gate:
     1) classify claim type — CW complex vs closed manifold
     2) precondition check — does Fowler extend to manifolds?
     3) obstruction scan — Novikov conjecture blocks surgery realization
     4) gate L-bridge on framing status

   Usage:
     clojure -M scripts/proof-p7-stepper-golden-path.clj

   See also:
     data/first-proof/problem7-writeup.md   — our (conditional YES) attempt
     data/first-proof/problem7-solution.md  — our detailed proof
     data/first-proof/comparison-with-official-solutions.md"
  (:require [clojure.string :as str]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.tools :as tools]))

(def cwd "/home/joe/code/futon6")

(def proof-be (pb/make-proof-backend {:cwd cwd} (tools/make-mock-backend)))
(def be proof-be)

(defn upsert-obligation!
  [problem-id item-id label depends-on unlocks]
  (tools/execute-tool be :ledger-upsert
    [problem-id item-id {:item/label label
                         :item/status :open
                         :item/depends-on depends-on
                         :item/unlocks unlocks
                         :item/artifact-paths []}]))

(defn print-ledger! [problem-id]
  (println "\n=== Ledger populated")
  (let [ledger (:result (tools/execute-tool be :ledger-query [problem-id]))]
    (doseq [[id item] (sort-by key ledger)]
      (println (format "  %s [%s] %s"
                       id (name (:item/status item)) (:item/label item))))))

(defn run-corpus-query! [q]
  (println "\n  [corpus-check] Query:" (pr-str q))
  (let [result (tools/execute-tool be :corpus-check [q {:top-k 5}])]
    (if (:ok result)
      (let [neighbors (get-in result [:result :neighbors])]
        (if (seq neighbors)
          (do
            (println "  Results:")
            (doseq [n neighbors]
              (println (format "    %d. %s (%.4f) %s"
                               (:rank n) (:id n) (:score n) (:title n ""))))
            neighbors)
          (do (println "  [no neighbors]") [])))
      (do
        (println "  [corpus-check unavailable:" (get-in result [:error :message]) "]")
        []))))

(defn framing-signal-hit? [neighbor]
  (let [s (str/lower-case (str (:id neighbor) " " (:title neighbor "")))]
    (boolean
      (some #(str/includes? s %)
            ["precondition" "assumption" "obstruction" "counterexample"
             "novikov" "assembly" "surgery" "manifold" "lattice"
             "torsion" "cw complex" "finiteness"]))))

(defn framing-signal-summary [neighbors]
  (let [hits (filter framing-signal-hit? neighbors)]
    {:total-neighbors (count neighbors)
     :hit-count (count hits)
     :hit-rate (if (seq neighbors)
                 (/ (double (count hits)) (double (count neighbors)))
                 0.0)}))

;; ============================================================
;; 1. Bootstrap: init P7
;; ============================================================

(def init (pb/init-problem! cwd "P7"
  (str "Gamma is a uniform lattice in a real semisimple Lie group with an "
       "element of order 2. Can Gamma be pi_1 of a closed manifold whose "
       "universal cover is Q-acyclic?")
  "Rigorous proof of YES (with construction) or NO (with obstruction)"))

(println "=== P7 initialized, version:" (:proof/version init))

;; ============================================================
;; 2. Ledger: framing obligations (P7-instantiated)
;; ============================================================

;; L-claim-type: the critical distinction — CW complex vs closed manifold.
;; Our writeup conflated these: discharged CW (Fowler) and assumed manifold
;; would follow. The stepper must force explicit classification.
(upsert-obligation! "P7" "L-claim-type"
  "Classify claim type: finite CW complex vs closed manifold (the problem asks for manifold)"
  #{}
  #{"L-preconditions" "L-obstruction-scan"})

;; L-preconditions: does Fowler's criterion give us what we need?
;; Fowler gives a finite CW complex, NOT a manifold. The gap between
;; "Gamma in FH(Q)" and "Gamma = pi_1(closed manifold with Q-acyclic cover)"
;; is the entire surgery theory problem.
(upsert-obligation! "P7" "L-preconditions"
  (str "Verify: Fowler gives CW complex, NOT manifold. "
       "Surgery upgrade requires: (1) Poincare complex structure, "
       "(2) normal map, (3) surgery obstruction vanishing. "
       "Does each step hold for lattices in semisimple Lie groups?")
  #{"L-claim-type"}
  #{"L-bridge"})

;; L-obstruction-scan: search for known obstructions to the CW→manifold upgrade.
;; The Novikov conjecture (assembly map injectivity) is the key obstruction.
;; Davis-Luck Z/2 exclusion theorem is also relevant (blocks integral version).
(upsert-obligation! "P7" "L-obstruction-scan"
  (str "Search for known obstructions to manifold realization of FH(Q) groups. "
       "Key: Novikov conjecture, assembly map injectivity, "
       "Davis-Luck Z/2 exclusion, surgery obstruction non-vanishing.")
  #{"L-claim-type"}
  #{"L-bridge"})

;; L-bridge: only after preconditions and obstructions are resolved.
;; In our wrong approach, we jumped to bridge arguments (surgery route analysis)
;; before establishing that the surgery obstruction actually vanishes.
(upsert-obligation! "P7" "L-bridge"
  (str "Only after preconditions+obstructions: evaluate surgery route. "
       "If obstruction scan surfaces Novikov-type blockers, "
       "the bridge argument shifts from YES-construction to NO-obstruction.")
  #{"L-preconditions" "L-obstruction-scan"}
  #{"L-conclusion"})

(upsert-obligation! "P7" "L-conclusion"
  "Conclude YES (with explicit manifold construction) or NO (with explicit obstruction)"
  #{"L-bridge"}
  #{})

(print-ledger! "P7")

;; ============================================================
;; 3. Cycle 1: Observe phase (framing checkpoint)
;; ============================================================

(println "\n=== Starting Cycle 1: observe L-claim-type + L-preconditions")
(def cycle1 (tools/execute-tool be :cycle-begin ["P7" "L-claim-type"]))
(def cycle-id (get-in cycle1 [:result :cycle/id]))
(println "  cycle:" cycle-id "phase:" (get-in cycle1 [:result :cycle/phase]))

(println "\n--- WRONG PATH (our writeup) ---")
(println "  1. Correctly identified Fowler's criterion → discharged CW complex (E2)")
(println "  2. Attempted five surgery approaches for manifold upgrade (S)")
(println "  3. Recommended rotation route: treated surgery obstruction as 'possibly vanishing'")
(println "  4. Concluded 'Conditional YES' — but the official answer is NO")
(println "  ERROR: conflated finite CW complex with closed manifold.")
(println "         The CW result is real; the manifold upgrade provably fails.")
(println "         Novikov conjecture → assembly map is injective for these lattices")
(println "         → surgery obstruction is nontrivial → no closed manifold exists.")

;; ============================================================
;; 4. GOLDEN PATH: obstruction-first scan + framing gate
;; ============================================================

(println "\n--- GOLDEN PATH (stepper) ---")

;; Generic framing queries (discipline signal — should fire from pattern library)
(def generic-queries
  ["preconditions for upgrading finite CW complex to closed manifold"
   "known obstructions to manifold realization from FH(Q) finiteness property"
   "false-premise trap: proving finiteness property when problem asks for manifold"])

;; Domain-specific queries (domain signal — needs topology/surgery corpus)
(def domain-queries
  ["Novikov conjecture obstruction to surgery realization for lattices in Lie groups"
   "assembly map injectivity blocks manifold realization"
   "Davis-Luck Z/2 exclusion theorem closed manifold fundamental group"
   "surgery obstruction non-vanishing for lattices with torsion in semisimple groups"])

(println "\n--- Generic framing queries (discipline signal) ---")
(def generic-neighbors
  (vec (mapcat run-corpus-query! generic-queries)))

(println "\n--- Domain-specific queries (domain signal) ---")
(def domain-neighbors
  (vec (mapcat run-corpus-query! domain-queries)))

(def all-neighbors (into generic-neighbors domain-neighbors))

(def signal (framing-signal-summary all-neighbors))

(println "\n--- FRAMING CHECKPOINT ---")
(println (format "  neighbors=%d framing-hits=%d hit-rate=%.2f"
                 (:total-neighbors signal) (:hit-count signal) (:hit-rate signal)))

;; Evaluate two-layer signal
(def generic-signal (framing-signal-summary generic-neighbors))
(def domain-signal-result (framing-signal-summary domain-neighbors))

(println (format "  discipline signal: %d/%d hits (%.2f)"
                 (:hit-count generic-signal)
                 (:total-neighbors generic-signal)
                 (:hit-rate generic-signal)))
(println (format "  domain signal: %d/%d hits (%.2f)"
                 (:hit-count domain-signal-result)
                 (:total-neighbors domain-signal-result)
                 (:hit-rate domain-signal-result)))

(cond
  ;; Best case: domain signal surfaces Novikov obstruction
  (pos? (:hit-count domain-signal-result))
  (do
    (println "  status: DOMAIN SIGNAL — specific obstruction surfaced")
    (println "  action: reframe from YES-construction to NO-obstruction argument.")
    (println "          Novikov conjecture → surgery obstruction nontrivial → answer is NO."))

  ;; Partial: discipline signal catches the CW/manifold conflation
  (pos? (:hit-count generic-signal))
  (do
    (println "  status: DISCIPLINE SIGNAL — framing gap detected")
    (println "  action: L-claim-type flags CW ≠ manifold. Do NOT assume surgery succeeds.")
    (println "          Keep L-preconditions open until manifold upgrade is proven.")
    (println "          This blocks L-bridge from promoting a YES-construction argument."))

  ;; Worst case: no signal at all
  :else
  (do
    (println "  status: UNRESOLVED — no corpus signal")
    (println "  action: keep L-preconditions open; do not promote L-bridge.")
    (println "  next: acquire topology/surgery corpus. Minimum seed:")
    (println "    - Novikov conjecture survey (e.g., Luck 2002, arXiv:math/0504564)")
    (println "    - Davis-Luck 2024 (arXiv:2303.15765) — Z/2 exclusion")
    (println "    - Bartels-Farrell-Luck (arXiv:1101.0469) — Farrell-Jones for lattices")))

;; ============================================================
;; 5. Record failed route (our wrong approach)
;; ============================================================

(println "\n=== Recording failed route (our wrong approach)")
(tools/execute-tool be :failed-route-add
  ["P7" {:route/blocker-id "L-preconditions"
         :route/approach
         (str "Discharged CW complex obligation (Fowler) then attempted surgery "
              "upgrade via five approaches (Wall surgery reflection/rotation, "
              "equivariant surgery reflection/rotation, orbifold resolution). "
              "Recommended rotation route, treating surgery obstruction as "
              "'possibly vanishing' and concluding Conditional YES.")
         :route/structural-obstruction
         (str "Conflated finite CW complex (FH(Q)) with closed manifold realization. "
              "The Novikov conjecture guarantees assembly map injectivity for "
              "cocompact lattices in semisimple Lie groups, which means the "
              "surgery obstruction is nontrivial. The CW→manifold upgrade "
              "provably fails for these groups — the answer is NO, not YES.")
         :route/failure-reason
         (str "YES bias: having successfully discharged the CW complex obligation "
              "(genuinely correct work), we assumed the manifold upgrade was "
              "'merely technical' rather than treating it as a potential blocker. "
              "The stepper's framing-first gate should have kept L-preconditions "
              "open until the surgery obstruction was proven to vanish — "
              "which it doesn't.")
         :route/evidence-refs ["data/first-proof/problem7-writeup.md"
                               "data/first-proof/problem7-solution.md"
                               "data/first-proof/comparison-with-official-solutions.md"]}])

;; ============================================================
;; 6. Comparison with P1: shared error pattern
;; ============================================================

(println "\n=== Class A error pattern (shared with P1)")
(println "  P1: assumed mu ~ mu_0 (measure equivalence) → wrong YES")
(println "  P7: assumed CW → manifold upgrade works   → wrong YES")
(println "  Both: valid downstream derivation from unchecked premise.")
(println "  Both: discipline signal (framing gate) would have paused.")
(println "  Both: domain signal (Hairer/Novikov) would have provided the answer.")
(println "  Stepper mechanism: L-preconditions stays OPEN → L-bridge BLOCKED.")

(println "\n=== P7 framing-first golden path complete.")
(println "=== Minimum ArXiv seed for P7 domain signal:")
(println "    - Luck 2002 survey, arXiv:math/0504564 (Novikov conjecture)")
(println "    - Davis-Luck 2024, arXiv:2303.15765 (Z/2 exclusion)")
(println "    - Bartels-Farrell-Luck 2011, arXiv:1101.0469 (Farrell-Jones for lattices)")
(println "=== Principle: no bridge argument before precondition+obstruction checks.")
