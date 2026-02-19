(ns scripts.proof-p1-stepper-golden-path
  "Framing-first golden path for First Proof Problem 1.

   P1 is used as the example instance:
     Are mu (Phi^4_3) and T_psi^* mu equivalent for smooth nonzero psi?

   The script now emphasizes a general reasoning pattern instead of
   a P1-specific answer script:
     1) classify claim type
     2) create precondition/obstruction obligations
     3) run obstruction-first corpus scan
     4) gate progress on framing status

   This avoids the false-premise trap:
   deriving valid downstream steps from an unchecked premise.

   Usage:
     clojure -M scripts/proof-p1-stepper-golden-path.clj

   See also:
     data/first-proof/problem1-writeup.md   — our (wrong) attempt
     data/first-proof/problem1-solution.md  — our (wrong) detailed proof
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
  (let [result (tools/execute-tool be :corpus-check
                 [q {:top-k 5
                     :sources [:futon3a :stackexchange-local]}])]
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
             "singular" "renormal" "equivalence" "quasi-invariant"
             "cameron-martin" "regularity"]))))

(defn framing-signal-summary [neighbors]
  (let [hits (filter framing-signal-hit? neighbors)]
    {:total-neighbors (count neighbors)
     :hit-count (count hits)
     :hit-rate (if (seq neighbors)
                 (/ (double (count hits)) (double (count neighbors)))
                 0.0)}))

;; ============================================================
;; 1. Bootstrap: init P1
;; ============================================================

(def init (pb/init-problem! cwd "P1"
  "Let mu be the Phi^4_3 measure on T^3, psi smooth nonzero. Are mu and T_psi^* mu equivalent?"
  "Rigorous proof of YES (with RN derivative) or NO (with distinguishing event)"))

(println "=== P1 initialized, version:" (:proof/version init))

;; ============================================================
;; 2. Ledger: generic framing obligations (P1-instantiated)
;; ============================================================

(upsert-obligation! "P1" "L-claim-type"
  "Classify claim type (equivalence/null-set/measure-class)"
  #{}
  #{"L-preconditions" "L-obstruction-scan"})

(upsert-obligation! "P1" "L-preconditions"
  "Verify premise-level conditions before any bridge argument"
  #{"L-claim-type"}
  #{"L-bridge"})

(upsert-obligation! "P1" "L-obstruction-scan"
  "Search for known obstruction/counterexample mechanisms"
  #{"L-claim-type"}
  #{"L-bridge"})

(upsert-obligation! "P1" "L-bridge"
  "Only after preconditions+obstructions: evaluate candidate bridge lemmas"
  #{"L-preconditions" "L-obstruction-scan"}
  #{"L-conclusion"})

(upsert-obligation! "P1" "L-conclusion"
  "Conclude equivalence or singularity with explicit framing evidence"
  #{"L-bridge"}
  #{})

(print-ledger! "P1")

;; ============================================================
;; 3. Cycle 1: Observe phase (framing checkpoint)
;; ============================================================

(println "\n=== Starting Cycle 1: observe L-preconditions")
(def cycle1 (tools/execute-tool be :cycle-begin ["P1" "L-preconditions"]))
(def cycle-id (get-in cycle1 [:result :cycle/id]))
(println "  cycle:" cycle-id "phase:" (get-in cycle1 [:result :cycle/phase]))

(println "\n--- WRONG PATH (our writeup) ---")
(println "  Jumped directly to bridge argument before framing was complete:")
(println "    - assumed mu ~ mu_0")
(println "    - applied Cameron-Martin downstream")
(println "    - reached a formally valid chain from an unchecked premise")

;; ============================================================
;; 4. GOLDEN PATH: obstruction-first scan + framing gate
;; ============================================================

(println "\n--- GOLDEN PATH (stepper) ---")
(def framing-queries
  ["preconditions for proving measure-class equivalence in interacting models"
   "known obstructions or counterexamples to measure equivalence under smooth shift"
   "false-premise trap in measure-theoretic equivalence arguments"
   ;; P1 instance (example, not the general policy itself):
   "Phi^4_3 measure equivalent to Gaussian free field measure"])

(def all-neighbors
  (vec (mapcat run-corpus-query! framing-queries)))

(def signal (framing-signal-summary all-neighbors))

(println "\n--- FRAMING CHECKPOINT ---")
(println (format "  neighbors=%d framing-hits=%d hit-rate=%.2f"
                 (:total-neighbors signal) (:hit-count signal) (:hit-rate signal)))

(if (zero? (:hit-count signal))
  (do
    (println "  status: UNRESOLVED")
    (println "  action: keep L-preconditions open; do not promote L-bridge.")
    (println "  next: acquire stronger corpus or external references before derivation."))
  (do
    (println "  status: PARTIAL SIGNAL")
    (println "  action: reframe blocker around surfaced obstructions/preconditions,")
    (println "          then test bridge lemmas against those constraints.")))

;; ============================================================
;; 5. Record failed route for honesty / replay
;; ============================================================

(println "\n=== Recording failed route (our wrong approach)")
(tools/execute-tool be :failed-route-add
  ["P1" {:route/blocker-id "L-preconditions"
         :route/approach
         "Attempt bridge argument before completing precondition and obstruction checks"
         :route/structural-obstruction
         (str "False-premise trap: a valid downstream derivation can still be wrong "
              "if premise-level equivalence assumptions are unchecked.")
         :route/failure-reason
         (str "Bypassing framing checkpoints caused us to treat an unresolved "
              "assumption as settled and derive a confident but wrong conclusion.")
         :route/evidence-refs ["data/first-proof/problem1-writeup.md"
                               "data/first-proof/problem1-solution.md"
                               "data/first-proof/comparison-with-official-solutions.md"]}])

(println "\n=== P1 framing-first golden path complete.")
(println "=== Principle: no bridge argument before precondition+obstruction checks.")
