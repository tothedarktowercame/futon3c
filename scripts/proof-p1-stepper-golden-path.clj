(ns scripts.proof-p1-stepper-golden-path
  "Golden path reconstruction for First Proof Problem 1.

   Problem: Are mu (Phi^4_3) and T_psi^* mu equivalent for smooth nonzero psi?
   Our answer: YES (wrong).
   Official answer: NO — mutually singular.

   This script sets up the stepper for P1 and demonstrates where
   :corpus-check should catch the framing error. The goal is to
   calibrate the stepper: does the corpus contain enough signal
   to surface the Phi^4_3 / free field singularity at Step 1?

   Usage:
     clojure -M scripts/proof-p1-stepper-golden-path.clj

   See also:
     data/first-proof/problem1-writeup.md   — our (wrong) attempt
     data/first-proof/problem1-solution.md  — our (wrong) detailed proof
     data/first-proof/comparison-with-official-solutions.md"
  (:require [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.tools :as tools]))

(def cwd "/home/joe/code/futon6")

;; ============================================================
;; 1. Bootstrap: init P1
;; ============================================================

(def real-be nil) ;; TODO: wire real backend when running live
(def proof-be (pb/make-proof-backend {:cwd cwd} (tools/make-mock-backend)))

(def init (pb/init-problem! cwd "P1"
  "Let mu be the Phi^4_3 measure on T^3, psi smooth nonzero. Are mu and T_psi^* mu equivalent?"
  "Rigorous proof of YES (with RN derivative) or NO (with distinguishing event)"))

(println "=== P1 initialized, version:" (:proof/version init))

;; ============================================================
;; 2. Ledger: the proof obligations
;; ============================================================

(def be proof-be)

;; Step 1: relationship between mu and mu_0
(tools/execute-tool be :ledger-upsert
  ["P1" "L-mu-mu0" {:item/label "Determine relationship: mu vs mu_0 (GFF)"
                     :item/status :open
                     :item/depends-on #{}
                     :item/unlocks #{"L-cameron-martin" "L-shifted-interaction"}
                     :item/artifact-paths []}])

;; Step 2: Cameron-Martin applicability
(tools/execute-tool be :ledger-upsert
  ["P1" "L-cameron-martin" {:item/label "Check Cameron-Martin applicability for shifted measure"
                             :item/status :open
                             :item/depends-on #{"L-mu-mu0"}
                             :item/unlocks #{"L-equivalence"}
                             :item/artifact-paths []}])

;; Step 3: shifted interaction analysis
(tools/execute-tool be :ledger-upsert
  ["P1" "L-shifted-interaction" {:item/label "Analyze V(phi-psi) - V(phi) renormalization"
                                  :item/status :open
                                  :item/depends-on #{"L-mu-mu0"}
                                  :item/unlocks #{"L-equivalence"}
                                  :item/artifact-paths []}])

;; Step 4: final conclusion
(tools/execute-tool be :ledger-upsert
  ["P1" "L-equivalence" {:item/label "Conclude equivalence or mutual singularity"
                          :item/status :open
                          :item/depends-on #{"L-cameron-martin" "L-shifted-interaction"}
                          :item/unlocks #{}
                          :item/artifact-paths []}])

(println "\n=== Ledger populated")
(let [ledger (:result (tools/execute-tool be :ledger-query ["P1"]))]
  (doseq [[id item] (sort-by key ledger)]
    (println (format "  %s [%s] %s"
                     id (name (:item/status item)) (:item/label item)))))

;; ============================================================
;; 3. Cycle 1: Observe phase — the critical framing check
;; ============================================================

(println "\n=== Starting Cycle 1: observe L-mu-mu0")
(def cycle1 (tools/execute-tool be :cycle-begin ["P1" "L-mu-mu0"]))
(def cycle-id (get-in cycle1 [:result :cycle/id]))
(println "  cycle:" cycle-id "phase:" (get-in cycle1 [:result :cycle/phase]))

;; ============================================================
;; 3a. WRONG PATH (what we actually did):
;;     Assert mu ~ mu_0 without checking the corpus.
;;     Then build 5 pages of correct proof on a false premise.
;; ============================================================

(println "\n--- WRONG PATH (our writeup) ---")
(println "  Asserted: mu ~ mu_0 via Barashkov-Gubinelli exp(-V) > 0")
(println "  Then: Cameron-Martin gives T_psi^* mu_0 ~ mu_0")
(println "  Then: integrability of cubic perturbation (log-Sobolev)")
(println "  Concluded: YES, equivalent")
(println "  ACTUAL ANSWER: NO, mutually singular")

;; ============================================================
;; 3b. GOLDEN PATH (what the stepper should do):
;;     Before asserting mu ~ mu_0, run :corpus-check.
;;     The corpus should surface the 3D singularity.
;; ============================================================

(println "\n--- GOLDEN PATH (stepper) ---")

;; Framing query 1: Is the Phi^4_3 measure equivalent to the free field?
(println "\n  [corpus-check] Query: 'Phi^4_3 measure equivalent to Gaussian free field'")
(def check1 (tools/execute-tool be :corpus-check
              ["Phi^4_3 measure equivalent to Gaussian free field measure"]))

(if (:ok check1)
  (do
    (println "  Results:")
    (doseq [n (get-in check1 [:result :neighbors])]
      (println (format "    %d. %s (%.4f) %s"
                       (:rank n) (:id n) (:score n) (:title n "")))))
  (println "  [corpus-check unavailable:" (get-in check1 [:error :message]) "]"))

;; Framing query 2: Does renormalization diverge under shift in 3D?
(println "\n  [corpus-check] Query: 'renormalization constants diverge under smooth shift Phi^4 3D'")
(def check2 (tools/execute-tool be :corpus-check
              ["renormalization constants diverge under smooth shift Phi^4 3D"]))

(if (:ok check2)
  (do
    (println "  Results:")
    (doseq [n (get-in check2 [:result :neighbors])]
      (println (format "    %d. %s (%.4f) %s"
                       (:rank n) (:id n) (:score n) (:title n "")))))
  (println "  [corpus-check unavailable:" (get-in check2 [:error :message]) "]"))

;; Framing query 3: mutual singularity of interacting measure
(println "\n  [corpus-check] Query: 'mutual singularity interacting quantum field measure shift'")
(def check3 (tools/execute-tool be :corpus-check
              ["mutual singularity interacting quantum field measure under shift"]))

(if (:ok check3)
  (do
    (println "  Results:")
    (doseq [n (get-in check3 [:result :neighbors])]
      (println (format "    %d. %s (%.4f) %s"
                       (:rank n) (:id n) (:score n) (:title n "")))))
  (println "  [corpus-check unavailable:" (get-in check3 [:error :message]) "]"))

;; ============================================================
;; 3c. GOLDEN PATH decision point
;; ============================================================

(println "\n--- DECISION POINT ---")
(println "  If corpus-check surfaces threads about Phi^4_3 singularity in 3D:")
(println "    -> REFRAME: investigate whether mu ~ mu_0 actually holds")
(println "    -> Discover: renormalization constants diverge under shift")
(println "    -> Arrive at: NO, mutually singular (correct answer)")
(println "")
(println "  If corpus-check returns nothing relevant:")
(println "    -> We know math.SE/MO doesn't have this signal")
(println "    -> Need ArXiv data (Hairer 2014, regularity structures)")
(println "    -> This calibrates the ArXiv pipeline requirements")

;; ============================================================
;; 4. Record the known-correct golden path as failed route
;;    (our wrong approach) + the correct reframing
;; ============================================================

(println "\n=== Recording failed route (our wrong approach)")
(tools/execute-tool be :failed-route-add
  ["P1" {:route/blocker-id "L-mu-mu0"
         :route/approach "Assert mu ~ mu_0 via exp(-V) > 0 (Barashkov-Gubinelli density argument)"
         :route/structural-obstruction
         (str "In 3D, renormalization constants in V(phi-psi) - V(phi) diverge "
              "under shift, creating genuinely new singular behavior. "
              "The exp(-V) > 0 argument is valid for the free field but does not "
              "survive the Phi^4_3 interaction. Hairer constructs a distinguishing "
              "event B_gamma with mu(B_gamma) = 1, T_psi^*mu(B_gamma) = 0.")
         :route/failure-reason
         (str "Answered YES (equivalent) when the correct answer is NO (mutually singular). "
              "Built 5 pages of valid proof on the false premise mu ~ mu_0.")
         :route/evidence-refs ["data/first-proof/problem1-writeup.md"
                               "data/first-proof/problem1-solution.md"
                               "data/first-proof/comparison-with-official-solutions.md"]}])

(println "\n=== P1 golden path script complete.")
(println "=== Next: run with live futon3a backend to test corpus signal.")
