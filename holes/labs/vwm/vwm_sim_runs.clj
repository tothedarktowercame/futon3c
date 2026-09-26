;; vwm_sim_runs.clj — VWM simulated runs: settle the M-wm-policies Car-3 act-gate by
;; SIMULATION, not arming (a lab script, no ns — load-file in the live JVM, like
;; e-rollout-v2-e2e.clj).
;;
;; For each stalled mission we compute BOTH candidate act-gates *without acting*:
;;   grain-2  cascade wholeness  C / size    (cascade-policy-for; sim-only, no writes)
;;   grain-3  rollout            G(π) / depth (futon2.aif.rollout; MUST-B zero :7071 writes)
;; and observe whether they AGREE on "is this mission actionable?". Where they disagree is
;; the diagnostic that tells us the act-gate must read both (combining-methods-as-diagnostic).
;;
;; This is what the VWM is for: "simulate the process of a few runs without actually doing
;; it" (Joe, 2026-06-24). NO live action — both scorers are read-only/sim-only.
;;
;; Run (live JVM has futon2 loaded):
;;   (load-file "/home/joe/code/futon3c/holes/labs/vwm/vwm_sim_runs.clj")
;;   (simulate-runs ["canon-fingerprint-store" "hypergraph-operator" ...])
(require '[futon2.aif.rollout :as rollout]
         '[futon2.report.cascade-lane :as casc]
         '[clojure.string :as str])

(def MOVE-SET "/home/joe/code/futon6/data/diffsub-moves.edn")     ; the v2 k=55 set (real mission roots)
(def moves (rollout/moves (rollout/load-move-set MOVE-SET)))

;; synthetic cap-overlay (structural reachability; same convention as e-rollout-v2-e2e).
(def cap-overlay
  (into {} (for [cid (keep :advances-cap moves)]
             [cid {:id (str "scope/capability/" cid)
                   :props {:capability/frontier? true :capability/status :held}}])))

(defn rollout-for
  "grain-3: best rollout G(π) over the moves touching this mission stem. A mission with no
   moves in the set has NO path (grain-3 'stuck' — itself a signal)."
  [stem]
  (let [mv (filter #(re-find (re-pattern stem) (str (:have %) (:want %))) moves)]
    (if (empty? mv)
      {:G nil :depth 0 :policy [] :note "no moves in v2 set — grain-3 has no path"}
      (let [seed (rollout/seed-roots {:arrows {} :cap-overlay cap-overlay :reachable #{}} mv)
            best (rollout/best-rollout seed mv :depth 5 :top-k 3 :gamma 0.9)]
        {:G (some-> (:G best) double) :depth (count (:policy best))
         :policy (mapv :move/id (:policy best))}))))

(defn cascade-for
  "grain-2: cascade wholeness C / coverage-saturated size for this mission's circumstance."
  [stem]
  (let [psi (str (str/replace stem #"-" " ")
                 " tension at mission " stem
                 ": min-incident-kappa -0.1667, resolvedness 0.10 — strain bridge")  ; live-shape ψ
        c   (casc/cascade-policy-for psi 6)
        free-energy (:F-free-energy c)]
    {:F-free-energy (some-> free-energy double) :accuracy (:accuracy c) :complexity (:complexity c)
     :wholeness (some-> (:wholeness c) double) :size (:size c)
     :gap? (or (nil? c) (nil? free-energy) (<= free-energy 0.0))}))   ; AIF gap: marginal-likelihood F ≤ 0

(defn simulate-runs
  "Run the two scorers for each mission stem and report agreement.
   g2-actionable = the cascade is NOT thin (a coherent argument exists).
   g3-actionable = a beneficial rollout path exists (G(π) < 0).
   agree? = the two gates concur on whether to act."
  [stems]
  (mapv (fn [s]
          (let [cg (cascade-for s)
                rg (rollout-for s)
                g2-act (not (:gap? cg))
                g3-act (boolean (and (:G rg) (neg? (:G rg))))]
            {:mission s
             :cascade cg
             :rollout (dissoc rg :policy)
             :g2-actionable g2-act
             :g3-actionable g3-act
             :agree? (= g2-act g3-act)}))
        stems))
