;; M-aif2 VERIFY Stage-B spike (read-only). Targeted risk reduction for the
;; ∇T-reader generalisation + κ mapping (futon2/holes/M-aif2.md §5). NOT
;; production wiring — a throwaway spike. Read-only against live substrate-2.
;;
;;   T-ΔT-regression : the existing reader runs live and exposes tension from
;;                     NON-sorry sources (mission/file), proving tension over
;;                     non-sorry relations is real and readable.
;;   T-tension-generates : a tension reading becomes a candidate-action purely
;;                     (no registry consulted) — tension GENERATES, not merely
;;                     ranks. And κ-with-no-S2-class → a candidate-CLASS signal
;;                     (Gap-1 false-floor → Gap-2 niche-construction trigger).
;;
;; Run: cd ~/code/futon3c && clojure -M scripts/aif2_tension_spike.clj [mission-endpoint]

(require '[futon3c.aif.mission-delta-t :as mdt])

;; κ: node-type → action-class to propose (over today's S2 set).
(def s2-classes #{:address-sorry :open-mission :fire-pattern})
(def kappa {:sorry :address-sorry :mission :open-mission
            :file :open-mission :pattern :fire-pattern})

(defn tension->candidate
  "Gap-1 generation step. A tension reading becomes a candidate-action, OR
   (false-floor → niche-construction) a candidate-CLASS signal when κ routes to
   a class absent from the current S2 set / has no route at all."
  [{:keys [node node-type tension]}]
  (let [cls (get kappa node-type)]
    (cond
      (nil? cls)             {:kind :candidate-class-signal :reason :no-κ-route
                              :node node :node-type node-type :tension tension}
      (not (s2-classes cls)) {:kind :candidate-class-signal :reason :class-absent-in-S2
                              :proposed-class cls :node node :tension tension}
      :else                  {:kind :candidate-action :type cls :target node
                              :proposer-id :s1/tension
                              :structural-pressure-per-action tension})))

(let [ep (or (first *command-line-args*)
             "futon0-d/mission/futonzero-capability")
      dt (mdt/delta-t-mission ep)
      contribs (:per-edge-contributions dt)
      non-sorry (filter #(not= :sorry (:source-type %)) contribs)
      reading {:node ep :node-type :mission :tension (:delta-T dt)}]
  (println "=== T-ΔT-regression — live reader on a real node (read-only) ===")
  (println "  mission:" ep)
  (println "  phase:" (:mission-phase dt) " mission-T:" (:mission-T dt)
           " ΔT:" (:delta-T dt) " n-edges:" (:n-edges dt))
  (println "  by-source-type:" (pr-str (:by-source-type dt)))
  (println "  non-sorry contributions:" (count non-sorry)
           "→ tension over non-sorry relations is" (if (seq non-sorry) "REAL" "absent on this node"))
  (when (seq non-sorry)
    (println "  sample non-sorry edge:" (pr-str (first non-sorry))))
  (println)
  (println "=== T-tension-generates — κ + candidate, registries NOT consulted ===")
  (println "  reading:" (pr-str reading))
  (println "  generated:" (pr-str (tension->candidate reading)))
  (println)
  (println "=== false-floor → niche-construction trigger (κ → class absent in S2) ===")
  (println "  novel-node reading:" (pr-str (tension->candidate
                                            {:node "x/novel-node" :node-type :novel-type :tension 0.7})))
  (println "  known-type-but-could-be-absent:" (pr-str (tension->candidate
                                            {:node "y/refactor-target" :node-type :code-node :tension 0.5}))))
