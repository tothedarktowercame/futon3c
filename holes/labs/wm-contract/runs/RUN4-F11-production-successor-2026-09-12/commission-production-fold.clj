(ns run4-f11-commission-production-fold
  (:require [clojure.edn :as edn]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.wm.run4-codex-fold :as fold]))

(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12")
(def prereg "/home/joe/code/futon2/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12/cohort.edn")
(def evidence-root (str packet "/production-fold-commissioning-v5"))
(def source-selection "/home/joe/run4/F11-production-successor-20260912/cohort/run4-f11-production-successor-20260912-v1/attempt-001/002-selection.edn")
(def source-checkpoint "/home/joe/run4/F11-production-successor-20260912/cohort/run4-f11-production-successor-20260912-v1/attempt-001/003-construction.edn")
(def authority
  {:schema :wm/codex-fold-authority-v1
   :root "/home/joe/code/futon3c"
   :plan-ref "holes/labs/wm-contract/runs/RUN4-codex-fold-port-commissioning-2026-09-12/fold-plan.edn"
   :plan-sha256 "09d1fe0e014f57cb0d8649a57e1350562c692bcf37ae1f7b8a6b57d3a5b2a6de"
   :seat "codex-21"
   :agency-base "http://127.0.0.1:7070"
   :caller "run4-f11-production-path-commissioning"})

(defn term [judgment]
  {:judgment judgment :ground {:kind :production-path-commissioning}})

(defn run! []
  (let [source (edn/read-string (slurp source-checkpoint))
        selection (edn/read-string (slurp source-selection))
        construction {:shown (get-in source [:payload :judgment :patterns])}
        _ (cohort/activate! prereg evidence-root)
        started (cohort/start-attempt!
                 prereg evidence-root
                 (term {:opportunity-id "f11-production-fold-commissioning"
                        :trigger :duree-click-on-demand
                        :machine-state {:click-shaped? true}
                        :agent-roster ["codex-21"]
                        :code-state {:git-sha "679746a3"
                                     :git-dirty? false
                                     :resolved-mode-flags {}
                                     :configuration-digest (:plan-sha256 authority)}
                        :semantic-epoch :full-loop-real-actuation-v6}))
        attempt (:attempt/id started)
        _ (cohort/append-checkpoint! prereg evidence-root attempt :selection
                                     (:payload selection))
        port (fold/make-port authority)
        fold-result (assoc (port construction) :fold/authority authority)
        gate (runner/construction-wiring-result construction
                                                (constantly fold-result) true)
        checkpoint
        (term {:mission "M-f11-find-production-successor"
               :cascade {:commissioning :production-fold-port}
               :deposit nil
               :sorries (vec (get-in gate [:fold-output :policy-holes]))
               :wiring (:wiring gate)
               :fold-output (:fold-output gate)
               :patterns (:shown construction)
               :selection-enaction
               (runner/selection-enaction-record
                {:type :advance-mission :target "M-f11-find-production-successor"}
                {:type :advance-mission :target "M-f11-find-production-successor"}
                {:source :authenticated-operator-task-pin})})]
    (when-not (= :wired (:status gate))
      (throw (ex-info "Positive fold commissioning did not wire" {:gate gate})))
    (cohort/append-checkpoint! prereg evidence-root attempt :construction checkpoint)
    {:status :commissioned
     :attempt-id attempt
     :job-id (get-in gate [:fold-output :fold/execution :job-id])
     :gate-status (:status gate)
     :checkpoint (str evidence-root "/run4-f11-production-successor-20260912-v1"
                      "/" attempt "/003-construction.edn")}))

(run!)
