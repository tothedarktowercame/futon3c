(ns run4-f11-install-live
  (:require [futon2.aif.full-loop-cohort :as cohort]
            [futon3c.transport.http :as http]
            [futon3c.wm.run4-boot :as boot]
            [futon3c.wm.run4-codex-fold :as fold]))

(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12")
(def prereg "/home/joe/code/futon2/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12/cohort.edn")
(def cohort-binding
  {:preregistration prereg
   :data-root "/home/joe/run4/F11-production-successor-20260912/cohort"
   :cohort-id :run4-f11-production-successor-20260912-v1
   :sha256 "681d6e42a3949bbf6225b6bda1c5708fd2d7c0fc2c5f6f652af71e8c6601de71"})
(def fold-authority
  {:schema :wm/codex-fold-authority-v1
   :root "/home/joe/code/futon3c"
   :plan-ref "holes/labs/wm-contract/runs/RUN4-codex-fold-port-commissioning-2026-09-12/fold-plan.edn"
   :plan-sha256 "09d1fe0e014f57cb0d8649a57e1350562c692bcf37ae1f7b8a6b57d3a5b2a6de"
   :seat "codex-21" :agency-base "http://127.0.0.1:7070"
   :caller "run4-f11-fold"})

(defn install! []
  (let [fragment
        (boot/materialize
         true {:read-template #(slurp (str packet "/server-config.disabled.edn"))
               :execution-cohort cohort-binding
               :cohort-preflight! cohort/execution-preflight
               :construction-wiring-fn (fold/make-port fold-authority)
               :construction-wiring-authority fold-authority})
        preflight (cohort/execution-preflight cohort-binding)
        result (http/reconfigure-handler! #(assoc % :run4 (:run4 fragment)))]
    {:installation result
     :cohort (select-keys preflight [:cohort-id :target :remaining])
     :series-id "run4-f11-production-successor-20260912-v1"
     :manifest-sha256 "4cc5068d5df95e9f92d86c38c361788eac6ce02db389c877d9eea101628d27a3"
     :fold-seat "codex-21"}))
