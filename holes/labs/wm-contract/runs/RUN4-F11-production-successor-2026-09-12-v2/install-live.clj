(ns run4-f11-v2-install-live
  (:require [clojure.java.io :as io]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon3c.transport.http :as http]
            [futon3c.wm.run4-boot :as boot]
            [futon3c.wm.run4-codex-fold :as fold]))

(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12-v2")
(def prereg "/home/joe/code/futon2/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12-v2/cohort.edn")
(def data-root "/home/joe/run4/F11-production-successor-20260912-v2/cohort")
(def cohort-binding
  {:preregistration prereg :data-root data-root
   :cohort-id :run4-f11-production-successor-20260912-v2
   :sha256 "821149cf56c048a44ee29f4f82ef00cdc9f8a142256055f930d8ce029b65c1d4"})
(def fold-authority
  {:schema :wm/codex-fold-authority-v1 :root "/home/joe/code/futon3c"
   :plan-ref "holes/labs/wm-contract/runs/RUN4-codex-fold-port-commissioning-2026-09-12/fold-plan.edn"
   :plan-sha256 "09d1fe0e014f57cb0d8649a57e1350562c692bcf37ae1f7b8a6b57d3a5b2a6de"
   :seat "codex-21" :agency-base "http://127.0.0.1:7070"
   :caller "run4-f11-v2-fold"})

(defn install! []
  (let [root "/home/joe/run4/F11-production-successor-20260912-v2"
        stores ["controller" "bindings" "projections" "run-records"
                "recordings" "visibility" "battery" "cohort"]]
    (doseq [name stores]
      (let [f (io/file root name)]
        (.mkdirs f)
        (.setReadable f false false) (.setWritable f false false)
        (.setExecutable f false false)
        (.setReadable f true true) (.setWritable f true true)
        (.setExecutable f true true)))
    (cohort/activate! prereg data-root)
    (let [fragment (boot/materialize
                    true {:read-template #(slurp (str packet "/server-config.disabled.edn"))
                          :execution-cohort cohort-binding
                          :cohort-preflight! cohort/execution-preflight
                          :construction-wiring-fn (fold/make-port fold-authority)
                          :construction-wiring-authority fold-authority})
          preflight (cohort/execution-preflight cohort-binding)
          installation (http/reconfigure-handler! #(assoc % :run4 (:run4 fragment)))]
      {:installation installation
       :cohort (select-keys preflight [:cohort-id :target :remaining])
       :manifest-sha256 "3e0e6db16cbc45fef001f29de54ed20f1b479c21476466f76cfdbcfdbb08d5c2"
       :fold-seat "codex-21"})))

(install!)
