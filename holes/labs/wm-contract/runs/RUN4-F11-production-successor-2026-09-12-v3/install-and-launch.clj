(do
  (require '[cheshire.core :as json] '[clojure.java.io :as io]
           '[futon2.aif.full-loop-cohort :as cohort]
           '[futon3c.transport.http :as http] '[futon3c.wm.run4-boot :as boot]
           '[futon3c.wm.run4-codex-fold :as fold] '[futon3c.agency.registry :as reg])
  (let [packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12-v3"
        prereg "/home/joe/code/futon2/holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12-v3/cohort.edn"
        root "/home/joe/run4/F11-production-successor-20260912-v3"
        data-root (str root "/cohort")
        binding {:preregistration prereg :data-root data-root
                 :cohort-id :run4-f11-production-successor-20260912-v3
                 :sha256 "f7330d3fa8170ac3a722bd9c74b4b7dd0fe7f64602ebed204567308e159a02e4"}
        authority {:schema :wm/codex-fold-authority-v1 :root "/home/joe/code/futon3c"
                   :plan-ref "holes/labs/wm-contract/runs/RUN4-codex-fold-port-commissioning-2026-09-12/fold-plan.edn"
                   :plan-sha256 "09d1fe0e014f57cb0d8649a57e1350562c692bcf37ae1f7b8a6b57d3a5b2a6de"
                   :seat "codex-21" :agency-base "http://127.0.0.1:7070" :caller "run4-f11-v3-fold"}]
    (doseq [name ["controller" "bindings" "projections" "run-records" "recordings" "visibility" "battery" "cohort"]]
      (let [f (io/file root name)] (.mkdirs f) (.setReadable f false false) (.setWritable f false false)
            (.setExecutable f false false) (.setReadable f true true) (.setWritable f true true) (.setExecutable f true true)))
    (when-not (.isFile (io/file data-root
                                "run4-f11-production-successor-20260912-v3"
                                "activation.edn"))
      (cohort/activate! prereg data-root))
    (let [fragment (boot/materialize true {:read-template #(slurp (str packet "/server-config.disabled.edn"))
                                           :execution-cohort binding :cohort-preflight! cohort/execution-preflight
                                           :construction-wiring-fn (fold/make-port authority)
                                           :construction-wiring-authority authority})
          _ (http/reconfigure-handler! #(assoc % :run4 (:run4 fragment)))
          roster (into {} (for [id ["codex-20" "codex-18" "codex-19" "codex-21"]]
                            [id (:agent/status (reg/get-agent id))]))
          _ (when-not (every? #(= :idle %) (vals roster))
              (throw (ex-info "Pinned actor unavailable" {:roster roster})))
          token ((ns-resolve 'futon3c.wm.run4-boot 'production-secret))
          client (java.net.http.HttpClient/newHttpClient)
          body (json/generate-string {:run4-series-ref "holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12-v3/series-pin.edn"})
          request (-> (java.net.http.HttpRequest/newBuilder (java.net.URI/create "http://127.0.0.1:7070/api/alpha/wm/run4/series/step"))
                      (.header "content-type" "application/json") (.header "authorization" (str "Bearer " token))
                      (.POST (java.net.http.HttpRequest$BodyPublishers/ofString body)) (.build))
          response (.send client request (java.net.http.HttpResponse$BodyHandlers/ofString))]
      {:cohort (select-keys (cohort/execution-preflight binding false) [:cohort-id :target :remaining])
       :roster roster :status (.statusCode response) :response (json/parse-string (.body response) true)})))
