(ns futon3c.wm.run4-effective-environment
  "Fail-closed RUN4 attestation for environment flags captured by loaded Futon2 namespaces.")

(def flag-spec
  {"FUTON_WM_FPI_DARK"
   ['futon2.report.war-machine '*f-pi-dark?*]
   "FUTON_WM_BETA_DARK"
   ['futon2.report.war-machine '*beta-dark?*]
   "FUTON_WM_TRACE_POLICY_DETAILS"
   ['futon2.aif.trace '*persist-policy-trace-details?*]})

(def required-hierarchy {:model :single-level :scope :RUN4})

(defn- refuse! [reason data]
  (throw (ex-info "RUN4 effective environment refused"
                  (merge {:error :run4-effective-environment-refused
                          :reason reason} data))))

(defn production-var-read [[ns-name var-name]]
  (when-let [n (find-ns ns-name)]
    (when-let [v (ns-resolve n var-name)]
      (when (bound? v) @v))))

(defn attest
  "Compare pinned `1` requirements with current environment and values already
  captured by loaded consumers. Does not require, reload, or bind a namespace."
  ([declaration]
   (attest declaration {:env-read #(System/getenv %)
                        :var-read production-var-read}))
  ([{:keys [required-environment hierarchy] :as declaration}
    {:keys [env-read var-read]}]
   (when-not (and (map? declaration)
                  (map? required-environment)
                  (= (set (keys flag-spec)) (set (keys required-environment)))
                  (every? #(= "1" %) (vals required-environment))
                  (= required-hierarchy hierarchy)
                  (fn? env-read) (fn? var-read))
     (refuse! :malformed-declaration {:declaration declaration}))
   (let [rows (mapv (fn [[flag var-ref]]
                      (let [observed (env-read flag)
                            effective (var-read var-ref)]
                        (when (nil? effective)
                          (refuse! :effective-consumer-unavailable
                                   {:flag flag :consumer var-ref}))
                        (when-not (boolean? effective)
                          (refuse! :effective-consumer-invalid
                                   {:flag flag :consumer var-ref :value effective}))
                        (when-not (and (= "1" observed) effective)
                          (refuse! :required-observed-effective-mismatch
                                   {:flag flag :required "1"
                                    :observed observed :effective effective}))
                        {:flag flag :required "1" :observed observed
                         :effective effective :consumer var-ref}))
                    (sort-by key flag-spec))]
     {:schema :wm/run4-effective-environment-attestation-v1
      :hierarchy hierarchy
      :flags rows
      :recording {:status :not-attested-by-this-component
                  :consumer "holes/labs/wm-contract/wm_step_observe.bb"}})))
