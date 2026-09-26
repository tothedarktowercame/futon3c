(ns futon3c.wm.run4-report-service
  "Authenticated, read-only construction of a RUN4 durable report."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-acceptance-report :as report]
            [futon3c.wm.run4-trusted-entry :as trusted]))

(defn- one-text [text]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [v (edn/read {:eof ::empty} r)]
      (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)) (not (map? v)))
        (throw (ex-info "Malformed RUN4 evidence" {:reason :malformed-evidence}))) v)))

(defn report!
  "Derive all authority from SERVER-CONFIG. HEADERS provide only authentication.
  This function performs no click, controller transition, publication, or pin."
  [server-config headers]
  (let [auth (trusted/authenticate server-config headers)
        _ (when-not (:ok auth)
            (throw (ex-info "RUN4 report authentication refused"
                            {:reason (:error auth)})))
        r4 (:run4 server-config)
        series (:series r4)
        manifest-ref (:manifest-ref series)
        _ (when-not (contains? (:manifest-allowlist series) manifest-ref)
            (throw (ex-info "RUN4 manifest refused" {:reason :manifest-not-allowlisted})))
        manifest-file (.getCanonicalFile (io/file (:manifest-root series) manifest-ref))
        manifest-text (slurp manifest-file)
        _ (when-not (= (:manifest-sha256 series) (digest/sha256 manifest-text))
            (throw (ex-info "RUN4 manifest drift" {:reason :manifest-drift})))
        manifest (one-text manifest-text)
        _ (when-not (and (= :wm/run4-series-pin-v1 (:schema manifest))
                         (seq (:trials manifest)) (= (:casting r4) (:casting manifest)))
            (throw (ex-info "RUN4 manifest invalid" {:reason :invalid-manifest})))
        requests (mapv (fn [trial]
                         (let [ref (get-in trial [:packet :path])
                               _ (when-not (contains? (:pin-allowlist r4) ref)
                                   (throw (ex-info "RUN4 pin refused"
                                                   {:reason :pin-not-allowlisted})))
                               text (slurp (io/file (:pin-root r4) ref))]
                           (when-not (= (:pin-sha256 trial) (digest/sha256 text))
                             (throw (ex-info "RUN4 pin drift" {:reason :pin-drift})))
                           {:attempt-id (:attempt-id trial)
                            :identity {:series-id (:series-id manifest)
                                       :trial-id (:trial-id trial)
                                       :pin-sha256 (:pin-sha256 trial)
                                       :casting (:casting manifest)}}))
                       (:trials manifest))
        starts (mapv (fn [trial]
                       (let [f (io/file (:controller-root series)
                                        (format "%03d-started.edn" (:ordinal trial)))]
                         (when (.isFile f) (one-text (slurp f))))) (:trials manifest))
        visibility-file (io/file (:visibility-root series) "run-visibility.json")
        acceptance (:acceptance r4)
        control-file (io/file (:control-map-root acceptance) (:control-map-ref acceptance))]
    (if (or (some nil? starts) (not (.isFile visibility-file)) (not (.isFile control-file)))
      {:schema :wm/run4-acceptance-report-v1 :decision :incomplete-durable-evidence
       :accepted? false :acceptance-authority :operator-reserved}
      (let [control-text (slurp control-file)]
        (when-not (= (:control-map-sha256 acceptance) (digest/sha256 control-text))
          (throw (ex-info "RUN4 control map drift" {:reason :control-map-drift})))
        (report/report-durable
         {:roots {:admission (:admission-root r4) :bindings (:binding-root series)
                  :projections (:projection-root series) :run-records (:run-record-root series)}
          :trials (mapv (fn [request started]
                          {:admission-request request :started started}) requests starts)
          :visibility (json/parse-string (slurp visibility-file) true)
          :expected-series-id (:series-id manifest)
          :expected-series-sha256 (digest/sha256 manifest-text)
          :observed-series-sha256 (digest/sha256 manifest-text)
          :control-map-text control-text
          :expected-control-map-sha256 (:control-map-sha256 acceptance)
          :recording-root (:recording-root series)})))))
