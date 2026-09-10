(ns futon3c.wm.run4-report-service
  "Authenticated, read-only construction of a RUN4 durable report."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-acceptance-report :as report]
            [futon3c.wm.run4-trusted-entry :as trusted]))

(defn- one [f]
  (with-open [r (java.io.PushbackReader. (io/reader f))]
    (let [v (edn/read {:eof ::empty} r)]
      (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)) (not (map? v)))
        (throw (ex-info "Malformed RUN4 evidence" {:reason :malformed-evidence}))) v)))

(defn report!
  "Derive all authority from SERVER-CONFIG. HEADERS provide only authentication.
  This function performs no click, controller transition, publication, or pin."
  [server-config headers]
  (let [r4 (:run4 server-config)
        series (:series r4)
        manifest-file (io/file (:manifest-root series) (:manifest-ref series))
        manifest-text (slurp manifest-file)
        manifest (one manifest-file)
        prepared (mapv (fn [trial]
                         (let [p (trusted/prepare server-config headers
                                                  {:run4-pin-ref (get-in trial [:packet :path])
                                                   :run4-attempt-id (:attempt-id trial)})]
                           (when-not (:ok p)
                             (throw (ex-info "RUN4 report authentication refused"
                                             {:reason :report-auth-or-pin-refused}))) p))
                       (:trials manifest))
        starts (mapv (fn [trial]
                       (let [f (io/file (:controller-root series)
                                        (format "%03d-started.edn" (:ordinal trial)))]
                         (when (.isFile f) (one f)))) (:trials manifest))
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
          :trials (mapv (fn [p s] {:admission-request (:admission-request p) :started s})
                        prepared starts)
          :visibility (json/parse-string (slurp visibility-file) true)
          :expected-series-id (:series-id manifest)
          :expected-series-sha256 (digest/sha256 manifest-text)
          :observed-series-sha256 (digest/sha256 (slurp manifest-file))
          :control-map-text control-text
          :expected-control-map-sha256 (:control-map-sha256 acceptance)
          :recording-root (:recording-root series)})))))
