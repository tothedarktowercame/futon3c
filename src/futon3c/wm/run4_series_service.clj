(ns futon3c.wm.run4-series-service
  "Disabled-by-default serving adapter for one durable RUN4 series transition.

  Authentication and task authority remain in run4-trusted-entry.  This
  adapter supplies only server-owned files, stores, and the existing click
  boundary; request data cannot supply ports or select a different manifest."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.wm.run4-series-controller :as controller]
            [futon3c.wm.run4-realized-recording :as realized]
            [futon3c.wm.run4-historical-projection :as historical]
            [futon3c.wm.run4-run-visibility :as visibility]
            [futon3c.wm.run4-terminal-evidence :as terminal]
            [futon3c.wm.run4-trusted-entry :as trusted]
            [futon3c.wm.runner-service :as runner]))

(def allowed-request-keys #{:run4-series-ref})

(defn- refuse! [reason & [data]]
  (throw (ex-info "RUN4 series serving refused"
                  (merge {:error :run4-series-serving-refused
                          :reason reason :status 403}
                         data))))

(defn- relative-ref? [x]
  (and (string? x) (not (str/blank? x)) (not (.isAbsolute (io/file x)))))

(defn- directory? [x]
  (and (string? x) (not (str/blank? x)) (.isDirectory (io/file x))))

(defn- exact-file [root allowlist ref kind]
  (when-not (and (directory? root) (set? allowlist) (contains? allowlist ref)
                 (relative-ref? ref))
    (refuse! :run4-series-file-refused {:kind kind}))
  (let [base (.getCanonicalFile (io/file root))
        file (.getCanonicalFile (io/file base ref))]
    (when-not (and (.startsWith (.toPath file) (.toPath base)) (.isFile file))
      (refuse! :run4-series-file-refused {:kind kind}))
    file))

(defn- serving-config! [config payload]
  (let [run4 (:run4 config)
        series (:series run4)]
    (when-not (and (map? run4) (map? series) (true? (:enabled? series)))
      (refuse! :run4-series-disabled))
    (when-not (and (map? payload)
                   (= allowed-request-keys (set (keys payload)))
                   (= (:manifest-ref series) (:run4-series-ref payload)))
      (refuse! :run4-series-request-invalid))
    (doseq [[kind root] [[:controller (:controller-root series)]
                         [:admission (:admission-root run4)]
                         [:bindings (:binding-root series)]
                         [:projections (:projection-root series)]
                         [:run-records (:run-record-root series)]]]
      (when-not (directory? root)
        (refuse! :run4-series-root-invalid {:kind kind})))
    ;; The accepted controller stores admission and series events beneath its
    ;; single durable root.  Refuse a split configuration rather than letting
    ;; the terminal reader consult a different admission store.
    (when-not (= (.getCanonicalPath (io/file (:controller-root series)))
                 (.getCanonicalPath (io/file (:admission-root run4))))
      (refuse! :run4-series-admission-root-mismatch))
    (when-not (and (set? (:manifest-allowlist series))
                   (set? (:pin-allowlist run4))
                   (set? (:source-allowlist run4)))
      (refuse! :run4-series-authority-invalid))
    (when (true? (:visibility-enabled? series))
      (when-not (directory? (:visibility-root series))
        (refuse! :run4-series-root-invalid {:kind :visibility})))
    (when (true? (:recording-enabled? series))
      (when-not (directory? (:recording-root series))
        (refuse! :run4-series-root-invalid {:kind :recording})))
    {:run4 run4 :series series}))

(defn- source-reader [{:keys [run4]}]
  (fn [ref]
    (cond
      (contains? (:pin-allowlist run4) ref)
      (slurp (exact-file (:pin-root run4) (:pin-allowlist run4) ref :pin))

      (contains? (:source-allowlist run4) ref)
      (slurp (exact-file (:source-root run4) (:source-allowlist run4) ref :source))

      :else (refuse! :run4-series-source-refused {:ref ref}))))

(defn step!
  "Authenticate all frozen trial pins, then advance at most one boundary.

  Missing configuration is a refusal.  There is no timer or import hook: the
  caller must explicitly invoke this function through the serving route."
  [config headers payload]
  (let [{:keys [series] :as cfg} (serving-config! config payload)
        run4 (:run4 cfg)
        manifest-file (exact-file (:manifest-root series)
                                  (:manifest-allowlist series)
                                  (:manifest-ref series) :manifest)
        manifest-text (slurp manifest-file)
        prepared (atom {})
        read-text (source-reader cfg)
        prepare-trial
        (fn [trial]
          (let [existing-start? (.isFile
                                 (io/file (:controller-root series)
                                          (format "%03d-started.edn" (:ordinal trial))))
                value (trusted/prepare
                       config headers
                       {:run4-pin-ref (get-in trial [:packet :path])
                        :run4-attempt-id (:attempt-id trial)}
                       {:require-cohort-capacity? (not existing-start?)})]
            (when-not (:ok value)
              (refuse! :run4-series-trial-refused
                       {:ordinal (:ordinal trial) :cause (:error value)}))
            (let [value (cond-> value existing-start?
                          (assoc :run4/existing-inspection-only? true))]
              (swap! prepared assoc (:ordinal trial) value)
              value)))
        evidence-roots {:admission (:admission-root run4)
                        :bindings (:binding-root series)
                        :projections (:projection-root series)
                        :run-records (:run-record-root series)}
        terminal-port
        (fn [started]
          (let [prepared-trial (get @prepared (:ordinal started))
                _ (when-not prepared-trial
                    (refuse! :run4-series-prepared-trial-missing))
                historical-bundle
                (historical/read-bundle! evidence-roots
                                         (:admission-request prepared-trial) started)
                bundle (when-not historical-bundle
                         (terminal/read-terminal-evidence-bundle
                          evidence-roots (:admission-request prepared-trial) started))]
            (when (and historical-bundle (true? (:recording-enabled? series)))
              (historical/persist-observation! (:recording-root series)
                                               historical-bundle))
            (when (and bundle (true? (:recording-enabled? series)))
              (realized/persist-bundle! (:recording-root series) bundle))
            ;; Historical admission is deliberately nil to the task controller:
            ;; it remains awaiting evidence and cannot advance the trial.
            (:classification bundle)))
        result (controller/step!
                (:controller-root series) manifest-text
                {:read-text read-text
                 :prepare-trial prepare-trial
                 :click! (fn [opts]
                           ;; click! captures these bindings before its daemon
                           ;; thread starts, keeping producer and reader roots
                           ;; identical across the asynchronous boundary.
                           (binding [runner/*click-run-binding-dir*
                                     (:binding-root series)
                                     runner/*run4-terminal-projection-dir*
                                     (:projection-root series)
                                     runner/*run4-historical-projection-dir*
                                     (:projection-root series)]
                             (runner/click!
                              (assoc opts :run-record-dir
                                     (:run-record-root series)))))
                 :terminal-evidence terminal-port})]
    (when (true? (:visibility-enabled? series))
      (let [lifecycle (controller/read-lifecycle!
                       (:controller-root series) manifest-text @prepared)
            observation (visibility/observe
                         (:controller-root series) manifest-text terminal-port
                         (str (java.time.Instant/now)) lifecycle)]
        (visibility/publish! (io/file (:visibility-root series) "run-visibility.json")
                             observation)))
    (assoc result :run4/series true)))
