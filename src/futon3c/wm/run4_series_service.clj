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
            [futon3c.wm.run4-historical-successor :as historical-successor]
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

(defn- execution-identity? [x]
  (and (map? x) (= #{:kind :id} (set (keys x)))
       (= :runner-execution (:kind x))
       (string? (:id x)) (not (str/blank? (:id x)))))

(defn- linked-successor! [run4 series manifest-text prepared evidence-roots]
  (when-let [link (:historical-successor run4)]
    (let [successor (:successor link)]
      (when-not (and (= #{:repair-id :verification-id :verification-attempt
                          :verification-cohort :successor}
                         (set (keys link)))
                     (every? #(and (string? %) (not (str/blank? %)))
                             ((juxt :repair-id :verification-id) link))
                     (execution-identity? (:verification-attempt link))
                     (map? (:verification-cohort link))
                     (= #{:series-id :trial-id :attempt-id} (set (keys successor)))
                     (every? #(or (and (string? %) (not (str/blank? %)))
                                  (keyword? %))
                             (vals successor)))
        (refuse! :historical-successor-link-invalid))
      (when (.isFile (io/file (:controller-root series) "series.edn"))
        (let [lifecycle (controller/read-lifecycle!
                         (:controller-root series) manifest-text prepared)
              row (some #(when (= (:trial-id successor)
                                  (get-in % [:trial :trial-id])) %)
                        (:trials lifecycle))]
          (when (and row (:terminal row))
            (let [identity (get-in (get prepared (:ordinal row))
                                   [:admission-request :identity])
                  attempt-id (get-in (get prepared (:ordinal row))
                                     [:admission-request :attempt-id])]
              (when-not (and (= (:series-id successor) (:series-id identity))
                             (= (:trial-id successor) (:trial-id identity))
                             (= (:attempt-id successor) attempt-id)
                             (:started row))
                (refuse! :historical-successor-link-mismatch))
              ;; The strict reader validates task success and all durable joins.
              ;; Any failure here stops before the controller can advance.
              (historical-successor/resolve-from-durable!
               {:repair-root (get-in run4 [:historical-action :repair-root])
                :evidence-roots evidence-roots
                :admission-request (:admission-request
                                    (get prepared (:ordinal row)))
                :started (:started row)
                :repair-id (:repair-id link)
                :verification-id (:verification-id link)
                :verification-attempt (:verification-attempt link)
                :verification-cohort (:verification-cohort link)
                :successor-cohort (:execution-cohort run4)}))))))))

(defn- reconcile-linked-row!
  [run4 prepared evidence-roots trial prepared-trial started terminal]
  (when-let [link (:historical-successor run4)]
    (let [successor (:successor link)
          identity (get-in prepared-trial [:admission-request :identity])
          attempt-id (get-in prepared-trial [:admission-request :attempt-id])]
      (when (= (:trial-id successor) (:trial-id trial))
        (when-not (and terminal started
                       (= (:series-id successor) (:series-id identity))
                       (= (:trial-id successor) (:trial-id identity))
                       (= (:attempt-id successor) attempt-id)
                       (= prepared-trial (get prepared (:ordinal trial))))
          (refuse! :historical-successor-link-mismatch))
        (historical-successor/resolve-from-durable!
         {:repair-root (get-in run4 [:historical-action :repair-root])
          :evidence-roots evidence-roots
          :admission-request (:admission-request prepared-trial)
          :started started
          :repair-id (:repair-id link)
          :verification-id (:verification-id link)
          :verification-attempt (:verification-attempt link)
          :verification-cohort (:verification-cohort link)
          :successor-cohort (:execution-cohort run4)})))))

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
                        :run-records (:run-record-root series)
                        :repair-root (get-in run4 [:historical-action :repair-root])
                        :cohort-preregistration
                        (get-in run4 [:execution-cohort :preregistration])
                        :cohort-data-root
                        (get-in run4 [:execution-cohort :data-root])}
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
        ;; Populate the same trusted values used by the controller, without
        ;; writing an admission or dispatching. This permits persisted-terminal
        ;; reconciliation to run before the controller considers a successor.
        _ (controller/preflight manifest-text
                                {:read-text read-text :prepare-trial prepare-trial})
        _ (linked-successor! run4 series manifest-text @prepared evidence-roots)
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
                 :terminal-evidence terminal-port
                 :before-terminal-advance
                 (fn [trial prepared-trial started terminal]
                   (reconcile-linked-row! run4 @prepared evidence-roots
                                          trial prepared-trial started terminal))})]
    (when (= :trial-terminal (:status result))
      (linked-successor! run4 series manifest-text @prepared evidence-roots))
    (when (true? (:visibility-enabled? series))
      (let [lifecycle (controller/read-lifecycle!
                       (:controller-root series) manifest-text @prepared)
            observation (visibility/observe
                         (:controller-root series) manifest-text terminal-port
                         (str (java.time.Instant/now)) lifecycle)]
        (visibility/publish! (io/file (:visibility-root series) "run-visibility.json")
                             observation)))
    (assoc result :run4/series true)))
