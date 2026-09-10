(ns futon3c.wm.run4-series-service
  "Disabled-by-default serving adapter for one durable RUN4 series transition.

  Authentication and task authority remain in run4-trusted-entry.  This
  adapter supplies only server-owned files, stores, and the existing click
  boundary; request data cannot supply ports or select a different manifest."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.wm.run4-series-controller :as controller]
            [futon3c.wm.run4-terminal-evidence :as terminal]
            [futon3c.wm.run4-trusted-entry :as trusted]))

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
          (let [value (trusted/prepare
                       config headers
                       {:run4-pin-ref (get-in trial [:packet :path])
                        :run4-attempt-id (:attempt-id trial)})]
            (when-not (:ok value)
              (refuse! :run4-series-trial-refused
                       {:ordinal (:ordinal trial) :cause (:error value)}))
            (swap! prepared assoc (:ordinal trial) value)
            value))
        evidence-roots {:admission (:admission-root run4)
                        :bindings (:binding-root series)
                        :projections (:projection-root series)
                        :run-records (:run-record-root series)}
        result (controller/step!
                (:controller-root series) manifest-text
                {:read-text read-text
                 :prepare-trial prepare-trial
                 :click! (fn [opts]
                           ((requiring-resolve 'futon3c.wm.runner-service/click!) opts))
                 :terminal-evidence (fn [started]
                                      ((terminal/terminal-evidence-port
                                        evidence-roots @prepared) started))})]
    (assoc result :run4/series true)))
