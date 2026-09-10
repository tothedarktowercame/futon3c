(ns futon3c.wm.run4-pinned-run-config
  "Materialize a task-pin-bound RUN4 sheet into existing full-loop options.

  The sheet may select only house options already consumed by Futon2.  It
  cannot provide functions, actors, endpoints, paths for output, or serving
  authority.  Explicit false values are retained; absent keys stay absent."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as c-fold]))

(def runner-option-keys
  #{:cohort? :window-days :accumulate-strategic-habit?
    :beta-habit-in-both? :policy-depth})
(def materialized-fold-keys
  [:ruled-outcome-c-enabled? :seeded-c :disposition-kernel :c-fold-provenance])

(defn- refuse! [reason & [data]]
  (throw (ex-info "RUN4 pinned run config refused"
                  (merge {:error :run4-pinned-run-config-refused :reason reason}
                         data))))

(defn- parse-one [text]
  (when-not (string? text) (refuse! :invalid-config-content))
  (try
    (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [value (edn/read {:eof ::empty} r)]
        (when (= ::empty value) (refuse! :empty-config))
        (when-not (= ::end (edn/read {:eof ::end} r))
          (refuse! :trailing-config-form))
        value))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse! :invalid-config-edn))))

(defn- valid-runner-options? [opts]
  (and (map? opts)
       (every? runner-option-keys (keys opts))
       (every? #(or (not (contains? opts %)) (boolean? (get opts %)))
               [:cohort? :accumulate-strategic-habit? :beta-habit-in-both?])
       (or (not (contains? opts :window-days)) (pos-int? (:window-days opts)))
       (or (not (contains? opts :policy-depth))
           (let [depth (:policy-depth opts)]
             (and (map? depth) (= #{:anticipation :cascade-rollout} (set (keys depth)))
                  (pos-int? (:anticipation depth))
                  (pos-int? (:cascade-rollout depth)))))))

(defn- valid-c-fold? [fold]
  (and (map? fold) (boolean? (:enabled? fold))
       (if (:enabled? fold)
         (and (= #{:enabled? :seed :kernel} (set (keys fold)))
              (= #{:id :path :sha256} (set (keys (:seed fold))))
              (= #{:adapter :path :sha256} (set (keys (:kernel fold)))))
         (= #{:enabled?} (set (keys fold))))))

(defn- normalized-ref [config-ref requested]
  (when-not (and (string? requested) (not (str/blank? requested))
                 (not (.isAbsolute (io/file requested))))
    (refuse! :config-artifact-reference-invalid))
  (let [parent (.getParentFile (io/file config-ref))]
    (.toString (.normalize (.toPath (io/file (or parent (io/file ".")) requested))))))

(defn load!
  "Load CONFIG-PIN via READ-TEXT and return a finite map of full-loop options."
  [{:keys [path sha256] :as config-pin} read-text]
  (when-not (and (map? config-pin) (string? path) (string? sha256) (fn? read-text))
    (refuse! :invalid-config-pin))
  (let [text (try (read-text path)
                  (catch Throwable _ (refuse! :config-unreadable)))
        _ (when-not (= sha256 (c-fold/sha256 text)) (refuse! :config-source-drift))
        sheet (parse-one text)]
    (when-not (and (map? sheet)
                   (= #{:schema :runner-options :c-fold} (set (keys sheet)))
                   (= :wm/run4-pinned-run-config-v1 (:schema sheet))
                   (valid-runner-options? (:runner-options sheet))
                   (valid-c-fold? (:c-fold sheet)))
      (refuse! :unsupported-config-shape))
    (let [reader (fn [requested]
                   (read-text (if (= requested path)
                                path
                                (normalized-ref path requested))))
          materialized (try
                         (c-fold/resolve-opts (:runner-options sheet) path reader)
                         (catch clojure.lang.ExceptionInfo e
                           (refuse! :c-fold-materialization-refused
                                    {:cause (:reason (ex-data e))})))]
      (merge (:runner-options sheet)
             (select-keys materialized materialized-fold-keys)
             {:run4/config-pin config-pin}))))
