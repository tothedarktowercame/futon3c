(ns futon3c.wm.run4-historical-projection
  "Immutable facts for a historical repair admission.  This is deliberately
  not a RUN4 task terminal projection and carries no task verdict."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.apm.library-loop-runner :as durable]))

(def ^:dynamic *atomic-write!* durable/atomic-write-edn!)

(defn- refuse! [reason]
  (throw (ex-info "RUN4 historical projection refused"
                  {:error :run4-historical-projection-refused :reason reason})))

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- sha? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))

(defn- parse-one [text]
  (try
    (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [v (edn/read {:eof ::empty} r)]
        (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)))
          (refuse! :invalid-run-record))
        v))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse! :invalid-run-record))))

(defn projection [click-id result]
  (let [requested (get-in result [:checkpoints :selection :ground :run4/requested-pin])
        enacted (get-in result [:checkpoints :selection :ground :run4/enacted-action])]
    (when requested
      (when-not (and (= :historical-verification-awaiting-validation (:outcome result))
                     (nonblank? click-id) (nonblank? (:run/id result))
                     (nonblank? (:attempt-id result))
                     (= :authenticated-not-enacted (:status requested))
                     (map? (:identity requested))
                     (sha? (get-in requested [:identity :pin-sha256]))
                     (map? enacted)
                     (= :revalidate-historical-repair (:type enacted)))
        (refuse! :malformed-historical-result))
      (let [path (:run-record result)
            file (when (nonblank? path) (.getCanonicalFile (io/file path)))
            bytes (try (java.nio.file.Files/readAllBytes (.toPath file))
                       (catch Throwable _ (refuse! :unreadable-run-record)))
            text (String. bytes java.nio.charset.StandardCharsets/UTF_8)
            record (parse-one text)
            transition (get-in result [:data :repair-obligation])
            adjudication (get-in result [:checkpoints :adjudication])]
        (when-not (and (= click-id (:click/id record))
                       (= (:run/id result) (:run/id record))
                       (= (:attempt-id result) (:attempt-id record))
                       (= requested (get-in record [:checkpoints :selection :ground
                                                    :run4/requested-pin]))
                       (= enacted (get-in record [:checkpoints :selection :ground
                                                  :run4/enacted-action]))
                       (= :wm/historical-repair-admission-v1 (:schema transition))
                       (= :awaiting-validation (:repair/status transition))
                       (= false (get-in adjudication [:judgment :repair-resolved?])))
          (refuse! :historical-run-record-binding-mismatch))
        {:schema :wm/run4-historical-admission-projection-v1
         :click/id click-id :run/id (:run/id result)
         :controller-attempt/id (:attempt-id result)
         :execution-attempt (:verification-attempt transition)
         :requested-pin requested :enacted-action enacted
         :repair {:id (:repair/id transition)
                  :status :awaiting-validation
                  :verification-id (:verification-id transition)
                  :verification-source (:verification-source transition)
                  :verification-artifact (:verification-artifact transition)
                  :resolved? false :production-successor-required? true}
         :cohort (:cohort result)
         :source {:run-record (.getPath file)
                  :run-record-sha256 (digest/sha256 text)}}))))

(defn persist! [root click-id result]
  (when-let [value (projection click-id result)]
    (when-not (and (string? root) (.isDirectory (io/file root)))
      (refuse! :invalid-projection-root))
    (let [file (io/file root (str "run4-historical-projection-" click-id ".edn"))
          sha (digest/sha256 (pr-str value))]
      (if (.exists file)
        (when-not (= value (parse-one (slurp file)))
          (refuse! :projection-replay-conflict))
        (*atomic-write!* file value))
      {:path (.getAbsolutePath file) :sha256 sha
       :source-sha256 (get-in value [:source :run-record-sha256])})))
