(ns futon3c.wm.run4-historical-projection
  "Immutable facts for a historical repair admission.  This is deliberately
  not a RUN4 task terminal projection and carries no task verdict."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.repair-obligation :as repair-store]
            [futon3c.wm.run4-realized-recording :as recording]
            [futon3c.wm.run4-terminal-evidence :as evidence]))

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
    (when (= :revalidate-historical-repair (:type enacted))
      (when-not (and (= :historical-verification-awaiting-validation (:outcome result))
                     (nonblank? click-id) (nonblank? (:run/id result))
                     (nonblank? (:attempt-id result))
                     (= :authenticated-not-enacted (:status requested))
                     (map? (:identity requested))
                     (sha? (get-in requested [:identity :pin-sha256]))
                     (map? enacted))
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
                       (= (:attempt-id result) (:runner-attempt/id record))
                       (= requested (:run4/requested-pin record))
                       (= enacted (:run4/enacted-action record))
                       (nonblank? (:run4/controller-attempt-id record))
                       (= transition (:historical-verification record))
                       (= :wm/historical-repair-admission-v1 (:schema transition))
                       (= :awaiting-validation (:repair/status transition))
                       (= false (get-in adjudication [:judgment :repair-resolved?])))
          (refuse! :historical-run-record-binding-mismatch))
        {:schema :wm/run4-historical-admission-projection-v1
         :click/id click-id :run/id (:run/id result)
         :controller-attempt/id (:run4/controller-attempt-id record)
         :runner-attempt/id (:attempt-id result)
         :execution-attempt (:verification-attempt transition)
         :requested-pin requested :enacted-action enacted
         :repair {:id (:repair/id transition)
                  :status :awaiting-validation
                  :verification-id (:verification-id transition)
                  :verification-source (:verification-source transition)
                  :verification-artifact (:verification-artifact transition)
                  :resolved? false :production-successor-required? true}
         :repair-transition transition
         :cohort (:execution-cohort record)
         :source {:run-record (.getPath file)
                  :run-record-sha256 (digest/sha256 text)}}))))

(defn persist! [root click-id result]
  (when-let [value (projection click-id result)]
    (when-not (and (nonblank? click-id)
                   (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" click-id)
                   (string? root) (.isDirectory (io/file root)))
      (refuse! :invalid-projection-root))
    (let [canonical-root (.getCanonicalFile (io/file root))
          file (.getCanonicalFile
                (io/file canonical-root
                         (str "run4-historical-projection-" click-id ".edn")))
          sha (digest/sha256 (pr-str value))]
      (when-not (= canonical-root (.getCanonicalFile (.getParentFile file)))
        (refuse! :projection-path-escape))
      (try
        (recording/publish-immutable! file value)
        (catch clojure.lang.ExceptionInfo e
          (refuse! (or (:reason (ex-data e)) :projection-publication-failed))))
      {:path (.getAbsolutePath file) :sha256 sha
       :source-sha256 (get-in value [:source :run-record-sha256])})))

(defn- confined-file [root path]
  (when-not (and (nonblank? root) (nonblank? path))
    (refuse! :invalid-evidence-reference))
  (let [r (.getCanonicalFile (io/file root)) f (.getCanonicalFile (io/file path))]
    (when-not (and (.isDirectory r) (.isFile f)
                   (.startsWith (.toPath f) (.toPath r)))
      (refuse! :evidence-reference-outside-authority))
    f))

(defn read-bundle!
  "Read an admitted historical outcome through the shared strict admission and
  click-binding join. Returns nil only before binding publication."
  [{:keys [projections run-records repair-root cohort-preregistration
           cohort-data-root] :as roots} admission-request started]
  (when-let [binding (evidence/read-admission-click-binding!
                      roots admission-request started)]
    (when (contains? binding :run4/historical-projection)
      (let [ref (:run4/historical-projection binding)]
      (when-not (and (map? ref) (= #{:path :sha256 :source-sha256} (set (keys ref)))
                     (sha? (:sha256 ref)) (sha? (:source-sha256 ref)))
        (refuse! :missing-historical-projection-reference))
      (let [file (confined-file projections (:path ref))
            text (slurp file)
            value (parse-one text)]
        (when-not (and (= #{:schema :click/id :run/id :controller-attempt/id
                            :runner-attempt/id :execution-attempt :requested-pin
                            :enacted-action :repair :repair-transition :cohort :source}
                          (set (keys value)))
                       (= (:sha256 ref) (digest/sha256 (pr-str value)))
                       (= :wm/run4-historical-admission-projection-v1 (:schema value))
                       (= (:click-id started) (:click/id value))
                       (= :historical-verification-awaiting-validation
                          (:outcome binding))
                       (= (:runner-attempt/id value) (:attempt/id binding))
                       (= (:run/id value) (get-in binding [:run-id-observation :value]))
                       (= (:attempt-id admission-request)
                          (:controller-attempt/id value))
                       (= :authenticated-not-enacted
                          (get-in value [:requested-pin :status]))
                       (= #{:status :identity :operator-selection}
                          (set (keys (:requested-pin value))))
                       (= (:identity admission-request)
                          (get-in value [:requested-pin :identity]))
                       (= #{:series-id :trial-id :pin-sha256 :casting}
                          (set (keys (get-in value [:requested-pin :identity]))))
                       (= :revalidate-historical-repair
                          (get-in value [:enacted-action :type]))
                       (= (get-in value [:repair :id])
                          (get-in value [:enacted-action :repair-obligation :repair/id]))
                       (= :awaiting-validation (get-in value [:repair :status]))
                       (= #{:id :status :verification-id :verification-source
                            :verification-artifact :resolved?
                            :production-successor-required?}
                          (set (keys (:repair value))))
                       (false? (get-in value [:repair :resolved?]))
                       (true? (get-in value [:repair :production-successor-required?]))
                       (= (:execution-attempt value)
                          (get-in value [:repair-transition :verification-attempt]))
                       (= (:runner-attempt/id value)
                          (get-in value [:execution-attempt :id]))
                       (= (get-in value [:repair :id])
                          (get-in value [:repair-transition :repair/id]))
                       (= (get-in value [:repair :verification-id])
                          (get-in value [:repair-transition :verification-id]))
                       (= (get-in value [:repair :verification-source])
                          (get-in value [:repair-transition :verification-source]))
                       (= (get-in value [:repair :verification-artifact])
                          (get-in value [:repair-transition :verification-artifact])))
          (refuse! :historical-projection-binding-mismatch))
        (when-not (and (nonblank? repair-root)
                       (nonblank? (get-in value [:repair :id]))
                       (map? (:repair-transition value))
                       (= (:repair-transition value)
                          (some->> (repair-store/obligation-history
                                    repair-root (get-in value [:repair-transition :failed-attempt]))
                                   (filter #(= (get-in value [:repair :id])
                                               (:repair/id %))) first
                                   :repair/verification)))
          (refuse! :historical-verification-store-mismatch))
        (when-not (= (get-in value [:cohort :sha256])
                     (try (digest/sha256 (slurp cohort-preregistration))
                          (catch Throwable _ (refuse! :historical-cohort-unreadable))))
          (refuse! :historical-cohort-source-mismatch))
        (let [ledger (try (cohort/ledger cohort-preregistration cohort-data-root)
                          (catch Throwable _ (refuse! :historical-cohort-unreadable)))
              attempt (some #(when (= (:runner-attempt/id value) (:attempt/id %)) %)
                            (:attempts ledger))]
          (when-not (and (map? (:activation ledger))
                         (= (get-in value [:cohort :cohort-id]) (:cohort/id ledger))
                         (:closed? attempt)
                         (= :historical-verification-awaiting-validation
                            (:outcome attempt)))
            (refuse! :historical-cohort-binding-mismatch)))
        (let [run-file (confined-file run-records (get-in value [:source :run-record]))
              run-text (slurp run-file)
              run-record (parse-one run-text)]
          (when-not (and (= (:source-sha256 ref) (digest/sha256 run-text))
                         (= (:source-sha256 ref)
                            (get-in value [:source :run-record-sha256]))
                         (= (:run/id value) (:run/id run-record))
                         (= (:click/id value) (:click/id run-record))
                         (= (:controller-attempt/id value)
                            (:run4/controller-attempt-id run-record)))
            (refuse! :historical-run-record-source-mismatch))
          {:schema :wm/run4-historical-admission-bundle-v1
           :identity (:identity admission-request)
           :attempt-id (:attempt-id admission-request)
           :started started :click-run-binding binding
           :projection value :run-record run-record
           :classification {:task-result :unknown
                            :repair-status :awaiting-validation
                            :infrastructure :unknown
                            :production-successor-required? true}}))))))

(defn persist-observation!
  "Persist an observational historical admission. It is not a task terminal."
  [root bundle]
  (when-not (and (string? root) (.isDirectory (io/file root))
                 (= :wm/run4-historical-admission-bundle-v1 (:schema bundle)))
    (refuse! :invalid-observation))
  (let [attempt (:attempt-id bundle)]
    (when-not (and (nonblank? attempt)
                   (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" attempt))
      (refuse! :invalid-observation-identity))
    (let [value {:schema :wm/run4-historical-admission-observation-v1
                 :attempt-id attempt :identity (:identity bundle)
                 :click-id (get-in bundle [:started :click-id])
                 :run-id (get-in bundle [:projection :run/id])
                 :repair (get-in bundle [:projection :repair])
                 :task-verdict :unknown
                 :controller-state :awaiting-terminal-evidence
                 :visibility {:stage "review" :result "pending"
                              :reason "historical repair awaiting validation"}}
          file (io/file root (str "run4-historical-observation-" attempt ".edn"))]
      (recording/publish-immutable! file value)
      {:path (.getAbsolutePath file) :sha256 (digest/sha256 (pr-str value))})))
