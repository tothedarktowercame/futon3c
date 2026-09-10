(ns futon3c.wm.run4-terminal-projection
  "Immutable projection of returned full-loop evidence. This records facts; it
  does not classify series outcomes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.apm.library-loop-runner :as durable]))

(def required-checkpoints [:selection :construction :dispatch :build :adjudication])
(def ^:dynamic *atomic-write!* durable/atomic-write-edn!)

(defn- refuse! [reason & [data]]
  (throw (ex-info "RUN4 terminal projection refused"
                  (merge {:error :run4-terminal-projection-refused :reason reason} data))))

(defn- parse-one-text [text]
  (try
    (with-open [reader (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [value (edn/read {:eof ::empty} reader)]
        (when (= ::empty value) (refuse! :empty-source))
        (when-not (= ::end (edn/read {:eof ::end} reader))
          (refuse! :trailing-source-form))
        value))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse! :unreadable-source))))

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- sha? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))

(defn- contains-in? [m [key & more]]
  (and (map? m) (contains? m key)
       (or (empty? more) (contains-in? (get m key) more))))

(defn- pin? [pin]
  (and (map? pin) (sha? (:sha256 pin))
       (some? (:series-id pin)) (some? (:trial-id pin))))

(defn- judgment-projection [phase judgment]
  (case phase
    :dispatch
    (select-keys judgment [:agent :availability :job-id :recovers])
    :build
    (let [validation (:validation judgment)]
      {:commits (vec (or (:commits judgment) []))
       :validation {:approved? (:approved? validation)
                    :review-job (:review-job validation)
                    :review-gate (select-keys (:review-gate validation)
                                              [:required? :executed? :tool-events
                                               :passed? :failure-kind])}})
    :adjudication
    {:build-match (select-keys (:build-match judgment) [:commit :review-approved?])
     :dial (select-keys (:dial judgment) [:moved? :implementation-id])}
    (select-keys judgment [:outcome :failure-kind :failure-stage :run4/task-pin])))

(defn- checkpoint-projection [checkpoints phase]
  (if (contains? checkpoints phase)
    (let [cell (get checkpoints phase)]
      (when-not (and (map? cell) (map? (:judgment cell)) (map? (:ground cell)))
        (refuse! :malformed-checkpoint {:checkpoint phase}))
      {:status :present
       :judgment (judgment-projection phase (:judgment cell))
       :ground (select-keys (:ground cell)
                            [:kind :reason :outcome :repository :run4/task-pin])})
    {:status :absent :reason :checkpoint-not-returned}))

(defn projection
  "Return nil for a legacy result, or a strict RUN4 evidence projection."
  [click-id result]
  (let [selection-path [:checkpoints :selection :ground :run4/task-pin]
        construction-path [:checkpoints :construction :judgment :run4/task-pin]
        selection-present? (contains-in? result selection-path)
        construction-present? (contains-in? result construction-path)
        run4-present? (or selection-present? construction-present?)
        selection-pin (get-in result selection-path)
        construction-pin (get-in result [:checkpoints :construction :judgment :run4/task-pin])
        pin (or selection-pin construction-pin)]
    (when run4-present?
      (when (or (and selection-present? (not (pin? selection-pin)))
                (and construction-present? (not (pin? construction-pin))))
        (refuse! :malformed-returned-evidence))
      (when-not (and (nonblank? click-id) (nonblank? (:run/id result))
                     (nonblank? (:attempt-id result)) (keyword? (:outcome result))
                     (map? (:checkpoints result)) (map? (:data result)) (pin? pin))
        (refuse! :malformed-returned-evidence))
      (when (and selection-pin construction-pin (not= selection-pin construction-pin))
        (refuse! :conflicting-task-pin-checkpoints))
      (let [run-record-path (:run-record result)]
        (when-not (nonblank? run-record-path) (refuse! :missing-run-record))
        (let [run-file (.getCanonicalFile (io/file run-record-path))
              run-bytes (try (java.nio.file.Files/readAllBytes (.toPath run-file))
                             (catch Throwable _ (refuse! :unreadable-run-record)))
              run-text (String. run-bytes java.nio.charset.StandardCharsets/UTF_8)
              run-record (parse-one-text run-text)]
          (when-not (and (map? run-record)
                         (= click-id (:click/id run-record))
                         (= (:run/id result) (:run/id run-record))
                         (= pin (:run4/task-pin run-record)))
            (refuse! :run-record-binding-mismatch))
          {:schema :wm-run4-terminal-projection-v1
           :click/id click-id :run/id (:run/id result)
           :attempt/id (:attempt-id result) :run4/task-pin pin
           :outcome (:outcome result)
           :checkpoints (into {} (map (juxt identity
                                            #(checkpoint-projection (:checkpoints result) %))
                                      required-checkpoints))
           :failure {:kind (get-in result [:data :failure-kind])
                     :stage (get-in result [:data :failure-stage])}
           :evidence {:commit (get-in result [:data :commit])
                      :author-job-id (get-in result [:data :author-job :job-id])
                      :reviewer-job-id (get-in result [:data :review-job :job-id])
                      :grounding-witness
                      (select-keys (get-in result [:data :witness])
                                   [:resolved? :dial-moved? :implementation-id
                                    :discharge-id :before :after])}
           :source {:run-record (.getPath run-file)
                    :run-record-sha256 (digest/sha256 run-text)}})))))

(defn persist!
  "Persist an immutable projection. Returns its path and content digest, or nil
  for a non-RUN4 result."
  [root click-id result]
  (when-let [value (projection click-id result)]
    (when-not (and (string? root) (.isDirectory (io/file root)))
      (refuse! :invalid-projection-root))
    (let [file (io/file root (str "run4-terminal-projection-" click-id ".edn"))
          content-sha256 (digest/sha256 (pr-str value))]
      (if (.exists file)
        (when-not (= value (parse-one-text (slurp file)))
          (refuse! :projection-replay-conflict))
        (*atomic-write!* file value))
      {:path (.getAbsolutePath file) :sha256 content-sha256
       :source-sha256 (get-in value [:source :run-record-sha256])})))
