(ns futon3c.wm.run4-series-controller
  "Durable one-transition-at-a-time controller for a frozen RUN4 series.

  This module never infers task completion from click acceptance or service
  status. Advancement requires explicit terminal evidence supplied by the
  eventual serving integration."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.apm.library-loop-runner :as durable]
            [futon3c.wm.run4-attempt-admission :as admission]))

(def task-results #{:succeeded :failed :blocked})
(def stop-rule :attempt-each-once-even-after-fail-or-block)
(defonce ^:private !locks (atom {}))
(def ^:dynamic *atomic-write!* durable/atomic-write-edn!)

(defn- refuse! [reason & [data]]
  (throw (ex-info "RUN4 series refused"
                  (merge {:error :run4-series-refused :reason reason} data))))

(defn- parse-one [text]
  (when-not (string? text) (refuse! :invalid-manifest-text))
  (try
    (with-open [reader (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [value (edn/read {:eof ::empty} reader)]
        (when (= ::empty value) (refuse! :empty-manifest))
        (when-not (= ::end (edn/read {:eof ::end} reader))
          (refuse! :trailing-manifest-form))
        value))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse! :invalid-manifest-edn))))

(defn- id? [value]
  (or (and (keyword? value) (not (str/blank? (name value))))
      (and (string? value) (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._:-]*" value)))))

(defn- sha? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{64}" value))))

(defn- casting? [value]
  (and (map? value)
       (every? #(and (string? %) (not (str/blank? %)))
               ((juxt :author :reviewer :repair-reviewer) value))
       (not= (:author value) (:reviewer value))))

(defn- verify-pin! [read-text {:keys [path sha256]}]
  (when-not (and (string? path) (not (str/blank? path)) (sha? sha256))
    (refuse! :invalid-source-pin))
  (let [text (try (read-text path) (catch Throwable _ (refuse! :unreadable-source {:path path})))]
    (when-not (= sha256 (digest/sha256 text))
      (refuse! :source-drift {:path path}))))

(defn preflight
  "Validate the complete frozen manifest and every prepared trial before use."
  [manifest-text {:keys [read-text prepare-trial]}]
  (when-not (and (fn? read-text) (fn? prepare-trial))
    (refuse! :missing-preflight-port))
  (let [manifest (parse-one manifest-text)
        trials (:trials manifest)
        ordinals (mapv :ordinal trials)]
    (when (= :wm/run4-series-preparation-v1 (:schema manifest))
      (refuse! :production-manifest-preparation-only))
    (when-not (= :wm/run4-series-pin-v1 (:schema manifest))
      (refuse! :unknown-series-schema))
    (when-not (and (id? (:series-id manifest)) (= :frozen (:status manifest))
                   (= :ordinal (:order manifest)) (= stop-rule (:stop-rule manifest))
                   (casting? (:casting manifest)) (vector? trials) (seq trials)
                   (= ordinals (vec (range 1 (inc (count trials))))))
      (refuse! :invalid-series-shape))
    (when-not (= (count trials) (count (distinct (map :trial-id trials))))
      (refuse! :duplicate-trial-id))
    (when-not (and (vector? (:source-pins manifest)) (seq (:source-pins manifest)))
      (refuse! :invalid-source-pins))
    (doseq [source (:source-pins manifest)] (verify-pin! read-text source))
    (let [prepared
          (mapv
           (fn [{:keys [trial-id attempt-id pin-sha256 packet] :as trial}]
             (when-not (and (id? trial-id) (id? attempt-id) (sha? pin-sha256)
                            (map? packet))
               (refuse! :invalid-trial-shape {:ordinal (:ordinal trial)}))
             (verify-pin! read-text packet)
             (let [value (prepare-trial trial)
                   identity (get-in value [:admission-request :identity])]
               (when-not (and (:ok value)
                              (= attempt-id (get-in value [:admission-request :attempt-id]))
                              (= {:series-id (:series-id manifest) :trial-id trial-id
                                  :pin-sha256 pin-sha256 :casting (:casting manifest)}
                                 identity))
                 (refuse! :prepared-trial-identity-mismatch
                          {:ordinal (:ordinal trial)}))
               value))
           trials)]
      {:manifest manifest :manifest-sha256 (digest/sha256 manifest-text)
       :prepared prepared})))

(defn- event-file [root ordinal phase]
  (io/file root (format "%03d-%s.edn" ordinal (name phase))))

(defn- read-event [file expected-schema]
  (when (.exists file)
    (let [path (.getAbsolutePath file)
          value (try (parse-one (slurp file))
                     (catch Throwable _
                       (refuse! :corrupt-series-event {:path path})))]
      (when-not (and (map? value) (= expected-schema (:schema value)))
        (refuse! :corrupt-series-event {:path path}))
      value)))

(defn- append-event! [file event]
  (if-let [existing (read-event file (:schema event))]
    (if (= existing event) existing
        (refuse! :series-event-conflict {:path (.getAbsolutePath file)}))
    (*atomic-write!* file event)))

(defn- terminal-evidence? [value]
  (and (map? value) (contains? task-results (:task-result value))
       (string? (:evidence-id value)) (not (str/blank? (:evidence-id value)))
       (contains? #{:safe :unsafe} (:infrastructure value))))

(defn- event-identity! [event trial]
  (when-not (= (select-keys trial [:ordinal :trial-id :attempt-id])
               (select-keys event [:ordinal :trial-id :attempt-id]))
    (refuse! :persisted-event-identity-mismatch {:ordinal (:ordinal trial)}))
  event)

(defn- mark-remaining! [root manifest from reason]
  (doseq [{:keys [ordinal trial-id attempt-id]} (drop from (:trials manifest))]
    (append-event! (event-file root ordinal :terminal)
                   {:schema :wm/run4-series-terminal-v1 :ordinal ordinal
                    :trial-id trial-id :attempt-id attempt-id
                    :task-result :not-attempted :infrastructure :unsafe
                    :reason reason})))

(defn step!
  "Advance at most one durable boundary. Never starts a successor in the same call
  that records its predecessor terminal."
  [root manifest-text {:keys [read-text prepare-trial click! terminal-evidence]}]
  (when-not (and (string? root) (fn? click!) (fn? terminal-evidence))
    (refuse! :missing-controller-port))
  (let [root-file (.getCanonicalFile (io/file root))
        key (.getPath root-file)
        mutex (get (swap! !locks #(if (contains? % key) % (assoc % key (Object.)))) key)]
    (when-not (.isDirectory root-file)
      (refuse! :invalid-controller-root {:root key}))
    (locking mutex
      (let [{:keys [manifest manifest-sha256 prepared]}
            (preflight manifest-text {:read-text read-text :prepare-trial prepare-trial})]
        (append-event! (io/file root-file "series.edn")
                       {:schema :wm/run4-series-open-v1
                        :series-id (:series-id manifest)
                        :manifest-sha256 manifest-sha256
                        :trial-count (count (:trials manifest))})
        (loop [index 0]
          (if (= index (count (:trials manifest)))
            {:status :series-terminal :series-id (:series-id manifest)}
            (let [trial (nth (:trials manifest) index)
                  prepared-trial (nth prepared index)
                  ordinal (:ordinal trial)
                  started-file (event-file root-file ordinal :started)
                  terminal-file (event-file root-file ordinal :terminal)
                  terminal (some-> (read-event terminal-file :wm/run4-series-terminal-v1)
                                   (event-identity! trial))
                  started (some-> (read-event started-file :wm/run4-series-started-v1)
                                  (event-identity! trial))]
              (cond
                terminal (recur (inc index))
                started
                (if-let [evidence (terminal-evidence started)]
                  (do
                    (when-not (terminal-evidence? evidence)
                      (refuse! :invalid-terminal-evidence {:ordinal ordinal}))
                    (append-event! terminal-file
                                   (merge {:schema :wm/run4-series-terminal-v1
                                           :ordinal ordinal
                                           :trial-id (:trial-id trial)
                                           :attempt-id (:attempt-id trial)} evidence))
                    (when (= :unsafe (:infrastructure evidence))
                      (mark-remaining! root-file manifest (inc index)
                                       :prior-infrastructure-stop))
                    {:status (if (= :unsafe (:infrastructure evidence))
                               :infrastructure-stopped :trial-terminal)
                     :ordinal ordinal :task-result (:task-result evidence)})
                  {:status :awaiting-terminal-evidence :ordinal ordinal
                   :click-id (:click-id started)})
                :else
                (let [reservation (admission/reserve!
                                   key (:admission-request prepared-trial))]
                  (cond
                    (not (:ok reservation)) reservation
                    (and (not (:new? reservation))
                         (= :reconciliation-required
                            (get-in reservation [:admission :state])))
                    {:status :reconciliation-required :ordinal ordinal}
                    :else
                    (let [prior-click (get-in reservation [:admission :result :click])
                          click-result (or prior-click (click! (:opts prepared-trial)))
                          admission-status (if prior-click (:admission reservation)
                                               (admission/record-click!
                                                key (:attempt-id trial) click-result))]
                      (if (= :already-running (:rejected click-result))
                        (do
                          (append-event! terminal-file
                                         {:schema :wm/run4-series-terminal-v1
                                          :ordinal ordinal :trial-id (:trial-id trial)
                                          :attempt-id (:attempt-id trial)
                                          :task-result :not-attempted
                                          :infrastructure :unsafe
                                          :reason :busy-admission-rejected})
                          (mark-remaining! root-file manifest (inc index)
                                           :prior-infrastructure-stop)
                          {:status :infrastructure-stopped :ordinal ordinal
                           :reason :busy-admission-rejected})
                        (let [event {:schema :wm/run4-series-started-v1
                                     :series-id (:series-id manifest)
                                     :manifest-sha256 manifest-sha256
                                     :ordinal ordinal :trial-id (:trial-id trial)
                                     :attempt-id (:attempt-id trial)
                                     :pin-sha256 (:pin-sha256 trial)
                                     :click-id (:click-id click-result)
                                     :started-at (:started-at click-result)
                                     :admission-state (:state admission-status)}]
                          (append-event! started-file event)
                          {:status :trial-started :ordinal ordinal
                           :click-id (:click-id click-result)})))))))))))))
