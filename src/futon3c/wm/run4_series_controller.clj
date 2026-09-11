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
            [futon3c.wm.run4-attempt-admission :as admission])
  (:import (java.nio.channels FileChannel)
           (java.nio.file StandardOpenOption)))

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
  (and (map? value)
       (= #{:task-result :evidence-id :infrastructure} (set (keys value)))
       (contains? task-results (:task-result value))
       (string? (:evidence-id value)) (not (str/blank? (:evidence-id value)))
       (contains? #{:safe :unsafe} (:infrastructure value))))

(defn- instant? [value]
  (and (string? value)
       (try (java.time.Instant/parse value) true (catch Throwable _ false))))

(defn- started-event? [event manifest-sha256 manifest trial]
  (and (= #{:schema :series-id :manifest-sha256 :ordinal :trial-id :attempt-id
            :pin-sha256 :click-id :started-at :admission-state}
          (set (keys event)))
       (= :wm/run4-series-started-v1 (:schema event))
       (= (:series-id manifest) (:series-id event))
       (= manifest-sha256 (:manifest-sha256 event))
       (= (:pin-sha256 trial) (:pin-sha256 event))
       (string? (:click-id event)) (not (str/blank? (:click-id event)))
       (instant? (:started-at event))
       (= :click-recorded (:admission-state event))))

(defn- terminal-event? [event manifest-sha256 manifest trial]
  (and (= :wm/run4-series-terminal-v1 (:schema event))
       (= (:series-id manifest) (:series-id event))
       (= manifest-sha256 (:manifest-sha256 event))
       (= (:pin-sha256 trial) (:pin-sha256 event))
       (contains? #{:safe :unsafe} (:infrastructure event))
       (if (= :not-attempted (:task-result event))
         (and (= #{:schema :series-id :manifest-sha256 :ordinal :trial-id
                   :attempt-id :pin-sha256 :task-result :infrastructure :reason}
                 (set (keys event)))
              (= :unsafe (:infrastructure event))
              (contains? #{:busy-admission-rejected :prior-infrastructure-stop}
                         (:reason event)))
         (and (= #{:schema :series-id :manifest-sha256 :ordinal :trial-id
                   :attempt-id :pin-sha256 :task-result :infrastructure :evidence-id}
                 (set (keys event)))
              (contains? task-results (:task-result event))
              (string? (:evidence-id event))
              (not (str/blank? (:evidence-id event)))))))

(defn- existing-admission! [root prepared-trial ordinal]
  (let [attempt-id (get-in prepared-trial [:admission-request :attempt-id])]
    (when-not (.isDirectory (io/file root attempt-id))
      (refuse! :missing-persisted-admission {:ordinal ordinal}))
    (let [reservation (admission/reserve! root (:admission-request prepared-trial))]
      (when-not (:ok reservation)
        (refuse! :persisted-admission-identity-mismatch {:ordinal ordinal}))
      (:admission reservation))))

(defn- started-admission! [root prepared-trial started ordinal]
  (let [status (existing-admission! root prepared-trial ordinal)
        click (get-in status [:result :click])]
    (when-not (and (= :click-recorded (:state status))
                   (= (select-keys started [:click-id :started-at]) click))
      (refuse! :started-admission-mismatch {:ordinal ordinal}))
    status))

(defn- terminal-lifecycle! [root prepared-trial started terminal ordinal]
  (if (= :not-attempted (:task-result terminal))
    (case (:reason terminal)
      :busy-admission-rejected
      (let [status (existing-admission! root prepared-trial ordinal)]
        (when-not (and (nil? started) (= :busy-rejected (:state status))
                       (= :already-running (get-in status [:result :click :rejected])))
          (refuse! :terminal-admission-mismatch {:ordinal ordinal})))
      :prior-infrastructure-stop
      ;; A legitimate predecessor stop is encountered and returned before this
      ;; marker is ever considered as the current trial.
      (refuse! :orphan-prior-stop-marker {:ordinal ordinal}))
    (when-not started
      (refuse! :terminal-without-start {:ordinal ordinal}))))

(defn- event-identity! [event trial]
  (when-not (= (select-keys trial [:ordinal :trial-id :attempt-id])
               (select-keys event [:ordinal :trial-id :attempt-id]))
    (refuse! :persisted-event-identity-mismatch {:ordinal (:ordinal trial)}))
  event)

(defn- event-base [manifest-sha256 manifest trial schema]
  {:schema schema :series-id (:series-id manifest)
   :manifest-sha256 manifest-sha256
   :ordinal (:ordinal trial) :trial-id (:trial-id trial)
   :attempt-id (:attempt-id trial) :pin-sha256 (:pin-sha256 trial)})

(defn- mark-remaining! [root manifest-sha256 manifest from reason]
  (doseq [{:keys [ordinal] :as trial} (drop from (:trials manifest))]
    (append-event! (event-file root ordinal :terminal)
                   (merge (event-base manifest-sha256 manifest trial
                                      :wm/run4-series-terminal-v1)
                          {:task-result :not-attempted :infrastructure :unsafe
                           :reason reason}))))

(defn read-lifecycle!
  "Read and validate the controller's durable lifecycle without advancing it.
  PREPARED-BY-ORDINAL contains the already trusted/preflighted trial values.
  Busy and predecessor-stop markers are accepted only through the same joins
  used by `step!`."
  [root manifest-text prepared-by-ordinal]
  (when-not (and (string? root) (map? prepared-by-ordinal))
    (refuse! :invalid-lifecycle-input))
  (let [root-file (.getCanonicalFile (io/file root))
        key (.getPath root-file)
        manifest (parse-one manifest-text)
        manifest-sha256 (digest/sha256 manifest-text)
        open (read-event (io/file root-file "series.edn") :wm/run4-series-open-v1)]
    (when-not (and open
                   (= {:schema :wm/run4-series-open-v1
                       :series-id (:series-id manifest)
                       :manifest-sha256 manifest-sha256
                       :trial-count (count (:trials manifest))}
                      open))
      (refuse! :series-open-conflict))
    {:schema :wm/run4-series-lifecycle-view-v1
     :series-id (:series-id manifest)
     :manifest-sha256 manifest-sha256
     :trials
     (loop [remaining (:trials manifest) unsafe-predecessor? false rows []]
       (if-let [trial (first remaining)]
         (let [ordinal (:ordinal trial)
               prepared-trial (get prepared-by-ordinal ordinal)
               started (some-> (read-event (event-file root-file ordinal :started)
                                           :wm/run4-series-started-v1)
                               (event-identity! trial))
               terminal (some-> (read-event (event-file root-file ordinal :terminal)
                                            :wm/run4-series-terminal-v1)
                                (event-identity! trial))]
           (when-not prepared-trial
             (refuse! :missing-prepared-lifecycle-trial {:ordinal ordinal}))
           (when (and started
                      (not (started-event? started manifest-sha256 manifest trial)))
             (refuse! :invalid-persisted-started {:ordinal ordinal}))
           (when (and terminal
                      (not (terminal-event? terminal manifest-sha256 manifest trial)))
             (refuse! :invalid-persisted-terminal {:ordinal ordinal}))
           (when started (started-admission! key prepared-trial started ordinal))
           (when terminal
             (if (= :prior-infrastructure-stop (:reason terminal))
               (when-not (and unsafe-predecessor? (nil? started))
                 (refuse! :orphan-prior-stop-marker {:ordinal ordinal}))
               (terminal-lifecycle! key prepared-trial started terminal ordinal)))
           (recur (next remaining)
                  (or unsafe-predecessor?
                      (and terminal (= :unsafe (:infrastructure terminal))
                           (not= :prior-infrastructure-stop (:reason terminal))))
                  (conj rows {:ordinal ordinal :trial trial
                              :started started :terminal terminal})))
         rows))}))

(defn step!
  "Advance at most one durable boundary. Never starts a successor in the same call
  that records its predecessor terminal."
  [root manifest-text {:keys [read-text prepare-trial click! terminal-evidence
                              before-terminal-advance]}]
  (when-not (and (string? root) (fn? click!) (fn? terminal-evidence)
                 (or (nil? before-terminal-advance)
                     (fn? before-terminal-advance)))
    (refuse! :missing-controller-port))
  (let [root-file (.getCanonicalFile (io/file root))
        key (.getPath root-file)
        mutex (get (swap! !locks #(if (contains? % key) % (assoc % key (Object.)))) key)]
    (when-not (.isDirectory root-file)
      (refuse! :invalid-controller-root {:root key}))
    (locking mutex
      (with-open [channel (FileChannel/open
                           (.toPath (io/file root-file ".series.lock"))
                           (into-array StandardOpenOption
                                       [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                  series-lock (.lock channel)]
        (when-not (.isValid series-lock)
          (refuse! :series-lock-unavailable))
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
              (when (and terminal
                         (not (terminal-event? terminal manifest-sha256 manifest trial)))
                (refuse! :invalid-persisted-terminal {:ordinal ordinal}))
              (when (and started
                         (not (started-event? started manifest-sha256 manifest trial)))
                (refuse! :invalid-persisted-started {:ordinal ordinal}))
              (when started
                (started-admission! key prepared-trial started ordinal))
              (when terminal
                (terminal-lifecycle! key prepared-trial started terminal ordinal))
              (when (and terminal before-terminal-advance)
                (before-terminal-advance trial prepared-trial started terminal))
              (cond
                terminal
                (if (= :unsafe (:infrastructure terminal))
                  (do
                    (mark-remaining! root-file manifest-sha256 manifest (inc index)
                                     :prior-infrastructure-stop)
                    {:status :infrastructure-stopped :ordinal ordinal
                     :reason (or (:reason terminal) :terminal-infrastructure-unsafe)})
                  (recur (inc index)))
                started
                (if-let [evidence (terminal-evidence started)]
                  (do
                    (when-not (terminal-evidence? evidence)
                      (refuse! :invalid-terminal-evidence {:ordinal ordinal}))
                    (append-event! terminal-file
                                   (merge evidence
                                          (event-base manifest-sha256 manifest trial
                                                      :wm/run4-series-terminal-v1)))
                    (when (= :unsafe (:infrastructure evidence))
                      (mark-remaining! root-file manifest-sha256 manifest (inc index)
                                       :prior-infrastructure-stop))
                    {:status (if (= :unsafe (:infrastructure evidence))
                               :infrastructure-stopped :trial-terminal)
                     :ordinal ordinal :task-result (:task-result evidence)})
                  {:status :awaiting-terminal-evidence :ordinal ordinal
                   :click-id (:click-id started)})
                (:run4/existing-inspection-only? prepared-trial)
                (refuse! :existing-start-disappeared-or-changed
                         {:ordinal ordinal})
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
                                         (merge (event-base
                                                 manifest-sha256 manifest trial
                                                 :wm/run4-series-terminal-v1)
                                                {:task-result :not-attempted
                                                 :infrastructure :unsafe
                                                 :reason :busy-admission-rejected}))
                          (mark-remaining! root-file manifest-sha256 manifest (inc index)
                                           :prior-infrastructure-stop)
                          {:status :infrastructure-stopped :ordinal ordinal
                           :reason :busy-admission-rejected})
                        (let [event (merge
                                     (event-base manifest-sha256 manifest trial
                                                 :wm/run4-series-started-v1)
                                     {:click-id (:click-id click-result)
                                      :started-at (:started-at click-result)
                                      :admission-state (:state admission-status)})]
                          (append-event! started-file event)
                          {:status :trial-started :ordinal ordinal
                           :click-id (:click-id click-result)}))))))))))))))
