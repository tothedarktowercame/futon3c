(ns futon3c.wm.run4-realized-recording
  "Construct the existing realized-recording envelope from a strictly joined
  RUN4 terminal bundle. Missing paired-step observations remain explicit
  unknowns; this adapter never manufactures a previous accepted WM step."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.realized-recording :as recording]))

(defn- safe-id? [x]
  (and (string? x) (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" x))))

(defn- refuse! [reason]
  (throw (ex-info "RUN4 realized recording refused"
                  {:error :run4-realized-recording-refused :reason reason})))

(defn from-terminal-bundle
  [bundle]
  (let [{:keys [identity attempt-id run-record projection-digest
                run-record-digest]} bundle
        run-id (:run/id run-record)
        trial-id (:trial-id identity)
        source :terminal-bundle
        unknown recording/unknown
        evidence {source {:digest projection-digest
                          :locator (str "run4-terminal-bundle:" run-id)
                          :adapter :wm/run4-terminal-evidence-bundle-v1
                          :run-record-digest run-record-digest}}
        projection (:terminal-projection bundle)
        outcome (:outcome projection)
        selected-action (get-in projection [:checkpoints :selection
                                            :judgment :selected-action])
        decision-ref (unknown :decision-identity-not-projected)
        tick (unknown :wm-tick-not-projected)
        context
        {:record/id (str run-id "/" trial-id "/realized/0")
         :revision 0 :supersedes nil
         :run/id run-id :decision/ref decision-ref :attempt/id attempt-id
         :policy selected-action :tick tick
         :window {:rule :durable-trial-evidence :version 1 :clock :utc
                  :decision (unknown :prior-accepted-step-not-recorded)
                  :start (unknown :dispatch-time-not-projected)
                  :cutoff (unknown :terminal-time-not-projected)
                  :observed-at (unknown :observation-time-not-projected)}
         :subject {:mission (get-in projection [:run4/task-pin :mission-id])
                   :before (unknown :prior-revision-not-recorded)
                   :after (unknown :terminal-revision-not-recorded)}
         :execution {:selected (some? selected-action) :state :terminal
                     :evidence source
                     :actor (recording/observed
                             (get-in identity [:casting :author]) :actor-id source)
                     :policy (if selected-action
                               (recording/observed selected-action
                                                   :mission-action source)
                               {:status :not-applicable
                                :reason :selected-policy-not-projected})}
         :expected-score nil :realized-score nil :scale :task-result
         :measurement {:id :run4-terminal-result-v1 :quantity :task-result
                       :units :task-result :sign :categorical
                       :method :strict-durable-join-v1
                       :expected-source :not-recorded
                       :expected-window :not-recorded
                       :realized-window :terminal-projection
                       :expected (unknown :forecast-not-recorded)
                       :realized (unknown :numeric-realization-not-applicable)}
         :outcome outcome
         :classification (if outcome
                           (merge (recording/observed outcome :wm/cohort-v1 source)
                                  {:classifier :run4-terminal-evidence-v1
                                   :grain :attempt :basis :strict-durable-join})
                           (merge (unknown :terminal-classification-unavailable)
                                  {:classifier :run4-terminal-evidence-v1
                                   :grain :attempt :basis :strict-durable-join}))
         :closure {:state :unknown :reason :operator-acceptance-not-recorded}
         :observations {:version 1 :attempt/id attempt-id :decision/ref decision-ref
                        :alignment {:rule :single-terminal-bundle :version 1}
                        :checkpoints (unknown :checkpoint-times-not-projected)
                        :channels {:pre (unknown :prior-accepted-step-not-recorded)
                                   :post (unknown :wm-trace-observation-not-recorded)}}
         :preferences {:module (unknown :module-not-pinned)
                       :predicted (unknown :forecast-not-recorded)
                       :realized (unknown :readings-not-recorded)
                       :assessment (unknown :assessment-not-performed)
                       :occurrences []}
         :evidence evidence
         :review {:state :proposed :scope :attempt :evidence source
                  :designated-reviewer "Joe"}}]
    (recording/envelope {} context)))

(defn- recording-file [root bundle]
  (when-not (and (string? root) (.isDirectory (io/file root))
                 (safe-id? (:attempt-id bundle)))
    (refuse! :invalid-recording-authority))
  (io/file root (str (:attempt-id bundle) ".edn")))

(defn persist-bundle!
  "Persist one immutable established-contract record. This is deliberately a
  producer operation separate from acceptance reporting."
  [root bundle]
  (let [value (from-terminal-bundle bundle)
        file (recording-file root bundle)]
    (recording/persist! (.getCanonicalPath file) value)
    {:path (.getCanonicalPath file) :record value}))

(defn read-bundle-recording!
  "Strictly reread one form and require exact equality with the record derived
  from the currently validated terminal bundle."
  [root bundle]
  (let [file (recording-file root bundle)]
    (when (.exists file)
      (let [text (slurp file)
            value (try
                    (with-open [r (java.io.PushbackReader.
                                   (java.io.StringReader. text))]
                      (let [v (edn/read {:eof ::empty} r)]
                        (when (or (= ::empty v)
                                  (not= ::end (edn/read {:eof ::end} r)))
                          (refuse! :invalid-recording-form))
                        v))
                    (catch clojure.lang.ExceptionInfo e (throw e))
                    (catch Throwable _ (refuse! :invalid-recording-form)))
            expected (from-terminal-bundle bundle)]
        (when-not (= expected value) (refuse! :recording-binding-mismatch))
        (recording/validate! value)))))
