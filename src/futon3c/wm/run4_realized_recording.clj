(ns futon3c.wm.run4-realized-recording
  "Construct the existing realized-recording envelope from a strictly joined
  RUN4 terminal bundle. Missing paired-step observations remain explicit
  unknowns; this adapter never manufactures a previous accepted WM step."
  (:require [futon2.aif.realized-recording :as recording]))

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
        context
        {:record/id (str run-id "/" trial-id "/realized/0")
         :revision 0 :supersedes nil
         :run/id run-id :decision/ref trial-id :attempt/id attempt-id
         :policy (get-in identity [:casting :author]) :tick trial-id
         :window {:rule :durable-trial-evidence :version 1 :clock :utc
                  :decision (unknown :prior-accepted-step-not-recorded)
                  :start (unknown :dispatch-time-not-projected)
                  :cutoff (unknown :terminal-time-not-projected)
                  :observed-at (unknown :observation-time-not-projected)}
         :subject {:mission (get-in projection [:run4/task-pin :mission-id])
                   :before (unknown :prior-revision-not-recorded)
                   :after (unknown :terminal-revision-not-recorded)}
         :execution {:selected true :state :terminal :evidence source
                     :policy (recording/observed
                              (get-in identity [:casting :author])
                              :actor-id source)}
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
         :observations {:version 1 :attempt/id attempt-id :decision/ref trial-id
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
