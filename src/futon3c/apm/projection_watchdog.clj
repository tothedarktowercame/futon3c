(ns futon3c.apm.projection-watchdog
  "Contract-aware health evaluation for an APM projection and its live job."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.queued-frame-terminal :as queued-terminal])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.file Files Path]
           [java.time Duration Instant]))

(def obligation-ids
  [:transition-log-readable :coordinator-readable :coordinator-running
   :coordinator-heartbeat-current :pending-intent-shaped
   :projection-publication-current :unattended-transition-current
   :active-phase-state-readable
   :projected-job-matches-durable-state :agency-agent-reachable
   :agency-job-running :terminal-job-collection-current
   :job-within-declared-timeout
   :terminal-budgets-valid :cascade-operation-conformant
   :coordinator-last-result-successful])

(def projection-catchup-grace-seconds 10)

(defn- age-seconds [^Instant now value]
  (when value
    (.getSeconds (Duration/between (Instant/parse value) now))))

(defn- finding [id code evidence]
  {:obligation id :error/code code :evidence evidence})

(defn evaluate
  "Pure watchdog evaluation. INPUTS must contain parsed durable observations."
  [{:keys [now coordinator coordinator-age-seconds transition publication
           phase-state agent job frame-closed? max-heartbeat-age-seconds
           cascade-operation]}]
  (let [operation (:operation transition)
        now-ms (.toEpochMilli ^Instant now)
        cascade-present? (some? cascade-operation)
        cascade-running? (= :running (:status cascade-operation))
        cascade-shaped?
        (and (= :memory-cascade-operation (:state/type cascade-operation))
             (= :memory-cascade-expansion (:operation cascade-operation))
             (string? (:operation/id cascade-operation))
             (not (str/blank? (:operation/id cascade-operation)))
             (string? (:frame-id cascade-operation))
             (string? (:problem-id cascade-operation))
             (keyword? (:phase cascade-operation))
             (pos-int? (:attempt cascade-operation))
             (contains? #{:running :succeeded :failed}
                        (:status cascade-operation))
             (nat-int? (:started-at-ms cascade-operation))
             (pos-int? (:budget-ms cascade-operation))
             (= (+ (:started-at-ms cascade-operation)
                   (:budget-ms cascade-operation))
                (:deadline-at-ms cascade-operation))
             (or cascade-running?
                 (and (nat-int? (:finished-at-ms cascade-operation))
                      (map? (:result cascade-operation))
                      (contains? #{:ok :failed :failed-503}
                                 (get-in cascade-operation
                                         [:result :outcome])))))
        cascade-expired? (and cascade-running? cascade-shaped?
                              (> now-ms (:deadline-at-ms cascade-operation)))
        cascade-within-bound? (and cascade-running? cascade-shaped?
                                   (not cascade-expired?)
                                   (= (:frame-id transition)
                                      (:frame-id cascade-operation))
                                   (= (:problem-id transition)
                                      (:problem-id cascade-operation)))
        transport-retry? (= :awaiting-transport-retry (:stage phase-state))
        substrate-resume-at-ms
        (when (and (true? (get-in coordinator [:regulator/last-result :ok]))
                   (= :awaiting-substrate
                      (get-in coordinator [:regulator/last-result :status])))
          (get-in coordinator
                  [:regulator/last-result :retry/not-before-ms]))
        substrate-wait? (and (nat-int? substrate-resume-at-ms)
                             (< now-ms substrate-resume-at-ms))
        ;; Solver phases wrap the same durable live-job state in the bounded
        ;; round machine. Observe its active member without weakening any job
        ;; identity, timeout, or terminal-budget check.
        phase-state (cond
                      (= :solver-rounds (:state/type phase-state))
                      (:active phase-state)

                      (= :promotion (:state/type phase-state))
                      (assoc phase-state :state/type :live-job-dispatched)

                      :else phase-state)
        complete? (and (= :complete (:regulator/status coordinator))
                       (true? (get-in coordinator [:regulator/last-result :ok]))
                       (= :frame-complete
                          (get-in coordinator [:regulator/last-result :status])))
        waiting? (and (not transport-retry?)
                      (not complete?)
                      (not frame-closed?)
                      (= :waiting-for-terminal-result (:status operation)))
        request (:request phase-state)
        ticket (:ticket phase-state)
        timeout-ms (:turn-timeout-ms request)
        budget (:terminal-budget request)
        job-age (age-seconds now (get-in agent [:agent :invoke-started-at]))
        terminal-job? (contains? #{"done" "failed" "cancelled" "timed-out"}
                                 (get-in job [:job :state]))
        terminal-age (age-seconds now (get-in job [:job :finished-at]))
        transition-age (age-seconds now (:event/observed-at transition))
        findings
        (cond-> []
          (not (or (= :running (:regulator/status coordinator)) complete?))
          (conj (finding :coordinator-running :coordinator-not-running
                         {:observed (:regulator/status coordinator)}))
          (and (> coordinator-age-seconds max-heartbeat-age-seconds)
               (not cascade-within-bound?))
          (conj (finding :coordinator-heartbeat-current
                         :coordinator-heartbeat-stale
                         {:age-seconds coordinator-age-seconds
                          :limit-seconds max-heartbeat-age-seconds}))
          (and (:coordinator/pending-intent coordinator)
               (not-every? #(some? (get (:coordinator/pending-intent coordinator) %))
                           [:intent/digest :dispatch/id :dispatch/action :job-id
                            :pre-state/digest :pre-state/version
                            :expected/postcondition]))
          (conj (finding :pending-intent-shaped :pending-intent-incomplete {}))
          (and (not transport-retry?)
               (not= (:event/id transition) (:transition/event-id publication)))
          (conj (finding :projection-publication-current
                         :projection-publication-diverged
                         {:transition (:event/id transition)
                          :publication-transition (:transition/event-id publication)}))
          (and (not transport-retry?) (not substrate-wait?) (not waiting?)
               (not frame-closed?) transition-age
               (> transition-age max-heartbeat-age-seconds)
               (not= :complete (:regulator/status coordinator)))
          (conj (finding :unattended-transition-current
                         :unattended-transition-stale
                         {:age-seconds transition-age
                          :limit-seconds max-heartbeat-age-seconds}))
          (and waiting? (not= :live-job-dispatched (:state/type phase-state)))
          (conj (finding :active-phase-state-readable :active-phase-state-invalid
                         {:observed (:state/type phase-state)}))
          (and waiting? (not= (:job-id operation) (:job-id ticket))
               (or (nil? transition-age)
                   (> transition-age projection-catchup-grace-seconds)))
          (conj (finding :projected-job-matches-durable-state
                         :projected-job-mismatch
                         {:projected (:job-id operation) :durable (:job-id ticket)
                          :projection-age-seconds transition-age
                          :catchup-grace-seconds
                          projection-catchup-grace-seconds}))
          (and waiting? (not (:ok agent)))
          (conj (finding :agency-agent-reachable :agency-agent-unreachable
                         {:agent-id (:agent-id operation)}))
          (and waiting? (:ok agent) (not terminal-job?)
               (or (not= "invoking" (get-in agent [:agent :status]))
                   (not= 1 (get-in agent [:agent :running-jobs]))))
          (conj (finding :agency-job-running :agency-job-not-running
                         {:status (get-in agent [:agent :status])
                          :running-jobs (get-in agent [:agent :running-jobs])}))
          (and waiting? terminal-job? terminal-age
               (not cascade-within-bound?)
               (> terminal-age max-heartbeat-age-seconds))
          (conj (finding :terminal-job-collection-current
                         :terminal-job-collection-stale
                         {:state (get-in job [:job :state])
                          :age-seconds terminal-age
                          :limit-seconds max-heartbeat-age-seconds}))
          (and waiting? (pos-int? timeout-ms) job-age
               (> (* 1000 job-age) timeout-ms))
          (conj (finding :job-within-declared-timeout :active-job-timeout
                         {:age-seconds job-age :timeout-ms timeout-ms}))
          (and waiting?
               (not (and (pos-int? (:collection-attempts budget))
                         (pos-int? (:repair-attempts budget)))))
          (conj (finding :terminal-budgets-valid :terminal-budget-invalid
                         {:observed budget}))
          (and cascade-present? (not cascade-shaped?))
          (conj (finding :cascade-operation-conformant
                         :cascade-operation-malformed
                         {:observed cascade-operation}))
          cascade-expired?
          (conj (finding :cascade-operation-conformant
                         :cascade-operation-deadline-exceeded
                         {:deadline-at-ms (:deadline-at-ms cascade-operation)
                          :observed-at-ms now-ms
                          :budget-ms (:budget-ms cascade-operation)}))
          (false? (get-in coordinator [:regulator/last-result :ok]))
          (conj (finding :coordinator-last-result-successful
                         :coordinator-last-result-failed
                         {:result (:regulator/last-result coordinator)})))]
    {:watch/status (cond (seq findings) :alert
                         transport-retry? :waiting
                         substrate-wait? :waiting
                         cascade-within-bound? :waiting
                         :else :healthy)
     :watch/checked obligation-ids
     :watch/findings findings
     :frame-id (:frame-id transition)
     :problem-id (:problem-id transition)
     :phase (:phase transition)
     :operation operation
     :coordinator/ticks (:regulator/ticks coordinator)
     :job/age-seconds job-age
     :transport-retry
     (when transport-retry?
       {:wake-at-ms (:transport-retry/not-before-ms phase-state)
        :attempt (:transport-retry/attempt phase-state)
        :max-attempts (:transport-retry/max-attempts phase-state)
        :last-failure (or (:transport-retry/last-error-code phase-state)
                          (some-> (:transport-retry/history phase-state)
                                  last :error/code))})
     :substrate-wait
     (when substrate-wait?
       {:wake-at-ms substrate-resume-at-ms})
     :cascade-operation cascade-operation}))

(defn- read-edn [path] (edn/read-string (slurp (io/file (str path)))))

(defn- last-edn-line [path]
  (with-open [reader (io/reader path)]
    (->> (line-seq reader) (remove str/blank?) last edn/read-string)))

(defn- http-json [url]
  (try
    (let [request (-> (HttpRequest/newBuilder (URI/create url)) .GET .build)
          response (.send (HttpClient/newHttpClient) request
                          (HttpResponse$BodyHandlers/ofString))]
      (if (= 200 (.statusCode response))
        (json/parse-string (.body response) true)
        {:ok false :http/status (.statusCode response)}))
    (catch Throwable t {:ok false :exception/message (.getMessage t)})))

(defn- phase-state-path
  "Resolve the canonical durable state selected by the projected operation.
  Guide promotion review is a nested, separately persisted phase machine; its
  typed promotion-proctor operation must not be compared with the completed
  outer Guide ticket."
  [frame-dir transition]
  (let [phase (:phase transition)
        promotion-review? (and (= :promotion-proctor
                                  (get-in transition [:operation :role]))
                               (str/starts-with? (name phase)
                                                 "guide-intervention-"))
        filename (str (name phase) (when promotion-review? "-review") ".edn")]
    (.resolve (.resolve frame-dir "live") filename)))

(defn observe [{:keys [transition-log coordinator-state agency-base
                       max-heartbeat-age-seconds]}]
  (let [transition (last-edn-line transition-log)
        coordinator (read-edn coordinator-state)
        frame-dir (.getParent (.toAbsolutePath (Path/of transition-log (make-array String 0))))
        terminal-path (.resolve frame-dir "terminal/frame-terminal.edn")
        terminal (when (Files/exists terminal-path
                                     (make-array java.nio.file.LinkOption 0))
                   (read-edn terminal-path))
        frame-closed? (and (= (:frame-id transition) (:frame/id terminal))
                           (contains? queued-terminal/frame-results
                                      (:frame/result terminal)))
        publication (read-edn (.resolve frame-dir "publications/latest.edn"))
        cascade-path (.resolve frame-dir "live/memory-cascade-operation.edn")
        cascade-operation
        (when (Files/exists cascade-path
                            (make-array java.nio.file.LinkOption 0))
          (read-edn cascade-path))
        phase-state (read-edn (phase-state-path frame-dir transition))
        agent-id (get-in transition [:operation :agent-id])
        job-id (get-in transition [:operation :job-id])]
    {:now (Instant/now)
     :coordinator coordinator
     :coordinator-age-seconds
     (quot (- (System/currentTimeMillis)
              (.toMillis
               (Files/getLastModifiedTime
                (Path/of coordinator-state (make-array String 0))
                (make-array java.nio.file.LinkOption 0)))) 1000)
     :max-heartbeat-age-seconds max-heartbeat-age-seconds
     :frame-closed? frame-closed?
     :cascade-operation cascade-operation
     :transition transition :publication publication :phase-state phase-state
     :job (if (and job-id (not frame-closed?))
            (http-json (str agency-base "/api/alpha/invoke/jobs/" job-id))
            {:ok true})
     :agent (if (and agent-id (not frame-closed?))
              (http-json (str agency-base "/api/alpha/agents/" agent-id))
              {:ok true})}))

(defn -main [& args]
  (when-not (<= 2 (count args) 4)
    (binding [*out* *err*]
      (println "usage: projection-watchdog TRANSITION_LOG COORDINATOR_STATE [MAX_AGE_SECONDS] [AGENCY_BASE]"))
    (System/exit 64))
  (let [[log state age base] args
        result (try
                 (evaluate (observe {:transition-log log :coordinator-state state
                                     :max-heartbeat-age-seconds
                                     (Long/parseLong (or age "120"))
                                     :agency-base (or base "http://localhost:7070")}))
                 (catch Throwable t
                   {:watch/status :alert :watch/checked obligation-ids
                    :watch/findings [(finding :transition-log-readable
                                              :watch-observation-failed
                                              {:message (.getMessage t)})]}))]
    (prn result)
    (shutdown-agents)
    (System/exit (if (= :healthy (:watch/status result)) 0 2))))
