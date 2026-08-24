(ns futon3c.apm.projection-watchdog
  "Contract-aware health evaluation for an APM projection and its live job."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
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
   :terminal-budgets-valid :coordinator-last-result-successful])

(def projection-catchup-grace-seconds 10)

(defn- age-seconds [^Instant now value]
  (when value
    (.getSeconds (Duration/between (Instant/parse value) now))))

(defn- finding [id code evidence]
  {:obligation id :error/code code :evidence evidence})

(defn evaluate
  "Pure watchdog evaluation. INPUTS must contain parsed durable observations."
  [{:keys [now coordinator coordinator-age-seconds transition publication
           phase-state agent job max-heartbeat-age-seconds]}]
  (let [operation (:operation transition)
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
        waiting? (and (not complete?)
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
          (> coordinator-age-seconds max-heartbeat-age-seconds)
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
          (not= (:event/id transition) (:transition/event-id publication))
          (conj (finding :projection-publication-current
                         :projection-publication-diverged
                         {:transition (:event/id transition)
                          :publication-transition (:transition/event-id publication)}))
          (and (not waiting?) transition-age
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
          (false? (get-in coordinator [:regulator/last-result :ok]))
          (conj (finding :coordinator-last-result-successful
                         :coordinator-last-result-failed
                         {:result (:regulator/last-result coordinator)})))]
    {:watch/status (if (seq findings) :alert :healthy)
     :watch/checked obligation-ids
     :watch/findings findings
     :frame-id (:frame-id transition)
     :problem-id (:problem-id transition)
     :phase (:phase transition)
     :operation operation
     :coordinator/ticks (:regulator/ticks coordinator)
     :job/age-seconds job-age}))

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

(defn observe [{:keys [transition-log coordinator-state agency-base
                       max-heartbeat-age-seconds]}]
  (let [transition (last-edn-line transition-log)
        coordinator (read-edn coordinator-state)
        frame-dir (.getParent (.toAbsolutePath (Path/of transition-log (make-array String 0))))
        publication (read-edn (.resolve frame-dir "publications/latest.edn"))
        phase-state (read-edn (.resolve (.resolve frame-dir "live")
                                       (str (name (:phase transition)) ".edn")))
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
     :transition transition :publication publication :phase-state phase-state
     :job (if job-id
            (http-json (str agency-base "/api/alpha/invoke/jobs/" job-id))
            {:ok true})
     :agent (if agent-id
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
