(ns futon3c.apm.teaching-exchange
  "Durable, bounded planning exchange preceding the existing construction job.
  Each tick performs one job transition; no polling loop or implicit retries."
  (:require [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.job-port :as jobs]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.teaching-plan :as plan]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.apm.role-memory-search :as memory])
  (:import [java.nio.file Path Files StandardOpenOption OpenOption]
           [java.nio.channels FileChannel OverlappingFileLockException]
           [java.nio.file.attribute FileAttribute]))

(defn state-path [^Path phase-path]
  (.resolveSibling phase-path (str (.getFileName phase-path) ".teaching.edn")))
(defn saved-request [phase-path]
  (:base-request (runtime/read-state (state-path phase-path))))
(defn- need! [condition code]
  (when-not condition (throw (ex-info (name code) {:error/code code}))))
(defn- persist! [path state]
  (need! (:ok (runtime/atomic-persist! path state)) :teaching-persistence-failed)
  state)
(defn- waiting [state]
  {:ok true :status :awaiting-terminal :teaching/status (:stage state)
   :job-id (get-in state [:request :submission/job-id])})
(defn- actor! [http base agent-id allow-unbound?]
  (need! (and (plan/text? agent-id) (re-matches #"[A-Za-z0-9_.:-]+" agent-id))
         :teaching-agent-invalid)
  (let [r (http "GET" (str base "/api/alpha/agents/" agent-id) nil)
        session (get-in r [:agent :session-id])]
    (need! (and (:ok r) (= 200 (:http/status r)) (= agent-id (:agent-id r))
                (or (plan/text? session) (and allow-unbound? (nil? session)))) :teaching-agent-unavailable)
    {:agent-id agent-id :session-id session}))
(defn- make-request [state phase revision previous judgment]
  (let [base (:base-request state) ta? (= phase :pattern-plan-review)
        actor (if ta? (:ta state) (:student state))
        teaching (cond-> {:version 1 :exchange-id (:exchange-id state) :revision revision}
                   (and (not ta?) (nil? (:session-id actor)))
                   (assoc :session-policy :fresh-after-reset
                          :prior-session-id (:student-prior-session state)
                          :other-session-id (get-in state [:ta :session-id]))
                   previous (assoc (if ta? :plan :prior-plan) previous)
                   judgment (assoc :judgment judgment))
        ;; Only Student-visible inputs travel. No Solver final head, trace or solution.
        body (merge (select-keys base [:frame-id :problem-id :memory-snapshot
                                      :shelf/holdout :shelf/withheld-ids])
                    (cond-> actor (nil? (:session-id actor)) (dissoc :session-id)) {:phase phase :role (if ta? :pattern-ta :student)
                           :v4/teaching teaching})]
    (-> (assoc body :dispatch/id (plan/digest body))
        submission/prepare-request submission/with-job-authority)))

(defn prompt [request base]
  (str "APM V4 teaching exchange. Work on the problem statement at "
       (pr-str (select-keys base [:workspace :problem-path :base-revision])) ". "
       "Read only. Do not edit a proof, run Lean, read the Solver solution, contact other agents, "
       "or publish library changes. An accepted plan is permission to attempt construction, not a proof. "
       (if (= :pattern-plan-review (:phase request))
         (str "Judge the exact Student plan below. Return :plan-review {:plan-digest SHA "
              ":verdict accept|revise|cannot-judge :reason TEXT :nodes [{:node-id ID "
              ":verdict suitable|revise|cannot-judge :diagnosis none|retrieval|applicability|execution|unknown "
              ":reason TEXT :instruction TEXT}]}. Cover every node. Inspect cited method revisions "
              "and their applicability; if the evidence is unavailable use cannot-judge. "
              "Give guidance on conditions and the next obligation, not the reference solution. ")
         (str "First retrieve relevant patterns using the job-bound mathematics search. "
              "Build a top-down plan warranted by definitions, ordinary reasoning, pinned patterns/memories, "
              "or explicit gaps. Do not fill gaps by pretending a citation is a proof. "
              "Return :plan {:version 1 :problem-id STRING :revision INTEGER :parent-digest SHA-OR-NIL "
              ":nodes [{:id STRING :parent STRING-OR-NIL :goal TEXT :definitions [TEXT] "
              ":conditions [{:statement TEXT :status open|established|refuted :argument TEXT}] "
              ":depends-on [ID] :warrant {:kind ordinary|pattern|memory|gap :explanation TEXT "
              ":id METHOD-ID :revision SHA256}}] :responses []}. One root, at most 64 nodes, no cycles. "
              "On revision, preserve old node IDs and respond to each old node with "
              "{:node-id ID :action changed|retained-with-reason :response TEXT}. "
              "Return :memory-use {:used-ids []} with actual uses only. "))
       "Frozen teaching authority: " (pr-str (:v4/teaching request)) "\n"
       "Reviewed Student shelf: " (pr-str (:memory-snapshot request)) "\n"
       (submission/command request {:job-id (:submission/job-id request)})))

(defn- collected! [request observed]
  (let [job-id (:submission/job-id request)
        checked (submission/authenticated-completion request {:job-id job-id})
        typed (:submission checked)]
    (need! (and (:ok observed) (= :done (:state observed)) (= job-id (:job-id observed))
                (= (:agent-id request) (:agent-id observed))
                (if (= :fresh-after-reset (get-in request [:v4/teaching :session-policy]))
                  (and (plan/text? (:session-id observed))
                       (not= (:session-id observed) (get-in request [:v4/teaching :prior-session-id]))
                       (not= (:session-id observed) (get-in request [:v4/teaching :other-session-id])))
                  (= (:session-id request) (:session-id observed)))) :teaching-executed-identity-invalid)
    (need! (and (:ok checked) (= (:submission/id typed)
                                (machine/ledger-digest [(dissoc typed :submission/id)]))
                (plan/payload-valid? request (:payload typed))) :teaching-submission-invalid)
    (when (= :student (:role request))
      (let [searches (memory/recorded-receipts-for-job job-id)
            surfaced (into (set (get-in request [:memory-snapshot :accessible-memory-ids]))
                           (mapcat memory/receipt-surfaced-ids searches))
            used (get-in typed [:payload :evidence :memory-use :used-ids])]
        (need! (and (vector? used) (every? surfaced used)
                    (not-any? (set (:shelf/withheld-ids request)) used)) :teaching-memory-use-invalid)))
    {:job-id job-id :role (:role request) :agent-id (:agent-id observed) :session-id (:session-id observed)
     :submission-id (:submission/id typed) :evidence (get-in typed [:payload :evidence])}))

(defn- advance [state observation]
  (let [request (:request state) phase (:phase request)
        revision (get-in request [:v4/teaching :revision])
        evidence (:evidence observation)
        state (cond-> (update state :history conj observation)
                (= :student (:role request))
                (assoc-in [:student :session-id] (:session-id observation)))]
    (if (= :pattern-plan-review phase)
      (let [judgment (:plan-review evidence) verdict (plan/tag (:verdict judgment))
            current (get-in request [:v4/teaching :plan])]
        (cond
          (= "accept" verdict)
          (let [body {:receipt/type :apm-teaching-exchange :exchange-id (:exchange-id state)
                      :plan current :plan-digest (plan/digest current)
                      :history (:history state) :mathematics/verified? false}]
            (assoc state :stage :ready :receipt (assoc body :receipt/id (plan/digest body))))
          (and (= "revise" verdict) (< revision (:max-revisions state)))
          (assoc state :stage :prepared :request
                 (make-request state :pattern-plan-revision (inc revision) current judgment))
          :else (assoc state :stage :needs-attention :reason
                       (if (= "cannot-judge" verdict) :ta-cannot-judge :revision-budget-exhausted))))
      (assoc state :stage :prepared :request
             (make-request state :pattern-plan-review revision (:plan evidence) nil)))))

(defn- tick! [{:keys [request phase-path http-fn agency-base prepare-fn]
              :or {http-fn runtime/http-json agency-base "http://127.0.0.1:7070"}}]
  (let [path (state-path phase-path) old (runtime/read-state path)
        config (:v4/teaching-config request)
        _ (need! (= 1 (:version config)) :teaching-config-invalid)
        _ (need! (and (int? (:max-revisions config)) (<= 0 (:max-revisions config) 2)
                      (pos-int? (:job-budget-ms config)) (<= (:job-budget-ms config) 900000))
                 :teaching-budget-invalid)
        _ (when old (need! (= request (:base-request old)) :teaching-base-authority-conflict))]
    (if-not old
      (let [prior (actor! http-fn agency-base (:agent-id request) true)
            ta (actor! http-fn agency-base (:ta-agent-id config) false)]
        (need! (and (not= (:agent-id prior) (:agent-id ta))
                    (not= (:session-id prior) (:session-id ta))) :teaching-review-not-independent)
        ;; Write the intent BEFORE reset. An interrupted reset needs explicit
        ;; reconciliation; it must never silently reset a new session on replay.
        (persist! path {:version 1 :stage :initializing :base-request request})
        (need! (fn? prepare-fn) :teaching-preparation-missing)
        (need! (:ok (prepare-fn request)) :teaching-preparation-failed)
        (let [student (actor! http-fn agency-base (:agent-id request) true)]
          (need! (nil? (:session-id student)) :teaching-session-reset-not-observed)
          (let [state {:version 1 :stage :prepared :base-request request
                       :exchange-id (plan/digest [(:dispatch/id request) config student ta])
                       :student student :student-prior-session (:session-id prior) :ta ta :history []
                       :max-revisions (:max-revisions config)}
                state (assoc state :request (make-request state :pattern-plan 0 nil nil))]
            (waiting (persist! path state)))))
      (case (:stage old)
        :initializing {:ok false :error/code :teaching-initialization-reconciliation-required}
        :ready {:ok true :status :ready :receipt (:receipt old)}
        :needs-attention {:ok false :error/code :teaching-needs-attention :reason (:reason old)}
        :prepared
        (let [req (assoc (:request old) :agency-base agency-base)
              job-id (:submission/job-id req)
              _ (need! (= (select-keys (assoc req :session-id (:session-id req)) [:agent-id :session-id])
                          (actor! http-fn agency-base (:agent-id req) true)) :teaching-session-drift)
              dispatch {:agent-id (:agent-id req) :job-id job-id :surface "bell"
                        :caller "apm-v4-teaching" :prompt (prompt req request)
                        :timeout-ms (:job-budget-ms config)}
              announced (jobs/announce! http-fn agency-base dispatch)]
          (need! (and (:ok announced) (= job-id (:job-id announced))) :teaching-announce-failed)
          (need! (:ok (submission/register! req {:job-id job-id})) :teaching-registration-failed)
          ;; Persist before activation; replay observes the same announced job.
          (waiting (persist! path (assoc old :stage :running :dispatch dispatch))))
        :running
        (let [req (:request old) observed (jobs/observe http-fn agency-base (:submission/job-id req))]
          (need! (:ok observed) :teaching-observation-failed)
          (cond
            (= :queued (:state observed))
            (do (need! (= (select-keys (assoc req :session-id (:session-id req)) [:agent-id :session-id])
                          (actor! http-fn agency-base (:agent-id req) true)) :teaching-session-drift)
                (need! (:ok (jobs/activate! http-fn agency-base (:dispatch old))) :teaching-activation-failed)
                (waiting old))
            (contains? #{:active :settling} (:state/class observed)) (waiting old)
            (= :done (:state observed)) (waiting (persist! path (advance old (collected! req observed))))
            :else {:ok false :error/code :teaching-job-terminal-failure
                   :job-id (:submission/job-id req) :job-state (:state observed)}))
        {:ok false :error/code :teaching-state-invalid}))))

(defn step!
  "One serialized tick in the private phase journal. Lock contention reports
  waiting; it never launches a concurrent exchange or overwrites a winner."
  [{:keys [phase-path] :as options}]
  (try
    (let [path (state-path phase-path) parent (.getParent (.toAbsolutePath path))]
      (Files/createDirectories parent (make-array FileAttribute 0))
      (with-open [channel (FileChannel/open (.resolveSibling path (str (.getFileName path) ".lock"))
                                            (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/WRITE]))]
        (if-let [lock (try (.tryLock channel) (catch OverlappingFileLockException _ nil))]
          (with-open [_lock lock] (tick! options))
          {:ok true :status :awaiting-terminal :teaching/status :busy})))
    (catch Exception e {:ok false :error/code (or (:error/code (ex-data e)) :teaching-io-failed)})))

(defn construction-request [base receipt]
  (let [body (-> base
                 (dissoc :submission/token :submission/job-id :dispatch/id)
                 (assoc :v4/teaching-receipt receipt))]
    (-> (assoc body :dispatch/id (plan/digest body)) submission/prepare-request)))
