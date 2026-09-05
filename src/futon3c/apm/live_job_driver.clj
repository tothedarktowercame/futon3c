(ns futon3c.apm.live-job-driver
  "Exactly-once durable boundary shared by every live APM role job.

   The canonical Agency job is announced, ticketed, and persisted before it is
   activated. A restart therefore polls the recorded job instead of dispatching
   a duplicate. Terminal evidence is delegated to a phase-specific validator."
  (:require [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.job-state :as job-state]
            [futon3c.apm.typed-role-submission :as submission]))

(def terminal-states job-state/terminal-states)
(def default-terminal-budget {:collection-attempts 1 :repair-attempts 1})
;; Keep one additional apparatus turn available when a repaired terminal reaches
;; a later transport boundary.  The agent repair budget remains independent.
(def default-apparatus-repair-attempts 2)
(def default-orphan-recovery-attempts 2)

(def ^:private codex-session-loss-pattern
  #"(?m)^\d{4}-\d{2}-\d{2}T\S+\s+ERROR\s+codex_core::session:\s+.*\bthread\s+([0-9a-f-]{36})\s+not found\s*$")

(defn session-orphan-observation
  "Return typed evidence only for a canonical Codex session-loss diagnostic.

  A nil session id or an old job is deliberately insufficient: neither proves
  that the externally owned session died.  The diagnostic must name the exact
  session the request was trying to resume."
  [request job]
  (let [expected (or (:session-id request) (:solver/prior-session-id request))
        diagnostic (str (or (:stderr job) (get-in job [:result :stderr])
                            (:terminal-message job) ""))
        observed (second (re-find codex-session-loss-pattern diagnostic))]
    (when (and (string? expected) (= expected observed))
      {:observation/type :job-owner-orphaned
       :finding :codex-session-not-found
       :job-id (:job-id job)
       :agent-id (:agent-id job)
       :session-id expected
       :evidence/source :canonical-codex-stderr})))

(defn- valid-orphan-observation? [observation ticket]
  (and (= :job-owner-orphaned (:observation/type observation))
       (= :codex-session-not-found (:finding observation))
       (= (:job-id ticket) (:job-id observation))
       (string? (:session-id observation))
       (= :canonical-codex-stderr (:evidence/source observation))))

(defn orphan-recovery-request
  "Address a fresh-session successor for a proved orphan without claiming a
  terminal result for its predecessor. ATTEMPT is durable bounded state, not a
  role repair budget."
  [request ticket observation attempt]
  (if-not (and (valid-orphan-observation? observation ticket)
               (pos-int? attempt))
    {:ok false :error/code :live-job-orphan-recovery-input-invalid}
    (let [body (-> request
                   (dissoc :dispatch/id :submission/token :submission/job-id
                           :session-id :solver/prior-session-id)
                   (assoc :fresh-session? true
                          :fresh-session-nonce
                          (machine/ledger-digest
                           [(:dispatch/id request) (:job-id ticket)
                            (:session-id observation) attempt])
                          :orphan/recovery-attempt attempt
                          :orphan/of-job-id (:job-id ticket)
                          :orphan/observation observation))
          addressed (assoc body :dispatch/id (machine/ledger-digest [body]))]
      {:ok true :request (submission/prepare-request addressed)})))

(defn- transport-failure?
  "True when THIS failure's own error envelope is a transport fault.

  Checks the same four paths as live-learning-phases/posthoc-fault-origin
  rather than walking the whole value: a tree-seq over the validated result
  would also match a transport envelope carried incidentally in nested
  context, and reclassify an agent fault as apparatus on that basis."
  [value]
  (= :transport (or (:error/component value)
                    (get-in value [:error :error/component])
                    (get-in value [:finding :error/component])
                    (get-in value [:finding :error :error/component]))))

(defn reopen-posthoc-rejection
  "Reopen an exact cached provider rejection after its apparatus was repaired.

  The terminal submission and collection remain immutable.  Callers must name
  the observed error and supply an operator-visible reason; the displaced
  rejection is retained in append-only reconciliation history."
  [state expected-error-code reason]
  (let [rejection (:posthoc-rejection state)]
    (cond
      (not= :live-job-dispatched (:state/type state))
      {:ok false :error/code :live-job-posthoc-reopen-state-invalid}
      (not= expected-error-code (:error/code rejection))
      {:ok false :error/code :live-job-posthoc-reopen-error-mismatch
       :expected expected-error-code :observed (:error/code rejection)}
      (not (and (string? reason) (not-empty reason)))
      {:ok false :error/code :live-job-posthoc-reopen-reason-invalid}
      :else
      {:ok true :state
       (-> state
           (update :posthoc-reconciliations (fnil conj [])
                   {:error/code expected-error-code
                    :repair/reason reason
                    :rejection rejection})
           (dissoc :posthoc-rejection))})))
(def default-provider-usage-limit-window-ms (* 5 60 60 1000))

(defn- lift-guide-mode
  "Lift the Guide's wire-level mode declarations into the legacy report view.

  Typed payload `:mode` is primary because it is part of the role's completion
  payload. Older submissions may declare the same value in `:channel-audit`,
  so that location remains a fallback. When both are present, retain a
  conflict marker instead of silently choosing between contradictory claims."
  [report payload]
  (let [payload-mode (:mode payload)
        audit-mode (get-in report [:channel-audit :mode])
        normalized-payload-mode (submission/wire-keyword payload-mode)
        normalized-audit-mode (submission/wire-keyword audit-mode)
        conflict? (and (some? payload-mode)
                       (some? audit-mode)
                       (not= normalized-payload-mode normalized-audit-mode))]
    (cond-> (merge report
                   (select-keys (:channel-audit report) [:candidates]))
      (some? (or payload-mode audit-mode))
      (assoc :mode (or payload-mode audit-mode))

      conflict?
      (assoc :guide-mode-declaration-conflict
             {:payload normalized-payload-mode
              :channel-audit normalized-audit-mode}))))

(defn typed-submission-report
  "Reconstruct the report view consumed by phase validators from a typed
  submission. This is the single JSON/EDN reading boundary; it does not fill
  absent evidence or change validator acceptance criteria."
  [active-request submission]
  (let [payload (:payload submission)
        report (merge (:authority submission)
                      (:evidence payload)
                      (select-keys payload
                                   [:command-own-exit
                                    :outcome
                                    :failure-account
                                    ;; Some JSON clients emit the Student's
                                    ;; query ledger beside :evidence.
                                    :queries]))]
    (if (= :guide-intervention (:dispatch/type active-request))
      (lift-guide-mode report payload)
      report)))

(def provider-usage-limit-signatures
  "Declared substrate signatures. Callers may append provider-specific entries
  through `:provider-usage-limit-signatures`; the generic entries deliberately
  describe conditions rather than one vendor's complete notice text."
  [{:signature/id :claude-cli-limit-message
    :provider :anthropic
    :pattern #"(?i)claude\.ai/settings/usage\?from=cc_cli_limit_message"}
   {:signature/id :usage-limit
    :provider :unspecified
    :pattern #"(?i)\busage[ -]limit(?:ed| reached| exceeded)?\b"}
   {:signature/id :quota-exhausted
    :provider :unspecified
    :pattern #"(?i)\bquota (?:is )?(?:exhausted|exceeded|reached)\b"}
   {:signature/id :rate-limit
    :provider :unspecified
    :pattern #"(?i)\brate[ -]limit(?:ed| reached| exceeded)?\b"}])

(defn provider-usage-limit
  "Classify an invalid terminal diagnostic against declared signatures."
  ([diagnostic] (provider-usage-limit diagnostic []))
  ([diagnostic additional-signatures]
   (let [text (pr-str diagnostic)]
     (some (fn [{:keys [pattern] :as signature}]
             (when (and (instance? java.util.regex.Pattern pattern)
                        (re-find pattern text))
               (dissoc signature :pattern)))
           (concat provider-usage-limit-signatures additional-signatures)))))

(def expected-role-terminal-conditions
  "Agency terminals which mean that a role attempt ended without completing,
  rather than that the role or campaign machinery failed. Exact triples keep
  the general terminal-failure branch closed to unfamiliar failures."
  [{:condition/type :wall-clock-budget-exhausted
    :state :failed
    :terminal-code :invoke-error
    :terminal-message "wall-clock-budget"}
   {:condition/type :provider-request-timeout
    :state :failed
    :terminal-code :invoke-exception
    :terminal-message "request timed out"}
   {:condition/type :provider-response-endpoint-not-found
    :state :failed
    :terminal-code :invoke-error
    :terminal-message-pattern
    #"^Exit 1: unexpected status 404 Not Found: Unknown error, url: https://chatgpt[.]com/backend-api/codex/responses, cf-ray: [A-Za-z0-9-]+$"}])

(defn expected-role-terminal-condition
  "Return the named expected condition for an exact Agency terminal, if any."
  [job]
  (some (fn [{:keys [state terminal-code terminal-message
                     terminal-message-pattern] :as condition}]
          (when (and (= state (:state job))
                     (= terminal-code (:terminal-code job))
                     (if terminal-message-pattern
                       (boolean (re-matches terminal-message-pattern
                                            (or (:terminal-message job) "")))
                       (= terminal-message (:terminal-message job))))
            (select-keys condition [:condition/type])))
        expected-role-terminal-conditions))

(defn reconciled-self-cancellation
  "Return typed evidence only when a cancelled job is the exact job named by
  this driver's durable cancellation record. Wrapper reconciliation and
  supersession use the same identity rule; cancellation prose is irrelevant."
  [state job]
  (when (= :cancelled (:state job))
    (let [job-id (:job-id job)
          wrapper (:wrapper/reconciliation state)
          superseded (some #(when (and (= job-id (:job-id %))
                                       (true? (get-in % [:cancellation
                                                        :reconciled?])))
                              %)
                           (:superseded-tickets state))]
      (cond
        (and (:ok wrapper) (= job-id (:job-id wrapper)))
        {:condition/type :driver-wrapper-reconciliation-cancellation
         :job-id job-id}
        superseded
        {:condition/type :driver-supersession-cancellation
         :job-id job-id}
        :else nil))))

(defn wall-clock-budget-exhausted?
  "Compatibility predicate for the first expected role terminal."
  [job]
  (= :wall-clock-budget-exhausted
     (:condition/type (expected-role-terminal-condition job))))

(defn terminal-collection-authority
  "Verify that a persisted collection is the content-addressed collection for
  EXPECTED-JOB-ID.  A nearby terminal observation is not collection authority."
  [expected-job-id terminal-collection]
  (let [collection (:evidence terminal-collection)
        collection-id (:collection/id collection)
        expected-id (when (map? collection)
                      (machine/ledger-digest
                       [(dissoc collection :collection/id)]))]
    (cond
      (not (and (map? collection) (seq collection-id)))
      {:ok false :error/code :terminal-collection-authority-missing}

      (not= collection-id expected-id)
      {:ok false :error/code :terminal-collection-authority-digest-mismatch
       :observed collection-id :expected expected-id}

      (not= (str expected-job-id) (str (:job-id collection)))
      {:ok false :error/code :terminal-collection-authority-stale
       :observed-job-id (:job-id collection)
       :expected-job-id expected-job-id}

      :else {:ok true :collection/id collection-id})))

(defn- successor-observation [job terminal-collection findings]
  (campaign-trace/validate-authoritative-observation
   :successor
  {:predecessor-id (:job-id job)
   :terminal-evidence-id (:job-id job)
   :collection-evidence-id (get-in terminal-collection
                                   [:evidence :collection/id])
   :disposition (pr-str (vec findings))
   :predecessor-persisted? true
   :successor-announced-id ""
   :successor-activated-id ""}))

(defn- update-last-successor-observation [state f]
  (let [index (dec (count (:superseded-terminals state)))]
    (update-in state [:superseded-terminals index
                      :trace/successor-observation]
               #(campaign-trace/validate-authoritative-observation
               :successor (f %)))))

(declare ticket)

(defn- substrate-wait-result [state]
  {:ok true :status :awaiting-substrate :state state
   :substrate/condition (:substrate/wait state)})

(defn- substrate-resumption-request [planned signature]
  (let [body (-> planned
                 (dissoc :dispatch/id :submission/token :submission/job-id
                         :repair/attempt :repair/of-job-id :repair/of-ticket-id
                         :repair/findings :repair/validation-output :repair/kind)
                 (assoc :substrate/resumption :provider-usage-limit
                        :substrate/signature-id (:signature/id signature)))
        addressed (assoc body :dispatch/id (machine/ledger-digest [body]))]
    (submission/prepare-request addressed)))

(defn- begin-substrate-wait!
  [{:keys [state active-request job validated signature now-ms window-ms
           terminal-repair-request-fn persist-fn]}]
  (let [planned (when (fn? terminal-repair-request-fn)
                  (terminal-repair-request-fn
                   active-request (:ticket state) job
                   (assoc validated
                          :repair/kind :provider-usage-limit
                          :repair/budget-consumed? false)))
        planned-request (:request planned)
        request (when (map? planned-request)
                  (substrate-resumption-request planned-request signature))]
    (if-not (and (:ok planned) (map? request)
                 (string? (:dispatch/id request)))
      {:ok false :error/code :live-job-substrate-resumption-plan-invalid
       :finding planned}
      (let [wait {:condition/type :provider-usage-limit
                  :signature/id (:signature/id signature)
                  :provider (:provider signature)
                  :observed-at-ms now-ms
                  :resume-at-ms (+ now-ms window-ms)
                  :window-ms window-ms
                  :repair/attempts-preserved
                  (or (:terminal-repair-attempts state) 0)
                  :resumption/request request}
            waiting-state (assoc state :substrate/wait wait)]
        (if (:ok (persist-fn waiting-state))
          (substrate-wait-result waiting-state)
          {:ok false :error/code :live-job-substrate-wait-persistence-failed
           :state state})))))

(defn- resume-after-substrate-wait!
  [{:keys [state job announce-fn activate-fn persist-fn ticket-register-fn]}]
  (let [wait (:substrate/wait state)
        request (:resumption/request wait)
        predecessor {:job job
                     :ticket (:ticket state)
                     :terminal-collection (:terminal-collection state)
                     :findings [:provider-usage-limit]
                     :substrate/condition (dissoc wait :resumption/request)
                     :trace/successor-observation
                     (successor-observation job (:terminal-collection state)
                                            [:provider-usage-limit])}
        already-archived?
        (= (:job-id job)
           (get-in (peek (:superseded-terminals state)) [:job :job-id]))
        archived-state (cond-> state
                         (not already-archived?)
                         (update :superseded-terminals (fnil conj []) predecessor))]
    (if-not (:ok (persist-fn archived-state))
      {:ok false :error/code :live-job-substrate-predecessor-persistence-failed
       :state state}
      (let [announced (ticket request (announce-fn request))]
        (if-not (:ok announced)
          (assoc (substrate-wait-result archived-state)
                 :substrate/resumption-finding announced)
          (let [announced-state
                (update-last-successor-observation
                 archived-state
                 #(assoc % :successor-announced-id
                         (get-in announced [:ticket :job-id])))
                next-state (-> announced-state
                               (dissoc :terminal-collection :substrate/wait)
                               (assoc :active-request request
                                      :ticket (:ticket announced)
                                      :activation/accepted? false
                                      :substrate/last-wait
                                      (dissoc wait :resumption/request)))]
            (if-not (:ok (persist-fn next-state))
              {:ok false :error/code :live-job-substrate-resumption-persistence-failed
               :state archived-state}
              (let [registered (if (fn? ticket-register-fn)
                                 (ticket-register-fn request (:ticket announced))
                                 {:ok true})
                    activated (when (:ok registered)
                                (activate-fn request (:ticket announced)))]
                (cond
                  (not (:ok registered))
                  {:ok false
                   :error/code :live-job-submission-authority-registration-failed
                   :state next-state :finding registered}
                  (not (:ok activated))
                  {:ok false :error/code :live-job-activation-failed
                   :state next-state :finding activated}
                  :else
                  (let [accepted (assoc next-state :activation/accepted? true)]
                    (if (:ok (persist-fn accepted))
                      {:ok true :status :awaiting-terminal
                       :substrate/resumed? true :state accepted}
                      {:ok false
                       :error/code
                       :live-job-activation-acceptance-persistence-failed
                       :state next-state})))))))))))

(def durable-reference-keys
  #{:job-id :solver/prior-job-id :repair/of-job-id :submission/id :receipt/id
    :prior-receipt-id :terminal-job-id})

(defn durable-references
  "Return durable job/submission/receipt references in stable path order."
  [state]
  (letfn [(walk [path value]
            (cond
              (map? value)
              (mapcat (fn [[k v]]
                        (let [p (conj path k)]
                          (if (and (contains? durable-reference-keys k)
                                   (string? v) (not-empty v))
                            [{:path p :key k :id v}]
                            (walk p v))))
                      (sort-by (comp pr-str key) value))
              (vector? value)
              (mapcat (fn [[i v]] (walk (conj path i) v))
                      (map-indexed vector value))
              (sequential? value)
              (mapcat (fn [[i v]] (walk (conj path i) v))
                      (map-indexed vector value))
              :else []))]
    (vec (walk [] state))))

(defn scan-durable-references
  "Synchronously resolve every durable attempt/job/submission/receipt reference.

   RESOLVE-FN receives one reference map. A readable target is `{:ok true
   :value ...}`. Returns the first missing/corrupt reference, or `{:ok true}`."
  [state resolve-fn]
  (if-not (and (map? state) (fn? resolve-fn))
    {:ok false :error/code :durable-reference-scan-input-invalid}
    (loop [[reference & more] (durable-references state)]
      (if-not reference
        {:ok true}
        (let [resolved (try
                         (resolve-fn reference)
                         (catch Exception e
                           {:ok false :error/code :durable-reference-corrupt
                            :exception/class (.getName (class e))
                            :exception/message (.getMessage e)}))]
          (if (and (:ok resolved) (some? (:value resolved)))
            (recur more)
            {:ok false
             :error/code (or (:error/code resolved)
                             :durable-reference-missing)
             :reference reference
             :finding (dissoc resolved :value)}))))))

(defn- supersede-unaccepted!
  [{:keys [active-request state announce-fn activate-fn persist-fn cancel-fn
           ticket-register-fn cancellation-observation]}]
  (let [old-ticket (:ticket state)
        cancelled (or cancellation-observation
                      (cancel-fn (:job-id old-ticket)))]
    (if-not (:ok cancelled)
      {:ok false :error/code :live-job-unaccepted-cancellation-failed
       :state state :finding cancelled}
      (let [predecessor (assoc old-ticket :cancellation cancelled)
            already-archived?
            (= (:job-id old-ticket)
               (:job-id (peek (:superseded-tickets state))))
            archived-state (cond-> state
                             (not already-archived?)
                             (update :superseded-tickets (fnil conj []) predecessor))
            archived-persisted (persist-fn archived-state)]
        (if-not (:ok archived-persisted)
          {:ok false :error/code :live-job-supersession-archive-persistence-failed
           :state state}
          (let [announced (ticket active-request (announce-fn active-request))]
            (cond
              (not (:ok announced)) announced
              (= (:job-id old-ticket) (get-in announced [:ticket :job-id]))
              {:ok false :error/code :live-job-supersession-identity-reused}
              :else
              (let [next-state (-> archived-state
                               (assoc :ticket (:ticket announced)
                                      :activation/accepted? false
                                      :activation/failure nil
                                      :activation-supersession-attempts 1))]
            (if-not (:ok (persist-fn next-state))
              {:ok false :error/code :live-job-supersession-persistence-failed}
              (let [registered (if (fn? ticket-register-fn)
                                 (ticket-register-fn active-request
                                                     (:ticket announced))
                                 {:ok true})
                    activated (when (:ok registered)
                                (activate-fn active-request (:ticket announced)))]
                (cond
                  (not (:ok registered))
                  {:ok false
                   :error/code :live-job-submission-authority-registration-failed
                   :state next-state :finding registered}
                  (not (:ok activated))
                  {:ok false :error/code :live-job-activation-failed
                   :state next-state :finding activated}
                  :else
                  (let [accepted (assoc next-state :activation/accepted? true)]
                    (if (:ok (persist-fn accepted))
                      {:ok true :status :awaiting-terminal
                       :supersession? true :state accepted}
                      {:ok false
                       :error/code :live-job-activation-acceptance-persistence-failed
                       :state next-state})))))))))))))

(defn ticket [request response]
  (if-not (and (:ok response) (string? (:job-id response))
               (not-empty (:job-id response)))
    ;; Keep the announce response. Discarding it left f85/b97A03 parked with
    ;; {:ok false :error/code :live-job-announce-failed} and no way to tell a
    ;; transient agency failure from a malformed reply.
    {:ok false :error/code :live-job-announce-failed
     :finding {:response response
               :agent-id (:agent-id request)
               :frame-id (:frame-id request)
               :phase (:phase request)}}
    (let [body {:dispatch/id (:dispatch/id request)
                :job-id (:job-id response) :agent-id (:agent-id request)
                :frame-id (:frame-id request) :problem-id (:problem-id request)
                :phase (:phase request)}]
      {:ok true :ticket (assoc body :ticket/id (machine/ledger-digest [body]))})))

(defn- terminal-budget [configured]
  (merge default-terminal-budget configured))

(defn- valid-terminal-budget? [configured]
  (let [{:keys [collection-attempts repair-attempts]} (terminal-budget configured)]
    (and (pos-int? collection-attempts) (pos-int? repair-attempts))))

(defn terminal-collection-record [request ticket job submission attempt]
  (let [body {:collection/type :typed-role-terminal
              :dispatch/id (:dispatch/id request)
              :job-id (:job-id ticket)
              :role (:role request)
              :terminal-state (:state job)
              :terminal-code (:terminal-code job)
              :attempt attempt
              :submission/available? (some? submission)
              :submission/id (:submission/id submission)}]
    (assoc body :collection/id (machine/ledger-digest [body]))))

(defn- orphan-recovery-failure!
  [state finding persist-fn max-attempts]
  (let [attempt (inc (:orphan/recovery-attempts state))
        next-state (-> state
                       (assoc :orphan/recovery-attempts attempt
                              :orphan/last-failure finding)
                       (update :orphan/recovery-history (fnil conj []) finding))]
    (if-not (:ok (persist-fn next-state))
      {:ok false :error/code :live-job-orphan-recovery-persistence-failed
       :state state}
      (if (>= attempt max-attempts)
        {:ok false :error/code :live-job-orphan-recovery-exhausted
         :recovery/attempts attempt
         :recovery/exhaustion-reason :failed-attempt-at-ceiling
         :finding finding :state next-state}
        {:ok true :status :awaiting-orphan-recovery
         :recovery/attempts attempt :finding finding :state next-state}))))

(defn- recover-orphan-attempt!
  [{:keys [state active-request announce-fn activate-fn persist-fn cancel-fn
           ticket-register-fn orphan-recovery-request-fn
           orphan-recovery-max-attempts]}]
  (let [observation (:orphan/observation state)
        old-ticket (:ticket state)
        attempt (inc (:orphan/recovery-attempts state))
        request-fn (or orphan-recovery-request-fn orphan-recovery-request)
        planned (request-fn active-request old-ticket observation attempt)
        recovery-request (:request planned)
        cancelled (when (and (:ok planned) (map? recovery-request)
                             (fn? cancel-fn))
                    (cancel-fn (:job-id old-ticket)))]
    (cond
      (not (and (:ok planned) (map? recovery-request)
                (string? (:dispatch/id recovery-request))))
      (orphan-recovery-failure!
       state {:error/code :live-job-orphan-recovery-request-invalid
              :finding planned}
       persist-fn orphan-recovery-max-attempts)

      (not (:ok cancelled))
      (orphan-recovery-failure!
       state {:error/code :live-job-orphan-wrapper-cancellation-failed
              :finding cancelled}
       persist-fn orphan-recovery-max-attempts)

      :else
      (let [announced (ticket recovery-request (announce-fn recovery-request))]
        (if-not (:ok announced)
          (orphan-recovery-failure!
           state {:error/code :live-job-orphan-session-mint-failed
                  :finding announced}
           persist-fn orphan-recovery-max-attempts)
          (let [next-state
                (-> state
                    (update :orphan/history (fnil conj [])
                            {:observation observation :ticket old-ticket
                             :cancelled cancelled})
                    (dissoc :terminal-collection :orphan/observation
                            :orphan/last-failure)
                    (assoc :active-request recovery-request
                           :ticket (:ticket announced)
                           :activation/accepted? false
                           :orphan/recovery-attempts attempt))]
            (if (= (:job-id old-ticket) (get-in announced [:ticket :job-id]))
              (orphan-recovery-failure!
              state {:error/code :live-job-orphan-successor-identity-reused
                      :job-id (:job-id old-ticket)}
               persist-fn orphan-recovery-max-attempts)
              (if-not (:ok (persist-fn next-state))
                {:ok false :error/code :live-job-orphan-successor-persistence-failed
                 :state state}
                (let [registered (if (fn? ticket-register-fn)
                                   (ticket-register-fn recovery-request
                                                       (:ticket announced))
                                   {:ok true})
                      activated (when (:ok registered)
                                  (activate-fn recovery-request
                                               (:ticket announced)))]
                  (cond
                    (not (:ok registered))
                    (orphan-recovery-failure!
                     next-state
                     {:error/code :live-job-orphan-authority-registration-failed
                      :finding registered}
                     persist-fn orphan-recovery-max-attempts)
                    (not (:ok activated))
                    (orphan-recovery-failure!
                     next-state {:error/code :live-job-orphan-activation-failed
                                 :finding activated}
                     persist-fn orphan-recovery-max-attempts)
                    :else
                    (let [accepted (assoc next-state :activation/accepted? true)]
                      (if (:ok (persist-fn accepted))
                        {:ok true :status :awaiting-terminal
                         :orphan/recovered? true :state accepted}
                        {:ok false
                         :error/code
                         :live-job-orphan-activation-acceptance-persistence-failed
                         :state next-state}))))))))))))

(defn- recover-orphan!
  [{:keys [state orphan-recovery-max-attempts] :as context}]
  (let [attempts (:orphan/recovery-attempts state)]
    (cond
      (not (nat-int? attempts))
      {:ok false
       :error/code :live-job-orphan-recovery-attempt-count-invalid
       :finding {:observed attempts :required :durable-natural-number}
       :state state}

      (>= attempts orphan-recovery-max-attempts)
      {:ok false
       :error/code :live-job-orphan-recovery-exhausted
       :recovery/attempts attempts
       :recovery/exhaustion-reason :attempt-ceiling-before-mint
       :state state}

      :else
      (recover-orphan-attempt! context))))

(defn drive!
  "Advance one job by at most one externally visible state transition."
  [{:keys [request state announce-fn activate-fn job-fn persist-fn
           terminal-validator receipt-provider terminal-repair-request-fn
           posthoc-fault-origin-fn
           ticket-register-fn terminal-submission-provider cancel-fn
           missing-observation-provider terminal-budget-config now-ms-fn
           orphan-recovery-request-fn
           orphan-recovery-max-attempts
           provider-usage-limit-signatures provider-usage-limit-window-ms]
    :or {now-ms-fn #(System/currentTimeMillis)
         orphan-recovery-max-attempts default-orphan-recovery-attempts
         provider-usage-limit-signatures []
         provider-usage-limit-window-ms default-provider-usage-limit-window-ms}}]
  (cond
    (not (and (map? request) (string? (:dispatch/id request))
              (every? fn? [announce-fn activate-fn job-fn persist-fn
                            terminal-validator receipt-provider])
              (valid-terminal-budget? terminal-budget-config)
              (pos-int? provider-usage-limit-window-ms)
              (pos-int? orphan-recovery-max-attempts)
              (sequential? provider-usage-limit-signatures)))
    {:ok false :error/code :live-job-driver-input-invalid}

    (nil? state)
    (let [announced (ticket request (announce-fn request))]
      (if-not (:ok announced)
        announced
        (let [next-state {:state/type :live-job-dispatched
                          :request request :ticket (:ticket announced)}
              persisted (persist-fn next-state)]
          (cond
            (not (:ok persisted))
            {:ok false :error/code :live-job-ticket-persistence-failed}

            :else
            (let [registered (if (fn? ticket-register-fn)
                               (ticket-register-fn request (:ticket announced))
                               {:ok true})
                  activated (when (:ok registered)
                              (activate-fn request (:ticket announced)))]
              (if-not (:ok registered)
                {:ok false :error/code :live-job-submission-authority-registration-failed
                 :finding registered :state next-state}
              (if (:ok activated)
                (let [accepted-state (assoc next-state :activation/accepted? true)
                      accepted-persisted (persist-fn accepted-state)]
                  (if (:ok accepted-persisted)
                    {:ok true :status :awaiting-terminal :state accepted-state}
                    {:ok false
                     :error/code :live-job-activation-acceptance-persistence-failed
                     :state next-state}))
                {:ok false :error/code :live-job-activation-failed
                 :state next-state :finding activated})))))))

    (not= :live-job-dispatched (:state/type state))
    {:ok false :error/code :live-job-state-invalid}

    (not= (:dispatch/id request) (get-in state [:request :dispatch/id]))
    {:ok false :error/code :live-job-request-state-mismatch}

    (not (:activation/accepted? state))
    (let [job (job-fn (get-in state [:ticket :job-id]))
          state-class (job-state/classify (:state job))
          observed-accepted? (contains? #{:terminal :settling :active}
                                        state-class)
          unaccepted-state? (and (not (:activation/accepted? state))
                                 (contains? #{:queued :cancelled} (:state job)))
          supersession-eligible?
          (and unaccepted-state? (fn? cancel-fn)
               (fn? terminal-submission-provider)
               (zero? (or (:activation-supersession-attempts state) 0))
               (or (:activation/failure state)
                   (pos? (or (:typed-submission-migration-attempts state) 0))))]
      (cond
        supersession-eligible?
        (supersede-unaccepted!
         {:active-request (or (:active-request state) request)
          :state state :announce-fn announce-fn :activate-fn activate-fn
          :persist-fn persist-fn :cancel-fn cancel-fn
          :ticket-register-fn ticket-register-fn
          :cancellation-observation
          (when (= :cancelled (:state job))
            {:ok true :state :cancelled
             :job-id (get-in state [:ticket :job-id])
             :reconciled? true})})

        observed-accepted?
        ;; A running or terminal canonical job is stronger durable evidence
        ;; than the lost local 202 observation.  Persist the reconciliation
        ;; before terminal validation; never reinterpret a client timeout or
        ;; an unchanged queued job as acceptance.
        (let [accepted-state (assoc state
                                    :activation/accepted? true
                                    :activation/reconciled-from (:state job))
              persisted (persist-fn accepted-state)]
          (if (:ok persisted)
            {:ok true :status :awaiting-terminal :state accepted-state}
            {:ok false
             :error/code :live-job-activation-acceptance-persistence-failed
             :state state}))
        :else
        (let [activated (activate-fn request (:ticket state))]
          (if-not (:ok activated)
            (let [failed-state (assoc state :activation/failure activated)]
              (if (:ok (persist-fn failed-state))
                {:ok false :error/code :live-job-activation-failed
                 :state failed-state :finding activated}
                {:ok false :error/code :live-job-activation-failure-persistence-failed
                 :state state}))
            (let [accepted-state (assoc state :activation/accepted? true)
                  persisted (persist-fn accepted-state)]
              (if (:ok persisted)
                {:ok true :status :awaiting-terminal :state accepted-state}
                {:ok false
                 :error/code :live-job-activation-acceptance-persistence-failed
                 :state state}))))))

    :else
    (let [active-request (or (:active-request state) request)
          ;; The controller-owned store is reconciliation authority. Read it
          ;; before polling or classifying the externally owned wrapper.
          submission-observation
          (when (and (fn? terminal-submission-provider)
                     (nil? (:terminal-collection state)))
            (terminal-submission-provider active-request (:ticket state) nil))
          submission-provider-error
          (when (and (map? submission-observation)
                     (false? (:ok submission-observation)))
            submission-observation)
          observed-submission
          (if (and (map? submission-observation)
                   (true? (:ok submission-observation))
                   (contains? submission-observation :submission))
            (:submission submission-observation)
            submission-observation)
          job (job-fn (get-in state [:ticket :job-id]))
          terminal? (contains? terminal-states (:state job))
          orphan-observation
          (when (and (nil? observed-submission) (not terminal?))
            (session-orphan-observation active-request job))
          job-usage-limit (provider-usage-limit
                           {:report (:report job)
                            :output (:output job)
                            :result (:result job)
                            :error (:error job)}
                           provider-usage-limit-signatures)]
      (cond
        submission-provider-error
        {:ok false :error/code :live-job-submission-reconciliation-invalid
         :finding submission-provider-error :state state}

        ;; An authenticated completion outranks the wrapper's stale live state.
        ;; Persist collection before any orphan or redispatch decision.
        (and observed-submission (nil? (:terminal-collection state)))
        (let [wrapper-reconciliation
              (when (and (not terminal?) (fn? cancel-fn))
                (cancel-fn (:job-id (:ticket state))))
              configured (terminal-budget terminal-budget-config)
              collection (terminal-collection-record
                          active-request (:ticket state) job
                          observed-submission 1)
              next-state
              (cond-> (assoc state :terminal-collection
                             {:evidence collection
                              :submission observed-submission
                              :budget configured})
                wrapper-reconciliation
                (assoc :wrapper/reconciliation wrapper-reconciliation))]
          (if (and wrapper-reconciliation (not (:ok wrapper-reconciliation)))
            {:ok false :error/code :live-job-wrapper-reconciliation-failed
             :finding wrapper-reconciliation :state state}
            (if (:ok (persist-fn next-state))
              {:ok true :status :terminal-collected :state next-state
               :collection collection}
              {:ok false :error/code :live-job-terminal-collection-persistence-failed
               :state state})))

        (:substrate/wait state)
        (if (< (now-ms-fn) (get-in state [:substrate/wait :resume-at-ms]))
          (substrate-wait-result state)
          (resume-after-substrate-wait!
           {:state state :job job :announce-fn announce-fn
            :activate-fn activate-fn :persist-fn persist-fn
            :ticket-register-fn ticket-register-fn}))

        (and job-usage-limit (:terminal-collection state))
        (begin-substrate-wait!
         {:state state :active-request active-request :job job
          :validated {:ok false :error/code :live-job-submission-missing
                      :findings [:typed-submission-missing]
                      :substrate/condition :provider-usage-limit}
          :signature job-usage-limit :now-ms (now-ms-fn)
          :window-ms provider-usage-limit-window-ms
          :terminal-repair-request-fn terminal-repair-request-fn
          :persist-fn persist-fn})

        (= :unknown (job-state/classify (:state job)))
        {:ok false :error/code :live-job-state-unclassified
         :finding {:job-id (:job-id job) :state (:state job)}}

        (:orphan/observation state)
        (recover-orphan!
         {:state state :active-request active-request
          :announce-fn announce-fn :activate-fn activate-fn
          :persist-fn persist-fn :cancel-fn cancel-fn
          :ticket-register-fn ticket-register-fn
          :orphan-recovery-request-fn orphan-recovery-request-fn
          :orphan-recovery-max-attempts orphan-recovery-max-attempts})

        (and orphan-observation
             (valid-orphan-observation? orphan-observation (:ticket state)))
        (let [next-state (cond->
                          (assoc state :orphan/observation orphan-observation)
                           (not (contains? state :orphan/recovery-attempts))
                           (assoc :orphan/recovery-attempts 0))]
          (if (:ok (persist-fn next-state))
            {:ok true :status :orphaned :state next-state
             :orphan/observation orphan-observation}
            {:ok false :error/code :live-job-orphan-observation-persistence-failed
             :state state}))

        (and orphan-observation
             (not (valid-orphan-observation? orphan-observation
                                             (:ticket state))))
        {:ok false :error/code :live-job-orphan-observation-invalid
         :finding orphan-observation :state state}

        (and (not terminal?)
             (nil? (:terminal-collection state)))
        {:ok true :status :awaiting-terminal :state state}

        (and terminal?
             (not= :done (:state job))
             (nil? (:terminal-collection state))
             (not (and (or (expected-role-terminal-condition job)
                           (reconciled-self-cancellation state job))
                       (fn? terminal-submission-provider))))
        {:ok false :error/code :live-job-terminal-failure
         :finding (select-keys job [:job-id :agent-id :state :terminal-code
                                    :terminal-message])}

        :else
        (if (and (fn? terminal-submission-provider)
                 (nil? (:terminal-collection state)))
          (let [submission observed-submission
                configured (terminal-budget terminal-budget-config)
                collection (terminal-collection-record
                            active-request (:ticket state) job submission 1)
                next-state (assoc state :terminal-collection
                                  {:evidence collection :submission submission
                                   :budget configured})]
            (if (:ok (persist-fn next-state))
              {:ok true :status :terminal-collected :state next-state
               :collection collection}
              {:ok false :error/code :live-job-terminal-collection-persistence-failed
               :state state}))
        (let [submission (if (fn? terminal-submission-provider)
                           (get-in state [:terminal-collection :submission])
                           nil)
              configured (terminal-budget terminal-budget-config)
              max-repairs (:repair-attempts configured)
              job (if submission
                    (let [report (typed-submission-report active-request
                                                          submission)]
                      (assoc job :report report
                             :state :done
                             :typed-submission submission))
                    job)
              validated (or (:posthoc-rejection state)
                            (if (and (fn? terminal-submission-provider)
                                     (nil? submission))
                              {:ok false :error/code :live-job-submission-missing
                               :findings [:typed-submission-missing]}
                              (terminal-validator active-request
                                                  (:ticket state) job)))
              ;; Reclassify a cached rejection through the current policy when
              ;; it has no origin yet, or when that policy says :apparatus.
              ;; Reclassifying unconditionally can DOWNGRADE an explicit
              ;; :apparatus to :agent, which is the direction this whole
              ;; change exists to prevent -- it would spend the role's repair
              ;; budget on a fault the role cannot repair.
              posthoc-origin (when (and (:posthoc-rejection state)
                                        (fn? posthoc-fault-origin-fn))
                               (posthoc-fault-origin-fn active-request
                                                        validated))
              validated (cond-> validated
                          (and posthoc-origin
                               (or (nil? (:repair/fault-origin validated))
                                   (= :apparatus posthoc-origin)))
                          (assoc :repair/fault-origin posthoc-origin))
              typed-contract-migration?
              (and (fn? terminal-submission-provider)
                   (= [:typed-submission-missing] (:findings validated))
                   (pos? (or (:terminal-repair-attempts state) 0))
                   (zero? (or (:typed-submission-migration-attempts state) 0)))
              usage-limit
              (when-not (:ok validated)
                (provider-usage-limit {:job job :validation validated}
                                      provider-usage-limit-signatures))]
          (if (:ok validated)
            (let [provided (receipt-provider active-request (:ticket state)
                                             job validated)]
              (cond
                ;; Promotion has already spent and certified its bounded
                ;; transport retries.  This typed result belongs to the frame
                ;; queue, which records the apparatus park and advances.  Do
                ;; not cache it as a role-terminal rejection: doing so asks
                ;; the completed role to repair substrate transport and hides
                ;; the queue transition behind a new invalid repair request.
                (= :promotion-apparatus-repair-exhausted
                   (:error/code provided))
                provided

                (not (:ok provided))
                (let [provided (cond-> provided
                                 (fn? posthoc-fault-origin-fn)
                                 (assoc :repair/fault-origin
                                        (posthoc-fault-origin-fn active-request
                                                                 provided)))
                      next-state (assoc state :posthoc-rejection provided)]
                  (if (:ok (persist-fn next-state))
                    {:ok true :status :awaiting-terminal :state next-state
                     :posthoc-rejection provided}
                    {:ok false
                     :error/code :live-job-posthoc-rejection-persistence-failed
                     :finding provided}))

                ;; A provider may defer certification behind a further job
                ;; (a Guide deposit's independent review); the validated
                ;; terminal is re-observed on the next tick.
                (= :awaiting-terminal (:status provided))
                (assoc provided :state state)

                ;; Receipt providers may hold at another durable boundary
                ;; (for example, a Guide promotion awaiting apparatus repair).
                ;; Only an explicit :certified result carrying a certificate
                ;; may turn the live job into :live-job-certified.
                (and (some? (:status provided))
                     (not= :certified (:status provided)))
                (cond-> (assoc provided :state state)
                  (:state provided)
                  (assoc :provider/state (:state provided)))

                (not (map? (:certificate provided)))
                {:ok false
                 :error/code :live-job-certificate-missing
                 :provider-result (dissoc provided :state)}

                :else
                (let [next-state (assoc state :state/type :live-job-certified
                                        :receipt (:certificate provided))]
                  (if (:ok (persist-fn next-state))
                    {:ok true :status :certified :state next-state
                     :certificate (:certificate provided)}
                    {:ok false
                     :error/code :live-job-receipt-persistence-failed}))))
            (cond
              usage-limit
              (begin-substrate-wait!
               {:state state :active-request active-request :job job
                :validated validated :signature usage-limit
                :now-ms (now-ms-fn)
                :window-ms provider-usage-limit-window-ms
                :terminal-repair-request-fn terminal-repair-request-fn
                :persist-fn persist-fn})

              :else
              (let [inferred-repair-origin
                    (if (transport-failure? validated)
                      :apparatus
                      (or (:repair/fault-origin validated) :agent))
                    agent-repairs (or (:terminal-repair-attempts state) 0)
                    apparatus-repairs (or (:apparatus-repair-attempts state) 0)
                    cached-repair-origin
                    (when (= (:terminal-repair/findings state)
                             (:findings validated))
                      (:terminal-repair/fault-origin state))
                    preliminary-repair-origin
                    (if (contains? #{:agent :apparatus} cached-repair-origin)
                      cached-repair-origin
                      inferred-repair-origin)
                    preliminary-exhausted?
                    (and (not typed-contract-migration?)
                         (if (= :apparatus preliminary-repair-origin)
                           (>= apparatus-repairs
                               default-apparatus-repair-attempts)
                           (>= agent-repairs max-repairs)))
                    repair (when (and (not preliminary-exhausted?)
                                      (fn? terminal-repair-request-fn))
                             (terminal-repair-request-fn
                              active-request (:ticket state) job
                              (cond-> (assoc validated
                                             :repair/fault-origin
                                             inferred-repair-origin)
                                typed-contract-migration?
                                (assoc :repair/kind
                                       :typed-submission-contract-migration)
                                (not typed-contract-migration?)
                                (assoc :repair/next-attempt
                                       (inc (if (= :apparatus
                                                   inferred-repair-origin)
                                              apparatus-repairs
                                              agent-repairs))))))
                    repair-request (:request repair)
                    declared-repair-origin (:repair/fault-origin repair-request)
                    repair-origin (if (contains? #{:agent :apparatus}
                                                 declared-repair-origin)
                                    declared-repair-origin
                                    inferred-repair-origin)
                    exhausted? (and (not typed-contract-migration?)
                                    (if (= :apparatus repair-origin)
                                      (>= apparatus-repairs
                                          default-apparatus-repair-attempts)
                                      (>= agent-repairs max-repairs)))]
                (cond
                  exhausted?
                  (if (and (= :agent repair-origin)
                           (= [:typed-submission-missing] (:findings validated))
                           (fn? missing-observation-provider))
                    (let [provided (missing-observation-provider
                                    active-request (:ticket state) job
                                    agent-repairs
                                    (get-in state [:terminal-collection :evidence]))]
                      (if-not (:ok provided)
                        provided
                        (let [receipt (:certificate provided)
                              recovered? (= :student-observation-recovered
                                            (:receipt/type receipt))
                              next-state
                              (assoc state :state/type :live-job-certified
                                     :receipt receipt :learning/outcome
                                     (if recovered? :observed :unobserved))]
                          (if (:ok (persist-fn next-state))
                            {:ok true :status :certified :state next-state
                             :certificate (:certificate provided)}
                            {:ok false
                             :error/code :live-job-receipt-persistence-failed}))))
                    (assoc validated
                           :error/code
                           (if (= :apparatus repair-origin)
                             :live-job-apparatus-repair-exhausted
                             :live-job-terminal-repair-exhausted)
                           :repair/fault-origin repair-origin
                           :repair/attempts
                           (if (= :apparatus repair-origin)
                             apparatus-repairs agent-repairs)
                           :repair/history (:repair-attempt-history state)))

                  (not (and (:ok repair) (map? repair-request)
                            (string? (:dispatch/id repair-request))))
                  (if (fn? terminal-repair-request-fn)
                    {:ok false
                     :error/code :live-job-terminal-repair-request-invalid
                     :finding repair}
                    validated)

                  :else
                  (let [predecessor
                        {:job job
                         :ticket (:ticket state)
                         :terminal-collection (:terminal-collection state)
                         :findings (:findings validated)
                         :repair/fault-origin repair-origin
                         :trace/successor-observation
                         (successor-observation job (:terminal-collection state)
                                                (:findings validated))}
                        already-archived?
                        (= (:job-id job)
                           (get-in (peek (:superseded-terminals state))
                                   [:job :job-id]))
                        archived-state
                        (cond-> (dissoc state :terminal-collection
                                        :posthoc-rejection)
                          (not already-archived?)
                          (update :superseded-terminals (fnil conj []) predecessor))
                        archived-persisted (persist-fn archived-state)]
                    (if-not (:ok archived-persisted)
                      {:ok false
                       :error/code :live-job-terminal-repair-archive-persistence-failed
                       :state state}
                      (let [announced (ticket repair-request
                                              (announce-fn repair-request))]
                        (if-not (:ok announced)
                          announced
                          (let [announced-state
                                (update-last-successor-observation
                                 archived-state
                                 #(assoc % :successor-announced-id
                                         (get-in announced [:ticket :job-id])))
                                next-state
                            (cond->
                             (assoc announced-state
                                    :active-request repair-request
                                    :ticket (:ticket announced)
                                    :activation/accepted? false
                                    :terminal-repair-attempts
                                    (if typed-contract-migration?
                                      agent-repairs
                                      (if (= :agent repair-origin)
                                        (inc agent-repairs)
                                        agent-repairs))
                                    :apparatus-repair-attempts
                                    (if (= :apparatus repair-origin)
                                      (inc apparatus-repairs)
                                      apparatus-repairs)
                                    :terminal-repair/original-job-id (:job-id job)
                                    :terminal-repair/findings (:findings validated)
                                    :terminal-repair/fault-origin repair-origin)
                              (not typed-contract-migration?)
                              (update :repair-attempt-history (fnil conj [])
                                      {:job-id (:job-id job)
                                       :fault-origin repair-origin
                                       :findings (:findings validated)})
                              typed-contract-migration?
                              (assoc :typed-submission-migration-attempts 1
                                     :typed-submission-migration/of-job-id
                                     (:job-id job)))]
                        (if-not (:ok (persist-fn next-state))
                          {:ok false
                           :error/code :live-job-terminal-repair-persistence-failed}
                          (let [registered (if (fn? ticket-register-fn)
                                             (ticket-register-fn
                                              repair-request (:ticket announced))
                                             {:ok true})
                                activated (when (:ok registered)
                                            (activate-fn repair-request
                                                         (:ticket announced)))]
                            (if-not (:ok registered)
                              {:ok false
                               :error/code :live-job-submission-authority-registration-failed
                               :state next-state :finding registered}
                            (if-not (:ok activated)
                              {:ok false :error/code :live-job-activation-failed
                               :state next-state :finding activated}
                              (let [accepted (-> next-state
                                                 (assoc :activation/accepted? true)
                                                 (update-last-successor-observation
                                                  #(assoc % :successor-activated-id
                                                          (get-in announced
                                                                  [:ticket :job-id]))))]
                                (if (:ok (persist-fn accepted))
                                  {:ok true :status :awaiting-terminal
                                   :repair? true :state accepted}
                                  {:ok false
                                   :error/code :live-job-activation-acceptance-persistence-failed
                                   :state next-state})))))))))))))))))))))
