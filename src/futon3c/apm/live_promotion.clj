(ns futon3c.apm.live-promotion
  "Durable two-seat promotion dispatcher."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.coined-pattern :as coined-pattern]
            [futon3c.apm.authority-port :as authority-port]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.promotion-candidate-store :as candidate-store]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.apm.promotion-review-store :as review-store]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.apm.transport-conformance :as transport])
  (:import [java.nio.file Path]))

(defn resolved-role-card-path [control-root request]
  (let [resolved (authority-port/resolve-path
                  {:control-root control-root} :control-root :role-card
                  (:role-card-path request))]
    (when (:ok resolved) (:path resolved))))

(declare drive!)

(def ^:private max-deposit-attempts 3)
(def ^:private default-transport-retry-delay-ms (* 10 60 1000))
(def ^:private default-transport-retry-max-attempts 3)
(declare transport-failure?)

(def ^:private implementation-resource "futon3c/apm/live_promotion.clj")
(defonce ^:private loaded-runtime-id
  (transport/source-resource-id implementation-resource))

(defn transport-implementation-identity []
  (transport/implementation-identity
   (transport/source-resource-id implementation-resource)
   loaded-runtime-id))

(defn- repaired-runtime-identity? [state]
  (let [{:keys [source-id loaded-runtime-id]}
        (transport-implementation-identity)]
    (and (seq (:findings state))
         (every? #(= :transport-conformance-runtime-identity-mismatch
                     (:error/code %))
                 (:findings state))
         (string? source-id)
         (= source-id loaded-runtime-id))))

(defn- legacy-approved-pattern-projection-failure? [state]
  (let [reviews (get-in state [:persisted-review-result :reviews])
        by-id (into {} (map (juxt :memory-id identity)) reviews)]
    (and (= :review-projection (:repair/kind state))
         (not (:pattern-contract-repair-attempted?
               (:last-valid-state state)))
         (some
          (fn [{:keys [memory-id failure finding]}]
            (let [review (by-id memory-id)]
              (and (= :promotion-review-projection-failed failure)
                   (= :approve (:verdict review))
                   (= (set (:review-patterns finding))
                      (set (:pattern-ids review)))
                   (not= (set (:edge-patterns finding))
                         (set (:review-patterns finding))))))
          (:findings state)))))

(defn- certificate-history [state]
  (mapv (fn [entry]
          {:attempt (:attempt entry)
           :operation (or (:transport/operation entry) :write)
           :acquired-outcome (or (:transport/acquired-outcome entry) :timeout)
           :evidence (or (:transport/evidence entry) :not-obtained)})
        (:transport-retry/history state)))

(defn- publication-certificate [state published result promotion-policy]
  (let [attempt (or (:transport-retry/attempt state) 0)
        max-attempts (or (:transport-retry-max-attempts promotion-policy)
                         default-transport-retry-max-attempts)
        acquired (or (:transport/acquired-outcome published)
                     (when (:ok published) :success)
                     (when (transport-failure? published) :timeout)
                     :authoritative-absence)
        classified (or (:transport/classified-outcome published)
                       (when (:ok published) :success)
                       (when (transport-failure? published) :timeout)
                       :authoritative-absence)
        evidence (or (:transport/evidence published)
                     (if (:ok published) :obtained
                         (if (contains? #{:timeout :unavailable :visibility-lag}
                                        acquired)
                           :not-obtained :obtained)))
        wake-at-ms (or (get-in result [:state :transport-retry/not-before-ms]) 0)
        decision (case (:status result)
                   :certified [:advance]
                   :transport-retry-scheduled [:retry wake-at-ms]
                   (if (= :promotion-substrate-retry-exhausted
                          (get-in result [:state :error/code]))
                     [:park :retry-exhausted]
                     (if (= :authoritative-absence classified)
                     [:park :authoritative-absence]
                     [:park :invalid-evidence])))]
    {:identity (transport-implementation-identity)
     :operation (or (:transport/operation published)
                    (if (:ok published) :post-publication-verification
                        :publication))
     :acquired-outcome acquired :classified-outcome classified
     :evidence evidence :attempt attempt :max-attempts max-attempts
     ;; Certificate history records completed predecessor attempts.  The
     ;; durable terminal state additionally records this final attempt.
     :wake-at-ms wake-at-ms :history (certificate-history state)
     :last-valid-state (pr-str (select-keys state [:state/type :stage :job]))
     :last-valid-evidence (pr-str (select-keys state [:reviews :candidates]))
     :decision decision}))

(defn- successor-observation [job-id terminal-collection findings]
  (let [collection-id
        (or (get-in terminal-collection [:evidence :collection/id])
            ;; Promotion's apparatus-repair path can begin after the role
            ;; terminal was durably observed but before a live-job-driver
            ;; collection envelope was retained.  Bind that collection to the
            ;; exact persisted predecessor/finding tuple; an announced
            ;; successor must never carry an empty collection witness.
            (machine/ledger-digest
             [{:collection/type :promotion-repair-terminal
               :job-id (str job-id)
               :findings (vec findings)}]))]
    (campaign-trace/validate-authoritative-observation
     :successor
  {:predecessor-id (str job-id)
   :terminal-evidence-id (str job-id)
   :collection-evidence-id (str collection-id)
   :disposition (pr-str (vec findings))
   :predecessor-persisted? true
   :successor-announced-id ""
   :successor-activated-id ""})))

(defn- bind-last-successor [state successor-id]
  (let [index (dec (count (:superseded-terminals state)))]
    (update-in state [:superseded-terminals index
                      :trace/successor-observation]
               #(campaign-trace/validate-authoritative-observation
                 :successor
                 (assoc % :successor-announced-id (str successor-id)
                        :successor-activated-id (str successor-id))))))

(defn- transport-failure? [value]
  (boolean
   (some #(and (map? %) (= :transport (:error/component %)))
         (tree-seq coll? seq value))))

(defn- pass-reviews [result]
  {:ok true :reviews (:reviews result)})

(defn- wire-keyword [value]
  (when (string? value)
    (keyword (if (.startsWith ^String value ":")
               (subs value 1)
               value))))

(defn- normalize-deposit-lane [lane]
  (cond-> lane
    (string? (:lane lane)) (update :lane wire-keyword)
    (string? (:status lane)) (update :status wire-keyword)))

(defn- normalize-deposit-report
  "Normalize only the typed JSON enum fields at the live boundary. The pure
  promotion gate continues to require keywords, so unknown strings still fail
  closed instead of being accepted by a widened contract."
  [report]
  (if (vector? (:lanes report))
    (update report :lanes #(mapv normalize-deposit-lane %))
    report))

(defn- submitted-report [typed]
  (let [payload (:payload typed)
        evidence (:evidence payload)
        encoded (:receipt evidence)
        receipt (cond
                  (map? encoded) encoded
                  (string? encoded)
                  (try
                    (let [parsed (edn/read-string encoded)]
                      (when (map? parsed) parsed))
                    (catch Throwable _ nil))
                  :else nil)]
    (normalize-deposit-report
     (cond-> (merge (:authority typed) evidence receipt
                    (select-keys payload
                                 [:command-own-exit :outcome :failure-account]))
       (and (= :zai-scribe (get-in typed [:authority :role]))
            (vector? (:memory-candidates evidence)))
       (assoc :candidates (:memory-candidates evidence))))))

(defn- reviewed-attachment-status [verdict]
  (case verdict
    (:approve :reassign) :reviewed
    :reject :proposed
    nil))

(defn- normalize-review-entry
  "Reviewer verdicts arrive through the typed JSON submission, which strings
  every keyword, while the substrate and the pure gates use keywords
  (:approve, :reviewed). Re-keyword the two enum fields at ingestion — the
  same normalization conductor-surface applies at its boundary. Without it,
  validate-review* filters every approval out and the union snapshot
  publishes empty (f28 promote-solver and guide-intervention-1, 2026-08-24)."
  [expected-reviewer review]
  (let [review (cond-> review
                 (string? (:verdict review)) (update :verdict wire-keyword)
                 (string? (:attachment-status review))
                 (update :attachment-status wire-keyword)
                 (string? (:witness-status review))
                 (update :witness-status wire-keyword)
                 (string? (:memory-use/kind review))
                 (update :memory-use/kind wire-keyword))]
    (assoc review
           :reported-reviewer (:reviewer review)
           :reported-attachment-status (:attachment-status review)
           :reported-witness-status (:witness-status review)
           :reviewer expected-reviewer
           :attachment-status
           (reviewed-attachment-status (:verdict review)))))

(defn- normalize-review-report
  [report expected-digest expected-base-blob expected-reviewer]
  (let [reviews (let [rs (or (:reviews report) (:promotion-reviews report))]
                  (if (vector? rs)
                    (mapv #(normalize-review-entry expected-reviewer %) rs)
                    rs))
        reviewer expected-reviewer]
    (cond
      (not= expected-digest (:candidate-set-digest report))
      {:ok false :error/code :promotion-review-candidate-digest-mismatch
       :expected expected-digest :observed (:candidate-set-digest report)}

      (not (vector? reviews))
      {:ok false :error/code :promotion-review-vector-missing}

      (not= expected-base-blob (:base-problem-blob report))
      {:ok false :error/code :promotion-review-base-blob-mismatch
       :expected expected-base-blob :observed (:base-problem-blob report)}

      (not (and (vector? (:open-residuals report))
                (every? #(and (integer? (:line %))
                              (string? (:summary %)))
                        (:open-residuals report))))
      {:ok false :error/code :promotion-review-open-residuals-missing}

      (not (and (string? reviewer) (not-empty reviewer)))
      {:ok false :error/code :promotion-review-attribution-ambiguous}

      :else {:ok true :reviewer reviewer :reviews reviews})))

(defn- reviewer-authority
  [reviewer-request candidates candidate-evidence]
  (assoc reviewer-request
         :candidates candidates
         :candidate-evidence candidate-evidence
         :phase :promotion-review
         :role :promotion-proctor
         :candidate-set-digest (machine/ledger-digest [candidates])))

(defn- review-read-instruction []
  (str "\nThe controller freshly read each complete persisted EvidenceEntry "
       "into :candidate-evidence. Treat its :entry :evidence/body as the "
       "authoritative body and its :read-ref as the dedicated full-entry "
       "read endpoint. A hyperedge-neighborhood projection intentionally "
       "embeds only an envelope-grade hook and is not a body read."))

(defn- review-output-instruction []
  (str " The complete report MUST include :candidate-set-digest and "
       ":base-problem-blob copied exactly from Authority, plus "
       ":open-residuals as a vector of {:line INT :summary STRING} maps "
       "(use [] when there are none); strings are not valid residual entries."))

(defn- agency-stage [agency-base request prompt]
  (let [request (submission/prepare-request request)]
   (fn
    ([]
     (let [request (submission/with-job-authority request)
           typed-prompt (str prompt
                             "\nCompletion is accepted only through the typed "
                             "submission tool under shared contract "
                             (pr-str submission/completion-contract) ". "
                             "Run the template command, fill every null in its "
                             "evidence object, then run the submit command:\n"
                             (submission/command request
                                                 {:job-id (:submission/job-id request)}))
           announced (job-port/announce!
                      agency-base
                      {:agent-id (:agent-id request) :prompt typed-prompt
                       :mode "work" :job-id (:submission/job-id request)})
           job-id (:job-id announced)
           ticket {:job-id job-id}
           registered (when (and (:ok announced) job-id)
                        (submission/register! request ticket))
           activated (when (:ok registered)
                       (job-port/activate!
                        agency-base
                        {:agent-id (:agent-id request)
                         :prompt typed-prompt
                         :mode "work" :job-id job-id}))]
       (if (and (:ok announced) (:ok activated))
         {:ok true :job job-id}
         {:ok false :error/code :promotion-stage-dispatch-failed
          :dispatch {:announced announced
                     :registered registered
                     :activated activated}})))
    ([job-id]
     (let [job (job-port/observe agency-base job-id)
           typed (submission/submitted job-id)
           report (when typed (submitted-report typed))]
       (case (job-port/classify-state (:state job))
         :terminal
         (if (and (= :done (:state job)) (map? report))
           {:ok true :job job-id :report report}
           {:ok false :error/code :promotion-stage-terminal-invalid :job job
            :report/error (if (= :done (:state job))
                            {:error/code :typed-submission-missing}
                            (:report/error job))})
         :active
         {:ok true :status :awaiting-terminal :job job-id}
         :settling
         {:ok true :status :awaiting-terminal :job job-id}
         {:ok false :error/code :promotion-stage-job-state-unclassified
          :job job}))))))

(defn run-live!
  [{:keys [state-path agency-base control-root deposit-request reviewer-request
           publish-fn promotion-policy contract-digest]
    :or {agency-base "http://localhost:7070"}}]
  (let [persist-fn #(runtime/atomic-persist! state-path %)
        stored-state (runtime/read-state state-path)
        deposit-request (or (:deposit-request stored-state) deposit-request)
        state-request (when (= :promotion (:state/type stored-state))
                        (if (= :independent-review (:stage stored-state))
                          reviewer-request
                          deposit-request))
        state (if (and state-request (:job stored-state)
                       (or (nil? (:request stored-state))
                           (not= (:job stored-state)
                                 (get-in stored-state [:ticket :job-id]))))
                (assoc stored-state :request state-request
                                    :ticket {:job-id (:job stored-state)})
                stored-state)
        deposit-card-path (resolved-role-card-path control-root deposit-request)
        deposit-prompt (str "Deposit promotion candidates. Authority:\n"
                            (pr-str deposit-request)
                            "\nRead and follow the frozen role card at " deposit-card-path
                            " (blob " (:role-card-blob deposit-request) ")."
                            "\nReturn exactly one parseable EDN map and no prose. "
                            "It must contain string :depositor and non-empty vector "
                            ":candidates and a complete vector :lanes report. "
                            "Each required lane (:solve, :arc, :trajectory, :challenge) "
                            "must occur exactly once as {:lane <keyword> :status one-of "
                            "#{:ran :ran-empty :not-run} :reason <nonblank string when "
                            "status is not :ran>}; do not encode status as a map key. "
                            "Every candidate must contain nonblank strings :name, "
                            ":hook and :body and a NON-EMPTY vector :pattern-ids. "
                            "The controller derives :memory-id, :content-digest, :kind, "
                            ":source-attempts, and depositor identity after persisting "
                            "the candidate; do not mint or restate those values. Each pattern id "
                            "names a pattern in the mathematics libraries (math-informal* / "
                            "math-formalization); create a library file if none fits. "
                            "A candidate with no bound pattern cannot be reviewed for "
                            "coherent fit and is rejected at the gate. EDN does not "
                            "concatenate adjacent string literals; use one string value "
                            "per field.")
        deposit-prompt (str deposit-prompt
                            " The conversational map is not authoritative. Put the "
                            "same complete EDN map in the typed submission evidence "
                            "field :receipt (encoded as a JSON object or EDN string); "
                            "the controller reads promotion data only from that field.")
        deposit-stage (agency-stage agency-base deposit-request deposit-prompt)
        deposit-fn (fn
                     ([] (deposit-stage))
                     ([value]
                      (if (string? value)
                        (deposit-stage value)
                        ((agency-stage
                          agency-base
                          (cond-> deposit-request
                            (:submission/attempt value)
                            (assoc :submission/attempt
                                   (:submission/attempt value)))
                          (str deposit-prompt
                               "\nThe previous response failed the EDN linter: "
                               (pr-str (select-keys value
                                                    [:error/code :report/error]))
                               (when (seq (:findings value))
                                 (str "\nThe parsed map failed the typed contract: "
                                      (pr-str (:findings value))
                                      ". Required lane shape is {:lane <keyword> "
                                      ":status one-of #{:ran :ran-empty :not-run} "
                                      ":reason <nonblank string when status is not :ran>}. "
                                      "Every candidate needs a non-empty :pattern-ids "
                                      "vector naming mathematics-library patterns."))
                               "\nRepair only the serialization/shape and return the complete map."))))))
        review-fn
        (fn
          ([candidates]
           (let [inputs (candidate-store/review-inputs candidates)]
             (if-not (:ok inputs)
               inputs
               (let [request (reviewer-authority
                              reviewer-request candidates
                              (:candidate-evidence inputs))
                     prompt (str "Independently review this exact candidate set. Authority:\n"
                                 (pr-str request)
                                 (review-read-instruction)
                                 "\nRead and follow the frozen role card at "
                                 (resolved-role-card-path control-root request)
                                 " (blob " (:role-card-blob request) ")."
                                 "\nFollow the pinned promotion Proctor card and return exactly one EDN map. "
                                 "Persist each approval's evidence body with nonblank "
                                 ":review/reason and :review/residual fields; return the "
                                 "same nonblank :reason and :residual on every review. "
                                 "Every review MUST state :pattern-ids as a non-empty "
                                 "vector copied from or explicitly reassigned against "
                                 "the candidate's reviewed pattern set. "
                                 "Every :approve or :reassign review MUST explicitly "
                                 "state :memory-use/kind as exactly :substitutive or "
                                 ":regulative; do not infer it from prose or legacy :kind."
                                 (review-output-instruction))]
                 ((agency-stage agency-base request prompt))))))
          ([job-id candidates]
           (let [digest (machine/ledger-digest [candidates])
                 request (assoc reviewer-request :candidates candidates
                                :phase :promotion-review
                                :role :promotion-proctor
                                :candidate-set-digest digest)
                 prompt (str "Independently review this exact candidate set. Authority:\n"
                             (pr-str request)
                             "\nRead and follow the frozen role card at "
                             (resolved-role-card-path control-root request)
                             " (blob " (:role-card-blob request) "). "
                             "Every review MUST state :pattern-ids as a non-empty "
                             "vector copied from or explicitly reassigned against "
                             "the candidate's reviewed pattern set. "
                             "Every :approve or :reassign review MUST explicitly "
                             "state :memory-use/kind as exactly :substitutive or "
                             ":regulative; do not infer it from prose or legacy :kind."
                             (review-output-instruction))
                 result ((agency-stage agency-base request prompt) job-id)]
             (if (:report result)
                 (let [normalized (normalize-review-report
                                 (:report result) digest
                                 (:base-problem-blob reviewer-request)
                                 (:agent-id reviewer-request))]
                 (if (:ok normalized)
                   (merge result (select-keys normalized [:reviewer :reviews]))
                   normalized))
               result)))
          ([candidates predecessor-job-id successor-attempt]
           (let [inputs (candidate-store/review-inputs candidates)]
             (if-not (:ok inputs)
               inputs
               (let [request (assoc
                              (reviewer-authority
                               reviewer-request candidates
                               (:candidate-evidence inputs))
                              :predecessor-job-id predecessor-job-id
                              :submission/attempt successor-attempt)
                     prompt (str "Independently review this exact candidate set as an "
                                 "append-only successor to terminal job "
                                 predecessor-job-id ". Authority:\n"
                                 (pr-str request)
                                 (review-read-instruction)
                                 "\nRead and follow the frozen role card at "
                                 (resolved-role-card-path control-root request)
                                 " (blob " (:role-card-blob request) ")."
                                 "\nFollow the pinned promotion Proctor card and return "
                                 "exactly one EDN map. Persist each approval's evidence "
                                 "body with nonblank :review/reason and :review/residual "
                                 "fields; return the same nonblank :reason and :residual "
                                 "on every review. Every review MUST state :pattern-ids "
                                 "as a non-empty vector copied from or explicitly "
                                 "reassigned against the candidate's reviewed pattern "
                                 "set. Every :approve or :reassign review "
                                 "MUST explicitly state :memory-use/kind as exactly "
                                 ":substitutive or :regulative; do not infer it from "
                                 "prose or legacy :kind."
                                 (review-output-instruction))]
                 ((agency-stage agency-base request prompt)))))))]
    ;; Upgrade legacy promotion envelopes before observing their terminal job.
    ;; This is a lossless durability migration: the job identity is unchanged.
    (when (not= stored-state state) (persist-fn state))
    (assoc
     (drive! {:state state :deposit-fn deposit-fn :review-fn review-fn
              :deposit-request deposit-request
              :reviewer-request reviewer-request
              :persist-candidates-fn #(candidate-store/persist!
                                       % deposit-request)
              :persist-reviews-fn review-store/persist!
              :candidate-visible-fn candidate-store/visible?
              :publish-fn publish-fn
              :certificate-emitter-fn
              (fn [certificate emitted-at-ms]
                (let [state-file (.toAbsolutePath
                                  (if (instance? Path state-path)
                                    state-path
                                    (Path/of (str state-path)
                                             (make-array String 0))))
                      directory (.resolve (.getParent state-file)
                                          "transport-certificates")]
                  (transport/persist-certificate!
                   directory
                   {:frame-id (or (:frame-id deposit-request) "unavailable")
                    :problem-id (or (:problem-id deposit-request) "unavailable")
                    :phase (or (:phase deposit-request) :promotion-review)
                    :attempt (:attempt certificate)}
                   certificate emitted-at-ms)))
              :promotion-policy promotion-policy
              :contract-digest contract-digest
              :persist-fn persist-fn})
     :promotion/state-path (str state-path))))

(defn- retry-deposit!
  [state failure deposit-fn persist-fn]
  (let [attempt (or (:attempt state) 1)
        format-failure? (contains? #{:report-edn-invalid
                                     :report-edn-lint-failed}
                                   (get-in failure [:report/error :error/code]))
        format-repairs (or (:format-repairs state) 0)
        format-repair? (and format-failure? (zero? format-repairs))
        finding-set (set (:findings failure))
        schema-failure? (or (and (seq finding-set)
                                 (every? #{:lane-report-invalid
                                           :candidate-patterns-missing}
                                         finding-set))
                            (= #{:depositor-missing :candidates-missing
                                 :lane-report-invalid}
                               finding-set))
        schema-repairs (or (:schema-repairs state) 0)
        schema-repair? (and schema-failure? (zero? schema-repairs))
        boundary-repair? (or format-repair? schema-repair?)]
    (if (and (>= attempt max-deposit-attempts) (not boundary-repair?))
      (assoc failure :error/code :promotion-deposit-retries-exhausted
             :attempts attempt)
      (let [repair-ordinal
            (inc (+ (count (:failed-attempts state))
                    format-repairs schema-repairs))
            predecessor {:job {:job-id (:job state)
                               :state :failed
                               :report failure}
                         :ticket (:ticket state)
                         :terminal-collection (:terminal-collection state)
                         :findings (vec (:findings failure))
                         :trace/successor-observation
                         (successor-observation (:job state)
                                                (:terminal-collection state)
                                                (:findings failure))}
            archived-state (update state :superseded-terminals
                                   (fnil conj []) predecessor)
            archived (persist-fn archived-state)]
        (if-not (:ok archived)
          {:ok false :error/code :promotion-deposit-archive-persistence-failed
           :state state}
          (let [retry (deposit-fn (assoc failure
                                         :submission/attempt repair-ordinal))]
            (if-not (:ok retry)
              retry
              (let [next-state
                (-> state
                    (assoc :superseded-terminals (:superseded-terminals archived-state))
                    (bind-last-successor (:job retry))
                    (assoc :job (:job retry)
                           :ticket {:job-id (:job retry)}
                           :attempt (if boundary-repair? attempt (inc attempt)))
                    (cond-> format-repair?
                      (assoc :format-repairs (inc format-repairs))
                      schema-repair?
                      (assoc :schema-repairs (inc schema-repairs)))
                    (update :failed-attempts (fnil conj [])
                            {:attempt attempt :job (:job state)
                             :failure (select-keys failure
                                                   [:error/code :findings])}))]
            (persist-fn next-state)
            {:ok true :status :awaiting-terminal :job-id (:job retry)
             :retry/reason (or (:error/code failure) :deposit-invalid)
             :state next-state}))))))))

(defn- persist-mechanical-reviews!
  [deposit reviews persist-reviews-fn]
  (if (empty? reviews)
    {:ok true :reviews []}
    (let [job-id (str "controller-mechanical-review-"
                      (subs (machine/ledger-digest
                             [{:deposit-candidates
                               (mapv :memory-id (:candidates deposit))
                               :reviews reviews}]) 0 32))]
      (persist-reviews-fn
       {:deposit deposit
        :reviewer pipeline/mechanical-reviewer
        :reviews reviews
        :review-job job-id}))))

(defn- hold-incomplete-pass!
  [state checked promotion-policy contract-digest persist-fn now-ms-fn]
  (let [projection-failure?
        (or (= :promotion-review-projection-failed (:error/code checked))
            (some #(= :promotion-review-projection-failed (:finding %))
                  (:findings checked)))
        unresolved-review?
        (some #(= :promotion-pass-unresolved (:finding %))
              (:findings checked))
        repair-kind (or (:repair/kind checked)
                        (cond
                          projection-failure? :review-projection
                          unresolved-review? :unresolved-review
                          :else :promotion-pass))
        transport? (transport-failure? checked)
        transport-finding
        (some #(when (and (map? %) (= :transport (:error/component %))) %)
              (tree-seq coll? seq checked))
        now-ms (long (now-ms-fn))
        transport-attempt (or (:transport-retry/attempt state) 0)
        transport-max (or (:transport-retry-max-attempts promotion-policy)
                          default-transport-retry-max-attempts)
        transport-delay (or (:transport-retry-delay-ms promotion-policy)
                            default-transport-retry-delay-ms)
        transport-history
        (cond-> (vec (:transport-retry/history state))
          transport?
          (conj (cond-> {:attempt transport-attempt
                         :failed-at-ms now-ms
                         :error/component :transport
                         :error/code (or (:error/code transport-finding)
                                         (:error/code checked))}
                  (:transport/operation transport-finding)
                  (assoc :transport/operation
                         (:transport/operation transport-finding))
                  (:transport/acquired-outcome transport-finding)
                  (assoc :transport/acquired-outcome
                         (:transport/acquired-outcome transport-finding))
                  (:transport/evidence transport-finding)
                  (assoc :transport/evidence
                         (:transport/evidence transport-finding)))))
        retryable-transport? (and transport?
                                  (< (inc transport-attempt) transport-max))
        hold {:state/type :promotion
              :stage :awaiting-apparatus-repair
              :last-valid-state state
              :contract-digest contract-digest
              :error/code (:error/code checked)
              :findings (:findings checked)
              :persisted-review-result
              (select-keys checked
                           [:review-job :returned-reviews :reviews :persisted])
              :repair/kind repair-kind
              :repair/attempts (or (:projection-repair-attempt state) 0)
              :repair/max-attempts
              (or (:projection-repair-max-attempts promotion-policy) 1)}
        exhausted-transport? (and transport? (not retryable-transport?))
        first-failed-at-ms (some-> transport-history first :failed-at-ms)
        escalation (when exhausted-transport?
                     {:error/code :promotion-substrate-retry-exhausted
                      :attempts (count transport-history)
                      :elapsed-ms (when first-failed-at-ms
                                    (max 0 (- now-ms first-failed-at-ms)))
                      :history transport-history})
        hold (if retryable-transport?
               (assoc hold
                      :stage :awaiting-transport-retry
                      :transport-retry/attempt transport-attempt
                      :transport-retry/max-attempts transport-max
                      :transport-retry/delay-ms transport-delay
                      :transport-retry/not-before-ms
                      (+ now-ms transport-delay)
                      :transport-retry/history transport-history)
               (cond-> hold
                 transport?
                 (assoc :error/code :promotion-substrate-retry-exhausted
                        :transport-retry/terminal? true
                        :transport/decision [:park :retry-exhausted]
                        :transport-retry/last-error-code
                        (or (:error/code transport-finding)
                            (:error/code checked))
                        :transport-retry/escalation escalation
                        :transport-retry/attempt transport-attempt
                        :transport-retry/max-attempts transport-max
                        :transport-retry/history transport-history
                        :repair/attempts (:repair/max-attempts hold))))]
    (persist-fn hold)
    {:ok true :status (if retryable-transport?
                        :transport-retry-scheduled
                        :awaiting-apparatus-repair)
     :state hold :findings (:findings checked)
     :transport-retry/escalation escalation}))

(defn- publish-completed-pass!
  [state action promotion-policy contract-digest publish-fn persist-fn
   certificate-emitter-fn now-ms-fn]
  (let [checked (if (:completed-pass-required promotion-policy)
                  (pipeline/validate-complete-dispositions
                   (:dispatched-candidates action) (:reviews action))
                  {:ok true :dispositions []})]
    (if-not (:ok checked)
      (hold-incomplete-pass!
       state
       (assoc checked :review-job (:job state) :reviews (:reviews action))
       promotion-policy contract-digest persist-fn now-ms-fn)
      (let [consumed-job-id (or (:job state)
                                (:deposit-job state)
                                (get-in state [:deposit :job-id]))
            published-action
            (cond-> action
              (string? consumed-job-id)
              (assoc :job-id consumed-job-id)
              (:completed-pass-required promotion-policy)
              (assoc :dispositions (:dispositions checked)))
            published (publish-fn published-action)
            result
            (if-not (:ok published)
              (hold-incomplete-pass!
               state
               {:ok false :error/code :promotion-publication-failed
                :repair/kind :promotion-publication
                :review-job (:job state)
                :reviews (:reviews action)
                :findings [published]}
               promotion-policy contract-digest persist-fn now-ms-fn)
              (let [done {:state/type :promotion-certified
                          :receipt (:receipt published)}
                    done (if (pos? (or (:transport-retry/attempt state) 0))
                           (assoc done :transport-retry/history
                                  (conj (vec (:transport-retry/history state))
                                        {:attempt (:transport-retry/attempt state)
                                         :succeeded-at-ms (long (now-ms-fn))}))
                           done)]
                {:ok true :status :certified :state done
                 :certificate (:receipt published)}))
            emitted-at-ms (long (now-ms-fn))
            certificate (publication-certificate state published result
                                                 promotion-policy)
            conformance (transport/validate-certificate certificate)
            emission (when certificate-emitter-fn
                       (certificate-emitter-fn certificate emitted-at-ms))
            emission-ok? (or (nil? emission) (not= false (:ok emission)))
            advance? (= :certified (:status result))]
        (if (and advance? (or (not (:ok conformance)) (not emission-ok?)))
          (let [hold {:state/type :promotion
                      :stage :awaiting-apparatus-repair
                      :last-valid-state state
                      :error/code :transport-certificate-nonconformant
                      :findings (vec (concat (:findings conformance)
                                             (when-not emission-ok?
                                               [{:error/code
                                                 :transport-certificate-persistence-failed
                                                 :emission emission}])))
                      :transport-certificate certificate}]
            (persist-fn hold)
            {:ok true :status :awaiting-apparatus-repair :state hold
             :findings (:findings hold)
             :transport-certificate emission})
          (let [terminal? (= :promotion-substrate-retry-exhausted
                             (get-in result [:state :error/code]))
                terminal-state (cond-> (:state result)
                                 terminal?
                                 (assoc :transport-certificate certificate
                                        :transport-certificate-emission emission))
                result (if terminal? (assoc result :state terminal-state) result)]
            (when (or advance? terminal?) (persist-fn (:state result)))
            (cond-> result emission (assoc :transport-certificate emission))))))))

(defn- drive-step!
  [{:keys [state deposit-fn review-fn publish-fn persist-fn certificate-emitter-fn
           prepare-patterns-fn persist-candidates-fn candidate-visible-fn
           persist-reviews-fn deposit-request reviewer-request
           promotion-policy contract-digest now-ms-fn]
    :as inputs
    :or {prepare-patterns-fn coined-pattern/publish!
         now-ms-fn #(System/currentTimeMillis)
         persist-candidates-fn
         (fn [deposit] {:ok true :deposit deposit
                        :candidates (:candidates deposit)})
         candidate-visible-fn (constantly true)
         persist-reviews-fn pass-reviews}}]
  (cond
    (and (= :awaiting-apparatus-repair (:stage state))
         (:transport-retry/terminal? state))
    (cond-> {:ok true :status :awaiting-apparatus-repair
             :state state :findings (:findings state)
             :transport-retry/escalation (:transport-retry/escalation state)}
      (:transport-certificate-emission state)
      (assoc :transport-certificate (:transport-certificate-emission state)))

    (nil? state)
    (let [r (deposit-fn)]
      (if-not (:ok r) r
        (let [s {:state/type :promotion :stage :deposit :job (:job r)
                 :request deposit-request :deposit-request deposit-request
                 :ticket {:job-id (:job r)}
                 :attempt 1}]
          (persist-fn s) {:ok true :status :awaiting-terminal
                          :job-id (:job r) :state s})))

    (= :deposit (:stage state))
    (let [r (deposit-fn (:job state))]
      (cond
        (= :awaiting-terminal (:status r)) (assoc r :job-id (:job state))
        (not (:ok r)) (retry-deposit! state r deposit-fn persist-fn)
        :else
        (let [persisted (persist-candidates-fn (:report r))]
          (cond
            (and (not (:ok persisted))
                 (= :promotion-candidate-content-invalid
                    (:error/code persisted)))
            (retry-deposit! state persisted deposit-fn persist-fn)

            (not (:ok persisted)) persisted

            :else
            (let [deposit (:deposit persisted)
                  checked (pipeline/validate-deposit
                           deposit
                           {:problem-id (:problem-id deposit-request)
                            :solver-certified-source
                            (:solver-certified-source deposit-request)})]
              (if-not (:ok checked)
                (retry-deposit! state checked deposit-fn persist-fn)
                (let [candidates (:candidates checked)
                      mechanical (:mechanical-reviews checked)
                      patterns (prepare-patterns-fn deposit)]
                  (if-not (:ok patterns)
                    (retry-deposit! state patterns deposit-fn persist-fn)
                    (let [review-successor? (some? (:abandoned-review-job state))
                          review-attempt (inc (or (:review-successor-attempt state) 0))
                          review (if (seq candidates)
                                   (if review-successor?
                                     (review-fn candidates
                                                (:abandoned-review-job state)
                                                review-attempt)
                                     (review-fn candidates))
                                   {:ok true :job nil})]
                      (if-not (:ok review)
                        review
                        (let [s (cond->
                                {:state/type :promotion
                                 :stage :independent-review
                                 :deposit deposit
                                 :candidates candidates
                                 :mechanical-reviews mechanical
                                 :deposit-job (:job state)
                                 :deposit-request deposit-request
                                 :superseded-terminals (:superseded-terminals state)
                                 :job (:job review)
                                 :request reviewer-request
                                 :ticket {:job-id (:job review)}}
                                  review-successor?
                                  (assoc :predecessor-job-id
                                         (:abandoned-review-job state)
                                         :review-successor-attempt review-attempt))]
                          (persist-fn s)
                          (if (seq candidates)
                            {:ok true :status :awaiting-terminal
                             :job-id (:job review) :state s}
                            (let [persisted-mechanical
                                  (if (:completed-pass-required promotion-policy)
                                    (persist-mechanical-reviews!
                                     deposit mechanical persist-reviews-fn)
                                    {:ok true :reviews mechanical})]
                              (if-not (:ok persisted-mechanical)
                                (hold-incomplete-pass!
                                 s (assoc persisted-mechanical
                                          :findings [persisted-mechanical])
                                 promotion-policy contract-digest persist-fn
                                 now-ms-fn)
                                (publish-completed-pass!
                                 s
                                 {:candidates []
                                  :dispatched-candidates
                                  (filterv
                                   (set (map :memory-id mechanical))
                                  (:candidates deposit))
                                 :deposit deposit
                                 :reviewer pipeline/mechanical-reviewer
                                  :reviews (:reviews persisted-mechanical)}
                                 promotion-policy contract-digest publish-fn
                                 persist-fn certificate-emitter-fn
                                 now-ms-fn)))))))))))))))

    ;; Entry for candidates gated elsewhere (a Guide's store-mode deposit):
    ;; no Scribe deposit job, straight to the independent reviewer.
    (= :review-pending (:stage state))
    (let [candidates (:candidates state)
          mechanical (:mechanical-reviews state)
          patterns (prepare-patterns-fn (:deposit state))]
      (if-not (:ok patterns) patterns
        (let [review (if (seq candidates)
                       (review-fn candidates)
                       {:ok true :job nil})]
         (if-not (:ok review) review
        (let [s {:state/type :promotion :stage :independent-review
                 :deposit (:deposit state) :candidates candidates
                 :mechanical-reviews mechanical
                 :job (:job review) :request reviewer-request
                 :ticket {:job-id (:job review)}}]
          (persist-fn s)
          (if (seq candidates)
            {:ok true :status :awaiting-terminal
             :job-id (:job review) :state s}
            (let [persisted-mechanical
                  (if (:completed-pass-required promotion-policy)
                    (persist-mechanical-reviews!
                     (:deposit state) mechanical persist-reviews-fn)
                    {:ok true :reviews mechanical})]
              (if-not (:ok persisted-mechanical)
                (hold-incomplete-pass!
                 s (assoc persisted-mechanical
                          :findings [persisted-mechanical])
                 promotion-policy contract-digest persist-fn now-ms-fn)
                (publish-completed-pass!
                 s
                 {:candidates []
                  :dispatched-candidates
                  (filterv
                   (set (map :memory-id mechanical))
                   (get-in state [:deposit :candidates]))
                  :deposit (:deposit state)
                  :reviewer pipeline/mechanical-reviewer
                  :reviews (:reviews persisted-mechanical)}
                 promotion-policy contract-digest
                 publish-fn persist-fn certificate-emitter-fn
                 now-ms-fn)))))))))

    (= :independent-review (:stage state))
    (let [invisible (filterv (complement candidate-visible-fn)
                             (:candidates state))]
      (if (seq invisible)
        (retry-deposit!
         (-> state
             (assoc :stage :deposit
                    :job (or (:deposit-job state)
                             (get-in state [:deposit :job-id]))
                    :abandoned-review-job (:job state)))
         {:ok false :error/code :promotion-candidates-not-persisted
          :findings (mapv (fn [candidate]
                            {:finding :candidate-not-visible
                             :memory-id (:memory-id candidate)})
                          invisible)}
         deposit-fn persist-fn)
        (let [r (review-fn (:job state) (:candidates state))]
          (if (= :awaiting-terminal (:status r))
            (assoc r :job-id (:job state))
            (let [checked (pipeline/validate-returned-review*
                           (:candidates state)
                           (:depositor (:deposit state))
                           (:reviewer r) (:reviews r))]
              (if-not (:ok checked)
                (if (:completed-pass-required promotion-policy)
                  (hold-incomplete-pass!
                   state (assoc checked :error/code :promotion-pass-incomplete)
                   promotion-policy contract-digest persist-fn now-ms-fn)
                  checked)
                (let [persisted
                      (persist-reviews-fn
                       {:deposit (:deposit state)
                        :reviewer (:reviewer r)
                        :reviews (:reviews r)
                        :review-job (:job state)})]
                  (if-not (:ok persisted)
                    (if (:completed-pass-required promotion-policy)
                      (hold-incomplete-pass!
                       state
                       (cond-> persisted
                         (empty? (:findings persisted))
                         (assoc :findings [persisted])
                         (seq (:reviews r))
                         (assoc :returned-reviews (:reviews r)))
                       promotion-policy contract-digest persist-fn now-ms-fn)
                      persisted)
                    (let [persisted-checked
                          (pipeline/validate-review*
                           (:candidates state)
                           (:depositor (:deposit state))
                           (:reviewer r) (:reviews persisted))]
                      (if-not (:ok persisted-checked)
                        (if (:completed-pass-required promotion-policy)
                          (hold-incomplete-pass!
                           state
                           (assoc persisted-checked
                                  :error/code
                                  :persisted-promotion-review-invalid)
                           promotion-policy contract-digest persist-fn now-ms-fn)
                          (assoc persisted-checked
                                 :error/code
                                 :persisted-promotion-review-invalid))
                        (let [persisted-mechanical
                              (if (:completed-pass-required promotion-policy)
                                (persist-mechanical-reviews!
                                 (:deposit state) (:mechanical-reviews state)
                                 persist-reviews-fn)
                                {:ok true
                                 :reviews (:mechanical-reviews state)})]
                          (if-not (:ok persisted-mechanical)
                            (hold-incomplete-pass!
                             state
                             (assoc persisted-mechanical
                                    :findings [persisted-mechanical])
                             promotion-policy contract-digest persist-fn now-ms-fn)
                            (publish-completed-pass!
                             state
                             {:candidates (:candidates persisted-checked)
                              :dispatched-candidates
                              (let [ids (set (map :memory-id
                                                  (:reviews persisted-mechanical)))]
                                (into (vec (:candidates state))
                                      (filter #(contains? ids (:memory-id %))
                                              (get-in state
                                                      [:deposit :candidates]))))
                              :deposit (:deposit state)
                              :reviewer (:reviewer r)
                              :reviews
                              (into (vec (:reviews persisted-mechanical))
                                    (:reviews persisted))}
                             promotion-policy contract-digest publish-fn
                             persist-fn certificate-emitter-fn
                             now-ms-fn)))))))))))))

    (= :awaiting-transport-retry (:stage state))
    (if (< (long (now-ms-fn)) (:transport-retry/not-before-ms state))
      {:ok true :status :transport-retry-scheduled :state state
       :retry/not-before-ms (:transport-retry/not-before-ms state)}
      (drive-step! (assoc inputs :state
                     (assoc (:last-valid-state state)
                            :transport-retry/attempt
                            (inc (:transport-retry/attempt state))
                            :transport-retry/history
                            (:transport-retry/history state)))))

    (= :awaiting-apparatus-repair (:stage state))
    (cond
      ;; A certificate deliberately refuses advancement when the namespace
      ;; loaded in the shared JVM differs from its source.  Once an operator
      ;; has reloaded that exact source, replay the last valid state through
      ;; the normal idempotent publication path.  Restrict this recovery to
      ;; the precise identity finding: no other apparatus failure is erased.
      (repaired-runtime-identity? state)
      (drive-step! (assoc inputs :state (:last-valid-state state)))

      (and (or (= :promotion-pass (:repair/kind state))
               (legacy-approved-pattern-projection-failure? state))
           (or (< (:repair/attempts state) (:repair/max-attempts state))
               (legacy-approved-pattern-projection-failure? state)
               (and (= [:review-patterns-invalid] (vec (:findings state)))
                    (not (:pattern-contract-repair-attempted?
                          (:last-valid-state state))))))
      (let [prior (:last-valid-state state)
            successor-attempt (inc (or (:review-successor-attempt prior) 0))
            predecessor {:job {:job-id (:job prior)
                               :state :done
                               :report (:persisted-review-result state)}
                         :ticket (:ticket prior)
                         :terminal-collection (:terminal-collection prior)
                         :findings (vec (:findings state))
                         :trace/successor-observation
                         (successor-observation (:job prior)
                                                (:terminal-collection prior)
                                                (:findings state))}
            archived-prior (update prior :superseded-terminals
                                   (fnil conj []) predecessor)
            archived (persist-fn archived-prior)]
        (if-not (:ok archived)
          {:ok false :error/code :promotion-review-archive-persistence-failed
           :state state}
          (let [successor (review-fn (:candidates prior) (:job prior)
                                     successor-attempt)]
            (if-not (:ok successor)
              successor
              (let [next-state (-> archived-prior
                                   (bind-last-successor (:job successor))
                                   (assoc :job (:job successor)
                                          :ticket {:job-id (:job successor)}
                                          :predecessor-job-id (:job prior)
                                          :review-successor-attempt successor-attempt
                                          :pattern-contract-repair-attempted? true
                                          :projection-repair-attempt
                                          (inc (:repair/attempts state))))]
                (persist-fn next-state)
                {:ok true :status :awaiting-terminal
                 :job-id (:job successor) :state next-state})))))

      (and (= :unresolved-review (:repair/kind state))
           (string? contract-digest)
           (not= contract-digest (:contract-digest state)))
      (let [prior (:last-valid-state state)
            successor-attempt (inc (or (:review-successor-attempt prior) 0))
            predecessor {:job {:job-id (:job prior)
                               :state :done
                               :report (:persisted-review-result state)}
                         :ticket (:ticket prior)
                         :terminal-collection (:terminal-collection prior)
                         :findings (vec (:findings state))
                         :trace/successor-observation
                         (successor-observation (:job prior)
                                                (:terminal-collection prior)
                                                (:findings state))}
            archived-prior (update prior :superseded-terminals
                                   (fnil conj []) predecessor)
            archived (persist-fn archived-prior)]
        (if-not (:ok archived)
          {:ok false :error/code :promotion-review-archive-persistence-failed
           :state state}
          (let [successor (review-fn (:candidates prior) (:job prior)
                                     successor-attempt)]
            (if-not (:ok successor)
              successor
              (let [next-state (-> archived-prior
                                  (bind-last-successor (:job successor))
                                  (assoc
                                  :job (:job successor)
                                  :ticket {:job-id (:job successor)}
                                  :predecessor-job-id (:job prior)
                                  :review-successor-attempt successor-attempt))]
            (persist-fn next-state)
            {:ok true :status :awaiting-terminal
             :job-id (:job successor) :state next-state})))))

      (and (string? contract-digest)
           (not= contract-digest (:contract-digest state)))
      (drive-step! (assoc inputs :state
                     (assoc (:last-valid-state state)
                            :projection-repair-attempt 0)))

      (and (contains? #{:review-projection :promotion-publication}
                      (:repair/kind state))
           (< (:repair/attempts state) (:repair/max-attempts state)))
      (drive-step! (assoc inputs :state
                     (assoc (:last-valid-state state)
                            :projection-repair-attempt
                            (inc (:repair/attempts state)))))

      (contains? #{:review-projection :promotion-publication}
                 (:repair/kind state))
      {:ok false
       :error/code :promotion-apparatus-repair-exhausted
       :state state
       :repair/kind (:repair/kind state)
       :repair/attempts (:repair/attempts state)
       :findings (:findings state)}

      :else
      {:ok true :status :awaiting-apparatus-repair
       :state state :findings (:findings state)})

    (= :promotion-certified (:state/type state))
    {:ok true :status :certified :state state :certificate (:receipt state)}

    :else {:ok false :error/code :live-promotion-state-invalid}))

(defn drive!
  "Drive one promotion step, classifying transport failures before they escape.

  `drive-step!` reports several failures by returning the raw failure map --
  the deposit-stage persistence result among them. A substrate write that
  times out is reported that way, so it reached the regulator as a plain tick
  failure and stopped the campaign, even though the review path already had a
  bounded transport retry. Classification belongs at the one exit rather than
  at each `(:ok ...)` branch, so every stage gets the same treatment."
  [{:keys [state promotion-policy contract-digest persist-fn now-ms-fn]
    :as inputs}]
  (let [result (drive-step! inputs)]
    (if (and persist-fn
             (map? result)
             (false? (:ok result))
             (transport-failure? result))
      (hold-incomplete-pass!
       state
       (-> result
           (update :repair/kind #(or % :promotion-transport))
           (update :findings #(if (seq %) (vec %) [result])))
       promotion-policy contract-digest persist-fn
       (or now-ms-fn #(System/currentTimeMillis)))
      result)))
