(ns futon3c.apm.live-promotion
  "Durable two-seat promotion dispatcher."
  (:require [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.promotion-pipeline :as pipeline]))

(defn resolved-role-card-path [control-root request]
  (let [path (:role-card-path request)
        file (when (string? path) (java.io.File. path))]
    (cond
      (nil? file) nil
      (.isAbsolute file) (.getCanonicalPath file)
      (string? control-root) (.getCanonicalPath (java.io.File. control-root path))
      :else path)))

(declare drive!)

(def ^:private max-deposit-attempts 3)

(defn- normalize-review-report [report expected-digest expected-base-blob]
  (let [reviews (or (:reviews report) (:promotion-reviews report))
        reviewers (set (keep :reviewer reviews))
        reviewer (or (:reviewer report)
                     (when (= 1 (count reviewers)) (first reviewers)))]
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

      (or (not (string? reviewer))
          (some #(not= reviewer (:reviewer %)) reviews))
      {:ok false :error/code :promotion-review-attribution-ambiguous}

      :else {:ok true :reviewer reviewer :reviews reviews})))

(defn- agency-stage [agency-base request prompt]
  (fn
    ([]
     (let [announced (runtime/http-json
                      "POST" (str agency-base "/api/alpha/invoke/announce")
                      {:agent-id (:agent-id request) :prompt prompt
                       :surface "emacs-repl" :caller "countdown-control"
                       :mode "work"})
           job-id (:job-id announced)
           activated (when (and (= 202 (:http/status announced)) job-id)
                       (runtime/http-json
                        "POST" (str agency-base "/api/alpha/invoke/activate")
                        {:agent-id (:agent-id request) :prompt prompt
                         :surface "emacs-repl" :caller "countdown-control"
                         :mode "work" :job-id job-id}))]
       (if (and (= 202 (:http/status announced)) (:ok announced)
                (= 202 (:http/status activated)) (:accepted activated))
         {:ok true :job job-id}
         {:ok false :error/code :promotion-stage-dispatch-failed})))
    ([job-id]
     (let [job (runtime/job->terminal
                (runtime/http-json
                 "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id)))]
       (if (contains? #{:done :failed :timeout :cancelled} (:state job))
         (if (and (= :done (:state job)) (map? (:report job)))
           {:ok true :job job-id :report (:report job)}
           {:ok false :error/code :promotion-stage-terminal-invalid :job job
            :report/error (:report/error job)})
         {:ok true :status :awaiting-terminal :job job-id})))))

(defn run-live!
  [{:keys [state-path agency-base control-root deposit-request reviewer-request
           publish-fn]
    :or {agency-base "http://localhost:7070"}}]
  (let [state (runtime/read-state state-path)
        deposit-card-path (resolved-role-card-path control-root deposit-request)
        deposit-prompt (str "Deposit promotion candidates. Authority:\n"
                            (pr-str deposit-request)
                            "\nRead and follow the frozen role card at " deposit-card-path
                            " (blob " (:role-card-blob deposit-request) ")."
                            "\nReturn exactly one parseable EDN map and no prose. "
                            "It must contain string :depositor and non-empty vector "
                            ":candidates and a complete vector :lanes report. "
                            "Every candidate must contain string "
                            ":memory-id, string :content-digest, vector :pattern-ids, "
                            "and vector :source-attempts. EDN does not concatenate "
                            "adjacent string literals; use one string value per field.")
        deposit-stage (agency-stage agency-base deposit-request deposit-prompt)
        deposit-fn (fn
                     ([] (deposit-stage))
                     ([value]
                      (if (string? value)
                        (deposit-stage value)
                        ((agency-stage
                          agency-base deposit-request
                          (str deposit-prompt
                               "\nThe previous response failed the EDN linter: "
                               (pr-str (select-keys value
                                                    [:error/code :report/error]))
                               "\nRepair only the serialization/shape and return the complete map."))))))
        review-fn
        (fn
          ([candidates]
           (let [digest (machine/ledger-digest [candidates])
                 request (assoc reviewer-request :candidates candidates
                                :candidate-set-digest digest)
                 prompt (str "Independently review this exact candidate set. Authority:\n"
                             (pr-str request)
                             "\nRead and follow the frozen role card at "
                             (resolved-role-card-path control-root request)
                             " (blob " (:role-card-blob request) ")."
                             "\nFollow the pinned promotion Proctor card and return exactly one EDN map. "
                             "Persist each approval's evidence body with nonblank "
                             ":review/reason and :review/residual fields; return the "
                             "same nonblank :reason and :residual on every review.")]
             ((agency-stage agency-base request prompt))))
          ([job-id candidates]
           (let [digest (machine/ledger-digest [candidates])
                 request (assoc reviewer-request :candidates candidates
                                :candidate-set-digest digest)
                 prompt (str "Independently review this exact candidate set. Authority:\n"
                             (pr-str request)
                             "\nRead and follow the frozen role card at "
                             (resolved-role-card-path control-root request)
                             " (blob " (:role-card-blob request) ").")
                 result ((agency-stage agency-base request prompt) job-id)]
             (if (:report result)
               (let [normalized (normalize-review-report
                                 (:report result) digest
                                 (:base-problem-blob reviewer-request))]
                 (if (:ok normalized)
                   (merge result (select-keys normalized [:reviewer :reviews]))
                   normalized))
               result))))]
    (drive! {:state state :deposit-fn deposit-fn :review-fn review-fn
             :publish-fn publish-fn
             :persist-fn #(runtime/atomic-persist! state-path %)})))

(defn- retry-deposit!
  [state failure deposit-fn persist-fn]
  (let [attempt (or (:attempt state) 1)
        format-failure? (= :report-edn-invalid
                           (get-in failure [:report/error :error/code]))
        format-repairs (or (:format-repairs state) 0)
        format-repair? (and format-failure? (zero? format-repairs))]
    (if (and (>= attempt max-deposit-attempts) (not format-repair?))
      (assoc failure :error/code :promotion-deposit-retries-exhausted
             :attempts attempt)
      (let [retry (deposit-fn failure)]
        (if-not (:ok retry)
          retry
          (let [next-state
                (-> state
                    (assoc :job (:job retry)
                           :attempt (if format-repair? attempt (inc attempt)))
                    (cond-> format-repair?
                      (assoc :format-repairs (inc format-repairs)))
                    (update :failed-attempts (fnil conj [])
                            {:attempt attempt :job (:job state)
                             :failure (select-keys failure
                                                   [:error/code :findings])}))]
            (persist-fn next-state)
            {:ok true :status :awaiting-terminal :job-id (:job retry)
             :retry/reason (or (:error/code failure) :deposit-invalid)
             :state next-state}))))))

(defn drive!
  [{:keys [state deposit-fn review-fn publish-fn persist-fn]}]
  (cond
    (nil? state)
    (let [r (deposit-fn)]
      (if-not (:ok r) r
        (let [s {:state/type :promotion :stage :deposit :job (:job r)
                 :attempt 1}]
          (persist-fn s) {:ok true :status :awaiting-terminal
                          :job-id (:job r) :state s})))

    (= :deposit (:stage state))
    (let [r (deposit-fn (:job state))]
      (cond
        (= :awaiting-terminal (:status r)) (assoc r :job-id (:job state))
        (not (:ok r)) (retry-deposit! state r deposit-fn persist-fn)
        :else
        (let [checked (pipeline/validate-deposit (:report r))]
          (if-not (:ok checked)
            (retry-deposit! state checked deposit-fn persist-fn)
            (let [review (review-fn (:candidates checked))]
              (if-not (:ok review) review
                (let [s {:state/type :promotion :stage :independent-review
                         :deposit (:report r) :candidates (:candidates checked)
                         :job (:job review)}]
                  (persist-fn s)
                  {:ok true :status :awaiting-terminal
                   :job-id (:job review) :state s})))))))

    (= :independent-review (:stage state))
    (let [r (review-fn (:job state) (:candidates state))]
      (if (= :awaiting-terminal (:status r)) (assoc r :job-id (:job state))
        (let [checked (pipeline/validate-review
                       (:deposit state) (:reviewer r) (:reviews r))]
          (if-not (:ok checked) checked
            (let [published (publish-fn
                             {:candidates (:candidates checked)
                              :deposit (:deposit state)
                              :reviewer (:reviewer r)
                              :reviews (:reviews r)})]
              (if-not (:ok published) published
                (let [s {:state/type :promotion-certified
                         :receipt (:receipt published)}]
                  (persist-fn s)
                  {:ok true :status :certified :state s
                   :certificate (:receipt published)})))))))

    (= :promotion-certified (:state/type state))
    {:ok true :status :certified :state state :certificate (:receipt state)}

    :else {:ok false :error/code :live-promotion-state-invalid}))
