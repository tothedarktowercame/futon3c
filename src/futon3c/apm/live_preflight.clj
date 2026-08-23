(ns futon3c.apm.live-preflight
  "Typed, fail-closed dispatch and ingestion boundary for live preflight."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-contract :as cycle]
            [futon3c.apm.toolchain-port :as toolchain-port]))

(def required-report-fields
  #{:command-own-exit :lean :clean-before?
    :clean-after? :mutations})

(defn normalize-report
  "Canonicalize the countable Lean evidence emitted by older Proctor prompts.

   No evidence is inferred: vector warnings/errors are counted, `sorry-count`
   is renamed, and the command-owned exit is copied into the nested Lean map.
   Any other shape is left untouched and therefore fails normal validation."
  [report]
  (let [report (if (and (map? (:evidence report))
                        (integer? (:command-own-exit report)))
                 (assoc (:evidence report)
                        :command-own-exit (:command-own-exit report))
                 report)
        mutations (:mutations report)
        report (if (and (map? mutations)
                        (= #{:made :revision :blob} (set (keys mutations)))
                        (vector? (:made mutations))
                        (string? (:revision mutations))
                        (string? (:blob mutations)))
                 (-> report
                     (assoc :mutations (:made mutations)
                            :problem-revision (:revision mutations)
                            :problem-blob (:blob mutations)))
                 report)
        lean (:lean report)]
    (if (and (map? lean)
             (sequential? (:warnings lean))
             (sequential? (:errors lean))
             (nat-int? (:sorry-count lean))
             (integer? (:command-own-exit report)))
      (assoc report :lean
             {:exit (:command-own-exit report)
              :warnings (count (:warnings lean))
              :sorry-warnings (:sorry-count lean)
              :errors (count (:errors lean))
              :output (str/join "\n" (concat (:warnings lean)
                                               (:errors lean)))})
      report)))

(defn build-request
  [{:keys [ledger unit role-card seat timeouts]}]
  (let [problem (:problem unit)
        findings (cond-> []
                   (not (pos-int? (:version ledger))) (conj :ledger-version-invalid)
                   (not (and (string? (:digest ledger))
                             (re-matches #"[0-9a-f]{64}" (:digest ledger))))
                   (conj :ledger-digest-invalid)
                   (not= :preflight (:phase ledger)) (conj :ledger-phase-mismatch)
                   (some? (:claim ledger)) (conj :ledger-claim-present)
                   (not (and (string? (:frame/id unit))
                             (re-matches #"f[0-9]+" (:frame/id unit))))
                   (conj :frame-mismatch)
                   (not (string? (:problem/id unit))) (conj :problem-mismatch)
                   (not= (str (:frame/id unit) "-proctor") (:agent-id seat))
                   (conj :seat-identity-mismatch)
                   (not= :codex (:type seat)) (conj :seat-type-mismatch)
                   (not= (:frame/id unit) (:frame-id seat))
                   (conj :seat-attribution-mismatch)
                   (not (true? (:invoke-ready? seat))) (conj :seat-not-ready)
                   (not= 3600000 (:turn-timeout-ms timeouts))
                   (conj :turn-timeout-mismatch)
                   (not (and (string? (:path role-card))
                             (string? (:blob role-card))))
                   (conj :role-card-pin-missing))]
    (if (seq findings)
      {:ok false :error/code :preflight-dispatch-input-invalid
       :findings findings}
      (let [body {:dispatch/type :frame-preflight
                  :phase :preflight
                  :agent-id (:agent-id seat)
                  :ledger-digest (:digest ledger)
                  :frame-id (:frame/id unit) :problem-id (:problem/id unit)
                  :role-card-path (:path role-card)
                  :role-card-blob (:blob role-card)
                  :problem-repository (:repository problem)
                  :problem-revision (:revision problem)
                  :problem-path (:path problem) :problem-blob (:blob problem)
                  :timeouts timeouts
                  :instructions
                  "Read-only preflight. Report command-own exit status, exact revision/blob, direct Lean warnings/errors/sorry count, and before/after cleanliness. Make no mutations."}]
        {:ok true :request (assoc body :dispatch/id (machine/ledger-digest [body]))}))))

(defn record-dispatch
  [request response]
  (if-not (and (= true (:ok response))
               (string? (:job-id response))
               (not-empty (:job-id response)))
    {:ok false :error/code :preflight-dispatch-not-acknowledged}
    (let [body {:dispatch/id (:dispatch/id request)
                :job-id (:job-id response) :agent-id (:agent-id request)
                :frame-id (:frame-id request) :problem-id (:problem-id request)}]
      {:ok true :ticket (assoc body :ticket/id (machine/ledger-digest [body]))})))

(defn validate-terminal
  [request ticket job]
  (let [report (normalize-report (:report job))
        missing (set/difference required-report-fields (set (keys report)))
        lean (:lean report)
        findings
        (cond-> []
          (not= (:job-id ticket) (:job-id job)) (conj :job-id-mismatch)
          (not= (:agent-id request) (:agent-id job)) (conj :agent-id-mismatch)
          (not= :done (:state job)) (conj :job-not-successfully-terminal)
          (seq missing) (conj :terminal-report-fields-missing)
          (not= 0 (:command-own-exit report)) (conj :command-own-exit-nonzero)
          ;; Preflight measures a non-vacuous unresolved baseline and no errors.
          ;; A problem may legitimately expose several helper declarations as
          ;; sorries; the later verification boundary, not preflight, requires
          ;; the terminal count to be zero.
          ;; Total warnings may also include independent compiler/linter
          ;; notices (for example deprecations); equating that count with the
          ;; sorry count rejects a sound baseline for unrelated reasons.
          (not (and (nat-int? (:warnings lean))
                    (<= (:sorry-warnings lean) (:warnings lean))
                    (toolchain-port/acceptable-preflight?
                     (assoc lean :blocking-warnings
                            (or (:blocking-warnings lean) 0)))))
          (conj :lean-baseline-mismatch)
          (not (true? (:clean-before? report))) (conj :workspace-not-clean-before)
          (not (true? (:clean-after? report))) (conj :workspace-not-clean-after)
          (seq (:mutations report)) (conj :preflight-mutations-observed)
          (and (:problem-revision report)
               (not= (:problem-revision request) (:problem-revision report)))
          (conj :problem-revision-mismatch)
          (and (:problem-blob report)
               (not= (:problem-blob request) (:problem-blob report)))
          (conj :problem-blob-mismatch))]
    (if (seq findings)
      {:ok false :error/code :preflight-terminal-invalid
       :findings findings :missing missing}
      {:ok true :report report})))

(defn receipt
  [contract request ticket job]
  (let [terminal (validate-terminal request ticket job)]
    (if-not (:ok terminal)
      terminal
      (let [report (:report terminal)
            body {:receipt/type :frame-preflight
                  :receipt/frame-id (:frame-id request)
                  :receipt/problem-id (:problem-id request)
                  :receipt/result :preflight-passed
                  :receipt/job-id (:job-id ticket)
                  :receipt/dispatch-id (:dispatch/id request)
                  :receipt/problem-revision (:problem-revision request)
                  :receipt/problem-blob (:problem-blob request)
                  :receipt/lean (:lean report)
                  :receipt/clean-before? (:clean-before? report)
                  :receipt/clean-after? (:clean-after? report)
                  :receipt/mutations (:mutations report)}
            addressed (assoc body :receipt/id (machine/ledger-digest [body]))
            checked (cycle/validate-receipt contract :preflight addressed)]
        (if (:ok checked)
          {:ok true :certificate addressed}
          checked)))))

(defn drive!
  "Advance preflight by at most one effect, persisting before returning.

   STATE is nil before dispatch or contains :request and :ticket afterwards.
   This makes retries/restarts reuse the recorded job identity."
  [{:keys [contract inputs state dispatch-fn activate-fn job-fn persist-fn]}]
  (cond
    (not (every? fn? [dispatch-fn activate-fn job-fn persist-fn]))
    {:ok false :error/code :preflight-effect-provider-missing}

    (nil? state)
    (let [built (build-request inputs)]
      (if-not (:ok built)
        built
        (let [request (:request built)
              dispatched (record-dispatch request (dispatch-fn request))]
          (if-not (:ok dispatched)
            dispatched
            (let [next-state {:state/type :preflight-dispatched
                              :request request :ticket (:ticket dispatched)}
                  persisted (persist-fn next-state)]
              (if-not (:ok persisted)
                {:ok false :error/code :preflight-ticket-persistence-failed}
                (let [activated (activate-fn request (:ticket dispatched))]
                  (if-not (:ok activated)
                    {:ok false :error/code :preflight-activation-failed
                     :state next-state :finding activated}
                    {:ok true :status :awaiting-terminal
                     :state next-state}))))))))

    (not= :preflight-dispatched (:state/type state))
    {:ok false :error/code :preflight-state-invalid}

    :else
    (let [job (job-fn (get-in state [:ticket :job-id]))]
      (if-not (= :done (:state job))
        {:ok true :status :awaiting-terminal :state state}
        (let [ingested (receipt contract (:request state) (:ticket state) job)]
          (if-not (:ok ingested)
            ingested
            (let [next-state (assoc state :state/type :preflight-certified
                                    :receipt (:certificate ingested))
                  persisted (persist-fn next-state)]
              (if-not (:ok persisted)
                {:ok false :error/code :preflight-receipt-persistence-failed}
                {:ok true :status :certified :state next-state
                 :certificate (:certificate ingested)}))))))))
