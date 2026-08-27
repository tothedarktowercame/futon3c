(ns futon3c.apm.campaign-trace
  "Canonical refinement trace exported for the Lean campaign checker."
  (:require [cheshire.core :as json]
            [clojure.edn]
            [clojure.java.io :as io]
            [futon3c.apm.generated-contract :as generated-contract]))

(def default-contract-path
  "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v4.json")

(defn observation-schemas
  ([] (observation-schemas default-contract-path))
  ([path]
   (let [loaded (generated-contract/read-contract path)
         validated (when (:ok loaded)
                     (generated-contract/validate (:contract loaded)))]
     (when-not (:ok validated)
       (throw (ex-info "Lean-generated trace observation schema unavailable"
                       (or validated loaded))))
     (get-in validated [:contract :trace-observation-schemas]))))

(defn project-operational-observations
  "Project every Lean-declared observation collection from durable decision
  records. No Clojure observation-kind or field registry exists: absence of a
  declared source collection is an error, including when a new Lean kind is
  added before its producer is wired."
  ([sources] (project-operational-observations (observation-schemas) sources))
  ([schemas sources]
   (into {}
         (map (fn [{:keys [kind collection-field fields]}]
                (let [source-key (keyword kind)]
                  (when-not (contains? sources source-key)
                    (throw (ex-info "Lean-declared trace producer missing"
                                    {:error/code :campaign-trace-producer-missing
                                     :observation/kind source-key})))
                  [collection-field
                   (mapv (fn [record]
                           (into {}
                                 (map (fn [{:keys [wire-name source-path]}]
                                        (let [path (mapv keyword source-path)
                                              marker (Object.)
                                              value (get-in record path marker)]
                                          (when (identical? marker value)
                                            (throw
                                             (ex-info "Durable observation field missing"
                                                      {:error/code :campaign-trace-field-missing
                                                       :observation/kind source-key
                                                       :source-path path})))
                                          [wire-name value])))
                                 fields))
                         (get sources source-key))])))
         schemas)))

(defn operational-sources-from-durable
  "Collect observation records already embedded by the deciding durable
  writers. This function never derives a verdict or reconstructs a missing
  observation from adjacent fields."
  [{:keys [watchdog-states successor-states delivery-ledgers]}]
  {:progress (into [] (keep :watchdog/trace-observation) watchdog-states)
   :successor (into []
                    (comp (mapcat #(or (:superseded-terminals %) []))
                          (keep :trace/successor-observation))
                    successor-states)
   :delivery (into []
                   (comp (mapcat #(vals (or (:jobs %) {})))
                         (keep :trace/delivery-observation))
                   delivery-ledgers)})

(defn require-complete-operational-sources [sources]
  (doseq [{:keys [kind]} (observation-schemas)]
    (let [source-key (keyword kind)]
      (when-not (seq (get sources source-key))
        (throw (ex-info "Lean-declared trace observation absent"
                        {:error/code :campaign-trace-observation-absent
                         :observation/kind source-key})))))
  sources)

(defn- canonical [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [(name k) (canonical v)])) x)
    (set? x) (mapv canonical (sort x))
    (sequential? x) (mapv canonical x)
    (keyword? x) (name x)
    :else x))

(def review-pass-phases
  [[:promote-solver 0] [:guide-intervention-1 1]
   [:guide-intervention-2 2] [:scribe-reduce 0]])

(defn review-passes-from-live
  "Read completed review verdicts in protocol order from a frame's durable
  live receipts. Missing phase files and missing verdict vectors are errors;
  replay must not manufacture a resolved pass."
  [frame-directory]
  (mapv
   (fn [[phase ordinal]]
     (let [path (io/file frame-directory "live" (str (name phase) ".edn"))
           state (clojure.edn/read-string (slurp path))
           reviews (get-in state [:receipt :receipt/promotion-reviews])]
       (when-not (vector? reviews)
         (throw (ex-info "Durable review pass missing"
                         {:phase phase :path (.getCanonicalPath path)})))
       {:phase phase :ordinal ordinal :verdicts (mapv :verdict reviews)}))
   review-pass-phases))

(defn trace
  [{:keys [campaign-id manifest-hash contract-id phase-order steps closed
           terminal-ledger-digest solver-snapshot-digest
           solver-snapshot-content-digest review-snapshots review-passes
           snapshot-admitted-after-solve-verify snapshot-depositor
           snapshot-reviewer student-bindings campaign-lanes
           phase-receipt-ids problem-outcome frame-result void-classification
           analyst-wakes operational-observations]}]
  (let [operational (project-operational-observations operational-observations)]
  (canonical
   (merge {"schemaVersion" 2
    "campaignId" campaign-id
    "manifestHash" manifest-hash
    "contractId" contract-id
    "phaseOrder" phase-order
    "steps" (mapv (fn [{:keys [from to ledger-before ledger-after
                                receipt-id prior-receipt-id job-id
                                activated-job-id activation-status
                                reactivated-job-id terminal-job-id
                                command-own-exit claim-persisted
                                receipt-persisted resumed-job-id
                                client-timeout-observed
                                timeout-treated-as-success
                                submission-registered submission-persisted
                                submission-schema-valid
                                submission-authority-derived
                                conversation-used-as-receipt
                                submission-job-id]}]
                     {"fromPhase" from "toPhase" to
                      "ledgerBefore" ledger-before "ledgerAfter" ledger-after
                      "receiptId" receipt-id
                      "priorReceiptId" prior-receipt-id
                      "jobId" job-id "activatedJobId" activated-job-id
                      "activationStatus" activation-status
                      "reactivatedJobId" reactivated-job-id
                      "terminalJobId" terminal-job-id
                      "commandOwnExit" command-own-exit
                      "claimPersisted" claim-persisted
                      "receiptPersisted" receipt-persisted
                      "resumedJobId" resumed-job-id
                      "clientTimeoutObserved" client-timeout-observed
                      "timeoutTreatedAsSuccess" timeout-treated-as-success
                      "submissionRegistered" submission-registered
                      "submissionPersisted" submission-persisted
                      "submissionSchemaValid" submission-schema-valid
                      "submissionAuthorityDerived" submission-authority-derived
                      "conversationUsedAsReceipt" conversation-used-as-receipt
                      "submissionJobId" submission-job-id})
                   steps)
    "closed" closed
    "terminalLedgerDigest" terminal-ledger-digest
    "solverSnapshotDigest" solver-snapshot-digest
    "solverSnapshotContentDigest" solver-snapshot-content-digest
    "reviewSnapshots"
    (mapv (fn [{:keys [ordinal snapshot-digest content-digest]}]
            {"ordinal" ordinal "snapshotDigest" snapshot-digest
             "contentDigest" content-digest})
          review-snapshots)
    "reviewPasses"
    (mapv (fn [{:keys [phase ordinal verdicts]}]
            {"phase" phase "ordinal" ordinal
             "verdicts" (mapv name verdicts)})
          review-passes)
    "snapshotAdmittedAfterSolveVerify" snapshot-admitted-after-solve-verify
    "snapshotDepositor" snapshot-depositor
    "snapshotReviewer" snapshot-reviewer
    "studentBindings" (mapv (fn [{:keys [ordinal session-id snapshot-digest]}]
                               {"ordinal" ordinal "sessionId" session-id
                                "snapshotDigest" snapshot-digest})
                             student-bindings)
    "campaignLanes"
    (mapv (fn [{:keys [campaign-id regulator-id problem-buffer
                       continuation-session analyst-session ledger-digest
                       projection-ledger-digest]}]
            {"campaignId" campaign-id "regulatorId" regulator-id
             "problemBuffer" problem-buffer
             "continuationSession" continuation-session
             "analystSession" analyst-session "ledgerDigest" ledger-digest
             "projectionLedgerDigest" projection-ledger-digest})
          campaign-lanes)
    "phaseReceiptIds" phase-receipt-ids
    "problemOutcome" problem-outcome
    "frameResult" frame-result
    "voidClassification" void-classification
    "analystWakes"
    (mapv (fn [{:keys [frame-id terminal ordinal series-input-version
                       append-only proposal-type proposal-digest
                       successor-handoff mutates-in-flight]}]
            {"frameId" frame-id "terminal" terminal "ordinal" ordinal
             "seriesInputVersion" series-input-version
             "appendOnly" append-only "proposalType" proposal-type
             "proposalDigest" proposal-digest
             "successorHandoff" successor-handoff
             "mutatesInFlight" mutates-in-flight})
          analyst-wakes)} operational))))

(defn from-durable-state
  "Project only witnessed durable ledger/job facts into a checker trace."
  [{:keys [registration observations closed terminal-ledger-digest
           memory campaign-lanes frame analyst-wakes operational-observations
           operational-authorities]}]
  (trace
   (merge registration
          {:closed closed :terminal-ledger-digest terminal-ledger-digest
           :solver-snapshot-digest (:snapshot-digest memory)
           :solver-snapshot-content-digest (:snapshot-content-digest memory)
           :review-snapshots (:review-snapshots memory)
           :review-passes (:review-passes memory)
           :snapshot-admitted-after-solve-verify (:admitted? memory)
           :snapshot-depositor (:depositor memory)
           :snapshot-reviewer (:reviewer memory)
           :student-bindings (:student-bindings memory)
           :campaign-lanes campaign-lanes
           :phase-receipt-ids (mapv #(get-in % [:receipt :id]) observations)
           :problem-outcome (:problem-outcome frame)
           :frame-result (:frame-result frame)
           :void-classification (:void-classification frame)
           :analyst-wakes analyst-wakes
           :operational-observations
           (or operational-observations
               (some-> operational-authorities
                       operational-sources-from-durable
                       require-complete-operational-sources))
           :steps
           (mapv
            (fn [{:keys [from to ledger-before ledger-after claim job receipt]}]
              {:from from :to to :ledger-before ledger-before
               :ledger-after ledger-after
               :receipt-id (:id receipt)
               :prior-receipt-id (:prior-id receipt)
               :job-id (:announced-id job)
               :activated-job-id (:activated-id job)
               :activation-status (:activation-status job)
               :reactivated-job-id (:reactivated-id job)
               :terminal-job-id (:terminal-id job)
               :command-own-exit (:command-own-exit job)
               :claim-persisted (:persisted? claim)
               :receipt-persisted (:persisted? receipt)
               :resumed-job-id (:resumed-id job)
               :client-timeout-observed (:client-timeout-observed? job)
               :timeout-treated-as-success (:timeout-treated-as-success? job)
               :submission-registered (get-in job [:submission :registered?])
               :submission-persisted (get-in job [:submission :persisted?])
               :submission-schema-valid (get-in job [:submission :schema-valid?])
               :submission-authority-derived
               (get-in job [:submission :authority-derived?])
               :conversation-used-as-receipt
               (get-in job [:submission :conversation-used-as-receipt?])
               :submission-job-id (get-in job [:submission :job-id])})
            observations)})))

(defn emit!
  "Atomically write a deterministic JSON trace. The Lean checker is the
   acceptance authority; this function intentionally performs no shadow
   protocol validation."
  [path input]
  (let [target (io/file path)
        parent (.getParentFile target)
        temp (java.io.File/createTempFile ".apm-trace-" ".json" parent)
        payload (str (json/generate-string (trace input)) "\n")]
    (try
      (spit temp payload)
      (java.nio.file.Files/move
       (.toPath temp) (.toPath target)
       (into-array java.nio.file.CopyOption
                   [java.nio.file.StandardCopyOption/ATOMIC_MOVE
                    java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
      {:ok true :path (.getCanonicalPath target) :bytes (count payload)}
      (finally
        (when (.exists temp) (.delete temp))))))
