(ns futon3c.apm.campaign-trace
  "Canonical refinement trace exported for the Lean campaign checker."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]))

(defn- canonical [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [(name k) (canonical v)])) x)
    (set? x) (mapv canonical (sort x))
    (sequential? x) (mapv canonical x)
    (keyword? x) (name x)
    :else x))

(defn trace
  [{:keys [campaign-id manifest-hash contract-id phase-order steps closed
           terminal-ledger-digest solver-snapshot-digest
           snapshot-admitted-after-solve-verify snapshot-depositor
           snapshot-reviewer student-bindings campaign-lanes
           phase-receipt-ids problem-outcome frame-result analyst-wakes]}]
  (canonical
   {"schemaVersion" 1
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
                                timeout-treated-as-success]}]
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
                      "timeoutTreatedAsSuccess" timeout-treated-as-success})
                   steps)
    "closed" closed
    "terminalLedgerDigest" terminal-ledger-digest
    "solverSnapshotDigest" solver-snapshot-digest
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
          analyst-wakes)}))

(defn from-durable-state
  "Project only witnessed durable ledger/job facts into a checker trace."
  [{:keys [registration observations closed terminal-ledger-digest
           memory campaign-lanes frame analyst-wakes]}]
  (trace
   (merge registration
          {:closed closed :terminal-ledger-digest terminal-ledger-digest
           :solver-snapshot-digest (:snapshot-digest memory)
           :snapshot-admitted-after-solve-verify (:admitted? memory)
           :snapshot-depositor (:depositor memory)
           :snapshot-reviewer (:reviewer memory)
           :student-bindings (:student-bindings memory)
           :campaign-lanes campaign-lanes
           :phase-receipt-ids (mapv #(get-in % [:receipt :id]) observations)
           :problem-outcome (:problem-outcome frame)
           :frame-result (:frame-result frame)
           :analyst-wakes analyst-wakes
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
               :timeout-treated-as-success (:timeout-treated-as-success? job)})
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
