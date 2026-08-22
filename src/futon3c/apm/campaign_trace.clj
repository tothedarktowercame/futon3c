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
           terminal-ledger-digest]}]
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
    "terminalLedgerDigest" terminal-ledger-digest}))

(defn from-durable-state
  "Project only witnessed durable ledger/job facts into a checker trace."
  [{:keys [registration observations closed terminal-ledger-digest]}]
  (trace
   (merge registration
          {:closed closed :terminal-ledger-digest terminal-ledger-digest
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
