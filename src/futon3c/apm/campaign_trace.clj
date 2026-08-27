(ns futon3c.apm.campaign-trace
  "Canonical refinement trace exported for the Lean campaign checker."
  (:require [cheshire.core :as json]
            [clojure.edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [futon3c.apm.generated-contract :as generated-contract]))

(def default-contract-path
  "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v4.json")

(declare canonical sha256 combined-trace-digest)

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

(defn validate-authoritative-observation
  "Validate one producer record through its Lean-emitted schema and return the
  unchanged durable record. Producers call this before their atomic persist;
  the projected wire record is deliberately not a second authority."
  [kind record]
  (let [schema (some #(when (= (name kind) (:kind %)) %) (observation-schemas))]
    (when-not schema
      (throw (ex-info "Trace observation kind not declared by Lean"
                      {:error/code :campaign-trace-kind-undeclared
                       :observation/kind kind})))
    (project-operational-observations [schema] {kind [record]})
    record))

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

(defn valid-combined-trace-receipt?
  "Closure gate over the Lean-declared observation inventory. The receipt is
  accepted only when the checker accepted the same digest and the trace says
  its observations were projected from durable state."
  [certificate]
  (let [schemas (observation-schemas)
        declared (set (map :kind schemas))
        observed (set (:trace/observation-kinds certificate))
        trace-body (:trace/combined certificate)
        trace-digest (:trace/digest certificate)
        checker (:trace/checker-receipt certificate)]
    (and (map? trace-body)
         (string? trace-digest)
         (boolean (re-matches #"[0-9a-f]{64}" trace-digest))
         (= trace-digest (combined-trace-digest trace-body))
         (= declared observed)
         (= (count schemas) (count (:trace/observation-kinds certificate)))
         (true? (:trace/projected-from-durable-state? certificate))
         (= :accepted (:checker/status checker))
         (= trace-digest (:trace/digest checker)))))

(defn- durable-records [documents record-key]
  (letfn [(walk [value]
            (cond
              (map? value)
              (concat (when (contains? value record-key)
                        [(get value record-key)])
                      (mapcat walk (vals value)))
              (sequential? value) (mapcat walk value)
              :else []))]
    (vec (mapcat walk documents))))

(defn assemble-combined-operational-trace
  "Total schema-driven projection from explicitly supplied durable documents.
  Durable record keys and wire fields both come from Lean."
  [durable-documents]
  (let [schemas (observation-schemas)
        sources
        (into {}
              (map (fn [{:keys [kind durable-record-key]}]
                     [(keyword kind)
                      (durable-records durable-documents
                                       (keyword durable-record-key))]))
              schemas)
        complete (require-complete-operational-sources sources)]
    (canonical
     (merge {"schemaVersion" 1
             "traceKind" "apm-combined-operational"}
            (project-operational-observations schemas complete)))))

(defn- sha256 [text]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1
                                            (.digest digest (.getBytes text "UTF-8"))))))

(defn combined-trace-digest
  "Digest the exact canonical JSON representation carried by a close receipt."
  [trace-body]
  (sha256 (str (json/generate-string (canonical trace-body)) "\n")))

(defn issue-combined-trace-receipt!
  "Persist a deterministic combined trace, invoke Lean, and return a closure
  certificate carrying the digest-bound checker receipt. CHECKER-FN is a
  hermetic test seam and receives the trace path."
  [{:keys [certificate durable-documents trace-path checker-fn]
    :or {checker-fn
         (fn [path]
           (shell/sh "bash" "-lc"
                     (str "cd /home/joe/code/apm-lean && "
                          "lake env lean --run DarkTower/APMCampaignTraceChecker.lean "
                          "--operational " (pr-str (str path)))))}}]
  (try
    (let [assembled (assemble-combined-operational-trace durable-documents)
          payload (str (json/generate-string assembled) "\n")
          digest (combined-trace-digest assembled)
          target (io/file trace-path)
          parent (.getParentFile target)
          _ (.mkdirs parent)
          temporary (java.io.File/createTempFile ".combined-trace-" ".json" parent)]
      (try
        (spit temporary payload)
        (java.nio.file.Files/move
         (.toPath temporary) (.toPath target)
         (into-array java.nio.file.CopyOption
                     [java.nio.file.StandardCopyOption/ATOMIC_MOVE
                      java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
        (let [checked (checker-fn (.getCanonicalPath target))]
          (if (and (zero? (:exit checked))
                   (.contains (str (:out checked))
                              "APM-OPERATIONAL-TRACE-ACCEPTED"))
            {:ok true
             :trace assembled
             :certificate
             (assoc certificate
                    :trace/combined assembled
                    :trace/path (.getCanonicalPath target)
                    :trace/digest digest
                    :trace/projected-from-durable-state? true
                    :trace/observation-kinds
                    (mapv :kind (observation-schemas))
                    :trace/checker-receipt
                    {:checker/status :accepted :trace/digest digest})}
            {:ok false :error/code :combined-trace-checker-rejected
             :checker checked :trace/digest digest}))
        (finally
          (when (.exists temporary) (.delete temporary)))))
    (catch clojure.lang.ExceptionInfo e
      {:ok false
       :error/code (or (:error/code (ex-data e))
                       :combined-trace-assembly-failed)
       :finding (ex-data e)})))

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
