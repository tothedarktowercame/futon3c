(ns futon3c.apm.fault-taxonomy
  "Classify failed APM transitions by the authority that may dispose them.")

(def integrity-codes
  "Findings that say durable evidence is corrupt or describes an impossible
   world. These stop the campaign; every other failure is confined to its
   frame and preserves the original result in a park record."
  #{:invalid-state
    :live-job-state-invalid
    :impossible-transition
    :failed-launch-audit
    :live-supervisor-launch-audit-failed
    :campaign-ledger-corrupt
    :campaign-ledger-digest-mismatch
    :registration-digest-mismatch
    :frame-void-digest-mismatch
    :statement-digest-mismatch
    :terminal-collection-authority-digest-mismatch
    :promotion-review-candidate-digest-mismatch
    :snapshot-expected-digest-mismatch
    :problem-projection-ledger-digest-mismatch
    :shelf-digest-mismatch
    :durable-reference-corrupt
    :preparation-content-address-invalid
    :countdown-manifest-content-address-invalid
    :generated-contract-digest-invalid
    :qualification-digest-invalid
    :ledger-digest-invalid})

(defn- finding-keywords [value]
  (cond
    (keyword? value) [value]
    (map? value) (mapcat finding-keywords
                         (keep #(get value %)
                               [:error/code :finding :findings
                                :validation/findings]))
    (coll? value) (mapcat finding-keywords value)
    :else []))

(defn- declared-fault-codes [value]
  ;; Results often contain sets such as :required or :known-statuses. Those
  ;; are vocabulary, not observations, and must not turn a frame fault into a
  ;; campaign stop merely because an integrity code is mentioned there.
  (if (map? value)
    (concat
     (when (keyword? (:error/code value)) [(:error/code value)])
     (mapcat finding-keywords
             (keep #(get value %)
                   [:finding :findings :validation/findings])))
    []))

(defn classify
  "Return the disposition for a failed tick result. Unknown codes park the
   frame and remain verbatim in :fault/result."
  [result]
  (if-let [integrity-code (some integrity-codes
                                (declared-fault-codes result))]
    {:fault/class :integrity
     :fault/disposition :campaign-stop
     :fault/code integrity-code
     :fault/result result}
    {:fault/class :frame
     :fault/disposition :frame-park
     :fault/code (or (:error/code result) :unrecognised-tick-failure)
     :fault/result result}))
