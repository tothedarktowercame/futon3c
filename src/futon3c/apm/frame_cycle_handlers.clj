(ns futon3c.apm.frame-cycle-handlers
  "Fail-closed campaign handlers backed by the complete frame-cycle contract.

  Receipt providers are effects supplied at the boundary. Validation here is
  pure: a handler cannot certify a phase unless its typed receipt and every
  required prior receipt agree with the registered frame and problem."
  (:require [futon3c.apm.frame-cycle-contract :as contract]))

(def executable-kinds
  #{:student-attempt :guide-intervention :scribe-reduce :close-frame})

(defn- phases-before [cycle-contract phase]
  (take-while #(not= phase %) (:phase-order cycle-contract)))

(defn- producer-phase [cycle-contract artifact]
  (some (fn [phase]
          (when (contains? (get-in cycle-contract [:phases phase :produces])
                           artifact)
            phase))
        (:phase-order cycle-contract)))

(defn- validate-prior-receipts [cycle-contract phase receipts]
  (reduce
   (fn [result prior-phase]
     (if-not (:ok result)
       (reduced result)
       (if-let [receipt (get receipts prior-phase)]
         (let [validated (contract/validate-receipt cycle-contract prior-phase
                                                    receipt)]
           (if (:ok validated)
             result
             (reduced {:ok false
                       :error/code :frame-cycle-prior-receipt-invalid
                       :finding {:phase prior-phase :validation validated}})))
         result)))
   {:ok true}
   (phases-before cycle-contract phase)))

(defn latest-snapshot-receipt
  "The snapshot a Student attempt at ATTEMPT-ORDINAL binds to: the most recent
  Guide intervention that published a reviewed union snapshot, else the
  Solver promotion. Binding stays exact and content-addressed; what moves is
  which reviewed snapshot is current."
  [receipts attempt-ordinal]
  (or (->> (range (dec attempt-ordinal) 0 -1)
           (map #(get receipts (keyword (str "guide-intervention-" %))))
           (filter #(string? (:receipt/snapshot-digest %)))
           first)
      (get receipts :promote-solver)))

(defn snapshot-binding [receipt]
  (when receipt
    {:receipt-id (:receipt/id receipt)
     :snapshot-id (:receipt/snapshot-id receipt)
     :snapshot-digest (:receipt/snapshot-digest receipt)}))

(defn- dependency-evidence [cycle-contract phase receipts]
  (let [required (get-in cycle-contract [:phases phase :requires])
        producers (into {} (map (fn [artifact]
                                  [artifact (producer-phase cycle-contract artifact)]))
                        required)
        missing (into #{} (keep (fn [[artifact producer]]
                                  (when-not (get receipts producer) artifact)))
                      producers)
        input-ids (into #{} (keep (fn [[_ producer]]
                                    (get-in receipts [producer :receipt/id])))
                        producers)]
    {:missing missing :input-ids input-ids}))

(defn- semantic-error [cycle-contract phase action receipt prior-receipts]
  (let [spec (get-in cycle-contract [:phases phase])
        ordinal (:ordinal spec)
        dependency (dependency-evidence cycle-contract phase prior-receipts)
        declared-inputs (some-> (:receipt/input-receipt-ids receipt) set)
        missing-observation? (= :student-observation-missing (:receipt/type receipt))
        student-sessions
        (->> prior-receipts
             (keep (fn [[prior-phase prior-receipt]]
                     (when (= :student-attempt
                              (get-in cycle-contract [:phases prior-phase :kind]))
                       (:receipt/fresh-session-id prior-receipt))))
             vec)]
    (cond
      (not= phase (:phase action))
      {:error/code :frame-cycle-action-phase-mismatch}

      (not= (:kind spec) (:kind action))
      {:error/code :frame-cycle-action-kind-mismatch}

      (not= (:role spec) (:role action))
      {:error/code :frame-cycle-action-role-mismatch}

      (not= (:frame-id action) (:receipt/frame-id receipt))
      {:error/code :frame-cycle-receipt-frame-mismatch}

      (not= (:problem-id action) (:receipt/problem-id receipt))
      {:error/code :frame-cycle-receipt-problem-mismatch}

      (and (= :student-attempt (:kind spec))
           (not= ordinal (:receipt/attempt-ordinal receipt)))
      {:error/code :frame-cycle-student-ordinal-mismatch}

      (and missing-observation?
           (not= {:author :controller :reason :typed-submission-missing}
                 {:author (:receipt/author receipt)
                  :reason (:receipt/reason receipt)}))
      {:error/code :frame-cycle-missing-observation-authority-invalid}

      (and missing-observation?
           (not (and (string? (:receipt/job-id receipt))
                     (pos-int? (:receipt/repair-attempts receipt))
                     (map? (:receipt/harness-observed receipt)))))
      {:error/code :frame-cycle-missing-observation-evidence-invalid}

      (and (= :student-attempt (:kind spec))
           (contains? (:requires spec) :solver-memory-snapshot)
           (not= (snapshot-binding
                  (latest-snapshot-receipt prior-receipts ordinal))
                 (:receipt/memory-snapshot receipt)))
      {:error/code :frame-cycle-student-memory-snapshot-mismatch}

      (and (= :guide-intervention (:kind spec))
           (string? (:receipt/snapshot-digest receipt))
           (not (and (string? (:receipt/snapshot-id receipt))
                     (string? (:receipt/snapshot-path receipt))
                     (vector? (:receipt/reviewed-memory-ids receipt))
                     (vector? (:receipt/promotion-reviews receipt))
                     (true? (:receipt/independent-review? receipt)))))
      {:error/code :frame-cycle-guide-snapshot-evidence-invalid}

      (and (= :guide-intervention (:kind spec))
           (not= ordinal (:receipt/intervention-ordinal receipt)))
      {:error/code :frame-cycle-guide-ordinal-mismatch}

      (and (= :guide-intervention (:kind spec))
           (not (contains? #{:store-mode :harness-mode}
                           (:receipt/mode receipt))))
      {:error/code :frame-cycle-guide-mode-invalid}

      (and (= :guide-intervention (:kind spec))
           (not= false (get-in receipt
                               [:receipt/channel-audit
                                :direct-student-contact?])))
      {:error/code :frame-cycle-guide-direct-channel-not-refuted}

      (seq (:missing dependency))
      {:error/code :frame-cycle-required-receipts-missing
       :finding {:missing (:missing dependency)}}

      (and declared-inputs (not= declared-inputs (:input-ids dependency)))
      {:error/code :frame-cycle-input-receipt-set-mismatch
       :finding {:expected (:input-ids dependency)
                 :actual declared-inputs}}

      (and (= :close-frame (:kind spec))
           (= :closed (:receipt/result receipt))
           (not= (count student-sessions)
                 (count (distinct student-sessions))))
      {:error/code :frame-cycle-student-sessions-not-distinct}

      :else nil)))

(defn validate-completion
  [cycle-contract action receipt prior-receipts]
  (let [phase (:phase action)
        contract-check (contract/validate-contract cycle-contract)
        receipt-check (when (:ok contract-check)
                        (contract/validate-receipt cycle-contract phase receipt))
        prior-check (when (:ok receipt-check)
                      (validate-prior-receipts cycle-contract phase
                                               prior-receipts))]
    (cond
      (not (:ok contract-check)) contract-check
      (not (:ok receipt-check)) receipt-check
      (not (:ok prior-check)) prior-check
      :else
      (if-let [error (semantic-error cycle-contract phase action receipt
                                     prior-receipts)]
        (assoc error :ok false)
        {:ok true :certificate receipt}))))

(defn make-handlers
  "Build handlers for the composite-role portion of a complete frame.

  RECEIPT-PROVIDER receives PHASE and ACTION. PRIOR-RECEIPTS-PROVIDER receives
  ACTION and returns phase->receipt. Missing providers and provider exceptions
  are explicit handler failures."
  [{:keys [cycle-contract receipt-provider prior-receipts-provider]}]
  (if-not (and (map? cycle-contract)
               (fn? receipt-provider)
               (fn? prior-receipts-provider))
    {:ok false :error/code :frame-cycle-handler-provider-required}
    {:ok true
     :handlers
     (into {}
           (map (fn [kind]
                  [kind
                   (fn [action]
                     (try
                       (let [phase (:phase action)
                             receipt (receipt-provider phase action)
                             prior (prior-receipts-provider action)]
                         (if-not (and (map? receipt) (map? prior))
                           {:ok false
                            :error/code :frame-cycle-handler-evidence-unavailable}
                           (validate-completion cycle-contract action receipt
                                                prior)))
                       (catch Throwable t
                         {:ok false :error/code :frame-cycle-handler-provider-failed
                          :finding {:message (.getMessage t)}})))])
                executable-kinds))}))
