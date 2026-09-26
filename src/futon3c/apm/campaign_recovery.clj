(ns futon3c.apm.campaign-recovery
  "Evidence-gated recovery for durably claimed campaign obligations."
  (:require [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]))

(def outcomes #{:not-started :completed :unknown})

(defn- verified-certificate? [certificate]
  (= (:certificate/id certificate)
     (machine/ledger-digest [(dissoc certificate :certificate/id)])))

(defn- make-event [certificate actor at type body]
  (let [event {:event/seq (:ledger/event-count certificate)
               :event/type type :event/campaign-id (:campaign/id certificate)
               :event/actor actor :event/at at
               :event/expected-version (:campaign/version certificate)
               :event/body body}]
    (assoc event :event/id (machine/ledger-digest [event]))))

(defn recover!
  "Apply an independent ASSESSMENT to CURRENT-CERTIFICATE's active claim.

  ASSESSMENT is {:outcome :not-started|:completed|:unknown :evidence map
  :effect-certificate map?}. Unknown never writes."
  [{:keys [ledger-path current-certificate assessment assessor at]}]
  (let [loaded (ledger/read-ledger ledger-path)
        projection (:projection loaded)
        claim (:active/claim current-certificate)
        outcome (:outcome assessment)]
    (cond
      (not (and (:ok loaded) (= :valid (:projection/status projection))))
      {:ok false :error/code :campaign-recovery-ledger-invalid :ledger loaded}

      (not (and (map? current-certificate)
                (verified-certificate? current-certificate)
                (= :valid (:snapshot/status current-certificate))))
      {:ok false :error/code :campaign-recovery-certificate-invalid}

      (or (not= (:campaign/version current-certificate)
                (:campaign/version projection))
          (not= (:ledger/digest current-certificate)
                (:ledger/digest projection)))
      {:ok false :error/code :campaign-recovery-certificate-stale}

      (or (nil? claim) (not= claim (:active/claim projection)))
      {:ok false :error/code :campaign-recovery-claim-mismatch}

      (not (contains? outcomes outcome))
      {:ok false :error/code :campaign-recovery-outcome-invalid}

      (not (map? (:evidence assessment)))
      {:ok false :error/code :campaign-recovery-evidence-required}

      (or (not (string? assessor)) (empty? assessor)
          (not (string? at)) (empty? at))
      {:ok false :error/code :campaign-recovery-identity-time-required}

      (= assessor (:actor claim))
      {:ok false :error/code :campaign-recovery-assessor-not-independent}

      (= :unknown outcome)
      {:ok false :error/code :campaign-recovery-outcome-unknown
       :claim-retained? true :obligation/id (:obligation/id claim)
       :evidence (:evidence assessment)}

      (= :not-started outcome)
      (let [event (make-event
                   current-certificate assessor at :obligation/released
                   {:obligation/id (:obligation/id claim)
                    :outcome :not-started :evidence (:evidence assessment)})
            appended (ledger/compare-and-append!
                      ledger-path (:campaign/version current-certificate)
                      (:ledger/digest current-certificate) event)]
        (if (:ok appended)
          {:ok true :released? true :receipt appended
           :obligation/id (:obligation/id claim)}
          {:ok false :error/code :campaign-recovery-release-refused
           :release appended}))

      :else
      (if-not (map? (:effect-certificate assessment))
        {:ok false :error/code :campaign-recovery-effect-certificate-required
         :claim-retained? true}
        (let [obligation (:obligation claim)
              completion (get-in obligation [:obligation/action :completion])
              body (assoc (:event/body completion)
                          :obligation/id (:obligation/id claim)
                          :certificate (:effect-certificate assessment)
                          :recovery? true
                          :recovery/evidence (:evidence assessment))
              event (make-event current-certificate assessor at
                                (:event/type completion) body)
              appended (ledger/compare-and-append!
                        ledger-path (:campaign/version current-certificate)
                        (:ledger/digest current-certificate) event)]
          (if (:ok appended)
            {:ok true :recovered-completion? true :receipt appended
             :obligation/id (:obligation/id claim)
             :effect-certificate (:effect-certificate assessment)}
            {:ok false :error/code :campaign-recovery-completion-refused
             :claim-retained? true :completion appended}))))))
