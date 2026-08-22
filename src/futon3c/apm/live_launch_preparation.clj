(ns futon3c.apm.live-launch-preparation
  "Fail-closed workspace and seat preparation before live countdown dispatch."
  (:require [futon3c.apm.campaign-machine :as machine]))

(def required-workspace-roles #{:solver :student})

(def required-seat-types
  {:solver :codex :student :zai :guide :claude :proctor :codex :scribe :zai
   :analyst :claude})

(def required-timeouts
  {:request-timeout-ms 300000 :turn-timeout-ms 3600000})

(declare validate)

(defn- prepare-workspace
  [result unit role leases workspace-exists? provision-fn validate-workspace-fn]
  (let [lease (get leases role)
        exists? (workspace-exists? unit role)]
    (cond
      (and exists? (nil? lease))
      (reduced {:ok false :error/code :existing-workspace-without-lease :role role})

      exists?
      (let [validation (validate-workspace-fn lease)]
        (if (:valid? validation)
          (assoc-in result [:workspaces role] {:lease lease :validation validation})
          (reduced {:ok false :error/code :existing-workspace-invalid
                    :role role :validation validation})))

      lease
      (reduced {:ok false :error/code :leased-workspace-missing :role role})

      :else
      (let [provisioned (provision-fn unit role)]
        (if-not (:ok provisioned)
          (reduced provisioned)
          (let [new-lease (:lease provisioned)
                validation (validate-workspace-fn new-lease)]
            (if (:valid? validation)
              (assoc-in result [:workspaces role]
                        {:lease new-lease :validation validation})
              (reduced {:ok false :error/code :new-workspace-invalid
                        :role role :validation validation}))))))))

(defn prepare!
  "Idempotently prepare one frame from pinned inputs and injected effects.

   A workspace which already exists may only be reused through its persisted
   content-addressed lease.  MINT-FN must itself implement deterministic seat
   minting; the returned roster is always revalidated below."
  [{:keys [unit ledger role-cards leases workspace-exists? provision-fn
           validate-workspace-fn mint-fn roster-fn]}]
  (let [frame-id (:frame/id unit)
        problem-id (:problem/id unit)
        bad-input? (or (not (and (string? frame-id)
                                 (re-matches #"f[0-9]+" frame-id)))
                       (not (string? problem-id))
                       (not (every? fn? [workspace-exists? provision-fn
                                         validate-workspace-fn mint-fn roster-fn])))
        prepared
        (when-not bad-input?
          (reduce (fn [result role]
                    (prepare-workspace result unit role leases workspace-exists?
                                       provision-fn validate-workspace-fn))
                  {:ok true :workspaces {}}
                  required-workspace-roles))]
    (cond
      bad-input?
      {:ok false :error/code :live-launch-input-invalid}

      (not (:ok prepared)) prepared

      :else
      (let [minted (mint-fn frame-id required-seat-types required-timeouts)]
        (if-not (:ok minted)
          {:ok false :error/code :seat-mint-failed :finding minted}
          (validate {:frame-id frame-id :problem-id problem-id :ledger ledger
                     :workspaces (:workspaces prepared) :seats (roster-fn frame-id)
                     :role-cards role-cards}))))))

(defn validate
  "Validate a preparation observation without trusting conversational state."
  [{:keys [frame-id problem-id ledger workspaces seats role-cards]}]
  (let [workspace-findings
        (mapcat (fn [role]
                  (let [{:keys [lease validation]} (get workspaces role)]
                    (cond-> []
                      (nil? lease) (conj {:finding :workspace-lease-missing :role role})
                      (not= frame-id (:frame/id lease))
                      (conj {:finding :workspace-frame-mismatch :role role})
                      (not= problem-id (:problem/id lease))
                      (conj {:finding :workspace-problem-mismatch :role role})
                      (not= role (:role lease))
                      (conj {:finding :workspace-role-mismatch :role role})
                      (not (true? (:valid? validation)))
                      (conj {:finding :workspace-validation-failed :role role
                             :details (:findings validation)}))))
                required-workspace-roles)
        seat-findings
        (mapcat (fn [[role expected-type]]
                  (let [seat (get seats role)
                        timeouts (:effective-timeouts seat)]
                    (cond-> []
                      (nil? seat) (conj {:finding :seat-missing :role role})
                      (not= (str frame-id "-" (name role)) (:agent-id seat))
                      (conj {:finding :seat-identity-mismatch :role role})
                      (not= expected-type (:type seat))
                      (conj {:finding :seat-type-mismatch :role role})
                      (not= frame-id (:frame-id seat))
                      (conj {:finding :seat-frame-mismatch :role role})
                      (not (true? (:invoke-ready? seat)))
                      (conj {:finding :seat-not-invoke-ready :role role})
                      (not= (:turn-timeout-ms required-timeouts)
                            (:turn-timeout-ms timeouts))
                      (conj {:finding :seat-turn-timeout-mismatch :role role})
                      (and (= :zai expected-type)
                           (not= (:request-timeout-ms required-timeouts)
                                 (:request-timeout-ms timeouts)))
                      (conj {:finding :seat-request-timeout-mismatch :role role}))))
                required-seat-types)
        card-findings
        (mapcat (fn [role]
                  (let [card (get role-cards role)]
                    (cond-> []
                      (not (and (string? (:path card)) (string? (:blob card))))
                      (conj {:finding :role-card-pin-missing :role role}))))
                (keys required-seat-types))
        ledger-findings
        (cond-> []
          (not= 5 (:version ledger)) (conj {:finding :ledger-version-mismatch})
          (not (and (string? (:digest ledger))
                    (re-matches #"[0-9a-f]{64}" (:digest ledger))))
          (conj {:finding :ledger-digest-invalid})
          (not= :preflight (:phase ledger)) (conj {:finding :ledger-phase-mismatch})
          (some? (:claim ledger)) (conj {:finding :ledger-claim-present}))
        findings (vec (concat ledger-findings workspace-findings
                              seat-findings card-findings))]
    (if (seq findings)
      {:ok false :error/code :live-launch-preparation-invalid
       :findings findings}
      (let [body {:receipt/type :live-launch-preparation
                  :frame/id frame-id :problem/id problem-id
                  :ledger ledger
                  :workspace/ids (into {} (map (fn [[role {:keys [lease]}]]
                                                 [role (:workspace/id lease)]))
                                             workspaces)
                  :seat/ids (into {} (map (fn [[role seat]]
                                            [role (:agent-id seat)])) seats)
                  :role-card/blobs (into {} (map (fn [[role card]]
                                                   [role (:blob card)])) role-cards)}]
        {:ok true
         :receipt (assoc body :receipt/id (machine/ledger-digest [body]))}))))
