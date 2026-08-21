(ns futon3c.apm.live-launch-preparation
  "Fail-closed workspace and seat preparation before live countdown dispatch."
  (:require [futon3c.apm.campaign-machine :as machine]))

(def required-workspace-roles #{:solver :student})

(def required-seat-types
  {:solver :codex :student :zai :guide :claude :proctor :codex :scribe :zai})

(def required-timeouts
  {:request-timeout-ms 300000 :turn-timeout-ms 3600000})

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
          (not= "ed49e674ccabb666f32faac12bb2eb0a69daaa091e551692be550267f1ca98b7"
                (:digest ledger)) (conj {:finding :ledger-digest-mismatch})
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
