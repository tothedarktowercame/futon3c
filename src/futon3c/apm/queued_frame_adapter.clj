(ns futon3c.apm.queued-frame-adapter
  "Ordering boundary for a just-in-time one-off frame.

  Registration/open establishes the authoritative :preflight ledger state;
  only then may workspaces and seats be provisioned and certified."
  (:require [futon3c.apm.campaign-machine :as machine]))

(defn mint
  [{:keys [problem ordinal queue/id frame-number-base campaign-prefix]}]
  (let [frame-id (str "f" (+ (or frame-number-base 1) ordinal))
        campaign-id (str (or campaign-prefix "apm-queued") "-" frame-id)
        body {:frame/id frame-id :problem/id (:problem/id problem)
              :problem problem :campaign/id campaign-id :queue/id id
              :ordinal ordinal}]
    {:ok true :frame (assoc body :frame/mint-id
                            (machine/ledger-digest [body]))}))

(defn valid-mint? [frame]
  (and (map? frame)
       (= (:frame/mint-id frame)
          (machine/ledger-digest [(dissoc frame :frame/mint-id)]))))

(defn qualify
  [{:keys [frame generated-contract-digest qualification-digest]}]
  (let [findings (cond-> []
                   (not (valid-mint? frame)) (conj :frame-mint-invalid)
                   (not (and (string? generated-contract-digest)
                             (re-matches #"[0-9a-f]{64}"
                                         generated-contract-digest)))
                   (conj :generated-contract-digest-invalid)
                   (not (and (string? qualification-digest)
                             (re-matches #"[0-9a-f]{64}"
                                         qualification-digest)))
                   (conj :qualification-digest-invalid))]
    (if (seq findings)
      {:ok false :error/code :queued-frame-qualification-invalid
       :findings findings}
      {:ok true :frame frame})))

(defn open-and-prepare!
  "Open the one-off ledger, observe exact preflight authority, then provision.

  OPEN-FRAME-FN owns registration/open effects. PREPARE-FRAME-FN owns
  workspace/seat effects and must return a content-addressed preparation."
  [{:keys [frame open-frame-fn preparation-observation-fn prepare-frame-fn
           persist-preparation-fn]}]
  (cond
    (not (valid-mint? frame))
    {:ok false :error/code :queued-frame-mint-invalid}
    (not (every? fn? [open-frame-fn preparation-observation-fn
                      prepare-frame-fn persist-preparation-fn]))
    {:ok false :error/code :queued-frame-provider-missing}
    :else
    (let [opened (open-frame-fn frame)]
      (if-not (:ok opened)
        opened
        (let [ledger (preparation-observation-fn frame)
              authoritative? (and (:ok ledger) (= 5 (:version ledger))
                                  (= :preflight (:phase ledger))
                                  (nil? (:claim ledger))
                                  (= (:frame/id frame) (:frame-id ledger))
                                  (= (:problem/id frame) (:problem-id ledger)))]
          (if-not authoritative?
            {:ok false :error/code :queued-frame-preparation-authority-invalid
             :finding ledger}
            (let [prepared (prepare-frame-fn frame ledger)]
              (if-not (:ok prepared)
                prepared
                (let [receipt (:preparation prepared)
                      addressed? (= (:preparation/id receipt)
                                    (machine/ledger-digest
                                     [(dissoc receipt :preparation/id)]))]
                  (if-not addressed?
                    {:ok false :error/code
                     :queued-frame-preparation-content-invalid}
                    (let [persisted (persist-preparation-fn frame receipt)]
                      (if (:ok persisted)
                        {:ok true :preparation/id (:preparation/id receipt)
                         :frame frame :ledger ledger}
                        {:ok false :error/code
                         :queued-frame-preparation-persistence-failed}))))))))))))
