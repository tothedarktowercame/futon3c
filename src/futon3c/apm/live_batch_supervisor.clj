(ns futon3c.apm.live-batch-supervisor
  "Fail-closed chaining over individually supervised live frames.

   The campaign ledger is the cursor. A content-addressed campaign batch permit
   is the authority. This layer never infers success from a park or timeout."
  (:require [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.phase-status :as phase-status]))

(defn contiguous-range
  "Resolve inclusive START..END from ordered manifest units."
  [units start end]
  (let [ids (mapv :frame/id units)
        start-index (.indexOf ids start)
        end-index (.indexOf ids end)]
    (if (and (nat-int? start-index) (nat-int? end-index)
             (<= start-index end-index))
      {:ok true :frames (subvec ids start-index (inc end-index))}
      {:ok false :error/code :live-batch-range-invalid
       :finding {:start start :end end :manifest-frames ids}})))

(defn cursor
  [{:keys [frames permit certificate]}]
  (let [body {:cursor/type :live-countdown-batch :cursor/version 1
              :frames frames :permit/id (:permit/id permit)
              :campaign/id (:campaign/id certificate)
              :campaign/version (:campaign/version certificate)
              :ledger/digest (:ledger/digest certificate)
              :action-index (get (:campaign/permit-usage certificate)
                                 (:permit/id permit) 0)}]
    (assoc body :cursor/id (machine/ledger-digest [body]))))

(defn valid-cursor? [value]
  (and (map? value)
       (= :live-countdown-batch (:cursor/type value))
       (= 1 (:cursor/version value))
       (= (:cursor/id value)
          (machine/ledger-digest [(dissoc value :cursor/id)]))))

(defn- addressed [body]
  (assoc (dissoc body :cursor/id) :cursor/id
         (machine/ledger-digest [(dissoc body :cursor/id)])))

(defn tick!
  "Execute at most one underlying frame-supervisor tick.

   FRAME-TICK-FN receives the authoritative current frame and permit. It must
   return one of the existing statuses :parked, :phase-advanced, or
   :frame-complete. CURSOR-PERSIST-FN must durably replace the cursor."
  [{:keys [units start-frame end-frame permit trusted-permit-id trusted-issuer
           actor inspect-fn frame-tick-fn cursor-read-fn cursor-persist-fn
           continue-fn]}]
  (let [range-result (contiguous-range units start-frame end-frame)]
    (cond
      (not (:ok range-result)) range-result
      (not (every? fn? [inspect-fn frame-tick-fn cursor-read-fn
                        cursor-persist-fn]))
      {:ok false :error/code :live-batch-provider-missing}
      :else
      (let [frames (:frames range-result)
            inspection (inspect-fn)
            certificate (get-in inspection [:checkpoint :certificate])
            obligation (:obligation inspection)
            action (get-in obligation [:obligation/action])
            frame-id (:frame-id action)
            manifest-frames (mapv :frame/id units)
            frame-index (.indexOf manifest-frames frame-id)
            end-index (.indexOf manifest-frames end-frame)
            action-index (get (:campaign/permit-usage certificate)
                              (:permit/id permit) 0)
            authorization
            (batch/authorize
             {:permit permit :trusted-permit-id trusted-permit-id
              :trusted-issuer trusted-issuer :actor actor
              :certificate certificate :obligation obligation
              :action-index action-index})
            prior (cursor-read-fn)
            expected-cursor (cursor {:frames frames :permit permit
                                     :certificate certificate})]
        (cond
          (not (:ok inspection)) inspection
          (not= :ready (:stepper/status inspection))
          {:ok false :error/code :live-batch-inspection-not-ready
           :inspection inspection}
          (not= (:permit/id permit)
                (machine/ledger-digest [(dissoc permit :permit/id)]))
          {:ok false :error/code :live-batch-permit-content-invalid}
          (and prior (not (valid-cursor? prior)))
          {:ok false :error/code :live-batch-cursor-invalid}
          (and prior
               (not= (select-keys prior [:frames :permit/id :campaign/id])
                     (select-keys expected-cursor
                                  [:frames :permit/id :campaign/id])))
          {:ok false :error/code :live-batch-cursor-authority-mismatch}
          (and prior
               (or (< (:campaign/version expected-cursor)
                      (:campaign/version prior))
                   (< (:action-index expected-cursor) (:action-index prior))
                   (and (= (:campaign/version expected-cursor)
                           (:campaign/version prior))
                        (not= (:ledger/digest expected-cursor)
                              (:ledger/digest prior)))))
          {:ok false :error/code :live-batch-cursor-ledger-regression}
          (= frame-index (inc end-index))
          (let [complete (addressed (assoc expected-cursor
                                           :cursor/status :complete))
                persisted (cursor-persist-fn complete)]
            (if (:ok persisted)
              {:ok true :status :batch-complete :batch/frames frames
               :batch/cursor complete}
              {:ok false :error/code :live-batch-cursor-persistence-failed}))
          (not (contains? (set frames) frame-id))
          {:ok false :error/code :live-batch-frame-outside-range
           :finding {:frame-id frame-id :frames frames}}
          (not (:ok authorization))
          {:ok false :error/code :live-batch-permit-refused
           :authorization authorization}
          :else
          (let [persisted (cursor-persist-fn expected-cursor)]
            (if-not (:ok persisted)
              {:ok false :error/code :live-batch-cursor-persistence-failed}
              (let [result (frame-tick-fn
                            {:frame-id frame-id :frames frames :permit permit
                             :trusted-permit-id trusted-permit-id
                             :trusted-issuer trusted-issuer :actor actor})]
                (if (and (:ok result)
                         (not= :unknown
                               (phase-status/classify :live-batch-frame
                                                      (:status result))))
                  (if (= :frame-complete (:status result))
                    (if-not (fn? continue-fn)
                      {:ok false :error/code :live-batch-continuation-provider-missing}
                      (let [continued (continue-fn)]
                        (if (:ok continued)
                          {:ok true :status :batch-advanced
                           :batch/frames frames :batch/frame frame-id
                           :batch/cursor expected-cursor
                           :continuation continued}
                          {:ok false :error/code :live-batch-continuation-failed
                           :finding continued})))
                    (assoc result :batch/frames frames :batch/frame frame-id
                           :batch/cursor expected-cursor))
                  (if (:ok result)
                    {:ok false
                     :error/code :live-batch-frame-status-vocabulary-incomplete
                     :finding {:status (:status result)
                               :known-statuses
                               (vec (sort (phase-status/known-statuses
                                           :live-batch-frame)))
                               :frame-result result}}
                    result))))))))))
