(ns futon3c.apm.queued-frame-terminal
  "Atomic terminal boundary for a just-in-time queued frame.

  Solved-problem banking retains the exact certified branch. Workspace removal
  is separately authorized by independent retirement audits; neither operation
  is allowed from a conversational or merely terminal-looking status."
  (:require [futon3c.apm.campaign-machine :as machine]))

(def frame-results #{:closed :partial :void})
(def learning-outcomes #{:observed :partially-observed :unobserved :skipped})

(defn addressed? [receipt id-key]
  (= (get receipt id-key)
     (machine/ledger-digest [(dissoc receipt id-key)])))

(defn validate-terminal [frame receipt]
  (let [progress-retry? (and (= :partial (:problem/outcome receipt))
                             (= :partial (:frame/result receipt)))
        findings
        (cond-> []
          (not= :frame-terminal (:receipt/type receipt))
          (conj :terminal-receipt-type-invalid)
          (not (addressed? receipt :receipt/id))
          (conj :terminal-receipt-content-invalid)
          (not= (:frame/id frame) (:frame/id receipt))
          (conj :terminal-frame-mismatch)
          (not= (:problem/id frame) (:problem/id receipt))
          (conj :terminal-problem-mismatch)
          (not (contains? frame-results (:frame/result receipt)))
          (conj :terminal-frame-result-invalid)
          (not (contains? #{:solved :partial :invalid} (:problem/outcome receipt)))
          (conj :terminal-problem-outcome-invalid)
          (not (contains? learning-outcomes (:learning/outcome receipt)))
          (conj :terminal-learning-outcome-invalid)
          (and (not progress-retry?)
               (not= :void (:frame/result receipt))
               (not (and (string? (:verify-receipt/id receipt))
                         (re-matches #"[0-9a-f]{64}"
                                     (:verify-receipt/id receipt)))))
          (conj :terminal-verify-receipt-invalid)
          (and progress-retry?
               (not (and (string? (:solver-progress-receipt/id receipt))
                         (re-matches #"[0-9a-f]{64}"
                                     (:solver-progress-receipt/id receipt)))))
          (conj :terminal-solver-progress-receipt-invalid)
          (not (and (string? (get-in receipt [:solver :branch]))
                    (string? (get-in receipt [:solver :head]))
                    (re-matches #"[0-9a-f]{40}"
                                (get-in receipt [:solver :head]))))
          (conj :terminal-solver-identity-invalid))
        heads (:workspace/terminal-heads receipt)]
    (if (and (= #{:solver :student} (set (keys heads)))
             (every? #(and (string? %) (re-matches #"[0-9a-f]{40}" %))
                     (vals heads))
             (= (get heads :solver) (get-in receipt [:solver :head])))
      (if (seq findings)
        {:ok false :error/code :queued-frame-terminal-invalid :findings findings}
        {:ok true :receipt receipt})
      {:ok false :error/code :queued-frame-terminal-invalid
       :findings (conj findings :terminal-workspace-heads-invalid)})))

(defn build-problem-bank [frame terminal]
  (let [progress-retry? (and (= :partial (:problem/outcome terminal))
                             (= :partial (:frame/result terminal)))
        body {:receipt/type (if progress-retry?
                              :queued-solver-progress-bank
                              :queued-problem-bank)
              :frame/id (:frame/id frame) :problem/id (:problem/id frame)
              :problem/outcome (:problem/outcome terminal)
              :frame/result (:frame/result terminal)
              :learning/outcome (:learning/outcome terminal)
              :verify-receipt/id (:verify-receipt/id terminal)
              :solver-progress-receipt/id
              (:solver-progress-receipt/id terminal)
              :retry/same-problem? progress-retry?
              :source/terminal-receipt-id (:receipt/id terminal)
              :solver/branch (get-in terminal [:solver :branch])
              :solver/head (get-in terminal [:solver :head])
              :workspace/terminal-heads (:workspace/terminal-heads terminal)
              :branch-retained? true}]
    (assoc body :receipt/id (machine/ledger-digest [body]))))

(defn retire!
  "Bank, independently audit, and retire one terminal frame.

  The bank receipt is persisted before workspace retirement. Every audit must
  be supplied by AUDIT-FN and satisfy workspace-lifecycle's full certificate."
  [{:keys [frame terminal-receipt leases audit-fn retire-workspace-fn
           retirement-status-fn persist-bank-fn retire-seats-fn]}]
  (let [terminal-check (validate-terminal frame terminal-receipt)]
    (cond
      (not (:ok terminal-check)) terminal-check
      (not (every? fn? [audit-fn retire-workspace-fn retirement-status-fn
                        persist-bank-fn retire-seats-fn]))
      {:ok false :error/code :queued-frame-terminal-provider-missing}
      (not= #{:solver :student} (set (keys leases)))
      {:ok false :error/code :queued-frame-terminal-leases-incomplete}
      :else
      (let [bank (build-problem-bank frame terminal-receipt)
            persisted (persist-bank-fn frame bank)]
        (if-not (:ok persisted)
          {:ok false :error/code :queued-frame-bank-persistence-failed}
          (let [retirements
                (reduce
                 (fn [result [role lease]]
                   (if-not (:ok result)
                     (reduced result)
                     (let [status (retirement-status-fn
                                   lease
                                   (get-in terminal-receipt
                                           [:workspace/terminal-heads role]))]
                       (cond
                         (not (:ok status)) (reduced status)
                         (= :already-retired (:status status))
                         (assoc-in result [:workspace-receipts role]
                                   (:receipt status))
                         (not= :not-retired (:status status))
                         (reduced {:ok false
                                   :error/code
                                   :queued-frame-retirement-status-invalid
                                   :role role :status status})
                         :else
                         (let [audit-result
                               (audit-fn frame terminal-receipt role lease)
                               audit (:audit audit-result)]
                           (if-not (:ok audit-result)
                             (reduced audit-result)
                             (let [retired (retire-workspace-fn lease audit)]
                               (if (:ok retired)
                                 (assoc-in result [:workspace-receipts role]
                                           (:receipt retired))
                                 (reduced retired)))))))))
                 {:ok true :workspace-receipts {}}
                 (sort-by (comp name key) leases))]
            (if-not (:ok retirements)
              retirements
              (let [seats (retire-seats-fn frame terminal-receipt)]
                (if-not (:ok seats)
                  seats
                  {:ok true :bank-receipt bank
                   :workspace-receipts (:workspace-receipts retirements)
                   :seat-retirement seats})))))))))
