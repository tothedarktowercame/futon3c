(ns futon3c.apm.series-terminal
  "Append-only closure of a campaign whose active frame ended with a partial result."
  (:require [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.time Instant]))

(defn- addressed [body]
  (assoc body :certificate/id (machine/ledger-digest [body])))

(defn- event [projection seq-offset type body actor at]
  (let [base {:event/seq (+ (:ledger/event-count projection) seq-offset)
              :event/type type :event/campaign-id (:campaign/id projection)
              :event/actor actor :event/at at
              :event/expected-version (+ (:campaign/version projection) seq-offset)
              :event/body body}]
    (assoc base :event/id (machine/ledger-digest [base]))))

(defn- claimed-pair [projection action completion-type completion-body
                     certificate actor at]
  (let [obligation-body
        {:obligation/type :campaign :obligation/action action
         :obligation/preconditions
         {:campaign/id (:campaign/id projection)
          :campaign/version (:campaign/version projection)
          :ledger/digest (:ledger/digest projection)}}
        obligation (assoc obligation-body :obligation/id
                          (machine/ledger-digest [obligation-body]))
        claim (event projection 0 :obligation/claimed {:obligation obligation}
                     actor at)
        completion (event projection 1 completion-type
                          (assoc completion-body
                                 :obligation/id (:obligation/id obligation)
                                 :certificate certificate)
                          actor at)]
    {:obligation obligation :claim-event claim :completion-event completion}))

(defn prepare
  [{:keys [projection events]}
   {:keys [frame-id problem-id final-head residual rounds actor now
           partial-reason problem-outcome proof-receipt-ids]}]
  (let [actor (or actor "countdown-control") at (str (or now (Instant/now)))
        active-frame (:active/frame projection)
        active-block (:active/block projection)
        stopped-frame (get-in projection [:campaign/frames frame-id])
        stage
        (cond
          (= :closed (:campaign/status projection)) :complete
          active-frame :frame
          active-block :block
          :else :campaign)]
    (cond
      (not= :valid (:projection/status projection))
      {:ok false :error/code :series-terminal-ledger-invalid}
      (:active/claim projection)
      {:ok false :error/code :series-terminal-active-claim}
      (= :complete stage)
      {:ok true :status :complete :projection projection}
      (and (= :frame stage)
           (not= [frame-id problem-id]
                 [(:frame-id active-frame) (:problem-id active-frame)]))
      {:ok false :error/code :series-terminal-frame-mismatch}
      (and (= :frame stage)
           (not (and (string? final-head)
                     (re-matches #"[0-9a-f]{40}" final-head)
                     (string? residual) (not-empty residual)
                     (pos-int? rounds)
                     (or (and (= :solve (:phase active-frame))
                              (or (nil? partial-reason)
                                  (= :solver-budget-exhausted partial-reason)))
                         (and (contains? #{:promote-solver :student-attempt-1
                                          :student-attempt-2 :student-attempt-3}
                                        (:phase active-frame))
                              (contains? #{:promotion-review-apparatus-invalid
                                           :student-dispatch-apparatus-invalid}
                                         partial-reason)
                              (= :solved problem-outcome)
                              (vector? proof-receipt-ids)
                              (<= 2 (count proof-receipt-ids))
                              (every? #(and (string? %) (not-empty %))
                                      proof-receipt-ids))))))
      {:ok false :error/code :series-terminal-partial-evidence-invalid}
      :else
      (let [[action completion-type completion-body certificate]
            (case stage
              :frame
              (let [reason (or partial-reason :solver-budget-exhausted)
                    cert (addressed
                          {:certificate/type :frame-partial
                           :frame/id frame-id :problem/id problem-id
                           :outcome :partial :reason reason
                           :stopped/phase (:phase active-frame)
                           :problem/outcome (or problem-outcome :unsolved)
                           :proof/receipt-ids (vec proof-receipt-ids)
                           :solver/final-head final-head :solver/rounds rounds
                           :solver/residual residual
                           :source/ledger-digest (:ledger/digest projection)})]
                [{:kind :partial-frame :frame-id frame-id :problem-id problem-id}
                 :frame/stopped
                 {:frame-id frame-id :reason reason}
                 cert])
              :block
              (let [partial-id (get-in stopped-frame [:stop :certificate
                                                       :certificate/id])
                    cert (addressed
                          {:certificate/type :block-partial-close
                           :block/id active-block :outcome :partial
                           :frame-partial-certificate-id partial-id})]
                [{:kind :close-partial-block :block-id active-block}
                 :block/closed {:block-id active-block} cert])
              :campaign
              (let [partial-id (get-in stopped-frame [:stop :certificate
                                                       :certificate/id])
                    cert (addressed
                          {:certificate/type :campaign-partial-close
                           :campaign/id (:campaign/id projection) :outcome :partial
                           :frame-partial-certificate-id partial-id})]
                [{:kind :close-partial-campaign}
                 :campaign/closed {} cert]))
            pair (claimed-pair projection action completion-type completion-body
                               certificate actor at)
            successor (machine/projection
                       (into events [(:claim-event pair) (:completion-event pair)]))]
        (if (= :valid (:projection/status successor))
          (assoc pair :ok true :stage stage :certificate certificate)
          {:ok false :error/code :series-terminal-successor-invalid
           :stage stage :projection successor})))))

(defn step! [{:keys [ledger-path] :as options}]
  (let [loaded (ledger/read-ledger ledger-path)]
    (if-not (:ok loaded)
      loaded
      (let [prepared (prepare loaded options)]
        (if (or (not (:ok prepared)) (= :complete (:status prepared)))
          prepared
          (let [projection (:projection loaded)
                claimed (ledger/compare-and-append!
                         ledger-path (:campaign/version projection)
                         (:ledger/digest projection) (:claim-event prepared))]
            (if-not (:ok claimed)
              claimed
              (let [completed (ledger/compare-and-append!
                               ledger-path (get-in claimed [:after :version])
                               (get-in claimed [:after :digest])
                               (:completion-event prepared))]
                (if (:ok completed)
                  {:ok true :status :advanced :stage (:stage prepared)
                   :certificate (:certificate prepared) :completion completed}
                  {:ok false :error/code :series-terminal-completion-refused
                   :claim-persisted? true :completion completed})))))))))

(defn close! [options]
  (loop [steps []]
    (let [result (step! options)]
      (cond
        (not (:ok result)) (assoc result :steps steps)
        (= :complete (:status result)) {:ok true :status :complete :steps steps}
        :else (recur (conj steps (select-keys result [:stage :certificate])))))))
