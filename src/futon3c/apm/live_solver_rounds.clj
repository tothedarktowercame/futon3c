(ns futon3c.apm.live-solver-rounds
  "Durable multi-round controller for a solver siege.

   A terminal Agency turn is not a terminal solve.  Unsolved turns are recorded
   and the same seat/session/branch is continued up to an explicit round cap."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-job-driver :as job-driver]))

(def default-max-rounds 50)

(defn round-request [base-request ordinal prior]
  (let [body (cond-> (-> base-request
                         (dissoc :dispatch/id)
                         (assoc :solver/round ordinal
                                :solver/max-rounds default-max-rounds
                                :solver/remaining-rounds
                                (- default-max-rounds ordinal)))
               prior
               (assoc :solver/prior-job-id (:job-id prior)
                      :solver/prior-session-id (:session-id prior)
                      :solver/prior-report (:report prior)))]
    (assoc body :dispatch/id (machine/ledger-digest [body]))))

(defn- container [state base-request]
  (cond
    (nil? state)
    {:state/type :solver-rounds :budget/max-rounds default-max-rounds
     :base-request base-request :rounds [] :active nil}

    (= :live-job-dispatched (:state/type state))
    {:state/type :solver-rounds :budget/max-rounds default-max-rounds
     :base-request base-request :rounds [] :active state}

    (= :solver-rounds (:state/type state)) state
    :else state))

(defn- persist-container [persist-fn state]
  (let [result (persist-fn state)]
    (if (:ok result) {:ok true :state state}
        {:ok false :error/code :solver-round-state-persistence-failed})))

(defn- dispatch-round!
  [{:keys [announce-fn activate-fn persist-fn]} state]
  (let [ordinal (inc (count (:rounds state)))
        prior (last (:rounds state))
        request (round-request (:base-request state) ordinal prior)
        announced (job-driver/ticket request (announce-fn request))]
    (if-not (:ok announced)
      announced
      (let [active {:state/type :live-job-dispatched :request request
                    :ticket (:ticket announced)}
            staged (assoc state :active active)
            persisted (persist-container persist-fn staged)]
        (if-not (:ok persisted)
          persisted
          (let [activated (activate-fn request (:ticket announced))]
            (if-not (:ok activated)
              {:ok false :error/code :solver-round-activation-failed
               :state staged :finding activated}
              (let [accepted (assoc-in staged [:active :activation/accepted?] true)
                    saved (persist-container persist-fn accepted)]
                (if (:ok saved)
                  {:ok true :status :awaiting-terminal :state accepted}
                  saved)))))))))

(defn- round-outcome [report]
  (cond
    (and (= :claimed-defect (:solver/outcome report))
         (string? (:residual report)) (not (str/blank? (:residual report))))
    :claimed-defect

    (and (= :progress (:solver/outcome report))
         (string? (:residual report)) (not (str/blank? (:residual report))))
    :progress

    :else :inadequate))

(defn- terminal-round [active job validation ordinal]
  {:ordinal ordinal
   :dispatch/id (get-in active [:request :dispatch/id])
   :job-id (get-in active [:ticket :job-id])
   :session-id (:session-id job)
   :terminal-state (:state job)
   :outcome (round-outcome (:report job))
   :report (:report job)
   :validation (select-keys validation [:ok :error/code :findings :missing])})

(defn drive!
  "Advance a solver siege by one durable boundary.

   VALIDATE-SOLVED and PROVIDE-RECEIPT retain the strict proof gate. Any
   terminal return that misses it is recorded as a spent round and continued;
   at MAX-ROUNDS the state requires human intervention and never pretends the
   problem is invalid."
  [{:keys [request state activate-fn job-fn persist-fn
           validate-solved provide-receipt max-rounds]
    :or {max-rounds default-max-rounds} :as effects}]
  (let [state (container state request)]
    (cond
      (= :live-job-certified (:state/type state))
      {:ok true :status :certified :state state :certificate (:receipt state)}

      (= :solver-human-intervention-required (:state/type state))
      {:ok false :error/code :solver-human-intervention-required :state state}

      (= :solver-defect-review-required (:state/type state))
      {:ok false :error/code :solver-defect-review-required :state state}

      (not= :solver-rounds (:state/type state))
      {:ok false :error/code :solver-round-state-invalid}

      (nil? (:active state))
      (dispatch-round! effects state)

      (not (get-in state [:active :activation/accepted?]))
      (let [active (:active state)
            activated (activate-fn (:request active) (:ticket active))]
        (if-not (:ok activated)
          {:ok false :error/code :solver-round-activation-failed
           :state state :finding activated}
          (let [accepted (assoc-in state [:active :activation/accepted?] true)
                saved (persist-container persist-fn accepted)]
            (if (:ok saved)
              {:ok true :status :awaiting-terminal :state accepted}
              saved))))

      :else
      (let [active (:active state)
            job (job-fn (get-in active [:ticket :job-id]))
            expected-session (:session-id (first (:rounds state)))]
        (if-not (contains? job-driver/terminal-states (:state job))
          {:ok true :status :awaiting-terminal :state state}
          (if (and expected-session (not= expected-session (:session-id job)))
            {:ok false :error/code :solver-session-mismatch
             :finding {:expected expected-session :actual (:session-id job)}}
          (let [validation (if (= :done (:state job))
                             (validate-solved (:request active) (:ticket active) job)
                             {:ok false :error/code :solver-job-terminal-failure})]
            (if (:ok validation)
              (let [receipt (provide-receipt (:request active) (:ticket active)
                                             job validation)]
                (if-not (:ok receipt)
                  receipt
                  (let [certified {:state/type :live-job-certified
                                   :base-request (:base-request state)
                                   :rounds (:rounds state) :active active
                                   :receipt (:certificate receipt)}
                        saved (persist-container persist-fn certified)]
                    (if (:ok saved)
                      {:ok true :status :certified :state certified
                       :certificate (:certificate receipt)}
                      saved))))
              (let [ordinal (or (get-in active [:request :solver/round])
                                (inc (count (:rounds state))))
                    completed (terminal-round active job validation ordinal)
                    rounds (conj (:rounds state) completed)
                    next-state (assoc state :rounds rounds :active nil)]
                (cond
                  (= :claimed-defect (:outcome completed))
                  (let [stopped (assoc next-state
                                       :state/type :solver-defect-review-required)
                        saved (persist-container persist-fn stopped)]
                    (if (:ok saved)
                      {:ok false :error/code :solver-defect-review-required
                       :state stopped}
                      saved))

                  (>= (count rounds) max-rounds)
                  (let [stopped (assoc next-state
                                       :state/type :solver-human-intervention-required
                                       :budget/max-rounds max-rounds)
                        saved (persist-container persist-fn stopped)]
                    (if (:ok saved)
                      {:ok false :error/code :solver-human-intervention-required
                       :state stopped}
                      saved))
                  :else
                  (let [saved (persist-container persist-fn next-state)]
                    (if (:ok saved)
                      (dispatch-round! effects next-state)
                      saved))))))))))))
