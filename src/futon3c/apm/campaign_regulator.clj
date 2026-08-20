(ns futon3c.apm.campaign-regulator
  "Deterministic next-obligation derivation from validated campaign certificates."
  (:require [futon3c.apm.campaign-machine :as machine]))

(defn- verified-certificate? [certificate]
  (= (:certificate/id certificate)
     (machine/ledger-digest [(dissoc certificate :certificate/id)])))

(defn- pins [certificate]
  {:campaign/id (:campaign/id certificate)
   :campaign/version (:campaign/version certificate)
   :ledger/digest (:ledger/digest certificate)
   :facts/digest (:facts/digest certificate)})

(defn- obligation [certificate action]
  (let [preconditions (pins certificate)
        body {:obligation/type :campaign
              :obligation/action action
              :obligation/preconditions preconditions}]
    (assoc body :obligation/id (machine/ledger-digest [body]))))

(defn- next-block [certificate]
  (let [opened (set (keys (:campaign/blocks certificate)))]
    (some #(when-not (contains? opened (:block-id %)) %)
          (:campaign/block-plan certificate))))

(defn- next-unit [certificate block-id]
  (let [opened (set (keys (:campaign/frames certificate)))]
    (some #(when-not (contains? opened (:frame-id %)) %)
          (get-in certificate [:campaign/blocks block-id :units]))))

(defn- frame-phase-action [certificate]
  (let [frame (:active/frame certificate)
        phase (:phase frame)
        phases (:campaign/phase-order certificate)
        plan (:campaign/obligation-plan certificate)
        spec (get plan phase)
        index (.indexOf ^java.util.List phases phase)
        last-phase? (= index (dec (count phases)))
        completion (if last-phase?
                     {:event/type :frame/closed
                      :event/body {:frame-id (:frame-id frame)}}
                     {:event/type :frame/advanced
                      :event/body {:frame-id (:frame-id frame)
                                   :from phase :to (nth phases (inc index))}})]
    (cond
      (not (vector? phases))
      {:error/code :campaign-phase-order-missing}

      (neg? index)
      {:error/code :campaign-active-phase-unknown :phase phase}

      (nil? spec)
      {:error/code :campaign-phase-obligation-missing :phase phase}

      :else
      {:kind (:kind spec) :role (:role spec)
       :frame-id (:frame-id frame) :problem-id (:problem-id frame)
       :block-id (:block-id frame) :phase phase
       :completion completion})))

(defn decide
  "Return {:decision :dispatch :obligation ...}, {:decision :stop ...}, or
  {:decision :complete ...}. Only a content-valid :valid snapshot dispatches."
  [certificate]
  (cond
    (not (map? certificate))
    {:decision :stop :reason :campaign-certificate-required}

    (not (verified-certificate? certificate))
    {:decision :stop :reason :campaign-certificate-invalid}

    (not= :valid (:snapshot/status certificate))
    {:decision :stop :reason :campaign-snapshot-not-valid
     :status (:snapshot/status certificate)
     :findings (get-in certificate [:reconciliation :findings])}

    (= :closed (:campaign/status certificate))
    {:decision :complete :reason :campaign-closed :preconditions (pins certificate)}

    (= :stopped (:campaign/status certificate))
    {:decision :stop :reason :campaign-stop-rule-fired
     :preconditions (pins certificate)}

    (:active/claim certificate)
    {:decision :stop :reason :campaign-obligation-claim-recovery-required
     :claim (:active/claim certificate) :preconditions (pins certificate)}

    (:active/frame certificate)
    (let [action (frame-phase-action certificate)]
      (if (:error/code action)
        {:decision :stop :reason (:error/code action) :finding action}
        {:decision :dispatch :obligation (obligation certificate action)}))

    (:active/block certificate)
    (let [block-id (:active/block certificate)]
      (if-let [unit (next-unit certificate block-id)]
        {:decision :dispatch
         :obligation
         (obligation certificate
                     {:kind :open-frame :block-id block-id
                      :frame-id (:frame-id unit) :problem-id (:problem-id unit)
                      :arm (:arm unit)
                      :completion {:event/type :frame/opened
                                   :event/body (assoc unit :block-id block-id)}})}
        {:decision :dispatch
         :obligation
         (obligation certificate
                     {:kind :close-block :block-id block-id
                      :completion {:event/type :block/closed
                                   :event/body {:block-id block-id}}})}))

    (seq (:campaign/block-plan certificate))
    (if-let [block (next-block certificate)]
      {:decision :dispatch
       :obligation
       (obligation certificate
                   {:kind :open-block :block-id (:block-id block)
                    :ordinal (:ordinal block) :units (:units block)
                    :completion {:event/type :block/opened
                                 :event/body block}})}
      {:decision :dispatch
       :obligation
       (obligation certificate
                   {:kind :close-campaign
                    :completion {:event/type :campaign/closed
                                 :event/body {}}})})

    :else
    {:decision :stop :reason :campaign-block-plan-missing}))

(defn authorize
  "Recheck OBLIGATION pins against CURRENT-CERTIFICATE immediately before an
  effect. This is the dispatch race boundary."
  [obligation current-certificate]
  (let [expected (:obligation/preconditions obligation)
        actual (when (and (map? current-certificate)
                          (verified-certificate? current-certificate))
                 (pins current-certificate))]
    (cond
      (nil? actual)
      {:ok false :error/code :campaign-authorization-certificate-invalid}

      (not= :valid (:snapshot/status current-certificate))
      {:ok false :error/code :campaign-authorization-snapshot-not-valid
       :status (:snapshot/status current-certificate)}

      (not= expected actual)
      {:ok false :error/code :campaign-obligation-preconditions-stale
       :expected expected :actual actual}

      (not= (:obligation/id obligation)
            (machine/ledger-digest [(dissoc obligation :obligation/id)]))
      {:ok false :error/code :campaign-obligation-id-invalid}

      :else
      {:ok true :authorized? true
       :obligation/id (:obligation/id obligation)
       :preconditions actual})))
