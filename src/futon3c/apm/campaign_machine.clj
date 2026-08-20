(ns futon3c.apm.campaign-machine
  "Pure, append-only state fold for long-running APM/BPM campaigns.

  The ledger is the authority.  Runtime jobs, buffers, and conversational
  sessions are projections or reconciliation inputs; none may advance this
  machine directly."
  (:require [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def event-types
  #{:campaign/registered :block/opened :frame/opened
    :frame/advanced :frame/stopped :frame/closed
    :block/closed :campaign/closed})

(def terminal-frame-statuses #{:closed :stopped})

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- canonical [x]
  (cond
    (map? x) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (vec (sort-by pr-str (map canonical x)))
    (sequential? x) (mapv canonical x)
    :else x))

(defn ledger-digest [events]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes (.digest digest (.getBytes (pr-str (canonical events))
                                       StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) bytes))))

(defn- refusal [state event code & [finding]]
  {:ok false
   :error/code code
   :event/id (:event/id event)
   :event/seq (:event/seq event)
   :state state
   :finding finding})

(defn- base-event-error [state event]
  (cond
    (not (map? event)) :campaign-event-not-map
    (not (nonblank? (:event/id event))) :campaign-event-id-required
    (not (integer? (:event/seq event))) :campaign-event-seq-required
    (not (contains? event-types (:event/type event))) :campaign-event-type-unknown
    (not (nonblank? (:event/campaign-id event))) :campaign-id-required
    (not (nonblank? (:event/actor event))) :campaign-event-actor-required
    (not (nonblank? (:event/at event))) :campaign-event-at-required
    (not (integer? (:event/expected-version event))) :campaign-version-required
    (not (map? (:event/body event))) :campaign-event-body-required
    (contains? (:event-ids state) (:event/id event)) :campaign-event-duplicate
    (not= (:next-seq state) (:event/seq event)) :campaign-sequence-gap
    (not= (:version state) (:event/expected-version event)) :campaign-version-stale
    (and (:campaign-id state)
         (not= (:campaign-id state) (:event/campaign-id event)))
    :campaign-id-mismatch))

(defn- phase-index [state phase]
  (.indexOf ^java.util.List (:phase-order state) phase))

(defn- adjacent-phase? [state from to]
  (let [from-index (phase-index state from)
        to-index (phase-index state to)]
    (and (<= 0 from-index) (= (inc from-index) to-index))))

(defn- active-frame [state]
  (when-let [frame-id (:active-frame-id state)]
    (get-in state [:frames frame-id])))

(defn- valid-block-plan? [plan]
  (and (vector? plan)
       (= (count plan) (count (distinct (map :block-id plan))))
       (every? (fn [{:keys [block-id units]}]
                 (and (nonblank? block-id) (vector? units)
                      (= (count units) (count (distinct (map :frame-id units))))
                      (every? #(and (nonblank? (:frame-id %))
                                    (nonblank? (:problem-id %))) units)))
               plan)))

(defn- valid-obligation-plan? [phase-order plan]
  (and (map? plan)
       (= (set phase-order) (set (keys plan)))
       (every? (fn [[_ {:keys [kind role]}]]
                 (and (keyword? kind) (or (nil? role) (keyword? role))))
               plan)))

(defn- next-planned-block [state]
  (some #(when-not (contains? (:blocks state) (:block-id %)) %)
        (:block-plan state)))

(defn- next-planned-unit [state block-id]
  (some #(when-not (contains? (:frames state) (:frame-id %)) %)
        (get-in state [:blocks block-id :units])))

(defn- apply-event [state event]
  (let [{event-type :event/type body :event/body} event]
    (case event-type
      :campaign/registered
      (cond
        (:campaign-id state)
        (refusal state event :campaign-already-registered)

        (not (and (vector? (:phase-order body))
                  (<= 2 (count (:phase-order body)))
                  (= (count (:phase-order body))
                     (count (distinct (:phase-order body))))))
        (refusal state event :campaign-phase-order-invalid)

        (and (contains? body :block-plan)
             (not (valid-block-plan? (:block-plan body))))
        (refusal state event :campaign-block-plan-invalid)

        (and (contains? body :obligation-plan)
             (not (valid-obligation-plan? (:phase-order body)
                                          (:obligation-plan body))))
        (refusal state event :campaign-obligation-plan-invalid)

        :else
        {:ok true
         :state (assoc state
                       :campaign-id (:event/campaign-id event)
                       :series (or (:series body) :apm)
                       :manifest-hash (:manifest-hash body)
                       :phase-order (:phase-order body)
                       :block-plan (:block-plan body)
                       :obligation-plan (:obligation-plan body)
                       :status :registered)})

      :block/opened
      (let [block-id (:block-id body)
            planned (when (seq (:block-plan state)) (next-planned-block state))
            units (or (:units body) (:units planned))]
        (cond
          (not= :registered (:status state))
          (refusal state event :campaign-not-available-for-block)
          (not (nonblank? block-id))
          (refusal state event :block-id-required)
          (contains? (:blocks state) block-id)
          (refusal state event :block-already-exists)
          (and planned (not= block-id (:block-id planned)))
          (refusal state event :block-plan-order-violation
                   {:expected (:block-id planned) :actual block-id})
          (and planned (:units body) (not= (:units planned) (:units body)))
          (refusal state event :block-plan-units-mismatch)
          :else
          {:ok true :state (-> state
                               (assoc :status :running :active-block-id block-id)
                               (assoc-in [:blocks block-id]
                                         {:block-id block-id :status :open
                                          :ordinal (:ordinal body)
                                          :units units
                                          :frame-ids []}))}))

      :frame/opened
      (let [frame-id (:frame-id body)
            block-id (:block-id body)
            first-phase (first (:phase-order state))
            planned (next-planned-unit state block-id)]
        (cond
          (not= block-id (:active-block-id state))
          (refusal state event :frame-block-not-active)
          (:active-frame-id state)
          (refusal state event :campaign-active-frame-conflict
                   {:active-frame-id (:active-frame-id state)})
          (not (and (nonblank? frame-id) (nonblank? (:problem-id body))))
          (refusal state event :frame-identity-required)
          (contains? (:frames state) frame-id)
          (refusal state event :frame-already-exists)
          (and planned
               (not= (select-keys planned [:frame-id :problem-id :arm])
                     (select-keys body [:frame-id :problem-id :arm])))
          (refusal state event :frame-plan-order-violation
                   {:expected planned
                    :actual (select-keys body [:frame-id :problem-id :arm])})
          :else
          {:ok true
           :state (-> state
                      (assoc :active-frame-id frame-id)
                      (assoc-in [:frames frame-id]
                                {:frame-id frame-id :block-id block-id
                                 :problem-id (:problem-id body)
                                 :arm (:arm body) :status :active
                                 :phase first-phase :version 0
                                 :registration-hash (:registration-hash body)
                                 :harness-hash (:harness-hash body)
                                 :required-receipt-kinds
                                 (set (:required-receipt-kinds body))})
                      (update-in [:blocks block-id :frame-ids] conj frame-id))}))

      :frame/advanced
      (let [frame (active-frame state)
            frame-id (:frame-id body)]
        (cond
          (nil? frame) (refusal state event :campaign-no-active-frame)
          (not= frame-id (:frame-id frame))
          (refusal state event :frame-not-active)
          (not= (:from body) (:phase frame))
          (refusal state event :frame-phase-stale
                   {:expected (:phase frame) :actual (:from body)})
          (not (adjacent-phase? state (:from body) (:to body)))
          (refusal state event :frame-transition-illegal
                   {:from (:from body) :to (:to body)})
          :else
          {:ok true :state (-> state
                               (assoc-in [:frames frame-id :phase] (:to body))
                               (update-in [:frames frame-id :version] inc)
                               (assoc-in [:frames frame-id :certificate]
                                         (:certificate body)))}))

      :frame/stopped
      (if-let [frame (active-frame state)]
        (if (= (:frame-id body) (:frame-id frame))
          {:ok true :state (-> state
                               (assoc :active-frame-id nil :status :stopped)
                               (assoc-in [:frames (:frame-id frame) :status] :stopped)
                               (assoc-in [:frames (:frame-id frame) :stop]
                                         (select-keys body [:reason :certificate])))}
          (refusal state event :frame-not-active))
        (refusal state event :campaign-no-active-frame))

      :frame/closed
      (if-let [frame (active-frame state)]
        (cond
          (not= (:frame-id body) (:frame-id frame))
          (refusal state event :frame-not-active)
          (not= (last (:phase-order state)) (:phase frame))
          (refusal state event :frame-close-before-terminal-phase)
          (not (map? (:certificate body)))
          (refusal state event :frame-close-certificate-required)
          :else
          {:ok true :state (-> state
                               (assoc :active-frame-id nil)
                               (assoc-in [:frames (:frame-id frame) :status] :closed)
                               (assoc-in [:frames (:frame-id frame) :close-certificate]
                                         (:certificate body)))})
        (refusal state event :campaign-no-active-frame))

      :block/closed
      (let [block-id (:block-id body)]
        (cond
          (not= block-id (:active-block-id state))
          (refusal state event :block-not-active)
          (:active-frame-id state)
          (refusal state event :block-close-with-active-frame)
          (not (map? (:certificate body)))
          (refusal state event :block-close-certificate-required)
          :else
          {:ok true :state (-> state
                               (assoc :active-block-id nil :status :registered)
                               (assoc-in [:blocks block-id :status] :closed)
                               (assoc-in [:blocks block-id :certificate]
                                         (:certificate body)))}))

      :campaign/closed
      (cond
        (:active-block-id state) (refusal state event :campaign-close-with-active-block)
        (not (map? (:certificate body)))
        (refusal state event :campaign-close-certificate-required)
        :else
        {:ok true :state (assoc state :status :closed
                                :close-certificate (:certificate body))}))))

(def initial-state
  {:campaign-id nil :status :empty :version 0 :next-seq 0
   :event-ids #{} :blocks {} :frames {}
   :active-block-id nil :active-frame-id nil})

(defn fold-ledger
  "Validate and fold EVENTS. Refuses at the first invalid event and retains the
  last valid state for diagnosis."
  [events]
  (loop [state initial-state, remaining (seq events)]
    (if-let [event (first remaining)]
      (if-let [code (base-event-error state event)]
        (refusal state event code
                 (when (= code :campaign-sequence-gap)
                   {:expected (:next-seq state) :actual (:event/seq event)}))
        (let [applied (apply-event state event)]
          (if-not (:ok applied)
            applied
            (recur (-> (:state applied)
                       (update :version inc)
                       (update :next-seq inc)
                       (update :event-ids conj (:event/id event)))
                   (next remaining)))))
      {:ok true :state state :event-count (:next-seq state)})))

(defn projection
  "Produce a certificate-bearing projection, or an explicit INVALID projection.
  EXPECTED-DIGEST may be pinned by a registration or external ledger receipt."
  ([events] (projection events nil))
  ([events expected-digest]
   (let [digest (ledger-digest events)
         folded (fold-ledger events)]
     (cond
       (not (:ok folded))
       {:projection/status :invalid :ledger/digest digest
        :error/code (:error/code folded) :finding (:finding folded)
        :last-valid-state (:state folded)}

       (and expected-digest (not= expected-digest digest))
       {:projection/status :invalid :ledger/digest digest
        :error/code :campaign-ledger-digest-mismatch
        :finding {:expected expected-digest :actual digest}}

       :else
       (let [state (:state folded)]
         {:projection/status :valid
          :ledger/digest digest
          :ledger/event-count (:event-count folded)
          :campaign/id (:campaign-id state)
          :campaign/series (:series state)
          :campaign/status (:status state)
          :campaign/version (:version state)
          :campaign/phase-order (:phase-order state)
          :campaign/block-plan (:block-plan state)
          :campaign/obligation-plan (:obligation-plan state)
          :campaign/blocks (:blocks state)
          :campaign/frames (:frames state)
          :active/block (:active-block-id state)
          :active/frame (active-frame state)
          :counts {:blocks (count (:blocks state))
                   :frames (count (:frames state))
                   :closed-frames (count (filter #(= :closed (:status %))
                                                (vals (:frames state))))
                   :stopped-frames (count (filter #(= :stopped (:status %))
                                                 (vals (:frames state))))}})))))
