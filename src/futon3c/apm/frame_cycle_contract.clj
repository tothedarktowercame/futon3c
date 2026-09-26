(ns futon3c.apm.frame-cycle-contract
  "Validation for the complete, operator-steppable APM frame schedule."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [futon3c.apm.campaign-machine :as machine]))

(defn read-contract [path]
  (try
    {:ok true :contract (edn/read-string (slurp path))}
    (catch Throwable t
      {:ok false :error/code :frame-cycle-contract-unreadable
       :finding {:message (.getMessage t)}})))

(defn validate-contract [contract]
  (let [order (:phase-order contract)
        phases (:phases contract)
        ordered (mapv phases order)
        produced-before
        (reductions into #{} (map #(or (:produces %) #{}) ordered))
        missing-inputs
        (->> (map vector order ordered produced-before)
             (keep (fn [[phase spec available]]
                     (let [missing (set/difference
                                    (or (:requires spec) #{}) available)]
                       (when (seq missing) [phase missing]))))
             (into {}))]
    (cond
      (not (and (vector? order) (seq order)))
      {:ok false :error/code :frame-cycle-phase-order-required}

      (not= (count order) (count (distinct order)))
      {:ok false :error/code :frame-cycle-phase-duplicate}

      (not= (set order) (set (keys phases)))
      {:ok false :error/code :frame-cycle-phase-map-mismatch}

      (some nil? ordered)
      {:ok false :error/code :frame-cycle-phase-spec-missing}

      (seq missing-inputs)
      {:ok false :error/code :frame-cycle-input-before-production
       :finding missing-inputs}

      :else
      {:ok true
       :phase-order order
       :obligation-plan
       (into {} (map (fn [[phase spec]]
                       [phase (select-keys spec [:kind :role :ordinal])])
                     phases))})))

(defn validate-receipt [contract expected-phase receipt]
  (let [phase (get-in contract [:phases expected-phase])
        expected-type (:receipt/type phase)
        actual-type (:receipt/type receipt)
        allowed-types (conj (or (:alternate-receipt/types phase) #{}) expected-type)
        required (get-in contract [:receipt/schemas actual-type :required])
        missing (set/difference (or required #{}) (clojure.core/set (keys receipt)))
        body (dissoc receipt :receipt/id)]
    (cond
      (nil? phase)
      {:ok false :error/code :frame-cycle-phase-unknown}

      (not (contains? allowed-types actual-type))
      {:ok false :error/code :frame-cycle-receipt-type-mismatch
       :finding {:expected expected-type :alternates (disj allowed-types expected-type)
                 :actual actual-type}}

      (nil? required)
      {:ok false :error/code :frame-cycle-receipt-schema-missing
       :finding {:actual actual-type}}

      (seq missing)
      {:ok false :error/code :frame-cycle-receipt-fields-missing
       :finding {:missing missing}}

      (not= (:receipt/id receipt) (machine/ledger-digest [body]))
      {:ok false :error/code :frame-cycle-receipt-content-invalid}

      :else {:ok true :receipt receipt})))
