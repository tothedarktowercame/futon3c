(ns futon3c.apm.generated-contract
  "Validation boundary for the canonical contract emitted by Lean."
  (:require [cheshire.core :as json]))

(def required-bounds
  {:solver-max-rounds 50
   :solver-checkpoint-every 10
   :student-attempts 3
   :guide-interventions 2
   :analyst-tenure-frames 2
   :seat-turn-timeout-ms 3600000
   :zai-request-timeout-ms 300000})

(def required-dispatch-policy
  {:preannounce-required true
   :activation-status 202
   :idempotent-reactivation true
   :terminal-command-own-exit 0
   :persist-claim true
   :persist-receipt true
   :restart-same-job true
   :client-timeout-is-success false})

(def required-memory-policy
  {:content-addressed-snapshot true :admit-after-solve-verify true
   :independent-review true :student-attempts 3
   :fresh-student-sessions true :exact-snapshot-binding true})

(def required-isolation-policy
  {:campaign-scoped-regulator true :campaign-scoped-problem-buffer true
   :distinct-continuation-session true :distinct-analyst-session true
   :projection-ledger-binding true})

(def required-terminal-policy
  {:certified-phase-receipts 11 :separate-problem-frame-outcomes true})

(def required-analyst-policy
  {:outside-frame-order true :wake-after-terminal-only true
   :exactly-once-per-frame true :append-only-series-input true
   :tenure-frames 2 :successor-handoff-required true
   :in-flight-mutation false})

(defn read-contract [path]
  (try
    {:ok true :contract (json/parse-string (slurp path) true)}
    (catch Exception e
      {:ok false :error/code :generated-contract-unreadable
       :exception/message (.getMessage e)})))

(defn expected-transitions [phase-order]
  (mapv (fn [from to] {:from from :to to})
        phase-order (concat (rest phase-order) [nil])))

(defn validate
  "Refuse an emitted artifact whose transition table is not total over its
   phase order, whose bounds drift, or whose verify edge bypasses promotion."
  [contract]
  (let [phase-order (:phase-order contract)
        transitions (:transitions contract)
        findings
        (cond-> []
          (not= 1 (:schema-version contract))
          (conj :generated-contract-schema-version-invalid)
          (not= "apm-complete-frame-cycle-v2" (:contract-id contract))
          (conj :generated-contract-id-invalid)
          (not= (count phase-order) (count (distinct phase-order)))
          (conj :generated-contract-phase-duplicate)
          (not= (expected-transitions phase-order) transitions)
          (conj :generated-contract-transition-table-invalid)
          (not= "promote-solver"
                (:to (some #(when (= "verify" (:from %)) %) transitions)))
          (conj :generated-contract-verify-bypasses-promotion)
          (not= required-bounds (:bounds contract))
          (conj :generated-contract-bounds-invalid)
          (not= required-dispatch-policy (:dispatch-policy contract))
          (conj :generated-contract-dispatch-policy-invalid)
          (not= required-memory-policy (:memory-policy contract))
          (conj :generated-contract-memory-policy-invalid)
          (not= required-isolation-policy (:isolation-policy contract))
          (conj :generated-contract-isolation-policy-invalid)
          (not= required-terminal-policy (:terminal-policy contract))
          (conj :generated-contract-terminal-policy-invalid)
          (not= required-analyst-policy (:analyst-policy contract))
          (conj :generated-contract-analyst-policy-invalid))]
    (if (seq findings)
      {:ok false :error/code :generated-contract-invalid :findings findings}
      {:ok true :contract contract})))

(defn validate-round-trip [path clojure-contract]
  (let [loaded (read-contract path)]
    (if-not (:ok loaded)
      loaded
      (let [validated (validate (:contract loaded))
            emitted-phases (mapv keyword (get-in loaded [:contract :phase-order]))]
        (if-not (:ok validated)
          validated
          (if (= emitted-phases (:phase-order clojure-contract))
            {:ok true :contract (:contract loaded)}
            {:ok false :error/code :generated-contract-round-trip-mismatch
             :finding {:emitted emitted-phases
                       :clojure (:phase-order clojure-contract)}}))))))
