(ns futon3c.agency.attestation-record
  "Pattern attestations written at a verified use boundary.

   Disposition order is deliberate: without a resolvable presentation the
   record cannot establish what the attester saw, even if its other fields
   resemble a proposer citation or echo. Proposer self-citation is then
   excluded before shown-list echo, because it remains a warrant regardless
   of exposure. Only an independently presented, non-proposer use counts.

   `proposal-author` is absent for library patterns. Resolving authorship from
   a draft candidate is deferred until there is one authoritative proposal
   registry; this namespace never guesses from a pattern id or prose.

   An unknown value is an absent key, never nil: futon1b (XTDB) does not store
   nil-valued keys, so a nil would fail the exact read-back after commit."
  (:require [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp]
            [futon3c.agency.rule-record :as store]))

(def attestation-type :pattern/attestation)
(def schema-version 1)
(def ^:private record-keys
  #{:id :kind :schema :pattern-id :attester :at :use :presentation
    :disposition :act/stamp :act/harness})
(def ^:private optional-keys #{:proposal-author})
(def ^:private dispositions
  #{:counts :proposer-warrant :shown-list-echo :presentation-unknown})

(defn- refuse! [reason field]
  (throw (ex-info "Invalid pattern attestation" {:reason reason :field field})))

(defn- text? [x] (and (string? x) (not (str/blank? x))))
(defn- act-id? [x] (and (text? x) (str/starts-with? x "act:")))

(defn disposition
  "Classify one use. PRESENTATION is resolvable only when it has a nonblank
   :ref and a vector :shown-pattern-ids copied from that record."
  [attester pattern-id presentation proposal-author]
  (cond
    (not (and (map? presentation) (text? (:ref presentation))
              (vector? (:shown-pattern-ids presentation))))
    :presentation-unknown

    (and (text? proposal-author) (= attester proposal-author))
    :proposer-warrant

    (some #{pattern-id} (:shown-pattern-ids presentation))
    :shown-list-echo

    :else :counts))

(defn validate!
  "Validate and return a closed schema-1 :pattern/attestation record."
  [record]
  (when-not (map? record) (refuse! :invalid-record :record))
  (let [ks (set (keys record))]
    (when-not (and (every? ks record-keys)
                   (every? (into record-keys optional-keys) ks))
      (refuse! :invalid-keys :record)))
  (when-not (act-id? (:id record)) (refuse! :invalid-act-id :id))
  (when-not (= attestation-type (:kind record)) (refuse! :wrong-kind :kind))
  (when-not (= schema-version (:schema record)) (refuse! :unsupported-schema :schema))
  (doseq [field [:pattern-id :attester :at]]
    (when-not (text? (get record field)) (refuse! :missing-field field)))
  (try (java.time.Instant/parse (:at record))
       (catch Exception _ (refuse! :invalid-at :at)))
  (let [use (:use record)]
    (when-not (and (= #{:kind :ref} (set (keys use)))
                   (= :pattern-card-selection (:kind use))
                   (act-id? (:ref use)))
      (refuse! :invalid-use :use)))
  (let [p (:presentation record)]
    (when-not (and (map? p)
                   (#{#{:shown-pattern-ids} #{:ref :shown-pattern-ids}} (set (keys p)))
                   (or (not (contains? p :ref)) (text? (:ref p)))
                   (vector? (:shown-pattern-ids p))
                   (every? text? (:shown-pattern-ids p))
                   (or (:ref p) (empty? (:shown-pattern-ids p))))
      (refuse! :invalid-presentation :presentation)))
  (when-not (or (not (contains? record :proposal-author))
                (text? (:proposal-author record)))
    (refuse! :invalid-proposal-author :proposal-author))
  (when-not (contains? dispositions (:disposition record))
    (refuse! :invalid-disposition :disposition))
  (when-not (= (:disposition record)
               (disposition (:attester record) (:pattern-id record)
                            (:presentation record) (:proposal-author record)))
    (refuse! :disposition-mismatch :disposition))
  (act-stamp/validate! (:act/stamp record))
  (act-harness/validate! (:act/harness record))
  (when-not (= (:attester record) (get-in record [:act/stamp :signer]))
    (refuse! :stamp-attester-mismatch :act/stamp))
  record)

(defn ->hyperedge [record]
  (let [r (validate! record)]
    {:hx/id (:id r)
     :hx/type attestation-type
     :hx/valid-time (:at r)
     :hx/endpoints [(str "pattern:" (:pattern-id r))
                    (str "agent:" (:attester r))
                    (get-in r [:use :ref])]
     :hx/props (dissoc r :id :kind)}))

(defn hyperedge->record [edge]
  (when-not (= attestation-type (:hx/type edge))
    (refuse! :wrong-kind :hx/type))
  (validate! (assoc (:hx/props edge) :id (:hx/id edge) :kind attestation-type)))

(defn attestation-count
  "Count only :counts records by pattern id; retain exclusions by disposition."
  [attestations]
  (let [records (mapv validate! attestations)]
    {:counts (frequencies (map :pattern-id (filter #(= :counts (:disposition %)) records)))
     :excluded (group-by :disposition (remove #(= :counts (:disposition %)) records))}))

(defn write!
  "Mint and exactly read back one attestation."
  [base record idempotency-key]
  (let [draft (validate! (assoc record :id "act:pending-mint"))
        payload (-> (->hyperedge draft)
                    (dissoc :hx/id)
                    (assoc :hx/mint-id true :hx/idempotency-key idempotency-key))
        receipt (store/request! base "POST" "/api/alpha/hyperedge" payload)
        id (:hx/id receipt)]
    (when-not (act-id? id) (refuse! :missing-minted-receipt :receipt))
    (let [stored (store/request! base "GET" (str "/api/alpha/hyperedge/" id) nil)
          record (hyperedge->record stored)]
      (when-not (= (dissoc payload :hx/mint-id :hx/idempotency-key)
                   (dissoc (->hyperedge record) :hx/id))
        (refuse! :readback-mismatch :receipt))
      {:receipt (assoc receipt :verified? true) :record record})))
