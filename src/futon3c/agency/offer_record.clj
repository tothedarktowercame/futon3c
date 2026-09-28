(ns futon3c.agency.offer-record
  "Pure validation, storage mapping, and valid-time projection for offers.

   An option scope may contain only a description: an offer is a proposal, not
   authority. Such a scope can be accepted and recorded, but it cannot later
   cover a grant-checked act until it has checkable act kinds or rule ids.

   `active-offers-as-of` accepts offer records, generic withdrawal effects, and
   schema-1 agreement records using :agreement/offer and :agreement/at. Stored effects are assumed
   to have passed their write-path authority checks. Effective effects remain
   final; provisional effects end an offer until a valid later reversal by the
   provisional author or Joe, matching pattern-card valid-time semantics."
  (:require [clojure.string :as str]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp])
  (:import [java.time Instant]))

(def offer-type :offer/record)
(def withdrawal-type :act/withdrawal)
(def agreement-type :agreement/record)

(defn- refuse! [reason field]
  (throw (ex-info "Invalid offer record" {:reason reason :field field})))

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn- act-id? [value]
  (and (text? value) (str/starts-with? value "act:")))

(defn- parse-instant! [value reason field]
  (try
    (Instant/parse value)
    (catch Exception _ (refuse! reason field))))

(defn validate!
  "Return a valid plain offer or throw ex-info with typed :reason and :field."
  [record]
  (when-not (map? record) (refuse! :invalid-record :record))
  (when-not (= offer-type (:kind record)) (refuse! :wrong-record-kind :kind))
  (when-not (act-id? (:id record)) (refuse! :invalid-act-id :id))
  (when-not (text? (:author record)) (refuse! :missing-author :author))
  (when-not (text? (:addressee record))
    (refuse! :missing-addressee :addressee))
  (let [seat (:seat record)]
    (when-not (and (map? seat) (text? (:agent seat)) (text? (:session seat)))
      (refuse! :missing-seat :seat)))
  (when (nil? (:at record)) (refuse! :missing-at :at))
  (let [at (parse-instant! (:at record) :invalid-at :at)]
    (when-let [until (:until record)]
      (let [end (parse-instant! until :invalid-until :until)]
        (when-not (.isBefore ^Instant at ^Instant end)
          (refuse! :invalid-interval :until)))))
  (let [offer-at (parse-instant! (:at record) :invalid-at :at)
        options (:options record)]
    (when-not (and (vector? options) (seq options))
      (refuse! :no-options :options))
    (doseq [option options]
      (when-not (text? (:option/id option))
        (refuse! :missing-option-id :option/id))
      (let [scope (:option/scope option)]
        (when-not (map? scope)
          (refuse! :missing-option-scope :option/scope))
        (when (contains? scope :act-kinds)
          (when-not (and (vector? (:act-kinds scope))
                         (every? keyword? (:act-kinds scope)))
            (refuse! :invalid-act-kinds :option/scope)))
        (when (contains? scope :rule-ids)
          (when-not (and (vector? (:rule-ids scope))
                         (every? string? (:rule-ids scope)))
            (refuse! :invalid-rule-ids :option/scope)))
        (when-let [grant-until (:grant-until scope)]
          (let [until (parse-instant! grant-until :invalid-grant-until
                                      :option/scope)]
            (when-not (.isBefore ^Instant offer-at ^Instant until)
              (refuse! :invalid-grant-until :option/scope))))))
    (when-not (= (count options) (count (set (map :option/id options))))
      (refuse! :duplicate-option-ids :options)))
  (when-not (contains? record :act/stamp)
    (refuse! :missing-act-stamp :act/stamp))
  (act-stamp/validate! (:act/stamp record))
  (when-not (= (:author record) (get-in record [:act/stamp :signer]))
    (refuse! :stamp-signer-mismatch :act/stamp))
  (when (contains? record :act/harness)
    (act-harness/validate! (:act/harness record)))
  record)

(defn- display-label [label]
  (let [clean (-> (if (string? label) label "")
                  (str/replace #"\p{C}" " ")
                  (str/replace #"\s+" " ")
                  str/trim)]
    (subs clean 0 (min 120 (count clean)))))

(defn display-lines
  "Render OFFER's structured choices for the operator before acceptance.
   A line describes grant authority only when its scope is checkable and has
   the finite :grant-until required by DERIVE-2 item 7. Labels are display text,
   never authority, and are flattened and capped before rendering."
  [offer]
  (let [offer (validate! offer)
        id (:id offer)]
    (into [(format "offer %s from %s (reply yes <n>, or yes %s <n>):"
                   id (:author offer) id)]
          (map (fn [option]
                 (let [scope (:option/scope option)
                       checkable? (or (seq (:act-kinds scope))
                                      (seq (:rule-ids scope)))
                       grant? (and checkable? (:grant-until scope))]
                   (format "  %s  %s  — %s"
                           (:option/id option)
                           (display-label (:option/label option))
                           (if grant?
                             (str "grants: act-kinds " (pr-str (vec (:act-kinds scope)))
                                  " rule-ids " (pr-str (vec (:rule-ids scope)))
                                  " until " (:grant-until scope))
                             "agreement only, no grant"))))
               (:options offer)))))

(defn record->hyperedge
  "Map a validated offer to a schema-1 hyperedge. :at remains in props because
   futon1b LIST responses do not return :hx/valid-time."
  [record]
  (let [record (validate! record)]
    {:hx/id (:id record)
     :hx/type offer-type
     :hx/valid-time (:at record)
     :hx/endpoints [(str "agent:" (get-in record [:seat :agent]))
                    (str "session:" (get-in record [:seat :session]))
                    (str "agent:" (:addressee record))]
     :hx/props (-> record
                   (dissoc :id :kind)
                   (assoc :offer/schema 1))}))

(defn hyperedge->record
  "Map one schema-1 offer hyperedge back to its validated plain record."
  [hyperedge]
  (when-not (= offer-type (:hx/type hyperedge))
    (refuse! :wrong-record-kind :hx/type))
  (when-not (= 1 (get-in hyperedge [:hx/props :offer/schema]))
    (refuse! :unsupported-schema :offer/schema))
  (let [props (:hx/props hyperedge)
        record (assoc (dissoc props :offer/schema :at)
                      :id (:hx/id hyperedge)
                      :kind offer-type
                      :at (or (:hx/valid-time hyperedge) (:at props)))]
    (validate! record)))

(defn- instant [stamp]
  (try (when stamp (Instant/parse stamp)) (catch Exception _ nil)))

(defn- at-or-before? [record t]
  (when-let [at (instant (:at record))]
    (not (.isAfter ^Instant at ^Instant t))))

(defn- before? [a b]
  (and (instant a) (instant b)
       (.isBefore ^Instant (instant a) ^Instant (instant b))))

(defn- valid-reversal? [effect provisional]
  (and (= (:reverses effect) (:id provisional))
       (before? (:at provisional) (:at effect))
       (or (= "joe" (:author effect))
           (= (:author provisional) (:author effect)))))

(defn- ignored [record reason]
  {:record-id (:id record) :reason reason})

(defn active-offers-as-of
  "Return {:offers [...], :ignored [...]} for exact SEAT at valid time T.
   Visibility is half-open at :until. Withdrawals and agreements never delete
   offers; they only remove them from this as-of projection."
  [records seat t]
  (let [as-of (instant t)
        offers (filter #(= offer-type (:kind %)) records)
        offers-by-id (into {} (map (juxt :id identity)) offers)
        local-offers (filter #(= seat (:seat %)) offers)
        visible-by-time (if as-of
                          (filter (fn [offer]
                                    (and (at-or-before? offer as-of)
                                         (or (nil? (:until offer))
                                             (.isBefore ^Instant as-of
                                                        ^Instant (instant (:until offer))))))
                                  local-offers)
                          [])
        effects (if as-of
                  (filter #(and (= withdrawal-type (:kind %))
                                (at-or-before? % as-of)) records)
                  [])
        relevant-effects (filter (fn [effect]
                                   (let [target (offers-by-id (:target effect))]
                                     (or (nil? target) (= seat (:seat target)))))
                                 effects)
        classified (map (fn [effect]
                          (let [target (offers-by-id (:target effect))]
                            (cond
                              (nil? target) [effect :ignored :unknown-target]
                              (before? (:at effect) (:at target))
                              [effect :ignored :effect-before-target]
                              (:reverses effect) [effect :reversal nil]
                              (= :provisional (:status effect))
                              [effect :provisional nil]
                              (= :effective (:status effect))
                              [effect :effective nil]
                              :else [effect :ignored :invalid-status])))
                        relevant-effects)
        reversals (map first (filter #(= :reversal (second %)) classified))
        provisionals (map first (filter #(= :provisional (second %)) classified))
        active-provisionals (remove (fn [provisional]
                                      (some #(valid-reversal? % provisional) reversals))
                                    provisionals)
        withdrawn (set (concat
                        (map :target active-provisionals)
                        (map (comp :target first)
                             (filter #(= :effective (second %)) classified))))
        provisional-by-id (into {} (map (juxt :id identity) provisionals))
        bad-reversals (->> reversals
                           (remove #(some-> (provisional-by-id (:reverses %))
                                            (->> (valid-reversal? %))))
                           (map #(ignored % :invalid-reversal)))
        accepted (if as-of
                   (->> records
                        (filter #(and (= agreement-type (:kind %))
                                      (when-let [at (instant (:agreement/at %))]
                                        (not (.isAfter ^Instant at ^Instant as-of)))))
                        (keep #(when (contains? offers-by-id (:agreement/offer %))
                                 (:agreement/offer %)))
                        set)
                   #{})
        ignored-effects (concat
                         (->> classified
                              (filter #(= :ignored (second %)))
                              (map (fn [[effect _ reason]] (ignored effect reason))))
                         bad-reversals)]
    {:offers (->> visible-by-time
                  (remove #(or (contains? withdrawn (:id %))
                               (contains? accepted (:id %))))
                  (sort-by (juxt (comp instant :at) (comp str :id)))
                  vec)
     :ignored (vec ignored-effects)}))
