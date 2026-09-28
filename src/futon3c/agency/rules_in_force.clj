(ns futon3c.agency.rules-in-force
  "Pure rule-family force projection with separately authorised withdrawals."
  (:require [futon3c.agency.overreach :as overreach]
            [futon3c.agency.rule-timeline :as timeline])
  (:import [java.time Instant]))

(defn- instant [value]
  (try (some-> value Instant/parse) (catch Exception _ nil)))

(defn- visible? [record t]
  (let [at (instant (:at record)) time (instant t)]
    (and at time (not (.isAfter at time)))))

(defn- ignored [record reason]
  {:record-id (:id record) :reason reason})

(defn- family-id [record]
  (let [stored (get-in record [:hx/props :rule/family])]
    (or (when stored
          (if (= :self stored) (:hx/id record) stored))
        ;; Schema-1 rule records predate :rule/family. Their timeline family
        ;; remains the stored family identity and keeps old fixtures readable.
        (get-in record [:hx/props :rule/timeline :family]))))

(defn- family-signer [records family]
  (some (fn [record]
          (when (= family (:hx/id record))
            (get-in record [:hx/props :act/stamp :signer])))
        records))

(defn- family-governs [records family]
  (or (some (fn [record]
              (when (= family (:hx/id record))
                (get-in record [:hx/props :rule/governs]))) records)
      []))

(defn- timeline-family [records]
  (some #(get-in % [:hx/props :rule/timeline :family]) records))

(defn- valid-reversal? [effect provisional]
  (and (= (:reverses effect) (:id provisional))
       (.isAfter ^Instant (instant (:at effect)) ^Instant (instant (:at provisional)))
       (or (= "joe" (:author effect))
           (= (:author effect) (:author provisional)))))

(def no-cover-reasons
  #{:no-grant :not-own-act :out-of-scope :grant-expired
    :grant-not-yet-valid :out-of-time :wrong-grantee :broken-delegation})

(defn rules-in-force-as-of
  "Project rule families at valid time T.

   Rule records are stored hyperedges carrying :rule/family. Withdrawals are
   plain act records. Existing timeline semantics are delegated unchanged to
   rule-timeline/as-of; this projection only subtracts whole families after an
   authorised family-targeted effect."
  [rule-records withdrawals grants t]
  (let [families (->> rule-records (keep family-id) distinct sort)
        family-set (set families)
        version-ids (set (keep :hx/id rule-records))
        visible-effects (filter #(visible? % t) withdrawals)
        interpretations (filter #(= :interpretation (:kind %)) visible-effects)
        effects (filter #(= :act/withdrawal (:kind %)) visible-effects)
        reversals (filter :reverses effects)
        ordinary (remove :reverses effects)
        initial {:in-force [] :ended [] :provisional [] :unresolved []
                 :ignored (mapv #(ignored % :interpretation-not-effect)
                                interpretations)}]
    (reduce
     (fn [result family]
       (let [records (filterv #(= family (family-id %)) rule-records)
             signer (family-signer records family)
             governs (set (family-governs records family))
             base (timeline/as-of records (timeline-family records) t)
             targeted (filter #(= family (:target %)) ordinary)
             classified
             (mapv (fn [effect]
                     [effect (when signer
                               (overreach/classify-act
                                (overreach/record->act effect signer) grants))])
                   targeted)
             provisional (->> classified
                              (filter (fn [[effect classification]]
                                        (and signer
                                             (not= signer (:author effect))
                                             (contains? governs (:author effect))
                                             (contains? no-cover-reasons
                                                        (get-in classification
                                                                [:finding :finding/reason])))))
                              (map first)
                              vec)
             active-provisional
             (filterv (fn [candidate]
                        (not-any? #(valid-reversal? % candidate) reversals))
                      provisional)
             ended-by (->> classified
                           ;; The grant check decides, not authorship: the "*"
                           ;; own-acts grant already covers only the signer, and a
                           ;; named grant lets another party withdraw (P10 (2)).
                           (filter (fn [[effect classification]]
                                     (and (= :effective (:status effect))
                                          (contains? #{:authorised :unverified-executor}
                                                     (:classification classification)))))
                           (map first)
                           (sort-by (juxt (comp instant :at) (comp str :id)))
                           last)
             unresolved (if signer [] targeted)
             provisional-ids (set (map :id provisional))
             ignored-effects
             (->> classified
                  (remove (fn [[effect classification]]
                            (or (nil? signer)
                                (= (:id effect) (:id ended-by))
                                (contains? provisional-ids (:id effect))
                                (and (= :effective (:status effect))
                                     (contains? #{:authorised :unverified-executor}
                                                (:classification classification))))))
                  (mapv (fn [[effect classification]]
                          (ignored effect
                                   (or (get-in classification [:finding :finding/reason])
                                       :unsupported-withdrawal)))))
             relevant-reversals (filter #(contains? provisional-ids (:reverses %)) reversals)
             bad-reversals (->> relevant-reversals
                                (remove (fn [effect]
                                          (some #(valid-reversal? effect %) provisional)))
                                (mapv #(ignored % :invalid-reversal)))
             result (-> result
                        (update :provisional into active-provisional)
                        (update :unresolved into unresolved)
                        (update :ignored into ignored-effects)
                        (update :ignored into bad-reversals))]
         (if ended-by
           (update result :ended conj {:family family :by (:id ended-by)})
           (update result :in-force conj {:family family :answer base}))))
     (let [unrelated (remove #(contains? family-set (:target %)) ordinary)
           target-ignored
           (mapv (fn [effect]
                   (ignored effect (if (contains? version-ids (:target effect))
                                     :targets-version :unknown-target)))
                 unrelated)
           orphan-reversals (remove #(some (fn [candidate]
                                             (= (:reverses %) (:id candidate))) ordinary)
                                    reversals)]
       (-> initial
           (update :ignored into target-ignored)
           (update :ignored into (map #(ignored % :invalid-reversal)
                                      orphan-reversals))))
     families)))
