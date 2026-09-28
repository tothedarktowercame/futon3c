(ns futon3c.agency.pattern-card-acts
  "Pure valid-time projection for pattern-card selections and withdrawals."
  (:import [java.time Instant]))

(defn- instant [stamp]
  (try
    (when stamp (Instant/parse stamp))
    (catch Exception _ nil)))

(defn- at-or-before? [record t]
  (when-let [at (instant (:at record))]
    (not (.isAfter ^Instant at ^Instant t))))

(defn- before? [a b]
  (and (instant a)
       (instant b)
       (.isBefore ^Instant (instant a) ^Instant (instant b))))

(defn- ignored [record reason]
  {:record-id (:id record) :reason reason})

(defn- selection? [record]
  (= :pattern-card/selection (:kind record)))

(defn- withdrawal? [record]
  (= :act/withdrawal (:kind record)))

(defn- exact-seat? [record agent-id session-id]
  (and (= agent-id (:agent record))
       (= session-id (:session record))))

(defn- valid-reversal? [effect provisional]
  (and (= (:reverses effect) (:id provisional))
       (before? (:at provisional) (:at effect))
       (or (= "joe" (:author effect))
           (= (:author provisional) (:author effect)))))

(defn card-as-of
  "Project one exact seat's active pattern card at valid time t.

   records are plain maps of these minimal shapes:

   selection:
     {:id ID :kind :pattern-card/selection :author AUTHOR :agent AGENT
      :session SESSION :at INSTANT :pattern-id PATTERN-ID}

   withdrawal effect:
     {:id ID :kind :act/withdrawal :author AUTHOR :at INSTANT :target ID
      :status (:effective | :provisional)
      :basis {:kind (:self | :grant | :provisional-interpretation) ...}
      :reverses EFFECT-ID-OR-NIL}

   interpretation:
     {:id ID :kind :interpretation :version VERSION :intent :withdraw
      :target ID ...}

   Interpretations never end a selection. The return value is
   {:active SELECTION-OR-NIL :provisional [EFFECT ...]
    :ignored [{:record-id ID :reason KEYWORD} ...]}."
  [records agent-id session-id t]
  (let [as-of (instant t)
        selections (filter selection? records)
        selection-by-id (into {} (map (juxt :id identity)) selections)
        local-selections (filter #(exact-seat? % agent-id session-id) selections)
        visible-local (if as-of
                        (filter #(at-or-before? % as-of) local-selections)
                        [])
        candidate (last (sort-by (juxt (comp instant :at) (comp str :id))
                                 visible-local))
        effects (if as-of
                  (filter #(and (withdrawal? %) (at-or-before? % as-of)) records)
                  [])
        relevant? (fn [effect]
                    (let [target (selection-by-id (:target effect))]
                      (or (nil? target)
                          (exact-seat? target agent-id session-id))))
        local-effects (filter relevant? effects)
        classified
        (map (fn [effect]
               (let [target (selection-by-id (:target effect))]
                 (cond
                   (nil? target) [effect :ignored :unknown-target]
                   (before? (:at effect) (:at target))
                   [effect :ignored :effect-before-target]
                   (:reverses effect) [effect :reversal nil]
                   (= :grant (get-in effect [:basis :kind]))
                   [effect :ignored :grant-check-not-implemented]
                   (and (= :self (get-in effect [:basis :kind]))
                        (not= (:author effect) (:author target)))
                   [effect :ignored :not-author]
                   (= :provisional (:status effect)) [effect :provisional nil]
                   (and (= :effective (:status effect))
                        (= :self (get-in effect [:basis :kind])))
                   [effect :effective nil]
                   :else [effect :ignored :unsupported-withdrawal])))
             local-effects)
        reversals (map first (filter #(= :reversal (second %)) classified))
        provisionals (->> classified
                          (filter #(= :provisional (second %)))
                          (map first)
                          (remove (fn [provisional]
                                    (some #(valid-reversal? % provisional) reversals)))
                          vec)
        effective-targets (->> classified
                               (filter #(= :effective (second %)))
                               (map (comp :target first))
                               set)
        ignored-effects (->> classified
                             (filter #(= :ignored (second %)))
                             (mapv (fn [[effect _ reason]]
                                     (ignored effect reason))))]
    {:active (when-not (contains? effective-targets (:id candidate)) candidate)
     :provisional provisionals
     :ignored ignored-effects}))
