(ns futon3c.agency.obligations
  "Pure P9 projection over already-read records.

   Promise history inputs are format-3 evidence rows. Outcomes are P5 evidence
   rows. Agreements and offers are their plain record forms. Every event whose
   event time is <= T applies at T; later events do not. Consequently an explicit
   release at T ends a debt at T.

   A plain :promise/released row ends a wait, not a checkable debt. Only a release
   carrying :release/basis :explicit discharges such a debt. Broken promise chains
   are returned only in :incomplete, never projected from their surviving rows."
  (:require [futon3c.agency.promise-history :as history])
  (:import [java.time Instant]))

(def creation-types #{:promise/park-made :promise/followup-enqueued})

(defn- instant [x]
  (try (some-> x str Instant/parse) (catch Exception _ nil)))

(defn- at-or-before? [row t]
  (when-let [at (instant (or (:evidence/at row)
                             (:agreement/at row)
                             (:at row)))]
    (not (.isAfter ^Instant at ^Instant t))))

(defn- promise-id [row]
  (let [body (:evidence/body row)]
    (or (:history/promise-id body) (:promise-id body)
        (:id body) (:followup-id body))))

(defn- decode-record [row]
  (try
    {:record (:record (history/payload row))}
    (catch Exception e
      {:error {:promise-id (promise-id row)
               :reason (or (:reason (ex-data e)) :invalid-history-payload)
               :record-id (:evidence/id row)}})))

(defn- outcome-promise-id [row]
  (get-in row [:evidence/body :promise-id]))

(defn- fact-ids [rows]
  (mapv :evidence/id rows))

(defn- incomplete [pid reason & [extra]]
  (merge {:obligation/id pid :reason reason} extra))

(defn- promise-row [pid creation lifecycle outcomes t]
  (let [rec (:record (decode-record creation))
        criterion (:fulfilment-criterion rec)
        deadline (:deadline rec)
        checkable? (or criterion deadline)
        explicit-release (some #(when (and (= :promise/released (:evidence/type %))
                                           (= :explicit (get-in % [:evidence/body :release/basis]))) %)
                               lifecycle)
        plain-release (some #(when (= :promise/released (:evidence/type %)) %) lifecycle)
        fulfilled (some #(when (= :promise/fulfilled (:evidence/type %)) %) outcomes)
        lapsed (some #(when (= :promise/lapsed (:evidence/type %)) %) outcomes)
        deadline-passed? (when-let [d (instant deadline)]
                           (not (.isAfter ^Instant d ^Instant t)))
        status (cond
                 explicit-release :released
                 fulfilled (if lapsed :completed-late :completed)
                 lapsed :overdue
                 deadline-passed? :overdue
                 :else :open)
        facts (cond-> (into (fact-ids lifecycle) (fact-ids outcomes))
                (and checkable? deadline-passed? (nil? lapsed) (nil? fulfilled))
                (conj :outcome-unknown))]
    {:row {:obligation/id pid
           :source/id (:evidence/id creation)
           :source/kind :promise
           :debtor (:agent rec)
           :creditor (:beneficiary rec)
           :deliverable (or criterion (:payload rec))
           :due-at deadline
           :status status
           :authority (or (:act/stamp rec) :unrecorded)
           :as-of (str t)
           :facts facts}
     :checkable? checkable?
     :plain-release? (boolean plain-release)}))

(defn- agreement-row [agreement offer t]
  {:obligation/id (:id agreement)
   :source/id (:id agreement)
   :source/kind :agreement
   :debtor (:agreement/offeror agreement)
   :creditor "joe"
   :deliverable (:agreement/scope agreement)
   :due-at nil
   :status :open
   :authority (:act/stamp agreement)
   :as-of (str t)
   :facts [(:id offer) (:id agreement)]})

(defn obligations-as-of
  "Project obligations visible at instant T for AGENT-ID.

   Returns {:owes :owed :unchecked :incomplete :ignored}. Closed checkable
   promises are retained in :ignored with their final :status for audit. An
   unchecked wait is removed after any release. Agreements are open and undated;
   :grant-until inside their scope is authority expiry and is never a due date."
  [{:keys [promise-history promise-outcomes agreements offers]} agent-id t]
  (let [t (or (instant t) (throw (ex-info "Invalid as-of instant" {:reason :invalid-as-of})))
        visible-history (vec (filter #(at-or-before? % t) promise-history))
        visible-outcomes (vec (filter #(at-or-before? % t) promise-outcomes))
        chain-issues (history/check-chains visible-history)
        bad-pids (set (keep :promise-id chain-issues))
        by-promise (group-by promise-id visible-history)
        outcomes-by-promise (group-by outcome-promise-id visible-outcomes)
        decoded-errors (keep (fn [row]
                               (when (creation-types (:evidence/type row))
                                 (:error (decode-record row))))
                             visible-history)
        bad-pids (into bad-pids (keep :promise-id decoded-errors))
        promise-results
        (reduce-kv
         (fn [acc pid rows]
           (if (contains? bad-pids pid)
             acc
             (if-let [creation (first (sort-by :evidence/at
                                               (filter #(creation-types (:evidence/type %)) rows)))]
               (conj acc (promise-row pid creation rows (get outcomes-by-promise pid []) t))
               acc))) [] by-promise)
        promise-open (mapv :row (filter (fn [{:keys [row checkable?]}]
                                          (and checkable?
                                               (not (contains? #{:released :completed :completed-late}
                                                               (:status row)))))
                                        promise-results))
        unchecked (mapv :row (filter (fn [{:keys [checkable? plain-release?]}]
                                       (and (not checkable?) (not plain-release?)))
                                     promise-results))
        closed (mapv :row (filter (fn [{:keys [row checkable?]}]
                                    (and checkable?
                                         (contains? #{:released :completed :completed-late}
                                                    (:status row))))
                                  promise-results))
        visible-agreements (filter #(at-or-before? % t) agreements)
        offers-by-id (into {} (map (juxt :id identity)) offers)
        agreement-pairs (keep (fn [a] (when-let [o (get offers-by-id (:agreement/offer a))]
                                        [a o])) visible-agreements)
        agreement-rows (mapv (fn [[a o]] (agreement-row a o t)) agreement-pairs)
        active (into promise-open agreement-rows)
        missing-beneficiary (for [row promise-open :when (nil? (:creditor row))]
                              (incomplete (:obligation/id row) :no-beneficiary
                                          {:source/id (:source/id row)}))
        no-due (for [row agreement-rows]
                 (incomplete (:obligation/id row) :no-due-at {:source/id (:source/id row)}))
        missing-offers (for [a visible-agreements
                             :when (nil? (get offers-by-id (:agreement/offer a)))]
                         (incomplete (:id a) :unknown-offer
                                     {:source/id (:id a) :offer (:agreement/offer a)}))
        incompletes (vec (concat chain-issues decoded-errors missing-beneficiary no-due missing-offers))]
    {:owes (vec (filter #(= agent-id (:debtor %)) active))
     :owed (vec (filter #(= agent-id (:creditor %)) active))
     :unchecked unchecked
     :incomplete incompletes
     :ignored closed}))
