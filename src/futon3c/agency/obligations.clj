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
        explicit-releases (filterv #(and (= :promise/released (:evidence/type %))
                                         (= :explicit (get-in % [:evidence/body :release/basis])))
                                   lifecycle)
        invalid-release (some #(when-not (contains? #{:creditor :debtor}
                                                     (get-in % [:evidence/body :release/role])) %)
                              explicit-releases)
        explicit-release (first explicit-releases)
        release-role (get-in explicit-release [:evidence/body :release/role])
        plain-release (some #(when (= :promise/released (:evidence/type %)) %) lifecycle)
        fulfilled (some #(when (= :promise/fulfilled (:evidence/type %)) %) outcomes)
        lapsed (some #(when (= :promise/lapsed (:evidence/type %)) %) outcomes)
        check (first (sort-by :evidence/at
                              #(compare %2 %1)
                              (filter #(= :promise/fulfilment-check (:evidence/type %)) outcomes)))
        check-body (:evidence/body check)
        check-verdict (:verdict check-body)
        deadline-passed? (when-let [d (instant deadline)]
                           (not (.isAfter ^Instant d ^Instant t)))
        fulfilled-after-check? (and fulfilled check
                                    (.isAfter ^Instant (instant (:evidence/at fulfilled))
                                              ^Instant (instant (:evidence/at check))))
        status (cond
                 invalid-release :invalid-release
                 explicit-release (if (= :creditor release-role) :released :abandoned)
                 fulfilled (if (or lapsed
                                   (and fulfilled-after-check?
                                        (contains? #{:unfulfilled :unable-to-determine}
                                                   check-verdict)))
                             :completed-late :completed)
                 (= :fulfilled check-verdict) (if lapsed :completed-late :completed)
                 (= :unfulfilled check-verdict) :overdue
                 (= :unable-to-determine check-verdict) (if deadline-passed? :overdue :open)
                 lapsed :overdue
                 deadline-passed? :overdue
                 :else :open)
        facts (into (fact-ids lifecycle) (fact-ids outcomes))
        check-summary (when check
                        {:id (:evidence/id check) :verdict check-verdict
                         :unable-reason (:unable-reason check-body)
                         :due-at (:due-at check-body)})
        check-incomplete (cond
                           (= :unable-to-determine check-verdict)
                           (incomplete pid :unable-to-determine
                                       {:record-id (:evidence/id check)
                                        :unable-reason (:unable-reason check-body)})
                           (and criterion (nil? check)
                                (or deadline-passed? (nil? deadline)))
                           (incomplete pid :check-pending {:source/id (:evidence/id creation)}))]
    {:row (cond-> {:obligation/id pid
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
            check-summary (assoc :check check-summary))
     :checkable? checkable?
     :error (when invalid-release
              (incomplete pid :invalid-release {:record-id (:evidence/id invalid-release)}))
     :check-incomplete check-incomplete
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
  [{:keys [promise-history promise-outcomes agreements offers reader-incomplete]} agent-id t]
  (let [t (or (instant t) (throw (ex-info "Invalid as-of instant" {:reason :invalid-as-of})))
        visible-history (vec (filter #(at-or-before? % t) promise-history))
        visible-outcomes (vec (filter #(at-or-before? % t) promise-outcomes))
        chain-issues (history/check-chains visible-history)
        bad-pids (set (keep :promise-id chain-issues))
        by-promise (group-by promise-id visible-history)
        outcomes-by-promise (group-by outcome-promise-id visible-outcomes)
        creation-pids (set (keep (fn [row]
                                   (when (creation-types (:evidence/type row))
                                     (promise-id row)))
                                 visible-history))
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
        promise-open (mapv :row (filter (fn [{:keys [row checkable? error]}]
                                          (and checkable? (nil? error)
                                               (not (contains? #{:released :abandoned :completed :completed-late}
                                                               (:status row)))))
                                        promise-results))
        unchecked (mapv :row (filter (fn [{:keys [checkable? plain-release? error]}]
                                       (and (nil? error) (not checkable?) (not plain-release?)))
                                     promise-results))
        closed (mapv :row (filter (fn [{:keys [row checkable? error]}]
                                    (and checkable? (nil? error)
                                         (contains? #{:released :abandoned :completed :completed-late}
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
        invalid-releases (keep :error promise-results)
        check-incompletes (keep :check-incomplete promise-results)
        orphan-checks (for [row visible-outcomes
                            :when (= :promise/fulfilment-check (:evidence/type row))
                            :let [pid (outcome-promise-id row)]
                            :when (not (contains? creation-pids pid))]
                        (incomplete pid :orphan-check {:record-id (:evidence/id row)}))
        incompletes (vec (concat reader-incomplete chain-issues decoded-errors invalid-releases
                                 check-incompletes orphan-checks
                                 missing-beneficiary no-due missing-offers))
        party? #(or (= agent-id (:debtor %)) (= agent-id (:creditor %)))]
    ;; :incomplete stays unfiltered: a broken chain or unreadable creation may
    ;; not reveal its parties, so it is shown to every asker rather than hidden.
    {:owes (vec (filter #(= agent-id (:debtor %)) active))
     :owed (vec (filter #(= agent-id (:creditor %)) active))
     :unchecked (vec (filter party? unchecked))
     :incomplete incompletes
     :ignored (vec (filter party? closed))}))
