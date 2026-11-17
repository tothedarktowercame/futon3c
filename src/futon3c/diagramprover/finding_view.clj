(ns futon3c.diagramprover.finding-view
  "Pure temporal views over supplied Arxana records; no store access."
  (:require [futon3c.arxana.records :as records])
  (:import [java.time Instant]))

(defn- instant [value]
  (try (Instant/parse value)
       (catch Exception _
         (throw (ex-info "Valid event-time/as-of instant required"
                         {:error :arxana.conflict/invalid-time :value value})))))

(defn supersedes-record
  "Use the registered successor/predecessor roles. Top-level :event-time is
  outside clusion identity, as in arxana/formal_f2.clj application-record."
  [successor-id predecessor-id event-time]
  (instant event-time)
  (records/make-clusion
   {:type-revision [:supersedes 1]
    :endpoints [{:role :successor :kind :clusion :target {:clusion/id successor-id}}
                {:role :predecessor :kind :clusion :target {:clusion/id predecessor-id}}]
    :asserted-content {} :event-time event-time}))

(defn- target [record role]
  (some #(when (= role (:role %)) (get-in % [:target :clusion/id])) (:endpoints record)))

(defn- visible? [record at]
  (not (.isAfter ^Instant (instant (:event-time record)) at)))

(defn finding-view
  "Read the named finding's evaluation at as-of. Supersession chains are read
  link by link: ONLY a direct predecessor matching rule-a/rule-b makes it stale.
  Latest visible residual wins (ties by id for deterministic input-order freedom).
  :display is the inner keyword, or {:stale keyword}; evaluation is never changed.
  A residual is returned even on a met/settled finding but only affects :failing.
  Relevant temporal records without valid event-time are refused, not guessed.
  The finding itself must be supplied; this is not a store-valid-time lookup."
  [record-set finding-id as-of]
  (let [at (instant as-of)
        finding (some #(when (and (= finding-id (:id %))
                                  (= [:conflict/finding 1] (:type-revision %))) %) record-set)]
    (when-not finding
      (throw (ex-info "Finding is absent" {:error :arxana.conflict/finding-absent :id finding-id})))
    (let [rules (set (map #(target finding %) [:rule-a :rule-b]))
          supersessions (filter #(and (= [:supersedes 1] (:type-revision %))
                                      (contains? rules (target % :predecessor))
                                      (visible? % at)) record-set)
          residuals (filter #(and (= [:residual/accepted 1] (:type-revision %))
                                  (= finding-id (target % :finding))
                                  (visible? % at)) record-set)
          residual (last (sort-by #(vector (instant (:event-time %)) (:id %)) residuals))
          evaluation (get-in finding [:asserted-content :evaluation])
          _ (when-not (#{:met :settled :unknown :failing :failing-accepted} evaluation)
              (throw (ex-info "Unknown evaluation" {:error :arxana.conflict/invalid-evaluation
                                                     :evaluation evaluation})))
          inner (if (and (= :failing evaluation) residual) :failing-accepted evaluation)
          because (vec (sort (distinct (map :id supersessions))))]
      {:evaluation evaluation :residual residual :stale? (boolean (seq because))
       :stale-because because :display (if (seq because) {:stale inner} inner)})))

(defn stale-findings [record-set as-of]
  (instant as-of)
  (set (keep (fn [r]
               (when (and (= [:conflict/finding 1] (:type-revision r))
                          (:stale? (finding-view record-set (:id r) as-of))) (:id r))) record-set)))
