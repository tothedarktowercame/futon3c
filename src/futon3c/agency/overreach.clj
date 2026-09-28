(ns futon3c.agency.overreach
  "Pure retrospective classification of stamped acts against stored grants.

   Scanner acts have :act/id, :act/kind, :act/rule-id, :act/at,
   :act/target-signer and :act/stamp. `record->act` adapts plain minted-act
   records to this shape. `scan-report` returns one classification per act;
   `scan` retains the P4-1 findings-only interface."
  (:require [futon3c.agency.act-stamp :as act-stamp]
            [futon3c.agency.grant-record :as grant-record])
  (:import [java.time Instant]))

(defn- finding [act reason detail]
  {:finding/act-id (:act/id act)
   :finding/reason reason
   :finding/detail detail})

(defn record->act
  "Adapt a plain stamped act RECORD. TARGET-SIGNER is required when the target
   is another act; selections default it to their exact-seat agent."
  ([record] (record->act record nil))
  ([record target-signer]
   {:act/id (:id record)
    :act/kind (:kind record)
    :act/rule-id (:rule-id record)
    :act/at (:at record)
    :act/effect-status (:status record)
    :act/dispatch-edge (get-in record [:act/stamp :authority :dispatch-edge])
    :act/target-signer (or target-signer
                           (when (= :pattern-card/selection (:kind record))
                             (:agent record)))
    :act/stamp (:act/stamp record)}))

(defn- grants-by-id [grants]
  (into {} (keep (fn [grant]
                   (when-let [id (or (:hx/id grant) (:act/id grant))]
                     [id grant]))) grants))

(defn- claimed-chain [leaf by-id]
  (loop [node leaf result [leaf] seen #{(:hx/id leaf)}]
    (if-let [parent-id (get-in node [:hx/props :grant/parent])]
      (if (contains? seen parent-id)
        result
        (if-let [parent (get by-id parent-id)]
          (recur parent (conj result parent) (conj seen parent-id))
          result))
      result)))

(defn- instant [value]
  (try (some-> value Instant/parse) (catch Exception _ nil)))

(defn- props [grant]
  (if (= :grant/record (:hx/type grant)) (:hx/props grant) grant))

(defn- time-reason
  "Preserve P4-1's early/expired vocabulary after the shared query reports its
   coarser :out-of-time. This classifies the supplied chain; it does not decide
   coverage."
  [records at]
  (let [t (instant at)
        intervals (map (comp :grant/interval props) records)]
    (cond
      (nil? t) :act-time-unknown
      (some (fn [{:keys [from]}]
              (when-let [start (instant from)] (.isBefore t start))) intervals)
      :grant-not-yet-valid
      (some (fn [{:keys [until]}]
              (when-let [end (instant until)] (not (.isBefore t end)))) intervals)
      :grant-expired
      :else :out-of-time)))

(defn- map-cover-reason [act records reason]
  (case reason
    :no-candidate :no-grant
    :out-of-time (time-reason records (:act/at act))
    :scope-unchecked :out-of-scope
    :out-of-scope :out-of-scope
    :not-own-act :not-own-act
    :broken-parent-chain :broken-delegation
    :parent-cycle :broken-delegation
    :delegation-identity-mismatch :broken-delegation
    :scope-exceeds-parent :broken-delegation
    :interval-exceeds-parent :broken-delegation
    :wildcard-not-delegable :broken-delegation
    :interpretation-not-grant :interpretation-as-grant
    reason))

(defn- cover-answer [act grants stamp]
  (let [grant-id (get-in stamp [:authority :grant])
        targets (remove nil? [(:act/kind act) (:act/rule-id act)])
        query (fn [target]
                (grant-record/grant-covers?
                 grants (:executor stamp) target (:act/at act)
                 {:leaf-id grant-id
                  :target-signer (:act/target-signer act)
                  :effect-status (:act/effect-status act)}))
        answers (mapv query targets)]
    (or (first (filter #(= :granted (:status %)) answers))
        (first answers)
        {:status :no-grant :reason :out-of-scope})))

(defn- overreach [act reason detail]
  {:act/id (:act/id act)
   :classification :overreach
   :finding (finding act reason detail)})

(defn classify-act
  "Classify one act as authorised, overreach, unverified executor, or outside
   coverage. Only overreach and unverified-executor carry findings."
  [act grants]
  (if (nil? (:act/stamp act))
    {:act/id (:act/id act) :classification :outside-coverage
     :reason :missing-act-stamp}
    (let [stamp (:act/stamp act)
          authority (:authority stamp)]
      (if (and (map? authority) (contains? authority :interpretation))
        (overreach act :interpretation-as-grant {:authority authority})
        (if (contains? authority :dispatch-edge)
          (if (and (= :disclosure/choice (:act/kind act))
                   (= (:dispatch-edge authority) (:act/dispatch-edge act)))
            (try
              (act-stamp/validate! stamp #{:dispatch-edge})
              {:act/id (:act/id act) :classification :authorised}
              (catch clojure.lang.ExceptionInfo e
                (overreach act (:reason (ex-data e))
                           {:field (:field (ex-data e))})))
            (overreach act :authority-kind-not-allowed
                       {:authority :dispatch-edge :act/kind (:act/kind act)}))
        (try
          (let [stamp (act-stamp/validate! stamp)
                grant-id (get-in stamp [:authority :grant])
                by-id (grants-by-id grants)
                operator? (= {:operator true} (:authority stamp))
                leaf (get by-id grant-id)]
            (cond
              operator?
              (if (= :declared (:executor-basis stamp))
                {:act/id (:act/id act) :classification :unverified-executor
                 :finding (finding act :unverified-executor
                                   {:executor (:executor stamp) :basis :declared})}
                {:act/id (:act/id act) :classification :authorised})

              (nil? leaf)
              (overreach act :no-grant {:authority grant-id
                                        :explanation :grant-not-found})

              (nil? (instant (:act/at act)))
              (overreach act :act-time-unknown {:at (:act/at act)})

              :else
              (let [answer (cover-answer act grants stamp)]
                (if (= :granted (:status answer))
                  (if (= :declared (:executor-basis stamp))
                    {:act/id (:act/id act) :classification :unverified-executor
                     :finding (finding act :unverified-executor
                                       {:executor (:executor stamp) :basis :declared})}
                    {:act/id (:act/id act) :classification :authorised})
                  (let [reason (if (and (= :no-candidate (:reason answer))
                                        (not= "*" (get-in leaf [:hx/props :grant/grantee]))
                                        (not= (:executor stamp)
                                              (get-in leaf [:hx/props :grant/grantee])))
                                 :wrong-grantee
                                 (map-cover-reason act (claimed-chain leaf by-id)
                                                   (:reason answer)))]
                    (overreach act reason {:authority grant-id
                                           :grant-reason (:reason answer)}))))))
          (catch clojure.lang.ExceptionInfo e
            (overreach act (:reason (ex-data e))
                       {:field (:field (ex-data e))}))))))))

(defn scan-report
  "Return one classification for every act, including unstamped history."
  [acts grants]
  (mapv #(classify-act % grants) acts))

(defn scan
  "Return findings for overreach and unverified executors. Authorised acts and
   unstamped acts have no finding; use `scan-report` to list all classes."
  [acts grants]
  (into [] (keep :finding) (scan-report acts grants)))
