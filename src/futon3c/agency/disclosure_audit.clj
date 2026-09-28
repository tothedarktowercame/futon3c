(ns futon3c.agency.disclosure-audit
  "Pure audit of the records that can substantiate disclosed choices.

   This can detect explicit citations without records, recorded negations with
   no effect, and effects with no correctly addressed routing job. It cannot
   detect a choice that was never disclosed or cited; absence of such a record
   is not evidence that no undisclosed choice was made."
  (:require [futon3c.agency.disclosure-record :as disclosure])
  (:import [java.nio.charset StandardCharsets]
           [java.util UUID]))

(defn routing-job-id [withdrawal-id]
  (str "invoke-disclosure-withdrawal-"
       (UUID/nameUUIDFromBytes
        (.getBytes (str withdrawal-id) StandardCharsets/UTF_8))))

(defn- value [m k]
  (or (get m k) (get-in m [:evidence/body k]) (get-in m [:hx/props k])))

(defn- record-id [m]
  (or (:id m) (:hx/id m) (:evidence/id m)))

(defn- withdrawal-target [m] (value m :target))

(defn- negates? [interpretation disclosure-id]
  (and (= disclosure-id (value interpretation :target))
       (or (= :withdraw (value interpretation :intent))
           (= "withdraw" (value interpretation :intent))
           (= :negation (value interpretation :kind)))))

(defn audit
  "Audit one job snapshot. Inputs are already-read plain records."
  [{:keys [job report-text disclosures withdrawals interpretations routing-jobs
           stored-act-ids]}]
  (let [withdrawals-by-target (group-by withdrawal-target withdrawals)
        routing-by-id (into {} (map (juxt #(or (:job-id %) (:id %)) identity)
                                     routing-jobs))
        statuses (mapv (fn [d]
                         (assoc d :status
                                (if (seq (get withdrawals-by-target (record-id d)))
                                  :withdrawn :standing)))
                       disclosures)
        unrecorded (disclosure/unrecorded-citations report-text stored-act-ids)
        negations (for [d disclosures
                        :let [did (record-id d)]
                        i interpretations
                        :when (and (negates? i did)
                                   (empty? (get withdrawals-by-target did)))]
                    {:reason :negation-without-effect
                     :disclosure-id did :interpretation-id (record-id i)})
        unrouted (for [d disclosures
                       w (get withdrawals-by-target (record-id d))
                       :let [wid (record-id w)
                             expected (routing-job-id wid)
                             routed (get routing-by-id expected)]
                       :when (not (and routed
                                       (= (:source-job d) (:bellback-of routed))
                                       (= (:author d) (:agent-id routed))))]
                   {:reason :effect-not-routed :disclosure-id (record-id d)
                    :withdrawal-id wid :expected-job-id expected})]
    {:job job
     :disclosures statuses
     :findings (vec (concat unrecorded negations unrouted))}))
