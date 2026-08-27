(ns futon3c.apm.memory-access-gate
  "One depositor-truth decision point for every memory-serving carrier."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]))

(def policy-version 1)

(def carrier-contract
  "The registry is the authority for channels which may cross a role boundary."
  {:search {:channel :search :purpose :role-memory-search}
   :cascade {:channel :cascade :purpose :memory-cascade}
   :shelf-materialization {:channel :shelf :purpose :snapshot-materialization}
   :inherited-repair {:channel :repair :purpose :inherited-repair-results}
   :prompt-assembly {:channel :prompt :purpose :role-prompt-assembly}})

(defn registered-carriers [] (set (keys carrier-contract)))

(defn candidate-id [memory]
  (or (:memory-id memory) (:memory/id memory) (:pattern-id memory)))

(defn- valid-provenance? [memory]
  (let [p (:provenance memory)
        frame (some->> (:depositor memory)
                       (re-matches #"^(f[0-9]+)-.+$") second)]
    (and (map? p)
         (every? #(and (string? %) (not (str/blank? %)))
                 ((juxt :campaign-id :frame-id :problem-id) p))
         (= frame (:frame-id p)))))

(defn may-serve?
  "Decide from the memory's persisted depositor provenance. Pattern records
   are not deposited memories; their supporting memory records are decided
   separately by the carrier which materializes them."
  [authority memory channel purpose]
  (let [id (candidate-id memory)
        holdout (:shelf/holdout authority)
        memory? (or (contains? memory :memory-id)
                    (contains? memory :memory/id))
        supports (:memory-support memory)]
    (cond
      (nil? holdout)
      {:allowed? true :reason :no-holdout :channel channel :purpose purpose}

      (not= :same-problem holdout)
      {:allowed? false :reason :unknown-holdout-policy :channel channel :purpose purpose}

      (contains? (set (:shelf/withheld-ids authority)) id)
      {:allowed? false :reason :authority-known-withheld
       :channel channel :purpose purpose}

      (and (not memory?) (seq supports))
      (let [support-decisions (mapv #(may-serve? authority % channel purpose)
                                    supports)]
        (if (every? :allowed? support-decisions)
          {:allowed? true :reason :supporting-memories-allowed
           :channel channel :purpose purpose}
          {:allowed? false :reason :supporting-memory-withheld
           :channel channel :purpose purpose
           :support-reasons (mapv :reason (remove :allowed? support-decisions))}))

      (not memory?)
      {:allowed? false :reason :unverifiable-memory-support
       :channel channel :purpose purpose}

      (not (and (string? id) (valid-provenance? memory)))
      {:allowed? false :reason :unverifiable-depositor-provenance
       :channel channel :purpose purpose}

      (= (:problem-id authority) (get-in memory [:provenance :problem-id]))
      {:allowed? false :reason :same-problem-depositor :channel channel :purpose purpose}

      :else
      {:allowed? true :reason :different-problem-depositor
       :channel channel :purpose purpose})))

(defn enforce-carrier
  "Return allowed candidates and content-addressed decision evidence."
  [carrier authority candidates]
  (let [{:keys [channel purpose] :as contract} (get carrier-contract carrier)]
    (when-not contract
      (throw (ex-info "unregistered memory carrier"
                      {:carrier carrier :registered (registered-carriers)})))
    (let [decisions (mapv (fn [candidate]
                            (merge {:candidate/id (candidate-id candidate)}
                                   (may-serve? authority candidate channel purpose)))
                          candidates)
          allowed (mapv first (filter (comp :allowed? second)
                                      (map vector candidates decisions)))
          excluded (->> decisions
                        (remove :allowed?)
                        (mapv #(select-keys % [:candidate/id :reason])))
          evidence {:policy/version policy-version
                    :authority/digest (machine/ledger-digest [authority])
                    :carrier carrier :channel channel :purpose purpose
                    :candidate-ids (mapv candidate-id candidates)
                    :excluded excluded}
          digest (machine/ledger-digest [evidence])]
      {:allowed allowed
       :excluded excluded
       :evidence (assoc evidence :decision/digest digest)})))
