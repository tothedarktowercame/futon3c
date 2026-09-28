(ns futon3c.agency.negation-interpretation
  "Pure target resolution for a negating interpretation.

   Resolution uses only an explicit act id or the cardinality of the standing
   disclosure population already bound to the operator turn's source jobs.
   Fragment prose is never compared with disclosure prose."
  (:require [clojure.string :as str]))

(defn- disclosure-id [record]
  (or (:id record) (:hx/id record) (:evidence/id record)))

(defn resolve-negation-target
  "Resolve FRAGMENT against STANDING-DISCLOSURES without reading prose."
  [{:keys [fragment-id fragment-text target]} standing-disclosures]
  (let [ids (->> standing-disclosures (map disclosure-id) (filter string?) distinct sort vec)
        explicit (when-not (str/blank? (some-> target str)) (str target))]
    (cond
      explicit
      (if (some #{explicit} ids)
        {:fragment-id fragment-id :fragment-text fragment-text
         :target explicit :resolution :explicit-id}
        {:fragment-id fragment-id :fragment-text fragment-text
         :resolution :target-not-in-source-jobs})

      (= 1 (count ids))
      {:fragment-id fragment-id :fragment-text fragment-text
       :target (first ids) :resolution :single-standing}

      (empty? ids)
      {:fragment-id fragment-id :fragment-text fragment-text
       :resolution :target-unresolved}

      :else
      {:fragment-id fragment-id :fragment-text fragment-text
       :resolution :target-ambiguous :candidates ids})))
