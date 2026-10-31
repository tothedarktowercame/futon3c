(ns futon3c.xiang.relationship-review
  "Produce review candidates when operator text concerns work other than the
  current clock target.  This records the observation; it never changes the
  clock or asserts what the relationship means."
  (:require [clojure.string :as str]))

(def ^:private work-id-pattern
  #"(?<![A-Za-z0-9_-])([MET]-[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9_-])?)")

(defn explicit-work-references
  "Exact M-/E-/T- identifiers and their verbatim mention in TEXT."
  [text]
  (mapv (fn [[mention id]] {:id id :quote mention :basis :explicit-id})
        (re-seq work-id-pattern (or text ""))))

(defn relationship-reviews
  "Return unresolved relationship candidates.

  RESOLVED-REFERENCES are supplied by a separate, evidenced resolver and must
  carry :id, :quote, and :basis.  A candidate is emitted only when a clocked
  target and operator-evidence-id exist and the discussed work differs from
  the clock."
  [{:keys [clocked-target operator-evidence-id operator-text
           resolved-references]}]
  (if (or (str/blank? clocked-target) (str/blank? operator-evidence-id))
    []
    (let [refs (concat (explicit-work-references operator-text)
                       resolved-references)]
      (->> refs
           (filter #(and (string? (:id %))
                         (not= clocked-target (:id %))
                         (string? (:quote %))
                         (str/includes? (or operator-text "") (:quote %))
                         (:basis %)))
           (map (fn [ref]
                  {:event :relationship-review
                   :status :proposed
                   :relation :unresolved
                   :clocked-work clocked-target
                   :discussed-work (:id ref)
                   :evidence {:operator-evidence-id operator-evidence-id
                              :quote (:quote ref)
                              :reference-basis (:basis ref)}}))
           (distinct)
           (sort-by (juxt :discussed-work #(get-in % [:evidence :quote])))
           vec))))
