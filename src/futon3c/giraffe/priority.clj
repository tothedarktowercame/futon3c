(ns futon3c.giraffe.priority
  "A bounded projection of Giraffe pass order through a received focus relation.
  Giraffe orders within each received relation band; it does not infer focus.")

(def relation-band
  {:focus :focus
   :associated :associated
   :useful-elsewhere :elsewhere})

(def band-order
  {:focus 0 :associated 1 :elsewhere 2 :unknown 3})

(defn priority-projection
  "Return every pass row, grouped by received relation then by pass rank.
  CUTOFF is a declared row-count boundary; rows are marked, never discarded.
  FOCUS-INPUT is {:relations {target relation} :evidence-pin pin}."
  [pass-output focus-input cutoff]
  {:pre [(nat-int? cutoff)]}
  (let [relations (:relations focus-input)
        evidence-pin (:evidence-pin focus-input)]
    (->> (:order pass-output)
         (map-indexed
          (fn [index item]
            (let [target (:id item)
                  relation (get relations target)
                  band (get relation-band relation :unknown)]
              (cond-> {:target target
                       :pass-rank (or (:rank item) (inc index))
                       :relation relation
                       :priority-band band
                       :evidence-pin evidence-pin}
                (= band :unknown) (assoc :reason :focus-relation-missing)))))
         (sort-by (juxt (comp band-order :priority-band) :pass-rank))
         (map-indexed (fn [index row]
                        (assoc row
                               :cutoff cutoff
                               :cutoff-status (if (< index cutoff)
                                                :within-cutoff
                                                :beyond-cutoff))))
         vec)))
