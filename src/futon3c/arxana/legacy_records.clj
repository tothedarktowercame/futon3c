(ns futon3c.arxana.legacy-records
  "Read-only typed views of the entity and hyperedge records that predate C1.

  The original record remains the write/evidence authority.  This adapter does
  not add store guarantees or make legacy rows writable through Formal Arxana."
  (:require [clojure.string :as str]))

(defn- record-family [record]
  (cond
    (or (contains? record :hx/id)
        (and (contains? record :hx/type) (contains? record :hx/endpoints)))
    :legacy/hyperedge

    (or (contains? record :xt/id)
        (contains? record :entity/id))
    :legacy/entity

    :else
    (throw (ex-info "record is not a recognized legacy entity or hyperedge"
                    {:error :arxana.legacy/unrecognized-record
                     :keys (set (keys record))}))))

(defn- entity-id [record family]
  (case family
    :legacy/hyperedge (:hx/id record)
    :legacy/entity (or (:entity/id record) (:xt/id record))))

(defn normalize
  "Return a typed, read-only view while retaining RECORD without alteration.

  Mission-scope prop/* values are exposed together under :legacy/properties;
  callers must use :legacy/original for evidence or any compatibility write."
  [record]
  (let [family (record-family record)
        properties (into {}
                         (filter (fn [[k _]]
                                   (and (keyword? k)
                                        (= "prop" (namespace k)))))
                         record)]
    (cond-> {:record/family family
             :record/id (entity-id record family)
             :record/type (case family
                            :legacy/hyperedge (:hx/type record)
                            :legacy/entity (:type record))
             :legacy/original record}
      (= family :legacy/hyperedge)
      (assoc :record/endpoints (:hx/endpoints record)
             :record/ends (:hx/ends record))

      (seq properties)
      (assoc :legacy/properties properties)

      (and (= family :legacy/hyperedge)
           (some #(or (= "mission-scope" (namespace %))
                      (str/starts-with? (name %) "mission-scope/"))
                 (:hx/labels record)))
      (assoc :legacy/write-state :frozen))))

(defn denormalize
  "Recover the exact observed record.  Views without an original are refused."
  [view]
  (if (contains? view :legacy/original)
    (:legacy/original view)
    (throw (ex-info "legacy view has no original record"
                    {:error :arxana.legacy/original-required}))))
