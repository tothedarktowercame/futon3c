(ns futon3c.evidence.subject
  "Canonical vocabulary and compatibility rules for evidence subjects."
  (:require [clojure.string :as str]))

(defn canonical-ref-type [ref-type]
  (let [ref-type (cond
                   (keyword? ref-type) ref-type
                   (and (string? ref-type) (not (str/blank? ref-type)))
                   (keyword (str/replace ref-type #"^:" ""))
                   :else ref-type)]
    (case ref-type
      (:apm-problem :bpm-problem) :problem
      ref-type)))

(defn normalize-ref [subject]
  (if (and (map? subject) (contains? subject :ref/type))
    (update subject :ref/type canonical-ref-type)
    subject))

(defn equivalent? [left right]
  (= (normalize-ref left) (normalize-ref right)))

(defn readable-ref-types [ref-type]
  (if (= :problem (canonical-ref-type ref-type))
    [:problem :apm-problem :bpm-problem]
    [(canonical-ref-type ref-type)]))
