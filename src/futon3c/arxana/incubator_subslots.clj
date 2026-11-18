(ns futon3c.arxana.incubator-subslots
  "Prompt-specific writing functions, not automatic passage classifications."
  (:require [clojure.java.io :as io] [cheshire.core :as json]
            [clojure.string :as str]))
(def schemas (json/parse-string (slurp (io/resource "arxana/incubator-subslots.json")) true))
(defn schema [slot-id] (get schemas (keyword slot-id) []))
(defn annotate [slot] (assoc slot :subslots (if (= :prose (:kind slot)) (schema (:id slot)) [])))
(defn assign
  "Rows have stable :id (placement id or edit-revision/sentence-offset), :slot and :text.
  Caller supplies actor/time. Assignment is scoped to a slot and row revision."
  [session rows {:keys [subject slot subslot actor time] :as act}]
  (when-not (and (some #(and (= subject (:id %)) (= slot (:slot %))) rows)
                 (string? actor) (not (str/blank? actor))
                 (or (nil? subslot) (some #(= subslot (:id %)) (schema slot))))
    (throw (ex-info "Invalid sub-slot assignment" {:reason :invalid-subslot-assignment :act act})))
  (java.time.Instant/parse time)
  (update session :acts conj (assoc act :kind :assign-subslot :scope :subslot
                                  :id (str "act/" (count (:acts session))))))
(defn assignment [acts row]
  (last (filter #(and (= :assign-subslot (:kind %)) (= (:id row) (:subject %)) (= (:slot row) (:slot %))) acts)))
(defn ordered [acts rows slot-id]
  (let [rank (zipmap (map :id (schema slot-id)) (range))]
    (vec (sort-by #(get rank (:subslot (assignment acts %)) 1000) rows))))
