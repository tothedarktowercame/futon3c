(ns futon3c.watcher.projections.flight
  "Project one conforming WM flight record into the essay/section/annotation
  shape used by Arxana. This is a consumability projection only: it refuses
  derivation-thin or transient records and does not write substrate-2."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def organ-order
  [:field-read
   :velocity
   :warrant
   :verification
   :attribution
   :prediction
   :counterfactual
   :begin-state
   :act
   :measurement
   :window
   :out-of-band
   :self-record])

(def allowed-measurement-classes
  "Mask-IN measurement classes for this one-record bootstrap. Transient,
  fallback, and confounded measurements are explicitly non-canonical."
  #{:clean :null})

(defn- slug [x]
  (let [s (-> (name x)
              str/lower-case
              (str/replace #"[^a-z0-9]+" "-")
              (str/replace #"(^-|-$)" ""))]
    (if (seq s) s "unknown")))

(defn- json-safe-value [v]
  (cond
    (keyword? v) (name v)
    (symbol? v) (str v)
    (map? v) (into {} (map (fn [[k val]] [(if (keyword? k) (name k) (str k))
                                           (json-safe-value val)]) v))
    (set? v) (mapv json-safe-value v)
    (sequential? v) (mapv json-safe-value v)
    :else v))

(defn- truncate [s n]
  (let [s (str s)]
    (if (<= (count s) n)
      s
      (str (subs s 0 (max 0 (- n 1))) "…"))))

(defn term-cell? [cell]
  (and (map? cell)
       (contains? cell :judgment)
       (contains? cell :ground)
       (not (contains? cell :sorry))))

(defn typed-sorry? [cell]
  (and (map? cell)
       (map? (:sorry cell))
       (contains? (:sorry cell) :kind)))

(defn- cell-state [cell]
  (cond
    (term-cell? cell) :term
    (typed-sorry? cell) :typed-sorry
    :else :invalid))

(defn- measurement-class [record]
  (get-in record [:organs :measurement :judgment :class]))

(defn ingestable-flight? [record]
  (and (= :full (:flight/derivation record))
       (string? (:flight/id record))
       (map? (:organs record))
       (seq (:organs record))
       (every? #(or (term-cell? %) (typed-sorry? %)) (vals (:organs record)))
       (let [measurement (get-in record [:organs :measurement])]
         (or (nil? measurement)
             (typed-sorry? measurement)
             (contains? allowed-measurement-classes (measurement-class record))))))

(defn assert-ingestable! [record]
  (when-not (ingestable-flight? record)
    (throw (ex-info "Flight record is not canonical-ingestable"
                    {:flight/id (:flight/id record)
                     :flight/derivation (:flight/derivation record)
                     :measurement-class (measurement-class record)})))
  record)

(defn read-record [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (edn/read {:readers *data-readers*} r)))

(defn- organ-keys [record]
  (let [ks (set (keys (:organs record)))
        ordered (filterv ks organ-order)
        extra (->> ks (remove (set organ-order)) sort vec)]
    (vec (concat ordered extra))))

(defn- essay-id [flight-id]
  (str "arxana/essay/flight/" flight-id))

(defn- section-id [essay-id organ]
  (str essay-id "/organ/" (slug organ)))

(defn- judgment-summary [cell]
  (cond
    (term-cell? cell)
    (truncate (pr-str (:judgment cell)) 220)

    (typed-sorry? cell)
    (let [s (:sorry cell)]
      (str "sorry " (name (:kind s))
           (when-let [why (:why s)]
             (str " — " (truncate why 180)))))

    :else
    (truncate (pr-str cell) 220)))

(defn- annotation-props [flight-id organ cell]
  (merge
   {:annotation/kind "flight/organ-ground"
    :flight/id flight-id
    :organ/key (name organ)
    :organ/state (name (cell-state cell))
    :judgment-summary (judgment-summary cell)}
   (if (typed-sorry? cell)
     {:sorry (json-safe-value (:sorry cell))}
     {:ground (json-safe-value (:ground cell))
      :judgment (json-safe-value (:judgment cell))})))

(defn project-record
  "Return the essay projection for an already-read, conforming flight record."
  [source-file record]
  (assert-ingestable! record)
  (let [flight-id (:flight/id record)
        eid (essay-id flight-id)
        source-file (str (.getAbsolutePath (io/file source-file)))
        organs (organ-keys record)
        sections (mapv (fn [idx organ]
                         (let [cell (get-in record [:organs organ])]
                           {:id (section-id eid organ)
                            :name (format "§%02d. %s" idx (name organ))
                            :type "arxana/essay-section"
                            :props {:essay-id eid
                                    :heading-text (name organ)
                                    :organ/key (name organ)
                                    :organ/state (name (cell-state cell))
                                    :organ/summary (judgment-summary cell)
                                    :flight/id flight-id}}))
                       (range 1 (inc (count organs)))
                       organs)
        annotations (mapv (fn [organ]
                            (let [sid (section-id eid organ)
                                  cell (get-in record [:organs organ])]
                              {:id (str sid "/annotation/ground")
                               :hx-type "arxana/flight-organ-annotation"
                               :endpoints [{:role "annotated" :entity-id sid}
                                           {:role "flight" :entity-id eid}]
                               :props (annotation-props flight-id organ cell)}))
                          organs)]
    {:essay {:id eid
             :name (str "Flight " flight-id)
             :type "arxana/essay"
             :source-file source-file
             :props {:label "wm-flight"
                     :source-file source-file
                     :record-kind "wm-flight"
                     :flight/id flight-id
                     :flight/derivation (name (:flight/derivation record))
                     :section-count (count sections)
                     :annotation-count (count annotations)}}
     :sections sections
     :annotations annotations}))

(defn collect-file [path]
  (project-record path (read-record path)))
