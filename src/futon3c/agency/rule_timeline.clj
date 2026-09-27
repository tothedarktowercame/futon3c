(ns futon3c.agency.rule-timeline
  "P13b: adoption, commit and application are independent sourced observations.
   In-force means APPLIED (Joe's ruling), not promulgated. Retrospective records
   do not invent P3 grants. A last notice is not evidence of withdrawal."
  (:require [clojure.string :as str])
  (:import [java.time Instant]))

(defn- refuse! [field]
  (throw (ex-info "Invalid sourced rule timeline" {:reason :invalid-rule-timeline :field field})))
(defn- text? [x] (and (string? x) (not (str/blank? x))))
(defn- instant [x]
  (try (Instant/parse x) (catch Exception _ (refuse! :at))))
(defn- event! [e]
  (instant (:at e))
  (when-not (and (map? (:source e)) (text? (get-in e [:source :ref]))) (refuse! :source)))

(defn validate! [t]
  (when-not (and (map? t) (text? (:family t)) (pos-int? (:version t))
                 (contains? #{:code :dispatcher-run} (:kind t))
                 (contains? #{:requisition-with-followups :followup-half-withdrawn} (:effect t)))
    (refuse! :identity))
  (doseq [k [:adopted :committed]]
    (when-not (and (vector? (get t k)) (seq (get t k))) (refuse! k))
    (doseq [e (get t k)] (event! e)))
  (doseq [e (:adopted t)]
    (when-not (contains? #{:operator-act-reconstruction :grant-record} (:basis e)) (refuse! :adoption-basis))
    (when (and (= :operator-act-reconstruction (:basis e))
               (not= :unrecorded (:grant-status e))) (refuse! :grant-status)))
  (doseq [e (:committed t)]
    (when-not (and (text? (:repo e)) (string? (:sha e)) (re-matches #"[0-9a-f]{40}" (:sha e)))
      (refuse! :commit)))
  (when-let [live (:live t)]
    (event! live)
    (when-not (contains? #{:applied :not-loaded} (:status live)) (refuse! :live-status))
    (if (= :applied (:status live))
      (when-not (= (case (:kind t) :code :load :dispatcher-run :execution)
                   (get-in live [:source :kind]))
        (refuse! :runtime-source-kind))
      (when-not (contains? #{:stale-runner-source :loaded-file-code-mismatch}
                           (get-in live [:source :kind])) (refuse! :negative-runtime-source))))
  t)

(defn- timelines [records]
  (let [ts (mapv #(validate! (:rule/timeline (:hx/props %)))
                 (filter #(get-in % [:hx/props :rule/timeline]) records))]
    (when-not (= (count ts) (count (set (map (juxt :family :version) ts))))
      (refuse! :duplicate-version))
    ts))

(defn intervals
  "Half-open application intervals derived only from positive runtime observations.
   Unloaded or unobserved versions never close an applied version's interval."
  [records family]
  (let [ts (->> (timelines records)
                (filter #(and (= family (:family %)) (= :applied (get-in % [:live :status]))))
                (sort-by #(instant (get-in % [:live :at]))) vec)
        starts (map #(instant (get-in % [:live :at])) ts)]
    (when-not (= (count starts) (count (set starts))) (refuse! :ambiguous-live-time))
    (mapv (fn [t next-t]
            {:version (:version t) :effect (:effect t)
             :from (get-in t [:live :at]) :until (get-in next-t [:live :at])})
          ts (concat (rest ts) [nil]))))

(defn as-of [records family at]
  (let [time (instant at)
        before? #(not (.isAfter (instant %) time))
        ts (filter #(= family (:family %)) (timelines records))
        active (last (filter #(and (before? (:from %))
                                   (or (nil? (:until %)) (.isBefore time (instant (:until %)))))
                            (intervals records family)))
        committed (filter #(some (comp before? :at) (:committed %)) ts)]
    (cond active (assoc active :answer (if (= :followup-half-withdrawn (:effect active))
                                        "followup half withdrawn" "requisition rule applied"))
          (seq committed) {:answer "committed, not yet live"}
          :else {:answer "no committed rule observed"})))

(defn promulgated-as-of
  "The separate adopted/committed reading, without claiming runtime application."
  [records family at]
  (let [time (instant at) seen? #(not (.isAfter (instant (:at %)) time))]
    (mapv #(-> (select-keys % [:family :version])
               (assoc :adopted (filterv seen? (:adopted %)) :committed (filterv seen? (:committed %))))
          (filter #(= family (:family %)) (timelines records)))))
