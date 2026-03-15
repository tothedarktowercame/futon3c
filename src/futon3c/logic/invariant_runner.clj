(ns futon3c.logic.invariant-runner
  "Lightweight runner for domain invariant layers.

   The important property here is load-profile honesty: dormant domains are
   reported as dormant, not as silently clean."
  (:require [clojure.string :as str]))

(defn loaded?
  [load-profile domain-id]
  (not (false? (get load-profile domain-id true))))

(defn run-domain
  "Run one domain check against a load profile.

   Domain spec keys:
   - :domain-id keyword
   - :input any
   - :check fn of input -> violations map"
  [load-profile {:keys [domain-id input check] :as domain-spec}]
  (when-not domain-id
    (throw (ex-info "Domain spec missing :domain-id" {:domain domain-spec})))
  (when-not (fn? check)
    (throw (ex-info "Domain spec missing :check fn" {:domain domain-spec})))
  (if (loaded? load-profile domain-id)
    (let [violations (check input)
          has-violations? (boolean (some (fn [[_k v]] (seq v)) violations))]
      {:domain-id domain-id
       :state :active
       :loaded? true
       :violations violations
       :has-violations? has-violations?})
    {:domain-id domain-id
     :state :dormant
     :loaded? false
     :violations nil
     :has-violations? false}))

(defn run-domains
  [load-profile domains]
  (mapv #(run-domain load-profile %) domains))

(defn summarize
  [reports]
  {:active (count (filter #(= :active (:state %)) reports))
   :dormant (count (filter #(= :dormant (:state %)) reports))
   :violating (count (filter :has-violations? reports))
   :clean-active (count (filter #(and (= :active (:state %))
                                      (not (:has-violations? %)))
                                reports))})

(defn report->line
  [report]
  (let [{:keys [domain-id state has-violations?]} report]
    (cond
      (= state :dormant) (str (name domain-id) ": dormant")
      has-violations? (str (name domain-id) ": active, violations present")
      :else (str (name domain-id) ": active, clean"))))

(defn render-report
  [reports]
  (str/join "\n" (map report->line reports)))
