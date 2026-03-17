(ns futon3c.logic.invariant-runner
  "Lightweight runner for domain invariant layers.

   The important property here is load-profile honesty: dormant domains are
   reported as dormant, not as silently clean."
  (:require [clojure.string :as str]
            [futon3c.logic.obligation :as obligation]))

(defn loaded?
  [load-profile domain-id]
  (not (false? (get load-profile domain-id true))))

(defn- resolve-input
  [{:keys [input input-fn] :as domain-spec}]
  (cond
    (fn? input-fn) (input-fn)
    (contains? domain-spec :input) input
    :else nil))

(defn- resolve-check
  [{:keys [check build-db query-violations] :as domain-spec}]
  (cond
    (fn? check) check
    (and (fn? build-db) (fn? query-violations))
    (fn [input]
      (-> input build-db query-violations))
    :else
    (throw (ex-info "Domain spec missing check surface"
                    {:domain domain-spec
                     :expected "Provide :check or both :build-db and :query-violations"}))))

(defn run-domain
  "Run one domain check against a load profile.

   Domain spec keys:
   - :domain-id keyword
   - :input any or :input-fn (fn [] -> input)
   - :check fn of input -> violations map, or
   - :build-db + :query-violations"
  [load-profile {:keys [domain-id] :as domain-spec}]
  (when-not domain-id
    (throw (ex-info "Domain spec missing :domain-id" {:domain domain-spec})))
  (if (loaded? load-profile domain-id)
    (let [input (resolve-input domain-spec)
          check (resolve-check domain-spec)
          violations (check input)
          has-violations? (boolean (some (fn [[_k v]] (seq v)) violations))]
      {:domain-id domain-id
       :state :active
       :loaded? true
       :input-ready? true
       :violations violations
       :has-violations? has-violations?})
    {:domain-id domain-id
     :state :dormant
     :loaded? false
     :input-ready? false
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

(defn run-aggregate
  "Run all domains and return a unified report with obligations and tasks.

   Output keys:
   - :reports domain-level violation reports
   - :summary domain summary plus obligation/task counts
   - :obligations flat obligation records
   - :obligations-by-actionability grouped obligations
   - :dispatchable-tasks queue-compatible auto-fixable tasks"
  [load-profile domains]
  (let [reports (run-domains load-profile domains)
        obligations (obligation/reports->obligations reports)
        grouped (obligation/group-by-actionability obligations)
        tasks (obligation/dispatchable-tasks obligations)]
    {:reports reports
     :summary (merge (summarize reports)
                     {:obligations-total (count obligations)
                      :auto-fixable (count (:auto-fixable grouped))
                      :needs-review (count (:needs-review grouped))
                      :informational (count (:informational grouped))
                      :dispatchable-tasks (count tasks)})
     :obligations obligations
     :obligations-by-actionability grouped
     :dispatchable-tasks tasks}))

(defn aggregate->lines
  [{:keys [reports summary obligations-by-actionability]}]
  (concat
   (map report->line reports)
   [(str "obligations: " (:obligations-total summary)
         " total, "
         (:auto-fixable summary) " auto-fixable, "
         (:needs-review summary) " review, "
         (:informational summary) " informational")]
   (when (seq (:auto-fixable obligations-by-actionability))
     [(str "next dispatchable: "
           (:id (first (:auto-fixable obligations-by-actionability))) " — "
           (:label (first (:auto-fixable obligations-by-actionability))))])))

(defn render-aggregate
  [aggregate]
  (str/join "\n" (aggregate->lines aggregate)))
