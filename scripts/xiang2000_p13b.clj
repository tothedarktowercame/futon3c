(ns xiang2000-p13b
  "Validate every version before append. Explicit --write opts into minted records;
   no existing P13a record is overwritten. Same fixture retries are P6b no-ops."
  (:require [clojure.edn :as edn]
            [futon3c.agency.rule-record :as record]
            [futon3c.agency.rules-in-force :as rules]
            [futon3c.agency.rule-timeline :as timeline]))

(defn- parse-args [args]
  (loop [args (seq args) options {:write? false :withdrawals-file nil}]
    (if-not args
      options
      (case (first args)
        "--write" (recur (next args) (assoc options :write? true))
        "--withdrawals"
        (if-let [path (second args)]
          (recur (nnext args) (assoc options :withdrawals-file path))
          (throw (ex-info "--withdrawals requires an EDN file"
                          {:reason :missing-withdrawals-file})))
        (throw (ex-info "Unknown argument"
                        {:reason :unknown-argument :argument (first args)}))))))

(defn- withdrawal-input [path]
  (if-not path
    {:withdrawals [] :grants []}
    (let [input (edn/read-string (slurp path))]
      (cond
        (vector? input) {:withdrawals input :grants []}
        (map? input) {:withdrawals (vec (or (:withdrawals input) []))
                      :grants (vec (or (:grants input) []))}
        :else (throw (ex-info "Withdrawal input must be a vector or map"
                              {:reason :invalid-withdrawals-input}))))))

(defn- force-at [records withdrawals grants family at]
  (let [projection (rules/rules-in-force-as-of records withdrawals grants at)
        family-answer (some #(when (= family (:family %)) (:answer %))
                            (:in-force projection))]
    {:family-answer family-answer
     :ended (:ended projection)
     :provisional (:provisional projection)
     :unresolved (:unresolved projection)
     :ignored (:ignored projection)}))

(try
  (let [{:keys [write? withdrawals-file]} (parse-args *command-line-args*)
        {:keys [withdrawals grants]} (withdrawal-input withdrawals-file)
        requests (edn/read-string (slurp "holes/labs/M-象-2000/P13b-requisition-versions.edn"))
        records (mapv record/payload requests)
        family "kimi-requisition-20260924"
        _ (timeline/intervals records family)
        receipts (when write?
                   (mapv #(record/write! (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073") %) requests))]
    (prn {:receipts receipts :intervals (timeline/intervals records family)
          :at-1700 (force-at records withdrawals grants family "2026-09-24T17:00:00Z")
          :at-2100 (force-at records withdrawals grants family "2026-09-25T21:00:00Z")})
    (shutdown-agents))
  (catch Exception e
    (prn {:ok false :message (.getMessage e) :detail (ex-data e)})
    (shutdown-agents)
    (System/exit 1)))
