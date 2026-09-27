(ns xiang2000-p13b
  "Validate every version before append. Explicit --write opts into minted records;
   no existing P13a record is overwritten. Same fixture retries are P6b no-ops."
  (:require [clojure.edn :as edn]
            [futon3c.agency.rule-record :as record]
            [futon3c.agency.rule-timeline :as timeline]))
(try
  (let [requests (edn/read-string (slurp "holes/labs/M-象-2000/P13b-requisition-versions.edn"))
        records (mapv record/payload requests)
        family "kimi-requisition-20260924"
        _ (timeline/intervals records family)
        receipts (when (= ["--write"] (vec *command-line-args*))
                   (mapv #(record/write! (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073") %) requests))]
    (prn {:receipts receipts :intervals (timeline/intervals records family)
          :at-1700 (timeline/as-of records family "2026-09-24T17:00:00Z")
          :at-2100 (timeline/as-of records family "2026-09-25T21:00:00Z")})
    (shutdown-agents))
  (catch Exception e
    (prn {:ok false :message (.getMessage e) :detail (ex-data e)})
    (shutdown-agents)
    (System/exit 1)))
