(ns futon3c.apm.campaign-trace
  "Canonical refinement trace exported for the Lean campaign checker."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]))

(defn- canonical [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [(name k) (canonical v)])) x)
    (set? x) (mapv canonical (sort x))
    (sequential? x) (mapv canonical x)
    (keyword? x) (name x)
    :else x))

(defn trace
  [{:keys [campaign-id manifest-hash contract-id phase-order steps closed
           terminal-ledger-digest]}]
  (canonical
   {"schemaVersion" 1
    "campaignId" campaign-id
    "manifestHash" manifest-hash
    "contractId" contract-id
    "phaseOrder" phase-order
    "steps" (mapv (fn [{:keys [from to ledger-before ledger-after
                                receipt-id prior-receipt-id]}]
                     {"fromPhase" from "toPhase" to
                      "ledgerBefore" ledger-before "ledgerAfter" ledger-after
                      "receiptId" receipt-id
                      "priorReceiptId" prior-receipt-id})
                   steps)
    "closed" closed
    "terminalLedgerDigest" terminal-ledger-digest}))

(defn emit!
  "Atomically write a deterministic JSON trace. The Lean checker is the
   acceptance authority; this function intentionally performs no shadow
   protocol validation."
  [path input]
  (let [target (io/file path)
        parent (.getParentFile target)
        temp (java.io.File/createTempFile ".apm-trace-" ".json" parent)
        payload (str (json/generate-string (trace input)) "\n")]
    (try
      (spit temp payload)
      (java.nio.file.Files/move
       (.toPath temp) (.toPath target)
       (into-array java.nio.file.CopyOption
                   [java.nio.file.StandardCopyOption/ATOMIC_MOVE
                    java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
      {:ok true :path (.getCanonicalPath target) :bytes (count payload)}
      (finally
        (when (.exists temp) (.delete temp))))))
