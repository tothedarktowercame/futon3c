(ns futon3c.apm.codex-only-frame-contract-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.frame-cycle-contract :as contract]))

(def codex-only-contract
  (edn/read-string
   (slurp
    "holes/labs/M-apm-demonstration/frame-cycle-contract-codex-only-v1.edn")))

(deftest codex-only-contract-rules-before-close
  (is (:ok (contract/validate-contract codex-only-contract)))
  (is (= [:preflight :solve :verify :bank :close-frame]
         (:phase-order codex-only-contract)))
  (is (= :frame-bank
         (get-in codex-only-contract [:phases :bank :receipt/type])))
  (is (contains? (get-in codex-only-contract
                         [:phases :close-frame :requires])
                 :bank-receipt)))

(deftest bank-schema-carries-the-ruling-authority
  (is (= #{:receipt/id :receipt/type :receipt/frame-id
           :receipt/problem-id :receipt/verify-receipt-id
           :receipt/ruling :receipt/lane-transition}
         (get-in codex-only-contract
                 [:receipt/schemas :frame-bank :required]))))
