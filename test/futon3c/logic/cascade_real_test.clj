(ns futon3c.logic.cascade-real-test
  "STANDARD-VERIFY L1 — the design is VERIFIED iff (a) the contract model yields zero
   violations AND (b) each adversarial model is CAUGHT by its check."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.logic.pldb :as pldb]
            [futon3c.logic.cascade-real :as cr]))

(deftest contract-model-is-consistent
  (testing "(a) the conforming cascade-real contract passes every L1 check"
    (let [v (cr/verify (cr/contract-db))]
      (is (= [] (:composition-violations v)) "dimensions compose: one shared ontology")
      (is (= [] (:self-deps v)) "no escrow self-loops")
      (is (= [] (:uncovered-standards v)) "all five CHARTER standards instantiated")
      (is (true? (:consistent? v)) "L1: the contract is consistent as a design")
      (is (= :O1 (:keystone v)) "D4 (=O1) is the keystone — the most-held-on node")
      (is (= #{:O4 :O5} (set (:owner-holes v)))
          "O4/O5 reported as the KNOWN honest holes, not failures"))))

(deftest composition-conflict-is-caught
  (testing "(b) a node claimed with conflicting types across dimensions is CAUGHT"
    ;; O5 mis-claims the shared pattern node PN as :capability instead of :pattern
    (let [bad (cr/db-from-data
               (conj cr/contract-facts [cr/claims-typeo :O5 "PN" :capability]))
          v (cr/verify bad)]
      (is (some #{"PN"} (:composition-violations v))
          "the shared-ontology violation on PN is detected")
      (is (false? (:consistent? v)) "an inconsistent ontology fails L1"))))

(deftest uncovered-standard-is-caught
  (testing "(b) a CHARTER standard no dimension instantiates is CAUGHT"
    ;; drop the only coverer of standard 4 (honest holes)
    (let [facts (remove #(= % [cr/coverso :O5 4]) cr/contract-facts)
          v (cr/verify (cr/db-from-data facts))]
      (is (some #{4} (:uncovered-standards v)) "standard 4 flagged as uncovered")
      (is (false? (:consistent? v))))))

(deftest self-dependency-is-caught
  (testing "(b) an escrow held-on self-loop is CAUGHT"
    (let [bad (cr/db-from-data (conj cr/contract-facts [cr/held-ono :O1 :O1]))
          v (cr/verify bad)]
      (is (some #{:O1} (:self-deps v)) "the self-loop on O1 is detected")
      (is (false? (:consistent? v))))))

(deftest keystone-tracks-the-dependency-structure
  (testing "the keystone is computed from held-on in-degree, not asserted"
    ;; if a different node accrues the most dependents, the keystone moves —
    ;; proving the keystone claim is earned by the structure
    (let [shifted (cr/db-from-data
                   (conj cr/contract-facts
                         [cr/held-ono :O3 :O2] [cr/held-ono :O5 :O2] [cr/held-ono :O7b :O2]))
          v (cr/verify shifted)]
      (is (= :O2 (:keystone v)) "keystone follows the in-degree (O2 now most-held-on)"))))
