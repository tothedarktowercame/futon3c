(ns futon3c.logic.structural-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.logic.pldb :as pldb]
            [futon3c.logic.structural-law :as law]))

(pldb/db-rel nodeo node-id)
(pldb/db-rel unlockso node-id unlocked-id)
(pldb/db-rel depends-ono node-id dep-id)
(pldb/db-rel item-statuso item-id status)
(pldb/db-rel cycle-phaseo cycle-id phase)
(pldb/db-rel cycle-outputo cycle-id phase output-key)
(pldb/db-rel cycle-completedo cycle-id phase)

(deftest query-paired-edge-mismatches-detects-and-clears-asymmetry
  (testing "paired-edge symmetry is reported only when the back-reference is missing"
    (let [bad-db (-> (pldb/db)
                     (pldb/db-fact unlockso "A" "B"))
          clean-db (-> (pldb/db)
                       (pldb/db-fact unlockso "A" "B")
                       (pldb/db-fact depends-ono "B" "A"))]
      (is (= [["A" "B" :unlocks-without-dep]]
             (law/query-paired-edge-mismatches
              bad-db
              {:forward-rel unlockso
               :backward-rel depends-ono
               :forward-label :unlocks-without-dep
               :backward-label :dep-without-unlock})))
      (is (empty?
           (law/query-paired-edge-mismatches
            clean-db
            {:forward-rel unlockso
             :backward-rel depends-ono
             :forward-label :unlocks-without-dep
             :backward-label :dep-without-unlock}))))))    

(deftest query-dangling-targets-detects-missing-entities
  (testing "references to non-existent entities are reported"
    (let [bad-db (-> (pldb/db)
                     (pldb/db-fact nodeo "A")
                     (pldb/db-fact depends-ono "A" "ghost"))
          clean-db (-> (pldb/db)
                       (pldb/db-fact nodeo "A")
                       (pldb/db-fact nodeo "B")
                       (pldb/db-fact depends-ono "A" "B"))]
      (is (= [["A" "ghost" :depends-on]]
             (law/query-dangling-targets
              bad-db
              {:entity-rel nodeo
               :ref-rel depends-ono
               :direction :depends-on})))
      (is (empty?
           (law/query-dangling-targets
            clean-db
            {:entity-rel nodeo
             :ref-rel depends-ono
             :direction :depends-on}))))))    

(deftest query-invalid-enum-values-detects-bad-status
  (testing "enum validation is shared without losing domain-specific status names"
    (let [bad-db (-> (pldb/db)
                     (pldb/db-fact item-statuso "A" :banana))
          clean-db (-> (pldb/db)
                       (pldb/db-fact item-statuso "A" :open))]
      (is (= [["A" :banana]]
             (law/query-invalid-enum-values
              bad-db
              {:value-rel item-statuso
               :allowed-values #{:open :partial :done}})))
      (is (empty?
           (law/query-invalid-enum-values
            clean-db
            {:value-rel item-statuso
             :allowed-values #{:open :partial :done}}))))))    

(deftest query-missing-phase-outputs-detects-skipped-records
  (testing "advancing beyond a phase requires the earlier phase outputs to exist"
    (let [bad-db (-> (pldb/db)
                     (pldb/db-fact cycle-phaseo "C1" :execute)
                     (pldb/db-fact cycle-outputo "C1" :observe :blocker-id))
          clean-db (-> (pldb/db)
                       (pldb/db-fact cycle-phaseo "C1" :execute)
                       (pldb/db-fact cycle-outputo "C1" :observe :blocker-id)
                       (pldb/db-fact cycle-outputo "C1" :propose :approach))]
      (is (= [{:cycle "C1" :phase :propose :missing #{:approach}}]
             (law/query-missing-phase-outputs
              bad-db
              {:cycle-phase-rel cycle-phaseo
               :cycle-output-rel cycle-outputo
               :phase-order [:observe :propose :execute]
               :phase-required-outputs {:observe #{:blocker-id}
                                        :propose #{:approach}}})))
      (is (empty?
           (law/query-missing-phase-outputs
            clean-db
            {:cycle-phase-rel cycle-phaseo
             :cycle-output-rel cycle-outputo
             :phase-order [:observe :propose :execute]
             :phase-required-outputs {:observe #{:blocker-id}
                                      :propose #{:approach}}}))))))    

(deftest query-phase-prefix-mismatches-detects-broken-linearity
  (testing "completed phases must form the exact prefix before the current phase"
    (let [bad-db (-> (pldb/db)
                     (pldb/db-fact cycle-phaseo "C1" :validate)
                     (pldb/db-fact cycle-completedo "C1" :observe))
          clean-db (-> (pldb/db)
                       (pldb/db-fact cycle-phaseo "C1" :validate)
                       (pldb/db-fact cycle-completedo "C1" :observe)
                       (pldb/db-fact cycle-completedo "C1" :propose)
                       (pldb/db-fact cycle-completedo "C1" :execute))]
      (is (= [{:cycle "C1"
               :current-phase :validate
               :expected #{:observe :propose :execute}
               :actual #{:observe}}]
             (law/query-phase-prefix-mismatches
              bad-db
              {:cycle-phase-rel cycle-phaseo
               :completed-phase-rel cycle-completedo
               :phase-order [:observe :propose :execute :validate]})))
      (is (empty?
           (law/query-phase-prefix-mismatches
            clean-db
            {:cycle-phase-rel cycle-phaseo
             :completed-phase-rel cycle-completedo
             :phase-order [:observe :propose :execute :validate]}))))))    
