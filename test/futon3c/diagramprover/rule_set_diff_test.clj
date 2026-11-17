(ns futon3c.diagramprover.rule-set-diff-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.arxana-records :as ar]
            [futon3c.diagramprover.opaque-rules :as opaque]
            [futon3c.diagramprover.publish-findings :as publish]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.type-registry :as types]))
;; IDs copied from first-write-2026-11-17.edn. Tests do not read live storage.
(def ids {:q56-raw "arxana:clusion:sha256:4e04f2018e4c0905f7dcfd25cf90ebfb6d91c43c018a9d7004b4d2320ebaca33"
          :q83 "arxana:clusion:sha256:777d1f59d5a939011126a8bebe9e4422cc9bc5b6d4799fa8451d75eca1c60bec"})
(def sup "arxana:clusion:sha256:8ca7b2066ebe6ea75e9a96a5cf6e248c549e3fabc3279e86afa8fcc01ac47464")
(defn real-diff [sha]
  (let [table (opaque/load-table "test/futon3c/diagramprover/fixtures/a97J08-step-table-q56-raw,q83.json")]
    (assoc (opaque/rule-set-diff table [:q56-raw] table [:q83] sup)
           :checker-sha sha :rule-form :opaque)))
(deftest real-diff-and-dry-validation
  (let [diff (real-diff "fixture-checker") r (ar/rule-set-diff-record diff ids sup)]
    (is (= :different (:comparison diff) (:observation-comparison diff)))
    (is (= [false true] (mapv #(get-in diff [% :observations 0 :well-nested?]) [:a :b])))
    (is (= ["e8fab561e5a27d78150adc110e2cd89cff7d59ca33d3d59953b653c010a4b61a"]
           (get-in r [:asserted-content :version-a :normal-forms])))
    (is (= ["18f0aa924019b7e2dcdc23ecc7ce724b12a6d523748a79e2708f7f361d355ce1"]
           (get-in r [:asserted-content :version-b :normal-forms])))
    (is (= (types/declaration [:rule-set/diff 1]) (types/validate-declaration (types/declaration [:rule-set/diff 1]))))
    (is (= {:validated 1} (publish/validate-batch! {:message-id "diff-dry-run" :rows [{:operation :clusion/assert :record r}]})))))
(deftest omitted-links-refused
  (let [d (real-diff "fixture")]
    (doseq [[mapping supersession expected] [[ids nil :arxana.diff/supersession-required]
                                           [(dissoc ids :q83) sup :arxana.diff/rule-id-required]]]
      (is (= expected (try (ar/rule-set-diff-record d mapping supersession) nil
                           (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))))))
(deftest order-mutation
  (let [d (-> (real-diff "fixture")
              (assoc-in [:a :rules] [:q56-raw :q83])
              (assoc-in [:a :normal-forms] (sorted-set "a" "z")))
        reversed (-> d (assoc-in [:a :rules] [:q83 :q56-raw])
                     (assoc-in [:a :normal-forms] (sorted-set-by #(compare %2 %1) "a" "z")))
        r (ar/rule-set-diff-record d ids sup)
        r2 (ar/rule-set-diff-record reversed ids sup)
        broken (fn [input]
                 (records/make-clusion
                  (-> r (update :asserted-content assoc :version-a
                                (assoc (get-in r [:asserted-content :version-a])
                                       :rules (mapv ids (get-in input [:a :rules]))
                                       :normal-forms (vec (get-in input [:a :normal-forms])))))))]
    (is (= (:id r) (:id r2)))
    (is (not= (:id (broken d)) (:id (broken reversed))))))
