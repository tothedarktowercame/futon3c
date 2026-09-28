(ns futon3c.agency.disclosure-audit-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.disclosure-audit :as audit]))

(def d1 {:id "act:d1" :author "codex-5" :source-job "invoke:j"})
(def d2 {:id "act:d2" :author "codex-5" :source-job "invoke:j"})
(def w2 {:id "act:w2" :target "act:d2"})
(def route2 {:job-id (audit/routing-job-id "act:w2")
             :bellback-of "invoke:j" :agent-id "codex-5"})

(deftest two-disclosures-with-one-routed-withdrawal-are-complete
  (let [result (audit/audit {:job "invoke:j" :report-text ""
                             :disclosures [d1 d2] :withdrawals [w2]
                             :interpretations [] :routing-jobs [route2]
                             :stored-act-ids ["act:d1" "act:d2" "act:w2"]})]
    (is (= [:standing :withdrawn]
           (mapv :status (:disclosures result))))
    (is (empty? (:findings result)))))

(deftest negation-without-effect-is-visible
  (let [result (audit/audit {:job "invoke:j" :report-text "" :disclosures [d1]
                             :withdrawals [] :routing-jobs [] :stored-act-ids ["act:d1"]
                             :interpretations [{:id "interpretation:1"
                                                :kind :interpretation
                                                :intent :withdraw :target "act:d1"}]})]
    (is (= [{:reason :negation-without-effect :disclosure-id "act:d1"
             :interpretation-id "interpretation:1"}]
           (:findings result)))))

(deftest missing-and-misdirected-routing-are-findings
  (doseq [jobs [[] [(assoc route2 :agent-id "other")]
                [(assoc route2 :bellback-of "invoke:other")]]]
    (is (= :effect-not-routed
           (-> (audit/audit {:job "invoke:j" :report-text "" :disclosures [d2]
                             :withdrawals [w2] :interpretations []
                             :routing-jobs jobs
                             :stored-act-ids ["act:d2" "act:w2"]})
               :findings first :reason)))))

(deftest explicit-citations-distinguish-missing-from-other-stored-acts
  (let [result (audit/audit {:job "invoke:j"
                             :report-text "choice act:ghost; grant act:grant"
                             :disclosures [] :withdrawals [] :interpretations []
                             :routing-jobs [] :stored-act-ids ["act:grant"]})]
    (is (= [{:id "act:ghost" :reason :disclosure-unrecorded}]
           (:findings result)))))
