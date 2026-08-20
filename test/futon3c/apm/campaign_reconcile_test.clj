(ns futon3c.apm.campaign-reconcile-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-reconcile :as reconcile])
  (:import [java.time Instant]))

(def now (Instant/parse "2026-08-20T12:00:00Z"))
(def observed-at "2026-08-20T11:59:50Z")

(defn event [seq type body]
  {:event/id (str "e" seq) :event/seq seq :event/type type
   :event/campaign-id "apm-200" :event/actor "regulator"
   :event/at observed-at :event/expected-version seq :event/body body})

(def active-projection
  (machine/projection
   [(event 0 :campaign/registered
           {:series :apm :manifest-hash "manifest"
            :phase-order [:probe :freeze :solve :verify :close]})
    (event 1 :block/opened {:block-id "b1"})
    (event 2 :frame/opened
           {:frame-id "f1" :block-id "b1" :problem-id "m97A06"
            :registration-hash "reg-hash" :harness-hash "harness-hash"
            :required-receipt-kinds [:soundness]})]))

(def valid-facts
  {:registration {:observed-at observed-at :frame-id "f1"
                  :problem-id "m97A06" :registration-hash "reg-hash"
                  :harness-hash "harness-hash"}
   :frame-record {:observed-at observed-at :frame-id "f1"
                  :problem-id "m97A06" :block-id "b1" :status :open}
   :binding {:observed-at observed-at :bound? true :frame-id "f1"
             :ledger-digest (:ledger/digest active-projection)}
   :jobs {:observed-at observed-at :items []}
   :receipts {:observed-at observed-at
              :items [{:kind :soundness :frame-id "f1" :digest "probe-hash"}]}})

(deftest agreeing-independent-routes-produce-valid-certificate
  (let [result (reconcile/reconcile active-projection valid-facts now)]
    (is (= :valid (:reconciliation/status result)))
    (is (empty? (:findings result)))
    (is (= "f1" (:active/frame-id result)))
    (is (= 64 (count (:facts/digest result))))))

(deftest observation-time-does-not-change-semantic-facts-digest
  (let [later (.plusSeconds now 30)
        earlier-result (reconcile/reconcile active-projection valid-facts now)
        later-facts (update-vals valid-facts
                                 #(assoc % :observed-at (str later)))
        later-result (reconcile/reconcile active-projection later-facts later)]
    (is (= :valid (:reconciliation/status later-result)))
    (is (= (:facts/digest earlier-result) (:facts/digest later-result)))))

(deftest missing-or-expired-observations-are-stale
  (testing "missing route"
    (let [result (reconcile/reconcile active-projection
                                      (dissoc valid-facts :frame-record) now)]
      (is (= :stale (:reconciliation/status result)))
      (is (some #(= :observation-missing (:code %)) (:findings result)))))
  (testing "expired route"
    (let [facts (assoc-in valid-facts [:jobs :observed-at]
                          "2026-08-20T11:00:00Z")
          result (reconcile/reconcile active-projection facts now)]
      (is (= :stale (:reconciliation/status result)))
      (is (some #(= :observation-expired (:code %)) (:findings result))))))

(deftest contradictions-dominate-missing-evidence
  (let [facts (-> valid-facts
                  (dissoc :receipts)
                  (assoc-in [:registration :problem-id] "different"))
        result (reconcile/reconcile active-projection facts now)]
    (is (= :conflict (:reconciliation/status result)))
    (is (some #(= :value-mismatch (:code %)) (:findings result)))
    (is (some #(= :observation-missing (:code %)) (:findings result)))))

(deftest foreign-live-jobs-and-duplicate-job-states-conflict
  (let [facts (assoc valid-facts :jobs
                     {:observed-at observed-at
                      :items [{:job-id "j1" :frame-id "f2" :role :solver
                               :state :running}
                              {:job-id "j1" :frame-id "f1" :role :solver
                               :state :done}]})
        result (reconcile/reconcile active-projection facts now)]
    (is (= :conflict (:reconciliation/status result)))
    (is (some #(= :foreign-live-job (:code %)) (:findings result)))
    (is (some #(= :job-id-contradiction (:code %)) (:findings result)))))

(deftest required-receipts-are-cardinality-and-frame-checked
  (let [facts (assoc valid-facts :receipts
                     {:observed-at observed-at
                      :items [{:kind :soundness :frame-id "f2" :digest "a"}
                              {:kind :soundness :frame-id "f1" :digest "b"}]})
        result (reconcile/reconcile active-projection facts now)]
    (is (= :conflict (:reconciliation/status result)))
    (is (some #(= :required-receipt-ambiguous (:code %)) (:findings result)))))

(deftest no-active-frame-requires-observed-quiet-runtime
  (let [projection (machine/projection
                    [(event 0 :campaign/registered
                            {:phase-order [:probe :close]})])
        quiet {:binding {:observed-at observed-at :bound? false}
               :jobs {:observed-at observed-at :items []}}
        live (assoc-in quiet [:jobs :items]
                       [{:job-id "orphan" :frame-id "f1" :state :running}])]
    (is (= :valid (:reconciliation/status
                   (reconcile/reconcile projection quiet now))))
    (is (= :conflict (:reconciliation/status
                      (reconcile/reconcile projection live now))))))

(deftest invalid-ledger-projection-can-never-reconcile-valid
  (let [invalid (machine/projection [(event 1 :campaign/registered
                                           {:phase-order [:a :b]})])]
    (is (= :conflict
           (:reconciliation/status
            (reconcile/reconcile invalid valid-facts now))))))
