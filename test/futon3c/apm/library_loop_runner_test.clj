(ns futon3c.apm.library-loop-runner-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.library-loop-runner :as runner])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "library-loop-runner-"
                                      (make-array FileAttribute 0))))

(defn- init! [dir]
  (runner/write-state!
   dir
   (runner/initial-state {:problem-id "t00J02"
                          :workspace "/tmp/apm-lean-t00J02"
                          :base-sha "base"
                          :head-sha "head"})))

(deftest atomic-state-round-trip-and-validation
  (let [dir (temp-dir)
        state (init! dir)]
    (is (= state (runner/read-state dir)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unsupported-schema"
                          (runner/write-state! dir (assoc state :schema 2))))))

(deftest receipts-are-append-only-and-idempotent
  (let [dir (temp-dir)
        _ (init! dir)
        intent (runner/begin-action! dir :turn)
        receipt (runner/append-receipt! dir intent {:head-sha "turn-head"})]
    (is (= receipt (runner/append-receipt! dir intent {:head-sha "turn-head"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"receipt-conflict"
                          (runner/append-receipt! dir intent
                                                  {:head-sha "other-head"})))))

(deftest restart-at-turn-running-never-launches-a-second-turn
  (let [dir (temp-dir)
        _ (init! dir)
        intent (runner/begin-action! dir :turn)
        observations (atom 0)]
    (is (= :turn-running (:phase (runner/read-state dir))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transition-not-allowed"
                          (runner/begin-action! dir :turn)))
    (is (= :pending
           (:status (runner/reconcile! dir (fn [observed]
                                             (swap! observations inc)
                                             (is (= intent observed))
                                             :pending)))))
    (is (= 1 @observations))
    (is (= :settled
           (:status (runner/reconcile! dir (fn [_]
                                             (swap! observations inc)
                                             {:head-sha "turn-head"})))))
    (is (= :gating (:phase (runner/read-state dir))))
    (is (= 2 @observations))
    (is (= :idle (:status (runner/reconcile! dir (fn [_]
                                                   (throw (Exception. "duplicate")))))))))

(deftest restart-at-gating-uses-one-receipt
  (let [dir (temp-dir)
        _ (init! dir)
        turn (runner/begin-action! dir :turn)]
    (runner/append-receipt! dir turn {:head-sha "h1"})
    (runner/reconcile! dir (constantly nil))
    (let [gate (runner/begin-action! dir :gate)]
      (runner/append-receipt! dir gate {:receipt/path "gates/turn-001.edn"
                                        :checkpoint-due? false})
      (is (= :settled (:status (runner/reconcile! dir (constantly nil)))))
      (is (= :turn-ready (:phase (runner/read-state dir))))
      (is (= 2 (:turn (runner/read-state dir))))
      (is (= :idle (:status (runner/reconcile! dir (fn [_]
                                                   (throw (Exception. "duplicate gate"))))))))))

(deftest restart-at-review-and-bank-pending-does-not-duplicate-effects
  (let [dir (temp-dir)
        _ (init! dir)
        turn (runner/begin-action! dir :turn)]
    (runner/append-receipt! dir turn {:head-sha "candidate"})
    (runner/reconcile! dir (constantly nil))
    (let [gate (runner/begin-action! dir :gate)]
      (runner/append-receipt! dir gate {:receipt/path "gates/turn-001.edn"
                                        :checkpoint-due? true})
      (runner/reconcile! dir (constantly nil)))
    (let [review (runner/begin-action! dir :review)]
      (is (= :review-pending (:phase (runner/read-state dir))))
      (is (= :pending (:status (runner/reconcile! dir (constantly :pending)))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transition-not-allowed"
                            (runner/begin-action! dir :review)))
      (runner/append-receipt! dir review {:ruling :approved})
      (runner/reconcile! dir (fn [_] (throw (Exception. "duplicate review")))))
    ;; Approval is settled while retaining :review-pending as the explicit
    ;; authority from which the bank intent is created.
    (is (nil? (:intent (runner/read-state dir))))
    (let [bank (runner/begin-action! dir :bank)
          bank-observations (atom 0)]
      (is (= :bank-pending (:phase (runner/read-state dir))))
      (is (= bank (:pending-bank (runner/read-state dir))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transition-not-allowed"
                            (runner/begin-action! dir :bank)))
      (is (= :pending (:status (runner/reconcile! dir (fn [_]
                                                       (swap! bank-observations inc)
                                                       :pending)))))
      (runner/append-receipt! dir bank {:bank-sha "banked"})
      (is (= :settled (:status (runner/reconcile! dir (fn [_]
                                                       (throw (Exception. "duplicate bank")))))))
      (is (= 1 @bank-observations))
      (is (= :turn-ready (:phase (runner/read-state dir))))
      (is (= "banked" (:base-sha (runner/read-state dir)))))))

(deftest rejects-out-of-order-transitions
  (let [dir (temp-dir)]
    (init! dir)
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transition-not-allowed"
                          (runner/begin-action! dir :bank)))))
