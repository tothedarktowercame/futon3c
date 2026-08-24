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
        receipt (runner/append-receipt! dir intent {:outcome :ok :head-sha "turn-head"})]
    (is (= receipt (runner/append-receipt! dir intent {:outcome :ok :head-sha "turn-head"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"receipt-conflict"
                          (runner/append-receipt! dir intent
                                                  {:outcome :ok :head-sha "other-head"})))))

(deftest restart-at-turn-running-never-launches-a-second-turn
  (let [dir (temp-dir)
        _ (init! dir)
        intent (runner/begin-action! dir :turn)
        observations (atom 0)]
    (is (= :turn-running (:phase (runner/read-state dir))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"action-already-pending"
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
                                             {:outcome :ok :head-sha "turn-head"})))))
    (is (= :gating (:phase (runner/read-state dir))))
    (is (= 2 @observations))
    (is (= :idle (:status (runner/reconcile! dir (fn [_]
                                                   (throw (Exception. "duplicate")))))))))

(deftest restart-at-gating-uses-one-receipt
  (let [dir (temp-dir)
        _ (init! dir)
        turn (runner/begin-action! dir :turn)]
    (runner/append-receipt! dir turn {:outcome :ok :head-sha "h1"})
    (runner/reconcile! dir (constantly nil))
    (let [gate (runner/begin-action! dir :gate)]
      (runner/append-receipt! dir gate {:outcome :green
                                        :receipt/path "gates/turn-001.edn"
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
    (runner/append-receipt! dir turn {:outcome :ok :head-sha "candidate"})
    (runner/reconcile! dir (constantly nil))
    (let [gate (runner/begin-action! dir :gate)]
      (runner/append-receipt! dir gate {:outcome :green
                                        :receipt/path "gates/turn-001.edn"
                                        :checkpoint-due? true})
      (runner/reconcile! dir (constantly nil)))
    (let [review (runner/begin-action! dir :review)]
      (is (= :review-pending (:phase (runner/read-state dir))))
      (is (= :pending (:status (runner/reconcile! dir (constantly :pending)))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"action-already-pending"
                            (runner/begin-action! dir :bank)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"action-already-pending"
                            (runner/begin-action! dir :review)))
      (runner/append-receipt! dir review
                              {:outcome :approved
                               :bank-authorized? true
                               :obligation-id :t00J02/producer
                               :consecutive-nonreductions 0
                               :progress-ruling :reduced
                               :review-rationale "The residual was reduced."
                               :checkpoint-digest (apply str (repeat 64 "a"))})
      (runner/reconcile! dir (fn [_] (throw (Exception. "duplicate review")))))
    ;; Approval is settled while retaining :review-pending as the explicit
    ;; authority from which the bank intent is created.
    (is (nil? (:intent (runner/read-state dir))))
    (let [bank (runner/begin-action! dir :bank)
          bank-observations (atom 0)]
      (is (= :bank-pending (:phase (runner/read-state dir))))
      (is (= bank (:pending-bank (runner/read-state dir))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"action-already-pending"
                            (runner/begin-action! dir :bank)))
      (is (= :pending (:status (runner/reconcile! dir (fn [_]
                                                       (swap! bank-observations inc)
                                                       :pending)))))
      (runner/append-receipt! dir bank {:outcome :banked :bank-sha "banked"})
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

(deftest failed-turn-and-gate-outcomes-pause-with-typed-findings
  (let [turn-dir (temp-dir)
        gate-dir (temp-dir)]
    (init! turn-dir)
    (let [intent (runner/begin-action! turn-dir :turn)]
      (runner/append-receipt! turn-dir intent {:outcome :failed :exit 1})
      (runner/reconcile! turn-dir (constantly nil))
      (is (= :paused (:phase (runner/read-state turn-dir))))
      (is (= :turn-failed (get-in (runner/read-state turn-dir)
                                  [:pause/finding :type]))))
    (init! gate-dir)
    (let [turn (runner/begin-action! gate-dir :turn)]
      (runner/append-receipt! gate-dir turn {:outcome :ok :head-sha "h1"})
      (runner/reconcile! gate-dir (constantly nil)))
    (let [gate (runner/begin-action! gate-dir :gate)]
      (runner/append-receipt! gate-dir gate {:outcome :red :finding :lean-failed})
      (runner/reconcile! gate-dir (constantly nil))
      (is (= :paused (:phase (runner/read-state gate-dir))))
      (is (= :gate-failed (get-in (runner/read-state gate-dir)
                                  [:pause/finding :type]))))))

(deftest only-an-approved-review-authorizes-bank
  (doseq [ruling [:rejected :unclear :equivalent]]
    (let [dir (temp-dir)]
      (init! dir)
      (let [turn (runner/begin-action! dir :turn)]
        (runner/append-receipt! dir turn {:outcome :ok :head-sha "candidate"})
        (runner/reconcile! dir (constantly nil)))
      (let [gate (runner/begin-action! dir :gate)]
        (runner/append-receipt! dir gate {:outcome :green
                                          :checkpoint-due? true})
        (runner/reconcile! dir (constantly nil)))
      (let [review (runner/begin-action! dir :review)]
        (runner/append-receipt! dir review {:ruling ruling})
        (runner/reconcile! dir (constantly nil)))
      (is (= :paused (:phase (runner/read-state dir))))
      (is (= :review-not-approved
             (get-in (runner/read-state dir) [:pause/finding :type])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transition-not-allowed"
                            (runner/begin-action! dir :bank))))))

(deftest checkpoint-review-decision-updates-durable-valve-state
  (let [first-dir (temp-dir)
        second-dir (temp-dir)]
    (doseq [dir [first-dir second-dir]]
      (runner/write-state!
       dir (assoc (runner/initial-state
                   {:problem-id "t00J02" :workspace "/tmp/apm-lean-t00J02"
                    :base-sha "base" :head-sha "candidate"})
                  :phase :checkpoint-ready
                  :obligation/id :t00J02/producer
                  :consecutive-nonreductions (if (= dir first-dir) 0 1))))
    (let [review (runner/begin-action! first-dir :review)]
      (runner/append-receipt!
       first-dir review
       {:outcome :approved :bank-authorized? true
        :obligation-id :t00J02/producer :consecutive-nonreductions 1
        :progress-ruling :equivalent :review-rationale "No semantic change."
        :checkpoint-digest (apply str (repeat 64 "a"))})
      (runner/reconcile! first-dir (constantly nil))
      (is (= :review-pending (:phase (runner/read-state first-dir))))
      (is (= 1 (:consecutive-nonreductions (runner/read-state first-dir)))))
    (let [review (runner/begin-action! second-dir :review)]
      (runner/append-receipt!
       second-dir review
       {:outcome :rejected :bank-authorized? false
        :finding :checkpoint-nonreduction-limit
        :obligation-id :t00J02/producer :consecutive-nonreductions 2
        :progress-ruling :equivalent :review-rationale "Still equivalent."
        :checkpoint-digest (apply str (repeat 64 "b"))})
      (runner/reconcile! second-dir (constantly nil))
      (is (= :paused (:phase (runner/read-state second-dir))))
      (is (= :checkpoint-nonreduction-limit
             (get-in (runner/read-state second-dir) [:pause/finding :type]))))))
