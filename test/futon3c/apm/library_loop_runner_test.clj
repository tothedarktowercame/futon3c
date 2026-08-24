(ns futon3c.apm.library-loop-runner-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.library-loop-checkpoint :as checkpoint]
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

(defn- checkpoint-claim
  ([] (checkpoint-claim :t00J02/producer "def Producer : Prop := True"))
  ([id statement]
   {:id id
    :declaration 'OrientedSurfacePreimageDuality.Producer
    :statement statement
    :statement-digest (checkpoint/statement-digest statement)
    :dependencies #{:surface/duality}
    :strength :equivalent
    :reduction-witness "A concrete premise was discharged."
    :next-plan "Construct the remaining producer."}))

(defn- review-result [dir overrides]
  (merge (:pending-checkpoint (runner/read-state dir))
         {:outcome :approved
          :bank-authorized? true
          :progress-ruling :reduced
          :review-rationale "The residual obligation was reduced."
          :consecutive-nonreductions 0}
         overrides))

(defn- pause-at-rejected-review! [dir]
  (runner/write-state!
   dir (assoc (runner/initial-state
               {:problem-id "t00J02" :workspace "/tmp/apm-lean-t00J02"
                :base-sha "base" :head-sha "candidate"})
              :phase :checkpoint-ready))
  (runner/install-checkpoint! dir (checkpoint-claim))
  (let [intent (runner/begin-action! dir :review)]
    (runner/append-receipt!
     dir intent (review-result dir {:outcome :rejected
                                    :bank-authorized? false
                                    :finding :review-not-approved}))
    (runner/reconcile! dir (constantly nil))
    intent))

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
    (runner/install-checkpoint! dir (checkpoint-claim))
    (let [review (runner/begin-action! dir :review)]
      (is (= :review-pending (:phase (runner/read-state dir))))
      (is (= :pending (:status (runner/reconcile! dir (constantly :pending)))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"action-already-pending"
                            (runner/begin-action! dir :bank)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"action-already-pending"
                            (runner/begin-action! dir :review)))
      (runner/append-receipt! dir review (review-result dir {}))
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
      (runner/append-receipt! dir bank {:outcome :banked :bank-sha "candidate"})
      (is (= :settled (:status (runner/reconcile! dir (fn [_]
                                                       (throw (Exception. "duplicate bank")))))))
      (is (= 1 @bank-observations))
      (is (= :turn-ready (:phase (runner/read-state dir))))
      (is (= "candidate" (:base-sha (runner/read-state dir)))))))

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
      (runner/append-receipt! gate-dir gate
                              {:outcome :red :finding :lean-failed
                               :failure-fingerprint "failure-a"})
      (runner/reconcile! gate-dir (constantly nil))
      (is (= :turn-ready (:phase (runner/read-state gate-dir))))
      (is (= 1 (:consecutive-same-failures
                (runner/read-state gate-dir)))))))

(deftest preceding-red-gate-feedback-is-exact-bounded-and-turn-bound
  (let [dir (temp-dir)]
    (init! dir)
    (let [turn (runner/begin-action! dir :turn)]
      (runner/append-receipt! dir turn {:outcome :ok :head-sha "candidate"})
      (runner/reconcile! dir (constantly nil)))
    (let [gate (runner/begin-action! dir :gate)]
      (runner/append-receipt!
       dir gate
       {:outcome :red
        :failure-fingerprint "malformed-ledger"
        :plan {:inputs {:head-sha "candidate"
                        :file-digests {"secret" "not-rendered"}}}
        :registration {:outcome :red
                       :finding :malformed-target-record
                       :snapshot {:files "not-rendered"}}
        :observation/evidence [{:stdout "not-rendered" :stderr "not-rendered"}]})
      (runner/reconcile! dir (constantly nil)))
    (let [feedback (runner/preceding-gate-feedback dir (runner/read-state dir))]
      (is (= {:schema 1
              :problem-id "t00J02"
              :turn 1
              :intent-id (:intent-id feedback)
              :head-sha "candidate"
              :outcome :red
              :finding :malformed-target-record
              :failure-fingerprint "malformed-ledger"
              :registration/outcome :red}
             feedback))
      (is (not (re-find #"not-rendered" (pr-str feedback)))))
    ;; Advancing the durable turn makes the old receipt ineligible for replay.
    (runner/write-state! dir (assoc (runner/read-state dir) :turn 3))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"preceding-gate-receipt-missing"
                          (runner/preceding-gate-feedback
                           dir (runner/read-state dir))))))

(deftest malformed-expected-gate-receipt-fails-closed
  (let [dir (temp-dir)]
    (init! dir)
    (let [turn (runner/begin-action! dir :turn)]
      (runner/append-receipt! dir turn {:outcome :ok :head-sha "candidate"})
      (runner/reconcile! dir (constantly nil)))
    (let [gate (runner/begin-action! dir :gate)]
      (runner/append-receipt!
       dir gate {:outcome :red :failure-fingerprint "red"
                 :plan {:inputs {:head-sha "candidate"}}
                 :registration {:outcome :red :finding :malformed-target-record}})
      (runner/reconcile! dir (constantly nil))
      ;; Model corruption/fabrication at the otherwise expected durable path.
      (spit (.toFile (runner/receipt-path dir gate))
            (pr-str {:schema 1 :problem-id "another-problem" :turn 1
                     :action :gate :intent-id (:id gate)
                     :result {:outcome :red
                              :plan {:inputs {:head-sha "candidate"}}}}))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"preceding-gate-receipt-mismatch"
                            (runner/preceding-gate-feedback
                             dir (runner/read-state dir)))))))

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
      (runner/install-checkpoint! dir (checkpoint-claim))
      (let [review (runner/begin-action! dir :review)]
        (runner/append-receipt!
         dir review
         (review-result dir {:outcome :rejected
                             :bank-authorized? false
                             :finding :review-not-approved
                             :progress-ruling :equivalent
                             :consecutive-nonreductions 1
                             :review-rationale (str "Reviewer did not approve " ruling)}))
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
    (runner/install-checkpoint! first-dir (checkpoint-claim))
    (runner/install-checkpoint! second-dir (checkpoint-claim))
    (let [review (runner/begin-action! first-dir :review)]
      (runner/append-receipt!
       first-dir review
       (review-result first-dir
                      {:consecutive-nonreductions 1
                       :progress-ruling :equivalent
                       :review-rationale "No semantic change."}))
      (runner/reconcile! first-dir (constantly nil))
      (is (= :review-pending (:phase (runner/read-state first-dir))))
      (is (= 1 (:consecutive-nonreductions (runner/read-state first-dir)))))
    (let [review (runner/begin-action! second-dir :review)]
      (runner/append-receipt!
       second-dir review
       (review-result second-dir
                      {:outcome :rejected :bank-authorized? false
                       :finding :checkpoint-nonreduction-limit
                       :consecutive-nonreductions 2
                       :progress-ruling :equivalent
                       :review-rationale "Still equivalent."}))
      (runner/reconcile! second-dir (constantly nil))
      (is (= :paused (:phase (runner/read-state second-dir))))
      (is (= :checkpoint-nonreduction-limit
             (get-in (runner/read-state second-dir) [:pause/finding :type]))))))

(deftest review-is-bound-to-installed-checkpoint-across-restart
  (let [dir (temp-dir)]
    (runner/write-state!
     dir (assoc (runner/initial-state
                 {:problem-id "t00J02" :workspace "/tmp/apm-lean-t00J02"
                  :base-sha "base" :head-sha "candidate"})
                :phase :checkpoint-ready))
    (let [identity (runner/install-checkpoint! dir (checkpoint-claim))
          review (runner/begin-action! dir :review)
          restarted (runner/read-state dir)]
      (is (= identity (:pending-checkpoint restarted)))
      (is (= identity (:checkpoint/identity (:intent restarted))))
      (runner/append-receipt! dir review (review-result dir {}))
      (runner/reconcile! dir (constantly nil))
      (is (= :review-pending (:phase (runner/read-state dir)))))))

(deftest arbitrary-or-stale-checkpoint-review-fails-closed
  (doseq [[label override]
          [[:arbitrary-digest {:checkpoint-digest (apply str (repeat 64 "f"))}]
           [:other-obligation {:obligation-id :t00J02/other}]
           [:prior-turn {:turn 0}]
           [:prior-head {:head-sha "prior-head"}]]]
    (let [dir (temp-dir)]
      (runner/write-state!
       dir (assoc (runner/initial-state
                   {:problem-id "t00J02" :workspace "/tmp/apm-lean-t00J02"
                    :base-sha "base" :head-sha "candidate"})
                  :phase :checkpoint-ready))
      (runner/install-checkpoint! dir (checkpoint-claim))
      (let [review (runner/begin-action! dir :review)]
        ;; Reading state here models process death/restart before terminal
        ;; collection; the installed identity and intent survive unchanged.
        (runner/read-state dir)
        (runner/append-receipt! dir review (review-result dir override))
        (runner/reconcile! dir (constantly nil))
        (is (= :paused (:phase (runner/read-state dir))) (name label))
        (is (= :review-checkpoint-identity-mismatch
               (get-in (runner/read-state dir) [:pause/finding :type]))
            (name label))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"transition-not-allowed"
                              (runner/begin-action! dir :bank)))))))

(deftest rejected-review-can-be-reconsidered-with-a-distinct-durable-intent
  (let [dir (temp-dir)
        original (pause-at-rejected-review! dir)
        original-receipt (runner/read-receipt dir original)
        reconsideration (runner/begin-review-reconsideration! dir)]
    (is (= :review-reconsideration (:action reconsideration)))
    (is (= {:action :review :intent-id (:id original)}
           (:prior-review reconsideration)))
    (is (not= (:id original) (:id reconsideration)))
    ;; Process restart preserves the exact same pending intent.
    (is (= reconsideration (:intent (runner/read-state dir))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"action-already-pending"
                          (runner/begin-review-reconsideration! dir)))
    (runner/append-receipt! dir reconsideration
                            (assoc (review-result dir {}) :prior-review
                                   (:prior-review reconsideration)))
    (runner/reconcile! dir (constantly nil))
    (is (= :review-pending (:phase (runner/read-state dir))))
    (is (nil? (:intent (runner/read-state dir))))
    (is (= original-receipt (runner/read-receipt dir original)))))

(deftest second-review-rejection-remains-paused-and-chains-forward
  (let [dir (temp-dir)
        original (pause-at-rejected-review! dir)
        second (runner/begin-review-reconsideration! dir)]
    (runner/append-receipt!
     dir second (assoc (review-result dir {:outcome :rejected
                                           :bank-authorized? false
                                           :finding :review-not-approved})
                       :prior-review (:prior-review second)))
    (runner/reconcile! dir (constantly nil))
    (is (= :paused (:phase (runner/read-state dir))))
    (is (= (:id second) (get-in (runner/read-state dir)
                                [:pause/finding :intent-id])))
    (let [third (runner/begin-review-reconsideration! dir)]
      (is (= {:action :review-reconsideration :intent-id (:id second)}
             (:prior-review third)))
      (is (not= (:id original) (:id third))))))

(deftest reconsideration-refuses-nonreview-pauses-and-corrupt-prior-authority
  (let [wrong-pause (temp-dir)]
    (init! wrong-pause)
    (runner/write-state! wrong-pause
                         (assoc (runner/read-state wrong-pause)
                                :phase :paused
                                :pause/finding {:type :turn-failed}))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"prior-review-receipt-invalid"
                          (runner/begin-review-reconsideration! wrong-pause))))
  (doseq [[label mutate]
          [[:digest #(assoc-in % [:result :checkpoint-digest]
                              (apply str (repeat 64 "f")))]
           [:head #(assoc-in % [:result :head-sha] "stale")]
           [:obligation #(assoc-in % [:result :obligation-id] :other/id)]
           [:fabricated-id #(assoc % :intent-id (apply str (repeat 64 "a")))]]]
    (let [dir (temp-dir)
          prior (pause-at-rejected-review! dir)
          path (.toFile (runner/receipt-path dir prior))
          altered (mutate (runner/read-receipt dir prior))]
      (spit path (str (pr-str altered) "\n"))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"prior-review-receipt-invalid"
                            (runner/begin-review-reconsideration! dir))
          (name label)))))
