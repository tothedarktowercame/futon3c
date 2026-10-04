(ns futon3c.agency.work-order-check-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.work-order-check :as check]))

;; Fixture of the live codex-18/19/20 stasis case
;; (holes/excursions/E-agency-work-orders.md, "Token design for the live
;; case"): codex-19 holds a root; it ordered codex-20 (implement) and
;; codex-18 (replay); codex-20's child was delivered and closed; codex-19's
;; bellback turn ended with nothing dispatched.

(def idle {:running-jobs 0 :queued-jobs 0 :parked? false})

(def root
  {:id "wo-root" :requester "joe" :debtor "codex-19" :parent nil
   :job-id "invoke-root" :text "replay + implement the oracle fix"
   :opened-at 1000 :state :open :closed-by nil :nudges []})

(def child-20
  {:id "wo-20" :requester "codex-19" :debtor "codex-20" :parent "wo-root"
   :job-id "invoke-20" :text "implement the fix"
   :opened-at 2000 :state :closed :closed-by "codex-19" :nudges []})

(def child-18
  {:id "wo-18" :requester "codex-19" :debtor "codex-18" :parent "wo-root"
   :job-id "invoke-18" :text "replay the fix"
   :opened-at 2100 :state :closed :closed-by "codex-19" :nudges []})

;; E2 moment: codex-18's job just ended, so its order is :delivered and
;; the token has returned to codex-19 — codex-18 holds nothing.
(def child-18-delivered
  (assoc child-18 :state :delivered :closed-by nil))

(def orders [root child-20 child-18])

(deftest live-case-first-stall-nudges-holder
  (testing "codex-19 idle, child closed, nothing dispatched: nudge on root"
    (let [action (check/check "codex-19" orders idle 5000)]
      (is (= :nudge (:action action)))
      (is (= "wo-root" (:order action)))
      (is (= "codex-19" (:to action)))
      (is (re-find #"wo-root" (:text action)))
      (is (re-find #"replay \+ implement the oracle fix" (:text action)))
      (is (re-find #"joe" (:text action)))
      (is (re-find #"agency_send\.py" (:text action)))
      (is (re-find #"work-orders/wo-root/close" (:text action))))))

(deftest live-case-repeat-stall-escalates
  (testing "nudge already recorded since last move: escalate to joe"
    (let [nudged (assoc root :nudges [{:at 4000 :to "codex-19" :kind :nudge}])
          action (check/check "codex-19" [nudged child-20 child-18] idle 5000)]
      (is (= :escalate (:action action)))
      (is (= "wo-root" (:order action)))
      (is (= "joe" (:to action)))))

  (testing "after one escalation, nil forever for that order"
    (let [escalated (assoc root :nudges [{:at 4000 :to "codex-19" :kind :nudge}
                                         {:at 4500 :to "joe" :kind :escalate}])]
      (is (nil? (check/check "codex-19" [escalated child-20 child-18] idle 5000)))))

  (testing "a nudge to a different agent does not count as this holder's nudge"
    (let [nudged (assoc root :nudges [{:at 4000 :to "codex-20" :kind :nudge}])
          action (check/check "codex-19" [nudged child-20 child-18] idle 5000)]
      (is (= :nudge (:action action))))))

(deftest movement-resets-the-nudge
  (testing "a child opened after the nudge is movement; a fresh stall nudges again"
    (let [nudged (assoc root :nudges [{:at 1500 :to "codex-19" :kind :nudge}])
          later-child (assoc child-20 :opened-at 3000 :state :closed)
          action (check/check "codex-19" [nudged later-child child-18] idle 5000)]
      (is (= :nudge (:action action))))))

(deftest explicit-moved-at-resets-the-nudge
  (let [nudged (assoc root :moved-at 4500
                      :nudges [{:at 4000 :to "codex-19" :kind :nudge}])]
    (is (= :nudge (:action (check/check "codex-19"
                                        [nudged child-20 child-18] idle 5000))))))

(deftest delivered-order-held-by-requester
  (testing "codex-18's job just ended: wo-18 is :delivered, so codex-18 holds nothing"
    (is (nil? (check/check "codex-18" [root child-20 child-18-delivered] idle 5000))))
  (testing "the requester codex-19 holds the delivered token and owes the next move"
    (let [action (check/check "codex-19" [child-18-delivered] idle 5000)]
      (is (= :nudge (:action action)))
      (is (= "wo-18" (:order action))))))

(deftest busy-agent-is-skipped
  (testing "running job"
    (is (nil? (check/check "codex-19" orders {:running-jobs 1 :queued-jobs 0 :parked? false} 5000))))
  (testing "queued job"
    (is (nil? (check/check "codex-19" orders {:running-jobs 0 :queued-jobs 1 :parked? false} 5000))))
  (testing "parked"
    (is (nil? (check/check "codex-19" orders {:running-jobs 0 :queued-jobs 0 :parked? true} 5000)))))

(deftest open-child-passes-token-down
  (testing "parent's holder is not stalled while a child is open"
    (let [open-child (assoc child-18 :state :open)]
      (is (nil? (check/check "codex-19" [root open-child] idle 5000)))))
  (testing "parent's holder is not stalled while a child is delivered"
    ;; rule 3 skips the root (token below it); codex-19 still holds wo-18
    ;; directly as its requester.
    (let [action (check/check "codex-19" [root child-18-delivered] idle 5000)]
      (is (= "wo-18" (:order action))))))

(deftest oldest-stalled-order-wins
  (let [older {:id "wo-old" :requester "joe" :debtor "codex-19" :parent nil
               :job-id "j1" :text "old root" :opened-at 100 :state :open
               :closed-by nil :nudges []}
        action (check/check "codex-19" (conj orders older) idle 5000)]
    (is (= "wo-old" (:order action)))))

(deftest closed-and-foreign-orders-ignored
  (is (nil? (check/check "codex-19" [child-20] idle 5000)))
  (is (nil? (check/check "codex-20" [root] idle 5000))))
