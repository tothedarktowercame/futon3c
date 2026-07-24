(ns futon3c.aif.stack-generator-test
  "Tests for the live AIF+ stack projection — in particular the
   E-wm-live-recommendation surface (`:reading :next-move-live`)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.aif.stack-generator :as sg]))

(def ^:private fake-snapshot
  "A synthetic WM snapshot mirroring the shape that
   `futon3c.wm.scheduler/snapshot-for-days` returns.  The :payload is
   JSON-key-stringified (string keys) because the scheduler renders the
   atom that way for HTTP serialization parity."
  {:days 14
   :as-of (java.time.Instant/now)
   :body-bytes 1234
   :duration-ms 9
   :payload
   {"judgement"
    {"mode" "recovery"
     "ranked-actions"
     [{"rank" 1
       "G-total" -4.99
       "controller-score" -4.99
       "habit-prior-bias" -2.0
       "action" {"type" "address-sorry"
                 "target" "sorry/foo"
                 "rationale" "open sorry: foo"}}
      {"rank" 2
       "G-total" -4.39
       "controller-score" -4.39
       "habit-prior-bias" -1.0
       "action" {"type" "address-sorry"
                 "target" "sorry/bar"
                 "rationale" "open sorry: bar"}}
      {"rank" 3
       "G-total" -4.39
       "controller-score" -4.39
       "habit-prior-bias" -3.0
       "action" {"type" "no-op"
                 "rationale" "wait"}}]
     "selection-gain" {"selection-gain" 1.0}
     "decision"
     {"action" {"type" "address-sorry"
                "target" "sorry/bar"
                "rationale" "open sorry: bar"}
      "selected-policy-id" "pi-s-test"
      "selected-mission-ids" ["sorry/bar"]
      "strategic-memory"
      {"influenced?" true
       "authority" "live"
       "memory-ids" ["e-test"]
       "counterfactuals"
       {"fixed" ["sorry/foo" "sorry/bar"]
        "additive-controller" ["sorry/foo" "sorry/bar"]
        "scheduler-habit" ["sorry/bar" "sorry/foo"]}
       "actuation" {"status" "pending-downstream-gates"
                    "authorized?" false
                    "executed?" false}}}
     "priorities"
     [{"type" "missing-head" "id" "h1" "summary" "no head h1"}
      {"type" "channel-gap" "id" "g1" "summary" "gap g1"}]}}})

(deftest derive-next-move-live-from-fake-snapshot
  (testing "Projects the reason-bearing decision, not ranked-actions top-1"
    (let [live (sg/derive-next-move-live fake-snapshot)]
      (is (some? live))
      (is (= 2 (:rank live)))
      (is (= -4.39 (:G-total live)))
      (is (= "recovery" (:mode live)))
      (is (= :judgement.decision (:source live)))
      (is (= "address-sorry sorry/bar" (:specifically live)))
      (is (= "open sorry: bar" (:rationale live)))
      (is (= :recommendation-issued (:status live)))
      (is (= "sorry/bar"
             (get-in live [:recommendation :target])))
      (is (false? (get-in live
                          [:selection-boundary
                           :recomputed?])))
      (is (= "sorry/bar"
             (get-in live
                     [:rankings :scheduler-habit :winner :target])))
      (is (true? (get-in live
                         [:strategic-memory :influenced?])))
      (is (= 300 (:scheduler-period-seconds live)))
      (is (false? (:stale? live)) "fresh snapshot is not stale")
      (testing "Alternatives exclude the authoritative action"
        (let [alts (:alternatives-considered live)]
          (is (contains? alts :alternative-1))
          (is (contains? alts :alternative-2))
          (is (re-find #"address-sorry sorry/foo"
                       (:alternative-1 alts)))
          (is (re-find #"no-op" (:alternative-2 alts)))))
      (testing "Priorities are carried through (top 5)"
        (let [prs (:priorities live)]
          (is (= 2 (count prs))))))))

(deftest derive-next-move-live-handles-nil
  (testing "Returns nil when there is no snapshot"
    (is (nil? (sg/derive-next-move-live nil)))))

(deftest derive-next-move-live-handles-empty-ranked
  (testing "Keeps the authoritative decision when rankings are empty"
    (let [empty-snap (assoc-in fake-snapshot
                               [:payload "judgement" "ranked-actions"] [])
          live (sg/derive-next-move-live empty-snap)]
      (is (= "sorry/bar" (get-in live [:recommendation :target])))
      (is (nil? (:G-total live)))
      (is (= [] (:tied-actions live))))))

(deftest derive-next-move-live-marks-stale
  (testing "An hours-old snapshot is marked stale (>2× period)"
    (let [old-as-of (.minusSeconds (java.time.Instant/now) (long 1200))
          stale-snap (assoc fake-snapshot :as-of old-as-of)
          live (sg/derive-next-move-live stale-snap)]
      (is (true? (:stale? live))))))
