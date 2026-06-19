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
       "action" {"type" "address-sorry"
                 "target" "sorry/foo"
                 "rationale" "open sorry: foo"}}
      {"rank" 2
       "G-total" -4.39
       "action" {"type" "address-sorry"
                 "target" "sorry/bar"
                 "rationale" "open sorry: bar"}}
      {"rank" 3
       "G-total" -4.39
       "action" {"type" "no-op"
                 "rationale" "wait"}}]
     "priorities"
     [{"type" "missing-head" "id" "h1" "summary" "no head h1"}
      {"type" "channel-gap" "id" "g1" "summary" "gap g1"}]}}})

(deftest derive-next-move-live-from-fake-snapshot
  (testing "Projects top-1 ranked-action into the next-move-tile shape"
    (let [live (sg/derive-next-move-live fake-snapshot)]
      (is (some? live))
      (is (= 1 (:rank live)))
      (is (= -4.99 (:G-total live)))
      (is (= "recovery" (:mode live)))
      (is (= :wm-judgement-ranked-actions (:source live)))
      (is (= "address-sorry sorry/foo" (:specifically live)))
      (is (= "open sorry: foo" (:rationale live)))
      (is (= 300 (:scheduler-period-seconds live)))
      (is (false? (:stale? live)) "fresh snapshot is not stale")
      (testing "Alternatives projection (ranks 2 + 3 only by default)"
        (let [alts (:alternatives-considered live)]
          (is (contains? alts :rank-2))
          (is (contains? alts :rank-3))
          (is (re-find #"address-sorry sorry/bar" (:rank-2 alts)))
          (is (re-find #"no-op" (:rank-3 alts)))))
      (testing "Priorities are carried through (top 5)"
        (let [prs (:priorities live)]
          (is (= 2 (count prs))))))))

(deftest derive-next-move-live-handles-nil
  (testing "Returns nil when there is no snapshot"
    (is (nil? (sg/derive-next-move-live nil)))))

(deftest derive-next-move-live-handles-empty-ranked
  (testing "Returns nil when ranked-actions is empty"
    (let [empty-snap (assoc-in fake-snapshot
                               [:payload "judgement" "ranked-actions"] [])]
      (is (nil? (sg/derive-next-move-live empty-snap))))))

(deftest derive-next-move-live-marks-stale
  (testing "An hours-old snapshot is marked stale (>2× period)"
    (let [old-as-of (.minusSeconds (java.time.Instant/now) (long 1200))
          stale-snap (assoc fake-snapshot :as-of old-as-of)
          live (sg/derive-next-move-live stale-snap)]
      (is (true? (:stale? live))))))
