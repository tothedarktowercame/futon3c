(ns futon3c.agents.chip-board-test
  "Acceptance tests for the chip-board runtime and the inbox-zero board
  (SPEC-chip-boards-v0.md). Each test names the spec property it pins."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.chip-board :as board]
            [futon3c.agents.inbox-zero-board :as izb]))

(def no-op (fn [_] nil))

(defn- effects-of [run] (mapcat :effects (:trace run)))

(deftest two-wire-validation-pins
  (testing "an observation chip missing a wire fails validation (two-wire)"
    (is (string? (board/validate-board
                  {:entry :a
                   :constants {}
                   :chips [{:chip/id :a :verb :smell :wires {:true :yield}}]}))))
  (testing "a wire to an unknown chip fails wiring validation"
    (is (string? (board/validate-wiring
                  {:chips [{:chip/id :a :verb :sing :wires {:true :nowhere}}]})))))

(deftest determinism-and-certificate
  (testing "identical board + inputs => identical trace (verify-trace)"
    (let [inputs (izb/observation-packet
                  [{:repo "futon3c" :clean? false :clauses-failed [:dirty]}]
                  #{} false)
          run (izb/run inputs no-op)]
      (is (:verified? (:certificate run)) "runtime certificate replays")
      (is (= :end/yield (:end-reason run))))))

(deftest inbox-zero-clean-path
  (testing "clean sweep => sing meters, yield, zero side effects on repos"
    (let [inputs (izb/observation-packet
                  [{:repo "futon3c" :clean? true :clauses-failed []}]
                  #{} false)
          run (izb/run inputs no-op)
          eff (effects-of run)]
      (is (some #(= :report (first %)) eff) "meters sung")
      (is (not (some #(= :commit (first %)) eff)) "nothing committed")
      (is (= :end/yield (:end-reason run)) "terminated at the named terminal"))))

(deftest inbox-zero-operator-pending-yields-first
  (testing "keypress true => immediate yield, sweep never smells"
    (let [inputs (izb/observation-packet
                  [{:repo "futon3c" :clean? false :clauses-failed [:dirty]}]
                  #{} true)
          run (izb/run inputs no-op)]
      (is (= 2 (count (:trace run))) "keypress then yield only")
      (is (not (some #(= :smell (:verb %)) (:trace run)))))))

(deftest inbox-zero-hazard-gate
  (testing "flagged repo with a LIVE turn => report path, no commit (U59)"
    ;; The U59 incident as a regression: the sweeper must not commit under
    ;; an in-flight edit. The safe arm reports; no refusal is needed because
    ;; ZAP is never reached — FEEL's true wire diverts to the report.
    (let [inputs (izb/observation-packet
                  [{:repo "futon3c" :clean? false :clauses-failed [:dirty]}]
                  ["futon3c"] false)
          run (izb/run inputs no-op)
          eff (effects-of run)]
      (is (not (some #(= :commit (first %)) eff)) "no commit under a live turn")
      (is (not (some #(= :zap (:verb %)) (:trace run))) "zap never fired")
      (is (some #(= :report (first %)) eff) "flag reported to the operator")))
  (testing "flagged repo, felt idle => commit fires (the act arm works)"
    (let [inputs (izb/observation-packet
                  [{:repo "futon3c" :clean? false :clauses-failed [:dirty]}]
                  [] false)
          run (izb/run inputs no-op)
          eff (effects-of run)]
      (is (some #(= :commit (first %)) eff))))
  (testing "ZAP without FEEL certification => typed refusal, never silent (C446)"
    (let [run (board/run-board
               {:entry :z :constants {}
                :chips [{:chip/id :z :verb :zap :args {:repo "futon3c"}
                         :wires {:true :y :false :y}}
                        {:chip/id :y :verb :yield}]}
               {} no-op)
          eff (effects-of run)]
      (is (not (some #(= :commit (first %)) eff)))
      (is (= :refusal (-> eff first first)))
      (is (= :zap-without-feel (-> eff first second :reason))))))

(deftest typed-none-on-unknown-repo
  (testing "LOOK at a repo absent from the sweep is a typed none, not an error"
    (let [run (board/run-board
               {:entry :l :constants {}
                :chips [{:chip/id :l :verb :look :args {:repo "ghost"}
                         :wires {:true :y :false :y}}
                        {:chip/id :y :verb :yield}]}
               {} no-op)]
      (is (= :end/yield (:end-reason run)))
      (is (-> run effects-of ffirst (= :typed-none))))))

(deftest fuel-exhaustion-is-typed
  (testing "fuel exhaustion ends the run as a typed end, not a crash
(a self-wire board is schema-valid; the caps are the cycle guard)"
    (let [run (board/run-board
               {:entry :a :constants {:fuel-budget 3}
                :chips [{:chip/id :a :verb :sing :wires {:true :a}}]}
               {} no-op)]
      (is (= :end/fuel-exhausted (:end-reason run))))))
