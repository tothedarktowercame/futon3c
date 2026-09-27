(ns futon3c.diagramprover.wm-wire-r10-observe-publication-r0-test-publication-observed-test
  "Wire [:r10-observe-publication :r0-test :publication-observed]: the
  publication observation reaching row 0's test box,
  futon2/test/futon2/aif/flight_enact_test.clj, whose line-45 assertion is

    (is (= {:absent :no-repair-obligation-for-target :target \"M-t\"}
           (:publication-observed enactment)) ...)

  Reading the test: its enact-fn (the `enact` helper) passes
  :fetch-run-record but no :publication-observation and no :repair-id-fn,
  so enact-fn's step-12 read IS the real observe-publication-fn — the test
  box drives the real writer var, and the with-redefs wrapper below
  confirms it (the writer's value is captured; tampering it fails the
  test with the tampered value in its own report).

  But the only value that ever crosses is the writer's typed absence:
  the test's chosen action discharges no repair obligation, so
  observe-publication-fn's honest product is {:absent
  :no-repair-obligation-for-target :target \"M-t\"}, and the first layer
  refuses a typed absence as a carried value. UNVERIFIED, with the
  reason — the plumbing is witnessed end to end; no observation value
  crosses this reader."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support]))

(def live-records-read
  [(assoc support/flight-278b6988
          :why "its one enactment is {:absent :no-dispatch-configured}: no enactment record was written, so no :publication-observed on either end")
   (assoc support/click-001-enactment
          :why "the hand-authored exemplar enactment (claude-10, 2026-09-24) predates H-publish: it carries no :publication-observed")])

(defn check [] (support/r0-test-observe identity))

(def wire
  {:wire [:r10-observe-publication :r0-test :publication-observed]
   :kind :unverified
   :test `the-test-box-consumes-the-writers-typed-absence
   :check check
   :live-records-read live-records-read
   :note "flight_enact_test DOES drive the real observe-publication-fn (enact-fn's default, no override), but the only value crossing is the writer's typed absence {:absent :no-repair-obligation-for-target}: the fixture discharges no repair obligation, so no observation value ever reaches this reader."})

(deftest the-test-box-consumes-the-writers-typed-absence
  (let [{:keys [writer reader report-type] :as o} (check)]
    (is (= {:absent :no-repair-obligation-for-target :target "M-t"} writer)
        "the real observe-publication-fn's honest product for this fixture")
    (is (= writer reader) "the test consumed the writer's value (its assertion passed)")
    (is (= :pass report-type))
    (is (w/typed-absence? reader) "but what crossed is a typed absence")
    (is (not (w/received? o)))))

(deftest tampering-the-writer-fails-the-test-with-the-tampered-value
  ;; the bad case, proving the reader receives what the writer wrote: a
  ;; real observation forged at the writer's door reaches the test's
  ;; assertion, which fails on it — the value in the test's own report is
  ;; the forged one, not a restated literal.
  (let [forged {:observed true :at "run-1" :evidence {:repair/id "forged"}}
        {:keys [writer reader report-type]} (support/r0-test-observe (constantly forged))]
    (is (= {:absent :no-repair-obligation-for-target :target "M-t"} writer))
    (is (= forged reader) "the assertion consumed the writer's (tampered) value")
    (is (= :fail report-type) "and the test failed on it, as it must")))
