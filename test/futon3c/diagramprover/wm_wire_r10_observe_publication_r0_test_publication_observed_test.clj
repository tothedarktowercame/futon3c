(ns futon3c.diagramprover.wm-wire-r10-observe-publication-r0-test-publication-observed-test
  "Wire [:r10-observe-publication :r0-test :publication-observed]: the
  publication observation reaching row 0's test box,
  futon2/test/futon2/aif/flight_enact_test.clj, whose
  a-discharged-repair-obligation-is-observed-as-published deftest (added
  in futon2 6d98d37c for this wire, E-kimi-task-75) asserts

    (is (= {:observed true :at \"run-pub\" :evidence discharged-repair-entry}
           (:publication-observed enactment)) ...)

  Reading the test: its enact-fn passes :repair-id-fn (the target's
  chosen action discharges repair id \"occ-t\") and a :fetch-run-record
  answering with a run record the REAL persist-run-record! wrote, whose
  :repair/publication carries the discharge receipt — and no
  :publication-observation override. So enact-fn's step-12 read IS the
  real observe-publication-fn: the test box drives the real writer var,
  and the writer's honest product is a PRESENT value,
  {:observed true :at \"run-pub\" :evidence {...}}.

  The with-redefs wrapper below confirms it: the writer's value is
  captured as the test consumes it, tampering it fails the test with the
  tampered value in its own report, and a typed absence at the field is
  refused the same way. WITNESSED-HERMETICALLY.

  (History: before futon2 6d98d37c this wire was UNVERIFIED — the box
  drove the real writer, but its fixture discharged no repair obligation
  so only the writer's typed absence crossed. The new deftest is the
  fixture that makes a present value cross; the wire is re-kinded.)"
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def present-observation
  "The value the real observe-publication-fn writes for the test's
  fixture: the discharge receipt committed for repair id \"occ-t\"."
  {:observed true :at "run-pub"
   :evidence {:status :receipt-committed :repair/id "occ-t" :repair/discharged? true}})

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "its one enactment is {:absent :no-dispatch-configured}: no enactment record was written, so no :publication-observed on either end"}
   {:path "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
    :sha256 "e51063896e2a42096718d902e0b4dfe0e4652323de0b42848c2c0cf318bf6c89"
    :why "the hand-authored exemplar enactment (claude-10, 2026-09-24) predates H-publish: it carries no :publication-observed"}])

(defn check [] (:fields (producer-record/record "publication-r0-test-observe")))

(def wire
  {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r10-observe-publication-r0-test-publication-observed-test/tampering-the-writer-fails-the-test-with-the-tampered-value :kind :value-varying
                  :product [:report-type] :intervention :before-reader}
  :wire [:r10-observe-publication :r0-test :publication-observed]
   :kind :witnessed-hermetically
   :test `the-test-box-consumes-the-writers-present-observation
   :check check
   :live-records-read live-records-read
   :note "flight_enact_test drives the real observe-publication-fn (enact-fn's default, no override) over a fixture whose target HAS a repair obligation discharged on a real persist-run-record!-written run record; the present value {:observed true :at \"run-pub\" :evidence {:repair/id \"occ-t\" :status :receipt-committed ...}} crosses into the test's assertion. No live record carries both ends, so witnessed hermetically. The values are read from the producer record."})

(deftest the-test-box-consumes-the-writers-present-observation
  (let [{:keys [writer reader report-type] :as o} (check)]
    (is (= present-observation writer)
        "the real observe-publication-fn's honest product for this fixture")
    (is (= writer reader) "the test consumed the writer's value (its assertion passed)")
    (is (= :pass report-type))
    (is (not (w/typed-absence? reader)) "what crossed is present, not a typed absence")
    (is (w/received? o))))

(deftest tampering-the-writer-fails-the-test-with-the-tampered-value
  ;; a bad case, proving the reader receives what the writer wrote: a
  ;; different observation forged at the writer's door reaches the test's
  ;; assertion, which fails on it — the value in the test's own report is
  ;; the forged one, not a restated literal.
  (let [forged {:observed true :at "run-pub" :evidence {:repair/id "forged"}}
        {:keys [writer reader report-type]} (:different (check))]
    (is (= present-observation writer))
    (is (= forged reader) "the assertion consumed the writer's (tampered) value")
    (is (= :fail report-type) "and the test failed on it, as it must")))

(deftest a-typed-absence-at-the-field-is-refused
  ;; the other bad case: a typed absence forged at the writer's door
  ;; reaches the test's assertion and fails there — the reader does not
  ;; receive.
  (let [absent {:absent :no-repair-obligation-for-target :target "M-t"}
        {:keys [writer reader report-type] :as o} (:absent (check))]
    (is (= present-observation writer))
    (is (= absent reader))
    (is (= :fail report-type))
    (is (w/typed-absence? reader))
    (is (not (w/received? o)))))
