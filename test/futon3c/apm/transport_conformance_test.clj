(ns futon3c.apm.transport-conformance-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.transport-conformance :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest lean-fixtures-have-the-same-conformance
  (testing "f63 visibility lag cannot be classified as authoritative absence"
    (let [result (sut/validate-certificate sut/f63-historical-certificate)]
      (is (false? (:ok result)))
      (is (= [:transport-conformance-outcome-mismatch
              :transport-conformance-decision-mismatch]
             (mapv :error/code (:findings result))))))
  (testing "f64 timeout, bounded retry, history, and visibility are conformant"
    (is (sut/conformant? sut/f64-transport-failure))
    (is (sut/conformant? sut/f64-successful-visibility))
    (is (empty? (sut/retry-then-success-findings
                 sut/f64-transport-failure
                 sut/f64-successful-visibility)))))

(deftest evidence-compatibility-and-retry-match-lean
  (is (sut/evidence-compatible? :success :obtained))
  (is (sut/evidence-compatible? :authoritative-absence :obtained))
  (is (sut/evidence-compatible? :malformed :invalid))
  (doseq [outcome [:timeout :unavailable :visibility-lag]]
    (is (sut/evidence-compatible? outcome :not-obtained))
    (is (sut/needs-retry? outcome :not-obtained)))
  (is (false? (sut/evidence-compatible? :authoritative-absence
                                        :not-obtained)))
  (is (false? (sut/needs-retry? :malformed :invalid))))

(deftest transport-failure-never-becomes-authoritative-absence
  (doseq [outcome [:timeout :unavailable]]
    (is (sut/transport-failure? outcome))
    (is (false? (sut/authoritative-absence? outcome)))
    (let [certificate (assoc sut/f64-transport-failure
                             :acquired-outcome outcome
                             :classified-outcome :authoritative-absence
                             :decision [:park :authoritative-absence])
          findings (sut/certificate-findings certificate)]
      (is (some #(= :transport-conformance-outcome-mismatch
                    (:error/code %)) findings))
      (is (some #(= :transport-conformance-decision-mismatch
                    (:error/code %)) findings)))))

(deftest retry-scheduling-preserves-last-valid-data
  (let [retry (sut/retry-state sut/f64-transport-failure)
        entry {:attempt 0 :operation :write
               :acquired-outcome :timeout :evidence :not-obtained}
        scheduled (sut/schedule-retry retry entry 180000)]
    (is (= 1 (:attempt scheduled)))
    (is (= 180000 (:wake-at-ms scheduled)))
    (is (= [entry] (:history scheduled)))
    (is (= (:last-valid-state retry) (:last-valid-state scheduled)))
    (is (= (:last-valid-evidence retry) (:last-valid-evidence scheduled)))))

(deftest decisions-are-deterministic-and-retries-are-bounded
  (let [retry (sut/retry-state sut/f64-transport-failure)
        first-decision (sut/decide retry :timeout :not-obtained)
        second-decision (sut/decide retry :timeout :not-obtained)]
    (is (= first-decision second-decision))
    (is (= [:retry 120000] first-decision))
    (is (nat-int? (second first-decision)))
    (is (nat-int? (:max-attempts retry)))
    (is (< (inc (:attempt retry)) (:max-attempts retry))))
  (is (= [:park :retry-exhausted]
         (sut/decide {:attempt 1 :max-attempts 2 :wake-at-ms 120000}
                     :timeout :not-obtained)))
  (is (= [:advance]
         (sut/decide {:attempt 0 :max-attempts 1 :wake-at-ms 0}
                     :success :obtained))))

(deftest malformed-and-unknown-wire-values-fail-closed
  (doseq [[mutation code]
          [[(assoc sut/f64-transport-failure :operation :delete)
            :transport-conformance-operation-unknown]
           [(assoc sut/f64-transport-failure :acquired-outcome :maybe)
            :transport-conformance-acquired-outcome-unknown]
           [(assoc sut/f64-transport-failure :classified-outcome :maybe)
            :transport-conformance-classified-outcome-unknown]
           [(assoc sut/f64-transport-failure :evidence :probably)
            :transport-conformance-evidence-unknown]
           [(assoc sut/f64-transport-failure :decision [:teleport])
            :transport-conformance-decision-constructor-unknown]
           [(assoc sut/f64-transport-failure :decision [:retry nil])
            :transport-conformance-retry-wake-invalid]
           [(update sut/f64-transport-failure :identity
                    dissoc :loaded-runtime-id)
            :transport-conformance-identity-keys-invalid]]]
    (let [result (sut/validate-certificate mutation)]
      (is (false? (:ok result)))
      (is (some #(= code (:error/code %)) (:findings result)))))
  (is (= :transport-conformance-certificate-not-map
         (-> (sut/validate-certificate []) :findings first :error/code))))

(deftest history-and-identity-shapes-are-validated
  (let [bad-history (assoc sut/f64-successful-visibility
                           :history [{:attempt -1 :operation :write
                                      :acquired-outcome :timeout
                                      :evidence :not-obtained}])
        bad-identity (assoc-in sut/f64-transport-failure
                               [:identity :loaded-runtime-id] 42)]
    (is (some #(= :transport-conformance-attempt-invalid (:error/code %))
              (:findings (sut/validate-certificate bad-history))))
    (is (some #(= :transport-conformance-identity-value-invalid
                  (:error/code %))
              (:findings (sut/validate-certificate bad-identity))))))

(deftest legacy-adaptation-is-narrow-and-explicit
  (is (= {:ok true :operation :read :acquired-outcome :timeout
          :evidence :not-obtained}
         (sut/adapt-legacy-finding
          {:ok false :error/code :futon1b-read-timeout})))
  (doseq [legacy [{:error/component :transport
                   :error/code :promotion-review-projection-failed}
                  {:error/code :memory-snapshot-review-not-visible}
                  {:finding :snapshot-review-not-visible}]]
    (is (= :transport-conformance-legacy-finding-ambiguous
           (:error/code (sut/adapt-legacy-finding legacy)))))
  (is (= :transport-conformance-legacy-finding-unclassified
         (:error/code (sut/adapt-legacy-finding
                       {:error/code :some-new-failure}))))
  (is (= :transport-conformance-legacy-finding-malformed
         (:error/code (sut/adapt-legacy-finding :timeout)))))

(defn- correlation [attempt]
  {:frame-id "f64" :problem-id "b01J03"
   :phase :promotion-review :attempt attempt})

(deftest persisted-retry-then-success-replays-and-is-append-only
  (let [dir (Files/createTempDirectory "transport-certificates-"
                                       (make-array FileAttribute 0))
        a (sut/persist-certificate! dir (correlation 0)
                                    sut/f64-transport-failure 1000)
        b (sut/persist-certificate! dir (correlation 1)
                                    sut/f64-successful-visibility 2000)]
    (is (:ok a))
    (is (:ok b))
    (is (not= (:path a) (:path b)))
    (is (= 2 (with-open [stream (Files/list dir)] (count (.toList stream)))))
    (is (= {:ok true :records 2} (sut/replay-directory dir)))))

(deftest invalid-certificates-remain-durable-and-replay-fails-closed
  (doseq [[certificate expected]
          [[sut/f63-historical-certificate
            :transport-conformance-outcome-mismatch]
           [(assoc-in sut/f64-transport-failure [:identity :loaded-runtime-id]
                      "different")
            :transport-conformance-runtime-identity-mismatch]
           [(assoc-in sut/f64-transport-failure [:identity :loaded-runtime-id]
                      "unavailable")
            :transport-conformance-loaded-runtime-id-unavailable]]]
    (let [dir (Files/createTempDirectory "transport-certificates-invalid-"
                                         (make-array FileAttribute 0))
          persisted (sut/persist-certificate! dir (correlation 0)
                                               certificate 1000)
          replayed (sut/replay-directory dir)]
      (is (:ok persisted))
      (is (false? (:certificate-valid? persisted)))
      (is (false? (:ok replayed)))
      (is (some #(or (= expected (:error/code %))
                     (some (fn [nested] (= expected (:error/code nested)))
                           (:findings %)))
                (:findings replayed))))))

(deftest malformed-persisted-record-fails-closed
  (is (= :transport-certificate-replay-failed
         (:error/code (sut/replay-records [{:malformed true}])))))
