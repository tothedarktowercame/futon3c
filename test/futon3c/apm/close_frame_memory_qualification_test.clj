(ns futon3c.apm.close-frame-memory-qualification-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.countdown-control :as countdown]
            [futon3c.apm.frame-fingerprint-audit :as fingerprint]
            [futon3c.apm.live-learning-phases :as phases]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.typed-role-submission :as submission])
  (:import [java.nio.file Files Path]))

(defn- temp-campaign []
  (str (Files/createTempDirectory
        "close-memory-qualification-"
        (make-array java.nio.file.attribute.FileAttribute 0))
       "/synthetic-memory-campaign"))

(defn- path [root child]
  (Path/of root (into-array String [child])))

(deftest synthetic-durable-close-qualification
  (let [campaign (temp-campaign)
        uuid-id "e-01c38dee-a698-41e3-af98-d6700d3dd55c"
        opaque-id "e-apm/opaque-memory:v2"
        student-1 {:receipt/id "student-1"
                   :receipt/type :student-attempt
                   :receipt/attempt-ordinal 1
                   :receipt/memory-use {:used-ids [uuid-id opaque-id]}}
        student-2 {:receipt/id "student-2"
                   :receipt/type :student-attempt
                   :receipt/attempt-ordinal 2
                   :receipt/memory-use {:used-ids [opaque-id]}}
        student-paths [(path campaign "live/student-attempt-1.edn")
                       (path campaign "live/student-attempt-2.edn")]
        _ (doseq [[p receipt] (map vector student-paths
                                   [student-1 student-2])]
            (is (:ok (runtime/atomic-persist! p receipt))))
        durable-receipts (into {}
                               (map-indexed
                                (fn [i p]
                                  [(keyword (str "student-attempt-" (inc i)))
                                   (runtime/read-state p)])
                                student-paths))
        memory-audit (phases/memory-use-audit durable-receipts)
        close-request
        (submission/prepare-request
         {:dispatch/type :close-frame
          :dispatch/id "synthetic-close-dispatch"
          :agent-id "synthetic-guide"
          :frame-id "synthetic-f1"
          :problem-id "synthetic-p1"
          :phase :close-frame
          :role :guide
          :memory-use-audit memory-audit})
        close-report {:command-own-exit 0
                      :frame-id "synthetic-f1"
                      :problem-id "synthetic-p1"
                      :trace-id "synthetic-trace"
                      :result :closed
                      :memory-use-audit memory-audit}
        close-job {:job-id "synthetic-close-job"
                   :agent-id "synthetic-guide"
                   :state :done
                   :report close-report}
        close-receipt {:receipt/id "synthetic-close-receipt"
                       :receipt/type :frame-close
                       :receipt/frame-id "synthetic-f1"
                       :receipt/problem-id "synthetic-p1"
                       :receipt/trace-id "synthetic-trace"
                       :receipt/result :closed
                       :receipt/memory-use-audit memory-audit}
        close-path (path campaign "live/close-frame.edn")
        _ (is (:ok (phases/validate-terminal
                    close-request {:job-id "synthetic-close-job"} close-job)))
        _ (is (:ok (runtime/atomic-persist! close-path close-receipt)))
        payload {:campaign "synthetic-memory-campaign"
                 :summary {:use-events 2 :fingerprinted 2}
                 :rows [{:frame "synthetic-f1" :attempt 1 :memory uuid-id
                         :verdict "fingerprinted"}
                        {:frame "synthetic-f1" :attempt 2 :memory opaque-id
                         :verdict "fingerprinted"}]}
        run-audit (fn [runner at]
                    (fn [_]
                      (fingerprint/audit!
                       {:campaign-directory campaign
                        :now-fn (constantly at)
                        :run-command-fn runner})))
        success-runner (constantly {:exit 0
                                    :out (json/generate-string payload)
                                    :err ""})
        failure-runner (constantly {:exit 17 :out ""
                                    :err "synthetic analyzer unavailable"})]
    (testing "durable receipts preserve UUID-like and opaque ids canonically"
      (is (= [{:memory-id uuid-id :attempt-ordinals [1]}
              {:memory-id opaque-id :attempt-ordinals [1 2]}]
             memory-audit)))
    (testing "the close uses the exact production typed evidence schema"
      (is (= #{:trace-id :result :memory-use-audit}
             (submission/evidence-required close-request)))
      (is (empty? (submission/validator-schema-findings close-request)))
      (is (= #{:command-own-exit :frame-id :problem-id :trace-id :result
               :memory-use-audit}
             (set (keys close-report)))))
    (with-redefs [countdown/record-analyst-wake!
                  (fn [frame receipt]
                    {:ok true :frame frame :close-receipt/id (:receipt/id receipt)})]
      (let [success (countdown/record-close-observations!
                     {:ok true :close/durable? true}
                     "synthetic-f1" close-receipt
                     (run-audit success-runner "2026-08-30T12:00:00Z"))
            artifact-path (str campaign "/analysis/fingerprint-audit.json")
            good-artifact (slurp artifact-path)
            durable-close-before (slurp (str close-path))
            failure (countdown/record-close-observations!
                     {:ok true :close/durable? true}
                     "synthetic-f1" close-receipt
                     (run-audit failure-runner "2026-08-30T12:01:00Z"))
            failure-status
            (edn/read-string
             (slurp (str campaign "/analysis/fingerprint-audit-status.edn")))]
        (testing "a successful close observation publishes the analyzer result"
          (is (:ok (:fingerprint-audit success)))
          (is (= payload (json/parse-string good-artifact true))))
        (testing "analyzer failure is explicit and preserves last valid evidence"
          (is (:close/durable? failure))
          (is (false? (get-in failure [:fingerprint-audit :ok])))
          (is (= :fingerprint-audit-command-failed
                 (get-in failure [:fingerprint-audit :error/code])))
          (is (= (:fingerprint-audit failure) failure-status))
          (is (= durable-close-before (slurp (str close-path))))
          (is (= close-receipt (runtime/read-state close-path)))
          (is (= good-artifact (slurp artifact-path))))))))
