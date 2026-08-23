(ns futon3c.apm.live-preflight-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-preflight :as sut]))

(def contract (edn/read-string
               (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")))
(def unit (second (:units (edn/read-string
                          (slurp "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")))))
(def inputs
  {:ledger {:version 5
            :digest "3d4df4ddcd2c0ab85fee7b011ddd50d89dcd73452564c29c4153d2527b6c9d1a"
            :phase :preflight :claim nil}
   :unit unit
   :role-card {:path "role-cards/proctor.md" :blob "63a64a878ce06ccdc8ec78e5ee19526bc9cef211"}
   :seat {:agent-id "f19-proctor" :type :codex :frame-id "f19" :invoke-ready? true}
   :timeouts {:request-timeout-ms 300000 :turn-timeout-ms 3600000}})

(defn- successful-job [request ticket]
  {:job-id (:job-id ticket) :agent-id (:agent-id request) :state :done
   :report {:command-own-exit 0
            :problem-revision (:problem-revision request)
            :problem-blob (:problem-blob request)
            :lean {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0
                   :output "declaration uses `sorry`"}
            :clean-before? true :clean-after? true :mutations []}})

(deftest request-ticket-and-receipt-are-content-addressed
  (let [request (:request (sut/build-request inputs))
        ticket (:ticket (sut/record-dispatch request {:ok true :job-id "job-f19-preflight"}))
        result (sut/receipt contract request ticket (successful-job request ticket))
        receipt (:certificate result)]
    (is (= (:dispatch/id request)
           (machine/ledger-digest [(dissoc request :dispatch/id)])))
    (is (= (:ticket/id ticket)
           (machine/ledger-digest [(dissoc ticket :ticket/id)])))
    (is (:ok result))
    (is (= (:receipt/id receipt)
           (machine/ledger-digest [(dissoc receipt :receipt/id)])))))

(deftest every-boundary-fails-closed
  (testing "dispatch input"
    (is (= :preflight-dispatch-input-invalid
           (:error/code (sut/build-request (assoc-in inputs [:ledger :claim]
                                                      {:claim/id "foreign"}))))))
  (testing "dispatch acknowledgement"
    (is (= :preflight-dispatch-not-acknowledged
           (:error/code (sut/record-dispatch {} {:ok true})))))
  (testing "terminal identity and evidence"
    (let [request (:request (sut/build-request inputs))
          ticket (:ticket (sut/record-dispatch request {:ok true :job-id "expected"}))
          bad (-> (successful-job request ticket)
                  (assoc :job-id "other")
                  (assoc-in [:report :command-own-exit] 1)
                  (assoc-in [:report :mutations] ["Main.lean"]))
          findings (set (:findings (sut/receipt contract request ticket bad)))]
      (is (= #{:job-id-mismatch :command-own-exit-nonzero
               :preflight-mutations-observed}
             findings)))))

(deftest legacy-vector-evidence-is-normalized-without-inference
  (let [request (:request (sut/build-request inputs))
        ticket (:ticket (sut/record-dispatch request {:ok true :job-id "legacy"}))
        legacy (assoc (successful-job request ticket)
                      :report {:command-own-exit 0
                               :problem-revision (:problem-revision request)
                               :problem-blob (:problem-blob request)
                               :lean {:warnings ["declaration uses `sorry`"]
                                      :errors [] :sorry-count 1}
                               :clean-before? true :clean-after? true
                               :mutations []})
        terminal (sut/validate-terminal request ticket legacy)]
    (is (:ok terminal))
    (is (= {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}
           (select-keys (get-in terminal [:report :lean])
                        [:exit :warnings :sorry-warnings :errors])))))

(deftest unrelated-lean-warnings-do-not-conflate-with-the-sorry-baseline
  (let [request (:request (sut/build-request inputs))
        ticket (:ticket (sut/record-dispatch request {:ok true :job-id "warnings"}))
        extra-warnings (assoc-in (successful-job request ticket)
                                 [:report :lean :warnings] 3)]
    (is (:ok (sut/validate-terminal request ticket extra-warnings)))
    (doseq [bad [(assoc-in extra-warnings [:report :lean :sorry-warnings] 2)
                 (assoc-in extra-warnings [:report :lean :errors] 1)
                 (assoc-in extra-warnings [:report :lean :warnings] 0)]]
      (is (= [:lean-baseline-mismatch]
             (:findings (sut/validate-terminal request ticket bad)))))))

(deftest typed-observation-is-unwrapped-while-artifact-identity-stays-authoritative
  (let [request (:request (sut/build-request inputs))
        ticket (:ticket (sut/record-dispatch request {:ok true :job-id "typed"}))
        flat (:report (successful-job request ticket))
        typed {:job-id (:job-id ticket) :agent-id (:agent-id request) :state :done
               :report {:command-own-exit 0 :outcome "complete"
                        :failure-account []
                        :evidence (-> flat
                                      (dissoc :command-own-exit
                                              :problem-revision
                                              :problem-blob)
                                      (assoc :statement-unchanged? true))}}
        result (sut/receipt contract request ticket typed)]
    (is (:ok result) (pr-str result))
    (is (= (:problem-revision request)
           (get-in result [:certificate :receipt/problem-revision])))
    (is (= (:problem-blob request)
           (get-in result [:certificate :receipt/problem-blob])))))

(deftest durable-drive-dispatches-exactly-once-across-continuations
  (let [dispatches (atom 0)
        persisted (atom [])
        job (atom {:state :running})
        effects {:contract contract :inputs inputs
                 :dispatch-fn (fn [_] (swap! dispatches inc)
                                {:ok true :job-id "durable-job"})
                 :activate-fn (fn [_ _] {:ok true})
                 :job-fn (fn [job-id] (assoc @job :job-id job-id
                                             :agent-id "f19-proctor"))
                 :persist-fn (fn [state] (swap! persisted conj state) {:ok true})}
        first-pass (sut/drive! effects)
        waiting (sut/drive! (assoc effects :state (:state first-pass)))
        _ (reset! job (successful-job
                       (get-in first-pass [:state :request])
                       (get-in first-pass [:state :ticket])))
        certified (sut/drive! (assoc effects :state (:state waiting)))]
    (is (= :awaiting-terminal (:status first-pass)))
    (is (= :awaiting-terminal (:status waiting)))
    (is (= :certified (:status certified)))
    (is (= 1 @dispatches))
    (is (= [:preflight-dispatched :preflight-certified]
           (mapv :state/type @persisted)))))
