(ns futon3c.apm.live-proof-phases-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.live-proof-phases :as sut]))

(def contract (edn/read-string
               (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")))
(def unit (second (:units (edn/read-string
                          (slurp "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")))))
(def workspace {:workspace/path "/tmp/f19-solver"
                :branch "exp/countdown-f19-a01J05-solver"
                :base-revision (get-in unit [:problem :revision])
                :problem/blob (get-in unit [:problem :blob])})
(def action {:frame-id "f19" :problem-id "a01J05"
             :timeouts {:turn-ms 3600000}})
(def role-card {:path "solver.md" :blob "solver-blob"})
(def seat {:agent-id "f19-solver" :invoke-ready? true})

(defn request [kind]
  (:request (sut/build-request
             {:kind kind :action action :ledger {:digest (apply str (repeat 64 "a"))}
              :unit unit :role-card role-card
              :seat (if (= :verify kind) (assoc seat :agent-id "f19-proctor") seat)
              :workspace workspace
              :solve-receipt (when (= :verify kind)
                               {:receipt/id "solve-r" :receipt/final-head
                                "1111111111111111111111111111111111111111"})})))

(defn job [kind req]
  {:job-id "job-1" :agent-id (:agent-id req) :state :done
   :report {:command-own-exit 0 :branch (:branch req)
            :base-revision (:base-revision req)
            :final-head (if (= :verify kind) (:certified-final-head req)
                            "2222222222222222222222222222222222222222")
            :committed? true :statement-unchanged? true
            :lean {:exit 0 :warnings 0 :sorry-warnings 0 :errors 0 :output ""}
            :axioms '[propext Classical.choice Quot.sound]
            :clean-before? true :clean-after? true :mutations []}})

(deftest solve-and-verify-produce-contract-valid-receipts
  (doseq [kind [:solve :verify]]
    (let [req (request kind)
          ticket {:job-id "job-1"}
          terminal (sut/validate-terminal kind req ticket (job kind req))
          result (sut/receipt contract kind req ticket (job kind req) terminal)]
      (is (:ok terminal) (name kind))
      (is (:ok result) (name kind)))))

(deftest preflight-request-carries-the-phase-required-by-the-live-prompt
  (let [preflight-action (assoc action :timeouts {:request-ms 300000
                                                   :turn-ms 3600000})
        result (sut/build-request
                {:kind :preflight :action preflight-action
                 :ledger {:version 5
                          :digest (apply str (repeat 64 "a"))
                          :phase :preflight :claim nil}
                 :unit unit :role-card role-card
                 :seat {:agent-id "f19-proctor" :type :codex
                        :frame-id "f19" :invoke-ready? true}})]
    (is (:ok result))
    (is (= :preflight (get-in result [:request :phase])))
    (is (string? (sut/prompt (:request result))))))

(deftest proof-terminal-refuses-unsound-or-misattributed-output
  (let [req (request :solve)
        ticket {:job-id "job-1"}
        bad (-> (job :solve req)
                (assoc :agent-id "other")
                (assoc-in [:report :lean :sorry-warnings] 1)
                (assoc-in [:report :axioms] '[propext unsafeAxiom])
                (assoc-in [:report :clean-after?] false))
        findings (set (:findings (sut/validate-terminal :solve req ticket bad)))]
    (is (= #{:agent-id-mismatch :lean-proof-invalid :axioms-not-permitted
             :workspace-not-clean-after}
           findings))))

(deftest verify-is-bound-to-certified-solve-head
  (let [req (request :verify)
        bad (assoc-in (job :verify req) [:report :final-head]
                      "3333333333333333333333333333333333333333")]
    (is (some #{:verify-final-head-mismatch}
              (:findings (sut/validate-terminal :verify req {:job-id "job-1"} bad))))))
