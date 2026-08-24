(ns futon3c.apm.live-proof-phases-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.live-proof-phases :as sut]
            [futon3c.apm.live-solver-rounds :as solver-rounds]))

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
              :checkpoint-role-card {:path "restrategize.md"
                                     :blob "restrategize-blob"}
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
                 :terminal-budget {:collection-attempts 2 :repair-attempts 1}
                 :seat {:agent-id "f19-proctor" :type :codex
                        :frame-id "f19" :invoke-ready? true}
                 :workspace workspace})]
    (is (:ok result))
    (is (= :preflight (get-in result [:request :phase])))
    (is (= {:collection-attempts 2 :repair-attempts 1}
           (get-in result [:request :terminal-budget])))
    (is (= "/tmp/f19-solver"
           (get-in result [:request :problem-repository])))
    (is (re-find #":sorry-warnings INT" (sut/prompt (:request result))))))

(deftest preflight-refuses-a-workspace-not-bound-to-the-problem-pin
  (let [result (sut/build-request
                {:kind :preflight
                 :action (assoc action :timeouts {:request-ms 300000
                                                   :turn-ms 3600000})
                 :ledger {:version 5 :digest (apply str (repeat 64 "a"))
                          :phase :preflight :claim nil}
                 :unit unit :role-card role-card
                 :seat {:agent-id "f19-proctor" :type :codex
                        :frame-id "f19" :invoke-ready? true}
                 :workspace (assoc workspace :problem/blob "wrong")})]
    (is (= :preflight-workspace-invalid (:error/code result)))
    (is (some #{:workspace-blob-mismatch} (:findings result)))))

(deftest solver-round-prompts-distinguish-opening-siege-from-continuation
  (let [opening (sut/prompt (assoc (request :solve) :solver/round 1))
        later (sut/prompt (assoc (request :solve) :solver/round 2
                                 :solver/prior-session-id "same-session"))]
    (is (re-find #"Opening siege" opening))
    (is (re-find #"same solver session and branch" later))))

(deftest tenth-round-prompt-requires-strategy-and-decomposition
  (let [checkpoint (sut/prompt
                    (assoc (request :solve) :solver/round 10
                           :solver/strategy-checkpoint? true
                           :solver/prior-session-id "same-session"))]
    (is (re-find #"ten-turn strategy checkpoint" checkpoint))
    (is (re-find #":solver/strategy" checkpoint))
    (is (re-find #":delegate\|:sequential" checkpoint))
    (is (re-find #"isolated branches/worktrees" checkpoint))))

(deftest solve-request-pins-both-ordinary-and-restrategize-cards
  (let [req (request :solve)]
    (is (= "solver.md" (:solver/regular-role-card-path req)))
    (is (= "solver-blob" (:solver/regular-role-card-blob req)))
    (is (= "restrategize.md" (:solver/restrategize-role-card-path req)))
    (is (= "restrategize-blob" (:solver/restrategize-role-card-blob req)))))

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

(deftest unrelated-lean-warnings-do-not-invalidate-a-closed-proof
  (let [req (request :solve)
        ticket {:job-id "job-1"}
        warned (assoc-in (job :solve req) [:report :lean :warnings] 2)]
    (is (:ok (sut/validate-terminal :solve req ticket warned)))
    (doseq [bad [(assoc-in warned [:report :lean :sorry-warnings] 1)
                 (assoc-in warned [:report :lean :errors] 1)
                 (assoc-in warned [:report :lean :warnings] -1)]]
      (is (= [:lean-proof-invalid]
             (:findings (sut/validate-terminal :solve req ticket bad)))))))

(deftest f20-equivalent-report-shape-normalizes-without-inventing-evidence
  (let [req (assoc (request :solve)
                   :problem-path "problems/a01J06/lean/Main.lean")
        strategy {:summary "Proof complete" :obligations [] :decomposition []
                  :next-plan "Certify the committed head."}
        shaped (-> (job :solve req)
                   (assoc-in [:report :lean]
                             {:exit 0 :warnings [] :solver/strategy strategy})
                   (assoc-in [:report :axioms]
                             "'apm_a01j06' depends on axioms: [propext, Classical.choice, Quot.sound]")
                   (assoc-in [:report :mutations]
                             ["problems/a01J06/lean/Main.lean"]))
        normalized (sut/normalize-proof-report (:report shaped))]
    (is (= {:exit 0 :warnings 0 :sorry-warnings 0 :errors 0}
           (select-keys (:lean normalized)
                        [:exit :warnings :sorry-warnings :errors])))
    (is (= '[propext Classical.choice Quot.sound] (:axioms normalized)))
    (is (= strategy (:solver/strategy normalized)))
    (is (:ok (sut/validate-terminal :solve req {:job-id "job-1"} shaped)))))

(deftest live-solve-routes-persisted-checkpoint-failure-to-collection-repair
  (let [called (atom nil)
        state {:state/type :solver-strategy-checkpoint-required
               :rounds [{:ordinal 10 :job-id "job-10"}] :active nil}]
    (with-redefs [runtime/read-state (constantly state)
                  solver-rounds/resume-strategy-checkpoint!
                  (fn [effects]
                    (reset! called effects)
                    {:ok true :status :awaiting-terminal})]
      (is (:ok (sut/run-live! {:kind :solve :contract {}
                               :request (request :solve)
                               :state-path "ignored.edn"})))
      (is (= state (:state @called))))))

(deftest proof-terminal-rejects-mutation-outside-registered-problem
  (let [req (request :solve)
        bad (assoc-in (job :solve req) [:report :mutations] ["lakefile.lean"])]
    (is (some #{:mutation-outside-problem-file}
              (:findings (sut/validate-terminal :solve req {:job-id "job-1"} bad))))))
(deftest persisted-relative-card-authority-replays-without-new-dispatch-identity
  (let [base {:phase :preflight :role :proctor :problem-id "m94A03"
              :role-card-blob "blob"}
        relative "holes/labs/M-apm-demonstration/role-cards/proctor.md"
        persisted (assoc base :role-card-path relative
                         :dispatch/id "historical" :submission/token "old")
        current (assoc base
                       :role-card-path
                       (str (.toAbsolutePath
                             (java.nio.file.Path/of relative
                                                    (make-array String 0))))
                       :dispatch/id "new" :submission/token "new")]
    (is (sut/request-replay-compatible? current persisted))
    (is (not (sut/request-replay-compatible?
              (assoc current :role-card-blob "changed") persisted)))))

(deftest json-vector-axioms-normalize-to-permitted-symbols
  (is (= '[propext Classical.choice Quot.sound]
         (:axioms
          (sut/normalize-proof-report
           {:axioms ["propext" "Classical.choice" "Quot.sound"]})))))
