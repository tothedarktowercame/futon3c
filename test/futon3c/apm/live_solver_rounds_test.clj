(ns futon3c.apm.live-solver-rounds-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.live-solver-rounds :as sut]))

(def base-request
  {:dispatch/id "opening" :agent-id "f19-solver" :frame-id "f19"
   :problem-id "a01J05" :phase :solve :branch "solver-branch"
   :role-card-path "solver.md" :role-card-blob "solver-blob"
   :solver/regular-role-card-path "solver.md"
   :solver/regular-role-card-blob "solver-blob"
   :solver/restrategize-role-card-path "solver-restrategize.md"
   :solver/restrategize-role-card-blob "restrategize-blob"})

(def legacy-state
  {:state/type :live-job-dispatched :request base-request
   :ticket {:job-id "job-1" :agent-id "f19-solver"}
   :activation/accepted? true})

(defn effects [persisted]
  {:request base-request :state legacy-state
   :announce-fn (fn [request]
                  {:ok true :job-id (str "job-" (:solver/round request))})
   :activate-fn (fn [_ _] {:ok true})
   :job-fn (fn [_]
             {:job-id "job-1" :agent-id "f19-solver" :session-id "solver-session"
              :state :done :report {:committed? false :residual "unfinished"}})
   :persist-fn (fn [state] (reset! persisted state) {:ok true})
   :validate-solved (fn [_ _ _]
                      {:ok false :error/code :live-proof-terminal-invalid
                       :findings [:final-head-not-committed]})
   :provide-receipt (fn [& _] (throw (ex-info "must not receipt" {})))})

(deftest legacy-failed-ticket-becomes-round-one-and-continues-same-session
  (let [persisted (atom nil)
        result (sut/drive! (effects persisted))]
    (is (:ok result))
    (is (= :awaiting-terminal (:status result)))
    (is (= "job-2" (:job-id result)))
    (is (= 1 (count (get-in result [:state :rounds]))))
    (is (= "solver-session"
           (get-in result [:state :rounds 0 :session-id])))
    (is (= 2 (get-in result [:state :active :request :solver/round])))
    (is (= "solver-session"
           (get-in result [:state :active :request :solver/prior-session-id])))
    (is (= 48 (get-in result [:state :active :request
                              :solver/remaining-rounds])))))

(deftest successor-waits-for-prior-job-to-finish-after-typed-submission
  ;; f202: round 23 created 02:29:44.887Z, round 22 finished 02:30:21.578Z.
  ;; The submission had completed the round, but its CLI still held the writer.
  (let [persisted (atom nil)
        calls (atom [])
        base (assoc (effects persisted)
                    :job-fn (fn [id]
                              (swap! calls conj [:job id])
                              {:job-id id :state :running
                               :session-id "solver-session"})
                    :terminal-submission-provider
                    (constantly {:payload {:outcome "progress"
                                          :evidence {:residual "unfinished"}}})
                    :announce-fn (fn [_] (swap! calls conj :announce))
                    :activate-fn (fn [& _] (swap! calls conj :activate)))
        collected (sut/drive! base)
        completed (sut/drive! (assoc base :state (:state collected)))
        state (:state completed)]
    (is (= :awaiting-prior-job-terminal (:status completed)))
    (is (= :done (get-in state [:rounds 0 :terminal-state]))
        "typed completion is not evidence that the raw job released the seat")
    (is (nil? (:active state)))
    (reset! calls [])
    (let [waiting (sut/drive! (assoc base :state state
                                   :persist-fn
                                   (fn [_] (swap! calls conj :persist))))]
      (is (= {:ok true :status :awaiting-prior-job-terminal
              :job-id "job-1" :state state}
             waiting))
      (is (= [[:job "job-1"]] @calls))
      (is (= state @persisted)))))

(deftest terminal-prior-job-dispatches-the-same-successor
  (let [persisted (atom nil)
        ;; Pin the existing legacy continuation result, including request,
        ;; session, budget, ticket and activation acceptance.
        expected (sut/drive! (effects persisted))
        state (assoc (:state expected) :active nil)
        calls (atom [])
        base (effects persisted)
        result (sut/drive!
                (assoc base :state state
                       :job-fn (fn [id]
                                 (swap! calls conj [:job id])
                                 {:job-id id :state :done})
                       :announce-fn (fn [request]
                                      (swap! calls conj :announce)
                                      ((:announce-fn base) request))
                       :activate-fn (fn [request ticket]
                                      (swap! calls conj :activate)
                                      ((:activate-fn base) request ticket))))]
    (is (= expected result))
    (is (= "job-2" (:job-id result)))
    (is (= [[:job "job-1"] :announce :activate] @calls))))

(deftest first-round-dispatches-without-observing-a-predecessor
  (let [persisted (atom nil)
        calls (atom [])
        base (effects persisted)
        result (sut/drive!
                (assoc base :state nil
                       :job-fn (fn [_] (swap! calls conj :job))
                       :announce-fn (fn [request]
                                      (swap! calls conj :announce)
                                      ((:announce-fn base) request))
                       :activate-fn (fn [request ticket]
                                      (swap! calls conj :activate)
                                      ((:activate-fn base) request ticket))))]
    (is (:ok result))
    (is (= :awaiting-terminal (:status result)))
    (is (= "job-1" (:job-id result)))
    (is (= [] (get-in result [:state :rounds])))
    (is (= (sut/round-request base-request 1 nil)
           (get-in result [:state :active :request])))
    (is (true? (get-in result [:state :active :activation/accepted?])))
    (is (= (:state result) @persisted))
    (is (= [:announce :activate] @calls))))

(deftest resuming-a-halted-siege-without-a-job-provider-names-the-gap
  ;; live_proof_phases/resume-solver-remediation-live! builds its own effects
  ;; map, and it had no :job-fn. Every resume of a halted siege -- the path
  ;; that would restart f202/m02A03 -- threw
  ;; "Cannot invoke IFn.invoke because job_fn is null" instead of dispatching.
  (let [state {:state/type :solver-remediation-required
               :base-request base-request
               :budget/max-rounds 50
               :rounds [{:ordinal 1 :job-id "apm-role-prior"
                         :terminal-state :done :outcome :inadequate :report {}}]}
        outcome (try
                  (sut/resume-remediation!
                   {:state state :request base-request
                    :announce-fn (fn [_] {:ok true :job-id "next"})
                    :activate-fn (fn [_ _] {:ok true})
                    :persist-fn (fn [_] {:ok true})})
                  (catch Throwable t {:threw (.getName (class t))}))]
    (is (nil? (:threw outcome)) (pr-str outcome))
    (is (= :solver-round-job-provider-missing (:error/code outcome)))))

(deftest checkpoint-round-selects-restrategize-card-and-next-round-restores-regular-card
  (let [checkpoint (sut/round-request base-request 10 nil)
        resumed (sut/round-request base-request 11 nil)]
    (is (= :restrategize (:solver/role-card-mode checkpoint)))
    (is (= "solver-restrategize.md" (:role-card-path checkpoint)))
    (is (= "restrategize-blob" (:role-card-blob checkpoint)))
    (is (= :regular (:solver/role-card-mode resumed)))
    (is (= "solver.md" (:role-card-path resumed)))
    (is (= "solver-blob" (:role-card-blob resumed)))
    (is (not= (:dispatch/id checkpoint) (:dispatch/id resumed)))))

(deftest partial-bank-strategy-runs-before-ordinary-solving
  (let [first-request (sut/round-request
                       (assoc base-request :solver/strategy-before-solve? true)
                       1 nil)
        second-request (sut/round-request
                        (assoc base-request :solver/strategy-before-solve? true)
                        2 {:job-id "strategy-job" :session-id "session"
                           :report {:solver/strategy {:summary "route"}}})]
    (is (= :restrategize (:solver/role-card-mode first-request)))
    (is (true? (:solver/strategy-checkpoint? first-request)))
    (is (= :regular (:solver/role-card-mode second-request)))
    (is (false? (:solver/strategy-checkpoint? second-request)))
    (is (= "strategy-job" (:solver/prior-job-id second-request)))))

(deftest solver-typed-output-is-collected-then-repaired-once
  (let [persisted (atom nil)
        base (assoc (effects persisted)
                    :terminal-submission-provider (constantly nil)
                    :ticket-register-fn (constantly {:ok true})
                    :announce-fn (fn [request]
                                   {:ok true
                                    :job-id (if (:repair/attempt request)
                                              "solver-repair-job" "job-1")})
                    :terminal-budget-config {:collection-attempts 1
                                             :repair-attempts 1})
        collected (sut/drive! base)
        repaired (sut/drive! (assoc base :state (:state collected)))
        repair-base (assoc base :state (:state repaired)
                           :job-fn (fn [_]
                                     {:job-id "solver-repair-job"
                                      :agent-id "f19-solver"
                                      :session-id "solver-session"
                                      :state :done}))
        repair-collected (sut/drive! repair-base)
        exhausted (sut/drive! (assoc repair-base
                                     :state (:state repair-collected)))]
    (is (= :terminal-collected (:status collected)))
    (is (= :awaiting-terminal (:status repaired)))
    (is (true? (:repair? repaired)))
    (is (= "job-1"
           (get-in repaired [:state :superseded-terminals 0 :job :job-id])))
    (is (= [:typed-submission-missing]
           (get-in repaired [:state :superseded-terminals 0 :findings])))
    (is (= "solver-repair-job"
           (get-in repaired [:state :superseded-terminals 0
                             :trace/successor-observation
                             :successor-activated-id])))
    (is (= :terminal-collected (:status repair-collected)))
    (is (= :solver-typed-submission-repair-exhausted
           (:error/code exhausted)))
    (is (= 1 (:repair/attempts exhausted)))))

(deftest typed-submission-reconciles-a-stranded-running-solver-job
  ;; Live pin: f83/b97A01 job apm-role-f65f2382682c9e5c76d69bfe29a903d8a4bac09061caba4db4555d60dde690c7
  ;; remained :running after submitting a clean Lean result at 05fdbfd181d29014b3482199b099575a1b4d9499.
  (let [persisted (atom nil)
        provider-calls (atom 0)
        submission {:schema :apm/role-submission-v1
                    :payload {:outcome "complete" :command-own-exit 0}}
        state {:state/type :solver-rounds
               :budget/max-rounds 50
               :base-request base-request
               :rounds [{:ordinal 8 :session-id "prior-solver-session"}]
               :active legacy-state}
        base (assoc (effects persisted)
                    :state state
                    :job-fn (fn [_]
                              {:job-id "job-1" :agent-id "f19-solver"
                               :state :running})
                    :terminal-submission-provider
                    (fn [& _]
                      (swap! provider-calls inc)
                      submission)
                    :validate-solved
                    (fn [_ _ job]
                      {:ok (and (= :done (:state job))
                                (= submission (:typed-submission job)))})
                    :provide-receipt
                    (fn [& _]
                      {:ok true :certificate {:receipt/id "solved"}}))
        collected (sut/drive! base)
        certified (sut/drive! (assoc base :state (:state collected)))]
    (is (= :terminal-collected (:status collected)))
    (is (= :running (get-in collected [:collection :terminal-state])))
    (is (= :certified (:status certified)))
    (is (= "solved" (get-in certified [:certificate :receipt/id])))
    (is (= 1 @provider-calls))))

(deftest terminal-repair-archive-failure-blocks-announcement
  (let [announcements (atom 0)
        persisted (atom [])
        base (assoc (effects persisted)
                    :terminal-submission-provider (constantly nil)
                    :ticket-register-fn (constantly {:ok true})
                    :announce-fn (fn [_]
                                   (swap! announcements inc)
                                   {:ok true :job-id "must-not-exist"})
                    :persist-fn (fn [state]
                                  (swap! persisted conj state)
                                  (if (:superseded-terminals state)
                                    {:ok false :error :disk-full}
                                    {:ok true}))
                    :terminal-budget-config {:collection-attempts 1
                                             :repair-attempts 1})
        collected (sut/drive! base)
        before @announcements
        repaired (sut/drive! (assoc base :state (:state collected)))]
    (is (= :solver-terminal-repair-archive-persistence-failed
           (:error/code repaired)))
    (is (= before @announcements))))

(deftest repeated-terminal-repairs-append-predecessors
  (let [next-job (atom 1)
        persisted (atom nil)
        fx {:announce-fn (fn [_]
                           {:ok true :job-id (str "repair-" (swap! next-job inc))})
            :activate-fn (fn [& _] {:ok true})
            :ticket-register-fn (fn [& _] {:ok true})
            :persist-fn (fn [state] (reset! persisted state) {:ok true})}
        state {:state/type :solver-rounds :base-request base-request :rounds []
               :active (assoc legacy-state :terminal-collection {:evidence :first})}
        first (#'sut/dispatch-terminal-repair!
               fx state [:first-invalid] {:job-id "job-1" :state :done})
        second-state (assoc-in (:state first) [:active :terminal-collection]
                               {:evidence :second})
        second (#'sut/dispatch-terminal-repair!
                fx second-state [:second-invalid]
                {:job-id "repair-2" :state :done})]
    (is (:ok first))
    (is (:ok second))
    (is (= ["job-1" "repair-2"]
           (mapv #(get-in % [:job :job-id])
                 (:superseded-terminals (:state second)))))
    (is (= [[:first-invalid] [:second-invalid]]
           (mapv :findings (:superseded-terminals (:state second)))))))

(deftest retry-carries-terminal-validation-remediation
  (let [persisted (atom nil)
        result (sut/drive! (effects persisted))
        request (get-in result [:state :active :request])]
    (is (= [:final-head-not-committed]
           (get-in request [:solver/prior-validation :findings])))
    (is (true? (get-in request [:solver/remediation :required?])))
    (is (re-find #"corrective commit"
                 (get-in request [:solver/remediation :instruction])))))

(deftest repeated-identical-invalid-artifact-halts-before-third-dispatch
  (let [persisted (atom nil)
        prior {:ordinal 1 :job-id "job-0" :session-id "solver-session"
               :terminal-state :done :outcome :inadequate
               :report {:committed? false :residual "unfinished"}
               :validation {:ok false :error/code :live-proof-terminal-invalid
                            :findings [:final-head-not-committed] :missing nil}}
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request :rounds [prior]
               :active legacy-state}
        result (sut/drive! (assoc (effects persisted) :state state))]
    (is (= :solver-remediation-required (:error/code result)))
    (is (= :solver-remediation-required (:state/type @persisted)))
    (is (= "Correct the repeated validator finding in committed state before resuming; identical terminal artifacts must not consume the proof-search budget."
           (get-in @persisted [:remediation :instruction])))
    (is (= 2 (count (:rounds @persisted))))
    (is (nil? (:active @persisted)))))

(deftest explicit-remediation-resume-dispatches-corrective-round
  (let [persisted (atom nil)
        state {:state/type :solver-remediation-required
               :budget/max-rounds 50 :base-request base-request
               :rounds [{:ordinal 1 :job-id "job-1"
                         :validation {:ok false :findings [:bad-path]}}]
               :active nil}
        result (sut/resume-remediation!
                (assoc (effects persisted) :state state))]
    (is (:ok result))
    (is (= :awaiting-terminal (:status result)))
    (is (= "job-2" (:job-id result)))
    (is (= 2 (get-in result [:state :active :request :solver/round])))
    (is (= [:bad-path]
           (get-in result [:state :active :request
                           :solver/remediation :findings])))))

(deftest legacy-checkpoint-resumes-only-for-repeated-identical-failure
  (let [persisted (atom nil)
        failure {:validation {:ok false :findings [:bad-path]}
                 :report {:head "same"}}
        state {:state/type :solver-strategy-checkpoint-required
               :budget/max-rounds 50 :base-request base-request
               :rounds (into (mapv (fn [ordinal]
                                     {:ordinal ordinal
                                      :job-id (str "job-" ordinal)})
                                   (range 1 9))
                             [(assoc failure :ordinal 9 :job-id "job-9")
                              (assoc failure :ordinal 10 :job-id "job-10")])
               :active nil}
        result (sut/resume-remediation!
                (assoc (effects persisted) :state state))]
    (is (:ok result))
    (is (= 11 (get-in result [:state :active :request :solver/round]))))
  (let [state {:state/type :solver-strategy-checkpoint-required
               :budget/max-rounds 50 :base-request base-request
               :rounds [{:ordinal 9 :report {:head "a"}}
                        {:ordinal 10 :report {:head "b"}}]
               :active nil}]
    (is (= :solver-remediation-resume-input-invalid
           (:error/code (sut/resume-remediation!
                         (assoc (effects (atom nil)) :state state)))))))

(deftest round-cap-requires-human-without-classifying-the-problem
  (let [persisted (atom nil)
        prior (mapv (fn [ordinal] {:ordinal ordinal :job-id (str "j" ordinal)})
                    (range 1 50))
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request :rounds prior :active legacy-state}
        result (sut/drive! (assoc (effects persisted) :state state
                                  :max-rounds 50))]
    (is (= :solver-human-intervention-required (:error/code result)))
    (is (= 50 (count (get-in result [:state :rounds]))))
    (is (nil? (get-in result [:state :problem/classification])))))

(deftest strict-success-is-the-only-path-to-a-solve-receipt
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :validate-solved (fn [_ _ _] {:ok true})
                       :provide-receipt
                       (fn [& _] {:ok true :certificate {:receipt/id "solved"}})))]
    (is (= :certified (:status result)))
    (is (= "solved" (get-in result [:certificate :receipt/id])))
    (is (= :live-job-certified (:state/type @persisted)))))

(deftest claimed-defect-stops-for-review-without-classifying-problem
  (let [persisted (atom nil)
        announcements (atom [])
        result (sut/drive!
                (assoc (effects persisted)
                       :announce-fn (fn [request]
                                      (swap! announcements conj request)
                                      {:ok true :job-id "must-not-dispatch"})
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f19-solver"
                                  :session-id "solver-session" :state :done
                                  :report {:solver/outcome :claimed-defect
                                           :residual "Hypotheses contradict conclusion at z=0."}})))]
    (is (= :solver-defect-review-required (:error/code result)))
    (is (= :claimed-defect (get-in result [:state :rounds 0 :outcome])))
    (is (= :solver-defect-review-required
           (get-in result [:state :state/type])))
    (is (= (:state result) @persisted))
    (is (empty? @announcements)
        "a claimed defect must park before announcing another solver round")
    (is (nil? (get-in result [:state :active])))
    (is (nil? (get-in result [:state :problem/classification])))))

(deftest claimed-defect-without-residual-still-stops-for-review
  ;; The f190/b98J04 shape, 2026-09-07: :solver/outcome :claimed-defect with NO
  ;; :residual, because a defect claim is the report that no proof work remains
  ;; to describe. round-outcome used to demand a residual, so 21 consecutive
  ;; rounds degraded to :inadequate and the frame redispatched a goal the solver
  ;; had already diagnosed as ill-typed. The diagnosis rides :failure-account,
  ;; which the role contract requires of every role.
  (let [persisted (atom nil)
        announcements (atom [])
        result (sut/drive!
                (assoc (effects persisted)
                       :announce-fn (fn [request]
                                      (swap! announcements conj request)
                                      {:ok true :job-id "must-not-dispatch"})
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f190-solver"
                                  :session-id "solver-session" :state :done
                                  :report
                                  {:solver/outcome :claimed-defect
                                   :failure-account
                                   ["Claimed defect with precise compiled falsifying witness."]}})))]
    (is (= :solver-defect-review-required (:error/code result)))
    (is (= :claimed-defect (get-in result [:state :rounds 0 :outcome])))
    (is (= :solver-defect-review-required (get-in result [:state :state/type])))
    (is (empty? @announcements)
        "a defect claim must park before announcing another solver round")))

(deftest claimed-defect-with-no-evidence-at-all-remains-inadequate
  ;; The guard the fix must not remove: a bare outcome keyword with nothing
  ;; behind it is not a defect claim, and must not be able to park a frame.
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :announce-fn (fn [_] {:ok true :job-id "next-round"})
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f190-solver"
                                  :session-id "solver-session" :state :done
                                  :report {:solver/outcome :claimed-defect}})))]
    (is (not= :solver-defect-review-required (:error/code result)))
    (is (= :inadequate (get-in result [:state :rounds 0 :outcome])))))

(deftest f202-committed-mutation-is-progress-without-residual
  ;; Supplied f202 report pin, including its truncated failure-account text.
  (let [report {:solver/outcome :progress
                :committed? true
                :mutations ["problems/m02A03/lean/Main.lean"]
                :failure-account ["Progress: committed an exact reduction from compactly supported smooth te..."]
                :axioms ["propext" "sorryAx" "Classical.choice" "Quot.sound"]
                :lean {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}}
        round (#'sut/terminal-round legacy-state {:report report} {:ok false} 1)]
    (is (= :progress (:outcome round)))
    (is (not (contains? (:report round) :residual)))))

(deftest literal-residual-remains-progress
  ;; f200-style shape; this text is a test residual, not a live report pin.
  (let [round (#'sut/terminal-round legacy-state
                                  {:report {:solver/outcome :progress
                                            :residual "remaining proof work"}}
                                  {:ok false} 1)]
    (is (= :progress (:outcome round)))))

(deftest unsupported-progress-claim-remains-inadequate
  (doseq [report [{:solver/outcome :progress :committed? false}
                  {:solver/outcome :progress :committed? true :mutations []}
                  {:solver/outcome :progress :committed? false
                   :mutations ["problems/m02A03/lean/Main.lean"]}]]
    (is (= :inadequate
           (:outcome (#'sut/terminal-round legacy-state {:report report}
                                          {:ok false} 1))))))

(deftest legacy-agent-nested-progress-fields-are-lifted-into-round-record
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f19-solver"
                                  :session-id "solver-session" :state :done
                                  :report {:lean {:solver/outcome :progress
                                                  :residual "exact remaining goal"
                                                  :artifact-commits ["abc"]}}})))]
    (is (= :progress (get-in result [:state :rounds 0 :outcome])))
    (is (= "exact remaining goal"
           (get-in result [:state :rounds 0 :report :residual])))
    (is (= ["abc"]
           (get-in result [:state :rounds 0 :report :artifact-commits])))))

(deftest later-round-session-drift-fails-closed
  (let [persisted (atom nil)
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request
               :rounds [{:ordinal 1 :session-id "original-session"}]
               :active (assoc legacy-state
                              :request (sut/round-request base-request 2 nil))}
        result (sut/drive!
                (assoc (effects persisted) :state state
                       :job-fn (fn [_]
                                 {:job-id "job-2" :agent-id "f19-solver"
                                  :session-id "different-session" :state :done
                                  :report {}})))]
    (is (= :solver-session-mismatch (:error/code result)))
    (is (nil? @persisted))))

(def f200-failed-dispatch
  ;; Created 2026-09-08T22:27:07.591Z; finished 2026-09-08T22:27:11.486Z.
  ;; execution was {executed: false, tool-events: 0, command-events: 0}.
  ;; The job port preserves these terminal fields, not the execution counters.
  {:job-id "apm-role-29c48563c4341a293808fa3a545be7808aea84282047b5874cff3635c0d4bfe0"
   :agent-id "f200-solver" :state :failed :terminal-code :invoke-error
   :session-id nil :report nil
   :terminal-message
   "Exit 1: thread-store conflict: thread 01a08287-25de-7dc3-80f8-d6ca7f6c11a6 already has an active writer. Error: thread/resume failed (code -32600)"})

(def f200-expected-session "01a08287-25de-7dc3-80f8-d6ca7f6c11a6")

(defn- f200-round-35-state []
  {:state/type :solver-rounds :budget/max-rounds 50
   :base-request base-request
   :rounds (mapv (fn [ordinal]
                   {:ordinal ordinal :session-id f200-expected-session
                    :report {:residual "unfinished"}
                    :validation {:ok false :findings [:prior-progress]}})
                 (range 1 35))
   :active (-> legacy-state
               (assoc :request (sut/round-request base-request 35 nil))
               (assoc-in [:ticket :job-id] (:job-id f200-failed-dispatch)))})

(deftest f200-failed-dispatch-without-session-records-terminal-failure
  (let [persisted (atom nil)
        base (assoc (effects persisted)
                    :state (f200-round-35-state)
                    :job-fn (constantly f200-failed-dispatch)
                    :terminal-submission-provider (constantly nil))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))
        completed (last (get-in result [:state :rounds]))]
    (is (= :terminal-collected (:status collected)))
    (is (not= :solver-session-mismatch (:error/code result)))
    (is (= :solver-job-terminal-failure
           (get-in completed [:validation :error/code])))
    (is (= (:job-id f200-failed-dispatch) (:job-id completed)))
    (is (= :failed (:terminal-state completed)))
    (is (= 35 (:ordinal completed)))
    (is (= :awaiting-terminal (:status result)))
    (is (= "job-36" (:job-id result)))))

(deftest f202-consecutive-apparatus-failures-spend-rounds-without-artifact-remediation
  ;; Live f202 dispatch pins: created 2026-09-09T02:29:44.887Z and
  ;; 2026-09-09T02:30:00.920Z respectively, both terminal-code :invoke-error.
  (let [job-ids ["apm-role-ca1000194250b9388b1ebf2a4c685df00049863b89b5dc7781fa5bf43d57cf7c"
                 "apm-role-4ee1f31b83817db20f86feca97159227b14bc0992e5125d53f4e546cdb67be14"]
        persisted (atom nil)
        ;; Prior rounds are scaffolding, not a transcription of their reports.
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request
               :rounds (mapv (fn [ordinal] {:ordinal ordinal}) (range 1 23))
               :active (-> legacy-state
                           (assoc :request (sut/round-request base-request 23 nil))
                           (assoc-in [:ticket :job-id] (first job-ids)))}
        base (assoc (effects persisted)
                    :terminal-submission-provider (constantly nil)
                    :job-fn (fn [id] {:job-id id :state :failed
                                     :terminal-code :invoke-error :report nil})
                    :announce-fn (fn [request]
                                   {:ok true :job-id (if (= 24 (:solver/round request))
                                                      (second job-ids) "next-round")}))
        result (reduce (fn [previous _]
                         (let [collected (sut/drive! (assoc base :state (:state previous)))]
                           (sut/drive! (assoc base :state (:state collected)))))
                       {:state state} job-ids)
        spent (subvec (get-in result [:state :rounds]) 22)]
    (is (not= :solver-remediation-required (:error/code result)))
    (is (not= :solver-remediation-required (get-in result [:state :state/type])))
    (is (= :awaiting-terminal (:status result)))
    (is (= [23 24] (mapv :ordinal spent)))
    (is (= job-ids (mapv :job-id spent)))
    (is (every? #(and (nil? (:report %))
                     (= :solver-job-terminal-failure
                        (get-in % [:validation :error/code]))) spent))))

(deftest failed-dispatch-with-a-different-session-still-fails-closed
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :state (f200-round-35-state)
                       :job-fn (constantly (assoc f200-failed-dispatch
                                                 :session-id "different-session"))))]
    (is (= :solver-session-mismatch (:error/code result)))
    (is (= {:expected f200-expected-session :actual "different-session"}
           (:finding result)))
    (is (nil? @persisted))))

(deftest done-without-session-and-with-typed-submission-still-certifies
  (let [persisted (atom nil)
        base (assoc (effects persisted)
                    :state (f200-round-35-state)
                    :job-fn (constantly (assoc f200-failed-dispatch :state :done))
                    :terminal-submission-provider
                    (constantly {:payload {:outcome "complete" :command-own-exit 0}})
                    :validate-solved (fn [_ _ job]
                                       {:ok (some? (:typed-submission job))})
                    :provide-receipt
                    (fn [& _] {:ok true :certificate {:receipt/id "solved"}}))
        collected (sut/drive! base)
        result (sut/drive! (assoc base :state (:state collected)))]
    (is (= :terminal-collected (:status collected)))
    (is (= :certified (:status result)))
    (is (= "solved" (get-in result [:certificate :receipt/id])))))

(deftest done-without-session-or-typed-submission-still-fails-closed
  (let [persisted (atom nil)
        result (sut/drive!
                (assoc (effects persisted)
                       :state (f200-round-35-state)
                       :job-fn (constantly (assoc f200-failed-dispatch :state :done))))]
    (is (= :solver-session-mismatch (:error/code result)))
    (is (nil? @persisted))))

(deftest every-tenth-round-is-an-addressed-strategy-checkpoint
  (doseq [ordinal [10 20 30 40 50]]
    (is (true? (:solver/strategy-checkpoint?
                (sut/round-request base-request ordinal nil)))))
  (doseq [ordinal [1 9 11 49]]
    (is (false? (:solver/strategy-checkpoint?
                 (sut/round-request base-request ordinal nil))))))

(deftest typed-progress-fields-normalize-into-round-semantics
  (let [normalize (deref #'sut/normalize-round-report)
        report (normalize {:outcome "progress"
                           :failure-account
                           ["residual: close the final equality"
                            "artifact-commits: abc123"]})]
    (is (= :progress (:solver/outcome report)))
    (is (= "close the final equality" (:residual report)))
    (is (= "abc123" (:artifact-commits report)))))

(deftest json-strategy-decision-values-normalize-losslessly
  (let [normalize (deref #'sut/normalize-round-report)
        report (normalize
                {:solver/strategy
                 {:summary "route" :obligations ["one"]
                  :decomposition [{:obligation "one"
                                   :decision "sequential"
                                   :reason "dependent"}]
                  :next-plan "continue"}})]
    (is (= :sequential
           (get-in report [:solver/strategy :decomposition 0 :decision])))))

(deftest missing-ten-round-strategy-stops-before-another-dispatch
  (let [persisted (atom nil)
        prior (mapv (fn [ordinal] {:ordinal ordinal :job-id (str "j" ordinal)})
                    (range 1 10))
        checkpoint-active (assoc legacy-state
                                 :request (sut/round-request base-request 10 nil))
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request :rounds prior :active checkpoint-active}
        result (sut/drive! (assoc (effects persisted) :state state))]
    (is (:ok result))
    (is (:repair? result))
    (is (= 10 (get-in @persisted [:active :request :solver/round])))
    (is (= [:solver-strategy-missing-or-invalid]
           (get-in @persisted [:active :request :repair/findings])))
    (is (= 9 (count (:rounds @persisted))))
    (is (= 1 (count (:checkpoint/invalid-observations @persisted))))))

(deftest persisted-checkpoint-failure-resumes-as-same-round-collection-repair
  (let [persisted (atom nil)
        rounds (mapv (fn [ordinal]
                       {:ordinal ordinal :job-id (str "j" ordinal)
                        :session-id "same" :terminal-state :done
                        :report {:outcome "progress"}})
                     (range 1 11))
        state {:state/type :solver-strategy-checkpoint-required
               :budget/max-rounds 50 :base-request base-request
               :rounds rounds :active nil}
        result (sut/resume-strategy-collection!
                (assoc (effects persisted) :state state))]
    (is (:ok result))
    (is (= 10 (get-in @persisted [:active :request :solver/round])))
    (is (= "j10" (get-in @persisted
                          [:active :request :repair/of-job-id])))
    (is (= 9 (count (:rounds @persisted))))
    (is (= (last rounds)
           (last (:checkpoint/invalid-observations @persisted))))))

(deftest persisted-valid-json-checkpoint-advances-to-ordinary-next-round
  (let [persisted (atom nil)
        strategy {:summary "Viable route" :obligations ["Close target"]
                  :decomposition [{:obligation "Close target"
                                   :decision "sequential"
                                   :reason "Single dependency chain"}]
                  :next-plan "Close it"}
        rounds (mapv (fn [ordinal]
                       {:ordinal ordinal :job-id (str "j" ordinal)
                        :report (cond-> {:outcome "progress"}
                                  (= ordinal 10)
                                  (assoc :solver/strategy strategy))})
                     (range 1 11))
        state {:state/type :solver-strategy-checkpoint-required
               :budget/max-rounds 50 :base-request base-request
               :rounds rounds :active nil}
        result (sut/resume-strategy-checkpoint!
                (assoc (effects persisted) :state state))]
    (is (:ok result))
    (is (= 11 (get-in @persisted [:active :request :solver/round])))
    (is (= :regular (get-in @persisted
                            [:active :request :solver/role-card-mode])))
    (is (= :sequential
           (get-in @persisted
                   [:rounds 9 :report :solver/strategy
                    :decomposition 0 :decision])))))

(deftest valid-ten-round-strategy-allows-next-episode
  (let [persisted (atom nil)
        prior (mapv (fn [ordinal] {:ordinal ordinal :job-id (str "j" ordinal)})
                    (range 1 10))
        checkpoint-active (assoc legacy-state
                                 :request (sut/round-request base-request 10 nil))
        state {:state/type :solver-rounds :budget/max-rounds 50
               :base-request base-request :rounds prior :active checkpoint-active}
        strategy {:summary "The factorization route remains viable."
                  :obligations ["Boundary extension" "Derivative count"]
                  :decomposition [{:obligation "Boundary extension"
                                   :decision :delegate
                                   :reason "Independent named lemma."}
                                  {:obligation "Derivative count"
                                   :decision :sequential
                                   :reason "Consumes the extension."}]
                  :next-plan "Integrate the extension, then close the count."}
        result (sut/drive!
                (assoc (effects persisted) :state state
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f19-solver"
                                  :session-id "solver-session" :state :done
                                  :report {:solver/outcome :progress
                                           :residual "Boundary extension"
                                           :solver/strategy strategy}})))]
    (is (:ok result))
    (is (= 11 (get-in result [:state :active :request :solver/round])))
    (is (= strategy (get-in result [:state :rounds 9 :report :solver/strategy])))))

(deftest repaired-checkpoint-certifies-without-dispatching-another-round
  (let [persisted (atom nil)
        rounds (mapv (fn [ordinal]
                       {:ordinal ordinal :job-id (str "j" ordinal)
                        :session-id "same" :terminal-state :done
                        :report {:head "proved"}})
                     (range 1 11))
        state {:state/type :solver-strategy-checkpoint-required
               :budget/max-rounds 50 :base-request base-request
               :rounds rounds :active nil}
        result (sut/repair-checkpoint!
                {:state state
                 :persist-fn #(do (reset! persisted %) {:ok true})
                 :validate-solved (fn [request ticket job]
                                    {:ok (and (= 10 (:solver/round request))
                                              (= "j10" (:job-id ticket))
                                              (= {:head "proved"} (:report job)))})
                 :provide-receipt (fn [& _]
                                    {:ok true :certificate {:receipt/id "solve"}})})]
    (is (:ok result))
    (is (= :live-job-certified (:state/type @persisted)))
    (is (= 10 (count (:rounds @persisted))))
    (is (= :solver-strategy-checkpoint-required (:repair/source-state @persisted)))))
