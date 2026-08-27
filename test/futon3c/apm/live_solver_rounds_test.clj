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
    (is (= :terminal-collected (:status repair-collected)))
    (is (= :solver-typed-submission-repair-exhausted
           (:error/code exhausted)))
    (is (= 1 (:repair/attempts exhausted)))))

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
        result (sut/drive!
                (assoc (effects persisted)
                       :job-fn (fn [_]
                                 {:job-id "job-1" :agent-id "f19-solver"
                                  :session-id "solver-session" :state :done
                                  :report {:solver/outcome :claimed-defect
                                           :residual "Hypotheses contradict conclusion at z=0."}})))]
    (is (= :solver-defect-review-required (:error/code result)))
    (is (= :claimed-defect (get-in result [:state :rounds 0 :outcome])))
    (is (nil? (get-in result [:state :problem/classification])))))

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
