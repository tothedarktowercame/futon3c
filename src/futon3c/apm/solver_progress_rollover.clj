(ns futon3c.apm.solver-progress-rollover
  "Certified same-problem rollover for an unsolved Solver checkpoint."
  (:require [futon3c.apm.campaign-machine :as machine]))

(defn addressed [body]
  (assoc body :receipt/id (machine/ledger-digest [body])))

(defn derive-terminal
  [{:keys [frame solve-state workspace-heads]}]
  (let [rounds (:rounds solve-state)
        last-round (last rounds)
        report (:report last-round)
        head (:final-head report)
        progress-body
        {:receipt/type :solver-progress-terminal
         :frame/id (:frame/id frame) :problem/id (:problem/id frame)
         :solver/rounds (count rounds) :solver/final-head head
         :solver/residual (last (:failure-account report))
         :source/dispatch-id (:dispatch/id last-round)}
        progress (addressed progress-body)
        terminal-body
        {:receipt/type :frame-terminal
         :frame/id (:frame/id frame) :problem/id (:problem/id frame)
         :frame/result :partial :problem/outcome :partial
         :learning/outcome :skipped :retry/same-problem? true
         :solver-progress-receipt/id (:receipt/id progress)
         :solver {:branch (:branch report) :head head}
         :workspace/terminal-heads workspace-heads}
        findings
        (cond-> []
          (not= :solver-strategy-checkpoint-required (:state/type solve-state))
          (conj :solver-checkpoint-state-required)
          (not= 10 (count rounds)) (conj :solver-round-ten-required)
          (some? (:active solve-state)) (conj :solver-active-job-present)
          (not= :done (:terminal-state last-round))
          (conj :solver-terminal-turn-required)
          (not (and (string? head) (re-matches #"[0-9a-f]{40}" head)))
          (conj :solver-head-invalid)
          (not= 0 (get-in report [:lean :exit])) (conj :solver-lean-failed)
          (not= 0 (get-in report [:lean :errors])) (conj :solver-lean-errors)
          (not (pos? (get-in report [:lean :sorry-warnings] 0)))
          (conj :solver-progress-not-partial)
          (not (true? (:committed? report))) (conj :solver-head-not-committed)
          (not (true? (:clean-after? report))) (conj :solver-worktree-dirty)
          (not= head (:solver workspace-heads))
          (conj :solver-workspace-head-mismatch)
          (not= #{:solver :student} (set (keys workspace-heads)))
          (conj :workspace-heads-incomplete))]
    (if (seq findings)
      {:ok false :error/code :solver-progress-rollover-invalid
       :findings findings}
      {:ok true :progress-receipt progress
       :terminal-receipt (addressed terminal-body)})))

(defn retry-problem [problem retained-branch retained-head retained-blob]
  (if (and (string? retained-head) (re-matches #"[0-9a-f]{40}" retained-head)
           (string? retained-blob) (re-matches #"[0-9a-f]{40}" retained-blob))
    (assoc problem :base-branch retained-branch
           :revision retained-head :blob retained-blob)
    {:error/code :solver-progress-retry-pin-invalid}))
