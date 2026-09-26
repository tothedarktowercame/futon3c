;; Register claude-20, the Opus TECH-LEAD seat for the R-node build (2026-08-30).
;; Modelled on register_opus_reviewer_claude4.clj. Live-state registration: does
;; NOT survive a futon3c-zone restart — re-run via:
;;   bash scripts/proof-eval.sh scripts/register_opus_tech_lead_claude20.clj
;; Created by claude-15 on Joe's direction ("bell out a tech-lead role to an Opus
;; agent"). Charter: futon2/holes/problems/BUILD-tech-lead-charter.md.
;; Reviewer (claude-4) != tech lead (claude-20) != owner (claude-15).
(do
  (require '[futon3c.agency.registry :as reg]
           '[clojure.java.io :as io])
  (let [agent-id "claude-20"
        sf (io/file "/tmp/futon-session-id-claude-20")
        sid-atom (atom nil)
        make-fn (resolve 'futon3c.dev/make-claude-invoke-fn)
        invoke-fn (@make-fn
                   {:claude-bin (or (System/getenv "CLAUDE_BIN") "claude")
                    :permission-mode "bypassPermissions"
                    :agent-id agent-id
                    :session-file sf
                    :session-id-atom sid-atom
                    :model "opus"
                    :cwd "/home/joe/code"})
        metadata {:auto-registered? false
                  :model "claude-opus-5"
                  :role "tech-lead"
                  :registered-by "claude-15"
                  :registered-note "R-node build tech lead, 2026-08-30; charter futon2/holes/problems/BUILD-tech-lead-charter.md"
                  :cwd "/home/joe/code"}]
    (reg/register-agent!
     {:agent-id {:id/value agent-id :id/type :continuity}
      :type :claude
      :invoke-fn invoke-fn
      :capabilities [:explore :edit :test :coordination/execute]
      :metadata metadata})
    ;; register-agent! no-ops the invoke-fn when the id already exists; update is load-bearing.
    (reg/update-agent! agent-id :agent/invoke-fn invoke-fn)
    (reg/update-agent! agent-id :agent/metadata metadata)
    {:registered agent-id :model "opus" :ok (reg/agent-registered? agent-id)}))
