;; Register claude-2, the Opus independent-reviewer seat on Zone.
;; Live-state registration: does NOT survive a futon3c-zone restart —
;; re-run via: bash scripts/proof-eval.sh scripts/register_opus_reviewer.clj
;; Created 2026-08-10 (Joe: Dionysus agents down; reviewer role moved to Zone
;; from claude-10). Author != reviewer: this seat approves scribe attachments.
(do
  (require '[futon3c.agency.registry :as reg]
           '[clojure.java.io :as io])
  (let [agent-id "claude-2"
        sf (io/file "/tmp/futon-session-id-claude-2")
        sid-atom (atom nil)
        make-fn (resolve 'futon3c.dev/make-claude-invoke-fn)
        invoke-fn (@make-fn
                   {:claude-bin (or (System/getenv "CLAUDE_BIN") "claude")
                    :permission-mode "bypassPermissions"
                    :agent-id agent-id
                    :session-file sf
                    :session-id-atom sid-atom
                    :model "claude-opus-5"
                    :cwd "/home/joe/code"})]
    (reg/register-agent!
     {:agent-id {:id/value agent-id :id/type :continuity}
      :type :claude
      :invoke-fn invoke-fn
      :capabilities [:explore :edit :test :coordination/execute]
      :metadata {:auto-registered? false
                 :model "claude-opus-5"
                 :role "independent-reviewer"
                 :registered-by "ams-claude-1"
                 :cwd "/home/joe/code"}})
    {:registered agent-id
     :model "claude-opus-5"
     :ok (reg/agent-registered? agent-id)}))
