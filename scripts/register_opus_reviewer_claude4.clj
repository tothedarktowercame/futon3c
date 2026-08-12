;; Register claude-4, the Opus independent-reviewer seat on Zone.
;; Live-state registration: does NOT survive a futon3c-zone restart —
;; re-run via: bash scripts/proof-eval.sh scripts/register_opus_reviewer_claude4.clj
;; Created 2026-08-12 by claude-2 on taking the captain seat
;; (E-captain-pass-to-claude-2.md): claude-2 moved operator->captain, so the
;; reviewer role moves to claude-4. Operator != reviewer != scribe.
;;
;; NOTE claude-4 was ALREADY on the roster as an auto-registered Emacs seat
;; (auto-registered? true, no :model, no :role, cwd /home/joe). Presence is
;; not configuration — silence-catalogue instance 5. register-agent! does NOT
;; replace an existing agent's invoke-fn, so the update-agent! call below is
;; the load-bearing one.
(do
  (require '[futon3c.agency.registry :as reg]
           '[clojure.java.io :as io])
  (let [agent-id "claude-4"
        sf (io/file "/tmp/futon-session-id-claude-4")
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
                 :registered-by "claude-2"
                 :registered-note "captain handover 2026-08-12; supersedes auto-registered Emacs seat"
                 :cwd "/home/joe/code"}})
    ;; Load-bearing on re-registration: register-agent! no-ops the invoke-fn
    ;; when the id already exists (2026-08-10 finding; cost three review passes).
    (reg/update-agent! agent-id :agent/invoke-fn invoke-fn)
    (reg/update-agent! agent-id :agent/metadata
                       {:auto-registered? false
                        :model "claude-opus-5"
                        :role "independent-reviewer"
                        :registered-by "claude-2"
                        :registered-note "captain handover 2026-08-12; supersedes auto-registered Emacs seat"
                        :cwd "/home/joe/code"})
    {:registered agent-id
     :model "claude-opus-5"
     :ok (reg/agent-registered? agent-id)}))
