;; Register claude-cs, the Claude seat that lives in Joe's code-server (2026-11-10).
;; Joe: "adding and registering claude-cs is great ... 'We heard you wanted more
;; Agency with your VS code and Claude, so here's the connector.'"
;;
;; How it fits together (same shape as the codex-vscode lane):
;; - code-server :8090 (workspace /home/joe/code) has the Claude Code extension
;;   (anthropic.claude-code), installed by claude-13 the same day.
;; - Bells to claude-cs run as headless turns of ONE Claude session, created by
;;   the first bell and kept in the session file below; later bells --resume it.
;; - Joe opens that conversation from the extension's history (same workspace,
;;   so the same project directory) and works with the agent there.
;; - Do not type in the panel while a bell turn is running: both would write to
;;   the same session.
;;
;; Live-state registration: does NOT survive a futon3c restart. Re-run with
;;   bash scripts/proof-eval.sh scripts/register_claude_cs.clj
;; The session file survives, so the seat comes back on the same conversation.
(do
  (require '[futon3c.agency.registry :as reg]
           '[clojure.java.io :as io]
           '[clojure.string :as str])
  (let [agent-id "claude-cs"
        sf (io/file (System/getProperty "user.home") ".local/state/futon3c/session-id-claude-cs")
        _ (io/make-parents sf)
        sid (let [s (when (.exists sf) (str/trim (slurp sf)))]
              (when (seq s) s))
        sid-atom (atom sid)
        make-fn (resolve 'futon3c.dev/make-claude-invoke-fn)
        warm-invoke-fn (@make-fn
                   {:claude-bin (or (System/getenv "CLAUDE_BIN") "claude")
                    :permission-mode "bypassPermissions"
                    :agent-id agent-id
                    :session-file sf
                    :session-id-atom sid-atom
                    :model "opus"
                    :cwd "/home/joe/code"})
        ;; Cold seat (Joe, 2026-11-10: "Let's just make the seat cold by default
        ;; so that we can use it with the usual Agency mental model"): Joe also
        ;; uses this conversation in the code-server panel, and a warm process
        ;; left resuming it between bells made the panel report it "still open
        ;; somewhere else".  After every turn the seat's warm pouch is evicted,
        ;; so nothing holds the conversation between bells.
        evict! (requiring-resolve 'futon3c.agency.agent-pouch/evict!)
        invoke-fn (fn [& args]
                    (try (apply warm-invoke-fn args)
                         (finally (evict! agent-id))))
        metadata {:auto-registered? false
                  :pouch "cold: evicted after every turn"
                  :role "code-server seat (Mojira dev)"
                  :surface "code-server :8090, Claude Code extension"
                  :registered-by "claude-13"
                  :registered-note "Joe 2026-11-10: an agent that lives in code-server; ideas are belled to it"
                  :cwd "/home/joe/code"}]
    (reg/register-agent!
     {:agent-id {:id/value agent-id :id/type :continuity}
      :type :claude
      :invoke-fn invoke-fn
      :capabilities [:explore :edit :test :coordination/execute]
      :metadata metadata})
    ;; register-agent! keeps an existing invoke-fn; update so a re-run takes effect.
    (reg/update-agent! agent-id :agent/invoke-fn invoke-fn)
    (reg/update-agent! agent-id :agent/metadata metadata)
    ;; The roster shows the registry's session id; without this a re-pointed
    ;; seat kept showing the old one until its next bell ran (2026-11-10).
    (when sid (reg/update-agent! agent-id :agent/session-id sid))
    {:registered agent-id :session sid :ok (reg/agent-registered? agent-id)}))
