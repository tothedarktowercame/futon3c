;; Register claude-cs, the Claude seat that lives in Joe's code-server (2026-11-10).
;; Joe: "adding and registering claude-cs is great ... 'We heard you wanted more
;; Agency with your VS code and Claude, so here's the connector.'"
;;
;; Pull-only seat (Clink lane, E-bell-clink-adapter), from 2026-11-10 stage 2:
;; - Bells to claude-cs are written to ~/.claude/agency-inbox/claude-cs/<job>.json
;;   and the job sits at `delivered`. Agency never starts a claude process for
;;   this seat, so nothing forks the conversation.
;; - Joe runs the conversation in a code-server terminal with
;;   futon6/tools/futon-relay-channel/claude-cs.sh, whose channel server pushes
;;   each inbox delivery into the session as a turn.
;; - The session answers and acks with clink-inbox.py ack --result-file, which
;;   moves the file to consumed/ and returns the answer to the sender.
;; - The session file below is kept for reference: claude-cs.sh resumes it.
;; Earlier (same day) the seat was cold push: each bell ran a headless
;; `claude --resume` of this session, which forked it while Joe used the panel.
;;
;; Live-state registration: does NOT survive a futon3c restart unless the
;; durable roster restores it. Re-run with
;;   bash scripts/proof-eval.sh scripts/register_claude_cs.clj
(do
  (require '[futon3c.agency.registry :as reg]
           '[clojure.java.io :as io]
           '[clojure.string :as str])
  (let [agent-id "claude-cs"
        sf (io/file (System/getProperty "user.home") ".local/state/futon3c/session-id-claude-cs")
        sid (let [s (when (.exists sf) (str/trim (slurp sf)))]
              (when (seq s) s))
        metadata {:auto-registered? false
                  :pouch "none: pull-only (Clink inbox)"
                  :role "code-server seat (Mojira dev)"
                  :surface "code-server :8090 terminal, futon6 tools/futon-relay-channel/claude-cs.sh"
                  :registered-by "claude-13; inbox lane by claude-cs"
                  :registered-note "Joe 2026-11-10: an agent that lives in code-server; ideas are belled to it"
                  :cwd "/home/joe/code"}]
    (reg/register-agent!
     {:agent-id {:id/value agent-id :id/type :continuity}
      :type :claude
      :invoke-fn nil
      :delivery-mode :inbox
      :capabilities [:explore :edit :test :coordination/execute]
      :metadata metadata})
    ;; register-agent! refuses an existing seat; update so a re-run takes effect.
    (reg/update-agent! agent-id
                       :agent/delivery-mode :inbox
                       :agent/invoke-fn nil
                       :agent/metadata metadata)
    (when sid (reg/update-agent! agent-id :agent/session-id sid))
    {:registered agent-id :session sid :ok (reg/agent-registered? agent-id)
     :delivery-mode (:agent/delivery-mode (reg/get-agent agent-id))}))
