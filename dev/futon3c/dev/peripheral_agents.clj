(ns futon3c.dev.peripheral-agents
  "Registration helpers for peripheral/support agents.

   These are distinct from the main Claude/Codex worker registrations:
   - `tickle-1` is a coordination/watchdog wrapper around Claude Haiku
   - `claude-2` is reserved for mentor-style peripheral work"
  (:require [futon3c.agency.registry :as reg]
            [futon3c.dev.config :as config]
            [futon3c.runtime.agents :as rt]
            [clojure.java.io :as io]))

(defn- short-session
  [session-id]
  (when session-id
    (subs session-id 0 (min 8 (count session-id)))))

(defn register-mentor-claude-agent!
  [{:keys [agent-id session-file socket metadata make-claude-invoke-fn read-session-id]}]
  (let [initial-sid (read-session-id session-file)
        sid-atom (atom initial-sid)
        invoke-fn (make-claude-invoke-fn
                   {:claude-bin (config/env "CLAUDE_BIN" "claude")
                    :permission-mode (config/env "CLAUDE_PERMISSION" "bypassPermissions")
                    :agent-id agent-id
                    :session-file session-file
                    :session-id-atom sid-atom
                    :emacs-socket socket})]
    (rt/register-claude! {:agent-id agent-id
                          :invoke-fn invoke-fn})
    (reg/update-agent! agent-id
                       :agent/type :claude
                       :agent/invoke-fn invoke-fn
                       :agent/metadata metadata
                       :agent/capabilities [:explore :edit :test :coordination/execute])
    (when initial-sid
      (reg/update-agent! agent-id :agent/session-id initial-sid))
    (println (str "[dev] Claude agent registered: " agent-id " (mentor, inline invoke)"
                  " (emacs: " socket ")"
                  (when initial-sid
                    (str " (session: " (short-session initial-sid) ")"))))
    {:invoke-fn invoke-fn
     :session-id initial-sid}))

(defn register-tickle-agent!
  [{:keys [make-claude-invoke-fn make-tickle-invoke-fn read-session-id]}]
  (let [session-file (io/file "/tmp/futon-tickle-1-session-id")
        initial-sid (read-session-id session-file)
        sid-atom (atom initial-sid)
        raw-fn (make-claude-invoke-fn
                {:claude-bin (config/env "CLAUDE_BIN" "claude")
                 :permission-mode "default"
                 :agent-id "tickle-1"
                 :session-file session-file
                 :session-id-atom sid-atom
                 :model "claude-haiku-4-5-20251001"})
        invoke-fn (make-tickle-invoke-fn raw-fn)]
    (reg/register-agent! {:agent-id "tickle-1"
                          :type :tickle
                          :invoke-fn invoke-fn
                          :capabilities [:coordination/orchestrate]
                          :metadata {:role "watchdog"}})
    (println (str "[dev] Tickle agent registered: tickle-1 (claude invoke)"
                  (when initial-sid
                    (str " (session: " (short-session initial-sid) ")"))))
    {:invoke-fn invoke-fn
     :session-id initial-sid}))
