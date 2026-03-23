(ns futon3c.agents.mfuton-invoke-override
  "mfuton-owned runtime invoke overrides.

   Keep the decision about replacing Claude-backed invoke paths with Codex in
   one mfuton-owned file. Generic futon owners should only branch here."
  (:require [futon3c.mfuton-mode :as mfuton-mode]))

(defn claude-role-codex-opts
  "Return Codex invoke opts for a Claude-role agent when mfuton mode is active.
   Returns nil outside mfuton mode so generic futon behavior stays unchanged."
  [{:keys [agent-id
           session-file
           session-id-atom
           profile
           model
           sandbox
           approval-policy
           reasoning-effort
           timeout-ms
           cwd]}]
  (when (mfuton-mode/mfuton-mode?)
    {:codex-bin "codex"
     :profile profile
     :model (or model "gpt-5-codex")
     :sandbox (or sandbox "workspace-write")
     :approval-policy (or approval-policy "untrusted")
     :reasoning-effort reasoning-effort
     :timeout-ms timeout-ms
     :cwd cwd
     :agent-id agent-id
     :session-file session-file
     :session-id-atom session-id-atom}))
