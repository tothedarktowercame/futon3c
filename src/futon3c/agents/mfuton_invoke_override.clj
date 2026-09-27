(ns futon3c.agents.mfuton-invoke-override
  "mfuton-owned runtime invoke overrides.

   Keep the decision about replacing Claude-backed invoke paths with Codex in
   one mfuton-owned file. Generic futon owners should only branch here."
  (:require [clojure.string :as str]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc]
            [futon3c.mfuton-mode :as mfuton-mode]))

(defn claude-role-backend
  "Which backend serves a Claude-role agent's invoke: :claude (the real `claude -p`
   path) or :codex (redirect to Codex).

   Decoupled from mfuton-mode ON PURPOSE so the backend can be switched back and
   forth without also changing prompt-override or IRC-projection behavior (both of
   which stay gated on mfuton-mode?). Explicit FUTON3C_CLAUDE_ROLE_BACKEND wins:
   \"claude\" => :claude, \"codex\" => :codex. When unset, falls back to mfuton-mode?
   for backward compatibility (mfuton mode historically implied the codex redirect),
   so existing deployments are unchanged."
  []
  (case (some-> (config/env "FUTON3C_CLAUDE_ROLE_BACKEND") str/trim str/lower-case not-empty)
    "claude" :claude
    "codex"  :codex
    (if (mfuton-mode/mfuton-mode?) :codex :claude)))

(defn claude-role-codex-opts
  "Return Codex invoke opts for a Claude-role agent when the Claude-role backend is
   :codex (see `claude-role-backend`). Returns nil when the backend is :claude, so
   the caller runs the real `claude -p` invoke path."
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
  (when (= :codex (claude-role-backend))
    {:codex-bin "codex"
     :profile profile
     :model model
     :sandbox (or sandbox "workspace-write")
     :approval-policy (or approval-policy "untrusted")
     :reasoning-effort reasoning-effort
     :timeout-ms timeout-ms
     :cwd cwd
     :agent-id agent-id
     :session-file session-file
     :session-id-atom session-id-atom}))

(defn- delivery-irc-channel
  []
  (or (some-> (config/env "IRC_CHANNEL") str str/trim not-empty)
      "#futon"))

(defn- delivery-irc-sender
  [agent-id]
  (or (config/irc-nick-for-agent-id agent-id)
      "tickle"))

(defn maybe-record-delivery!
  "Project invoke delivery to IRC when mfuton mode is active.

   Returns nil outside mfuton mode so generic futon owners can keep their
   normal delivery-recording path."
  [{:keys [agent-id invoke-trace-id receipt receipt-line]}]
  (when (mfuton-mode/mfuton-mode?)
    (let [aid (some-> agent-id str str/trim)
          tid (some-> invoke-trace-id str str/trim)
          surface (some-> (:surface receipt) str str/trim str/lower-case)
          channel (delivery-irc-channel)
          from-nick (delivery-irc-sender aid)
          message (str "[invoke-delivery] " aid " " receipt-line)
          delivered? (if (= "irc" surface)
                       true
                       (dev-irc/send-irc! channel from-nick message))]
      (when-not delivered?
        (println (str "[invoke-delivery] failed for " aid
                      " trace-id=" tid
                      " status=irc-send-failed"
                      " channel=" channel
                      " from=" from-nick))
        (flush))
      delivered?)))
