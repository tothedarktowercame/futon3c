(ns futon3c.agents.mfuton-invoke-override
  "mfuton-owned runtime invoke overrides.

   Keep the decision about replacing Claude-backed invoke paths with Codex in
   one mfuton-owned file. Generic futon owners should only branch here."
  (:require [clojure.string :as str]
            [futon3c.dev.config :as config]
            [futon3c.dev.irc :as dev-irc]
            [futon3c.mfuton-mode :as mfuton-mode]))

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
  [{:keys [agent-id invoke-trace-id receipt-line]}]
  (when (mfuton-mode/mfuton-mode?)
    (let [aid (some-> agent-id str str/trim)
          tid (some-> invoke-trace-id str str/trim)
          channel (delivery-irc-channel)
          from-nick (delivery-irc-sender aid)
          message (str "[invoke-delivery] " aid " " receipt-line)
          delivered? (dev-irc/send-irc! channel from-nick message)]
      (when-not delivered?
        (println (str "[invoke-delivery] failed for " aid
                      " trace-id=" tid
                      " status=irc-send-failed"
                      " channel=" channel
                      " from=" from-nick))
        (flush))
      delivered?)))
