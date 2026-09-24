(ns futon3c.agency.roles
  "Provider and role facts about an agent, read from what the agent declared
   at registration, never parsed out of its id (M-futon-seams instance 4).

   Before this namespace, two routing decisions in transport/http.clj read
   the provider from the id's prefix: execution-evidence enforcement applied
   to ids starting with \"codex\", and stale-session recovery to ids starting
   with \"claude\". The registry already records the provider as :agent/type,
   so an id and its declared type could disagree. On 2026-09-24 the running
   registry held 22 agents of type :codex whose ids do not start with
   \"codex\" (oxf-codex-*, lon-codex-*, chi-codex-*, wm-author, wm-reviewer),
   and the enforcement skipped all of them.

   An agent that is not registered has no declared provider; that is
   reported as nil, never guessed from the id."
  (:require [futon3c.agency.registry :as reg]))

(defn provider
  "The provider AGENT-ID declared at registration (:agent/type), or nil when
   the agent is not registered."
  [agent-id]
  (:agent/type (reg/get-agent agent-id)))

(defn provider?
  "True when AGENT-ID is registered with provider type PROVIDER."
  [agent-id provider-type]
  (= provider-type (provider agent-id)))

(defn requires-execution?
  "Whether replies from AGENT-ID must carry execution evidence in work mode.
   An explicit :require-execution? in the agent's metadata wins; otherwise
   Codex-provided agents require it."
  [agent-id]
  (let [metadata (:agent/metadata (reg/get-agent agent-id))
        explicit (if (contains? metadata :require-execution?)
                   (get metadata :require-execution?)
                   (get metadata "require-execution?"))]
    (if (some? explicit)
      (boolean explicit)
      (provider? agent-id :codex))))
