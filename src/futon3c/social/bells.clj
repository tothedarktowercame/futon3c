(ns futon3c.social.bells
  "Bell dispatcher — coordination triggers for multi-agent rendezvous.

   Bells are async fire-and-forget signals that get agents into a shared
   context. The standup bell is the primary coordination primitive: it
   joins all registered agents into an IRC room for co-present conversation.

   Architecture:
   - Bells consult the agent registry to find participants
   - IRC transport provides the meeting venue (virtual nicks + relay bridge)
   - Evidence store records the bell ring and agent arrivals
   - No subprocess spawning — agents are already running, bells just route them

   Bell types:
   - :standup  — co-present IRC conversation (all agents join a room)
   - :test-bell — liveness proof (agent acks with secret)

   Pattern references:
   - realtime/rendezvous-handshake: bell ring + ack = co-presence proof
   - realtime/liveness-heartbeats: test-bell proves agent is receiving"
  (:require [futon3c.agency.registry :as registry]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str [] (str (Instant/now)))

(defn- make-bell-evidence
  "Create an evidence entry for a bell event."
  [bell-type room agents author]
  {:evidence/id (str "e-" (UUID/randomUUID))
   :evidence/subject {:ref/type :session :ref/id (str "bell/" (name bell-type) "/" room)}
   :evidence/type :coordination
   :evidence/claim-type :step
   :evidence/author author
   :evidence/at (now-str)
   :evidence/body {:bell-type bell-type
                   :room room
                   :agents (mapv :id/value agents)
                   :status :rung}
   :evidence/tags [:bell (keyword "bell" (name bell-type)) :coordination]
   :evidence/session-id (str "bell-" (UUID/randomUUID))})

(defn- make-arrival-evidence
  "Create an evidence entry for an agent arriving in the standup room."
  [bell-type room agent-id nick bell-session-id]
  {:evidence/id (str "e-" (UUID/randomUUID))
   :evidence/subject {:ref/type :agent :ref/id (str (:id/value agent-id))}
   :evidence/type :coordination
   :evidence/claim-type :observation
   :evidence/author "bell-dispatcher"
   :evidence/at (now-str)
   :evidence/body {:bell-type bell-type
                   :room room
                   :nick nick
                   :agent-id (:id/value agent-id)
                   :status :arrived}
   :evidence/tags [:bell :arrival (keyword "bell" (name bell-type))]
   :evidence/session-id bell-session-id})

(defn ring-standup!
  "Ring the standup bell — join all registered agents into an IRC room.

   config:
     :room             — IRC channel to join (e.g. \"#standup\")
     :prompt           — opening message sent to the room
     :author           — who rang the bell (e.g. \"joe\")
     :join-virtual-nick! — (fn [channel nick]) from IRC server
     :join-agent!      — (fn [agent-id nick channel ws-send-fn]) from relay bridge
     :send-to-channel! — (fn [channel from-nick text]) from IRC server
     :evidence-store   — atom (optional)
     :registry         — registry atom (optional, uses default if nil)
     :agents           — :all or vector of agent-id strings (default :all)

   Returns:
     {:bell/type :standup
      :bell/room channel
      :bell/agents [TypedAgentId ...]
      :bell/prompt prompt
      :bell/at timestamp
      :bell/session-id string}

   Or {:ok false :error ...} on failure."
  [{:keys [room prompt author join-virtual-nick! join-agent!
           send-to-channel! evidence-store registry agents]
    :or {agents :all author "bell-dispatcher"}}]
  (cond
    (nil? room)
    {:ok false :error "standup bell requires :room"}

    (nil? join-virtual-nick!)
    {:ok false :error "standup bell requires :join-virtual-nick!"}

    (nil? send-to-channel!)
    {:ok false :error "standup bell requires :send-to-channel!"}

    :else
    (let [;; Get agents from registry
          all-agents (if registry
                       (mapv :agent/id (vals @registry))
                       (registry/registered-agents))
          target-agents (if (= agents :all)
                          all-agents
                          (filterv (fn [aid]
                                     (contains? (set agents) (:id/value aid)))
                                   all-agents))
          ;; Emit bell-ring evidence
          bell-ev (make-bell-evidence :standup room target-agents author)
          bell-session-id (:evidence/session-id bell-ev)
          _ (when evidence-store
              (estore/append* evidence-store bell-ev))
          ;; Join each agent to the IRC room
          joined (atom [])]
      (doseq [agent-id target-agents]
        (let [nick (:id/value agent-id)]
          ;; Add virtual nick to IRC room (visible in NAMES/WHO)
          (join-virtual-nick! room nick)
          ;; Join agent in relay bridge if provided
          (when join-agent!
            ;; ws-send-fn placeholder — the relay bridge will use
            ;; the agent's actual WS connection when available
            (join-agent! nick nick room
                         (fn [_msg] :bell-placeholder)))
          ;; Emit arrival evidence
          (when evidence-store
            (estore/append* evidence-store
                            (make-arrival-evidence :standup room agent-id nick bell-session-id)))
          (swap! joined conj agent-id)))
      ;; Send opening prompt to room
      (when prompt
        (send-to-channel! room (or author "standup-bell") prompt))
      {:bell/type :standup
       :bell/room room
       :bell/agents @joined
       :bell/prompt prompt
       :bell/at (now-str)
       :bell/session-id bell-session-id})))

(defn ring-test-bell!
  "Ring a test bell — verify agent liveness via secret ack.

   config:
     :agent-id — specific agent to test (TypedAgentId or string)
     :evidence-store — atom (optional)

   Returns:
     {:bell/type :test-bell
      :bell/agent-id TypedAgentId
      :bell/secret string
      :bell/at timestamp}

   The agent should ack by returning the secret value."
  [{:keys [agent-id evidence-store]}]
  (let [aid-val (if (map? agent-id) (:id/value agent-id) (str agent-id))
        agent (registry/get-agent aid-val)]
    (if-not agent
      {:ok false :error (str "Agent not registered: " aid-val)}
      (let [secret (str (UUID/randomUUID))
            ev {:evidence/id (str "e-" (UUID/randomUUID))
                :evidence/subject {:ref/type :agent :ref/id aid-val}
                :evidence/type :coordination
                :evidence/claim-type :step
                :evidence/author "bell-dispatcher"
                :evidence/at (now-str)
                :evidence/body {:bell-type :test-bell
                                :agent-id aid-val
                                :secret-issued true}
                :evidence/tags [:bell :test-bell :liveness]}]
        (when evidence-store
          (estore/append* evidence-store ev))
        {:bell/type :test-bell
         :bell/agent-id (:agent/id agent)
         :bell/secret secret
         :bell/at (now-str)}))))
