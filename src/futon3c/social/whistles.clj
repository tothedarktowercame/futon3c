(ns futon3c.social.whistles
  "Whistle dispatcher — synchronous request-response coordination primitive.

   Whistles are blocking requests that invoke an agent and wait for a response.
   The counterpart to bells (async fire-and-forget): bell is 'tell', whistle
   is 'ask'.

   Architecture:
   - Whistles delegate to registry/invoke-agent! for the actual invocation
   - Evidence store records the whistle exchange (request + response)
   - No subprocess spawning — agents are already running, whistles just invoke them
   - Timeout handling inherited from invoke-agent!

   Pattern references:
   - realtime/rendezvous-handshake: whistle send + response = exchange proof
   - realtime/liveness-heartbeats: whistle to idle agent proves it is responding"
  (:require [futon3c.agency.registry :as registry]
            [futon3c.evidence.store :as estore]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str [] (str (Instant/now)))

(defn- truncate [s max-len]
  (let [s (str s)]
    (if (<= (count s) max-len)
      s
      (subs s 0 max-len))))

(defn- make-whistle-evidence
  "Create a combined evidence entry for a whistle exchange."
  [agent-id prompt response status author session-id]
  {:evidence/id (str "e-" (UUID/randomUUID))
   :evidence/subject {:ref/type :agent :ref/id (str agent-id)}
   :evidence/type :coordination
   :evidence/claim-type :step
   :evidence/author (or author "whistle-dispatcher")
   :evidence/at (now-str)
   :evidence/body {:agent-id agent-id
                   :prompt (truncate prompt 200)
                   :response (truncate response 500)
                   :status status}
   :evidence/tags [:whistle :coordination]
   :evidence/session-id (or session-id (str "whistle-" (UUID/randomUUID)))})

(defn whistle!
  "Send a synchronous request to an agent and wait for response.
   Bell is 'tell', whistle is 'ask'.

   config:
     :agent-id       — target agent (string or TypedAgentId)
     :prompt         — the question/request to send
     :author         — who initiated the whistle (e.g. \"joe\", \"claude-1\")
     :timeout-ms     — optional, default 60000
     :evidence-store — optional (emits coordination evidence if provided)

   Returns:
     {:whistle/ok true  :whistle/response \"...\" :whistle/agent-id \"...\"
      :whistle/session-id \"...\" :whistle/at \"...\"}

   Or on failure:
     {:whistle/ok false :whistle/error \"...\" :whistle/agent-id \"...\"
      :whistle/at \"...\"}"
  [{:keys [agent-id prompt author timeout-ms evidence-store]}]
  (cond
    (nil? agent-id)
    {:whistle/ok false :whistle/error "whistle requires :agent-id"
     :whistle/at (now-str)}

    (nil? prompt)
    {:whistle/ok false :whistle/error "whistle requires :prompt"
     :whistle/agent-id (str agent-id) :whistle/at (now-str)}

    :else
    (let [aid-val (if (map? agent-id) (:id/value agent-id) (str agent-id))
          timeout (or timeout-ms 60000)
          result (registry/invoke-agent! aid-val prompt timeout)
          status (cond
                   (:ok result) :completed
                   (let [e (:error result)
                         msg (if (map? e) (:error/message e) (str e))]
                     (and (string? msg) (str/includes? msg "timeout")))
                   :timeout
                   :else :error)
          response-str (if (:ok result)
                         (str (:result result))
                         (let [e (:error result)]
                           (if (map? e) (:error/message e) (str e))))]
      ;; Emit evidence
      (when evidence-store
        (estore/append* evidence-store
                        (make-whistle-evidence
                         aid-val prompt response-str status
                         (or author "whistle-dispatcher")
                         (:session-id result))))
      ;; Return whistle-shaped result
      (if (:ok result)
        {:whistle/ok true
         :whistle/response (:result result)
         :whistle/agent-id aid-val
         :whistle/session-id (:session-id result)
         :whistle/at (now-str)}
        {:whistle/ok false
         :whistle/error response-str
         :whistle/agent-id aid-val
         :whistle/at (now-str)}))))
