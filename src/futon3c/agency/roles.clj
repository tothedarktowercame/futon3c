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
   reported as nil, never guessed from the id.

   ROLES. Code that needs work done asks for a role (seat-for :reviewer), and
   resources/roles.edn says which seat plays it. The binding is data with a
   source; two bindings for one role are refused. A role says nothing about
   provider: rebinding :reviewer from a Claude seat to a Codex seat changes
   which agent is invoked and nothing else."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.agency.registry :as reg]))

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

;; ------------------------------------------------------------------ roles

(defn load-role-table
  "Read the role vocabulary and bindings from resources/roles.edn."
  []
  (if-let [r (io/resource "roles.edn")]
    (edn/read-string (slurp r))
    {:roles {} :bindings []}))

(def ^:dynamic *role-table*
  "The role table in force. Bound in tests to exercise a rebinding; in
   production it is the resource file, re-read on each resolution so an edit
   to roles.edn takes effect without a restart."
  nil)

(defn- table [] (or *role-table* (load-role-table)))

(defn seat-for
  "The seat bound to ROLE. Throws ex-info with :refusal
   :unknown-role (ROLE not in the vocabulary), :role-unbound (no binding), or
   :conflicting-bindings (more than one binding for ROLE)."
  [role]
  (let [{:keys [roles bindings]} (table)
        bs (filter #(= role (:role %)) bindings)]
    (cond
      (not (contains? roles role))
      (throw (ex-info (str "unknown role " role) {:refusal :unknown-role :role role}))
      (empty? bs)
      (throw (ex-info (str "no seat bound to role " role) {:refusal :role-unbound :role role}))
      (> (count (distinct (map :seat bs))) 1)
      (throw (ex-info (str "conflicting bindings for role " role)
                      {:refusal :conflicting-bindings :role role :bindings (vec bs)}))
      :else (:seat (first bs)))))
