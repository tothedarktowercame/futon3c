(ns futon3c.agency.roles-test
  "M-futon-seams instance 4: routing follows the provider an agent declared,
   not its id. The redirect test (the mission's IDENTIFY exit): change the
   binding and confirm behaviour follows it, with the id held fixed."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.roles :as roles]
            [futon3c.transport.http]))

(use-fixtures :each (fn [t] (reg/reset-registry!) (t) (reg/reset-registry!)))

(defn- register! [id type & [metadata]]
  (reg/register-agent!
   (cond-> {:agent-id {:id/value id :id/type :continuity}
            :type type
            :invoke-fn (fn [_ _] {:result "ok"})
            :capabilities [:coordination/execute]}
     metadata (assoc :metadata metadata))))

(def requires-execution? #'futon3c.transport.http/agent-requires-execution?)
(def claude-stale-session? #'futon3c.transport.http/claude-missing-conversation-result?)
(def stale {:ok false :error "No conversation found with session ID: abc"})

(deftest provider-comes-from-registration
  (register! "oxf-codex-3" :codex)
  (register! "codex-99" :claude)
  (is (= :codex (roles/provider "oxf-codex-3")))
  (is (= :claude (roles/provider "codex-99")))
  (testing "an unregistered agent has no provider, not one guessed from its id"
    (is (nil? (roles/provider "codex-1")))))

(deftest execution-enforcement-follows-declared-provider
  (register! "oxf-codex-3" :codex)
  (register! "wm-author" :codex)
  (register! "codex-99" :claude)
  (testing "Codex agents whose ids do not start with \"codex\" are enforced (the prefix rule skipped them)"
    (is (true? (requires-execution? "oxf-codex-3")))
    (is (true? (requires-execution? "wm-author"))))
  (testing "an id starting with \"codex\" is not enforced when the declared provider is Claude"
    (is (false? (requires-execution? "codex-99"))))
  (testing "explicit metadata still wins in both directions"
    (register! "claude-7" :claude {:require-execution? true})
    (register! "codex-8" :codex {:require-execution? false})
    (is (true? (requires-execution? "claude-7")))
    (is (false? (requires-execution? "codex-8")))))

(deftest stale-session-recovery-follows-declared-provider
  (register! "oxf-claude-2" :claude)
  (register! "claude-looking-codex" :codex)
  (is (true? (claude-stale-session? "oxf-claude-2" stale)))
  (is (false? (claude-stale-session? "claude-looking-codex" stale))))

(deftest redirect-the-binding-and-behaviour-follows
  (testing "same id, provider re-declared: both routing decisions change with it"
    (register! "seat-1" :claude)
    (is (false? (requires-execution? "seat-1")))
    (is (true? (claude-stale-session? "seat-1" stale)))
    (reg/unregister-agent! {:id/value "seat-1" :id/type :continuity})
    (register! "seat-1" :codex)
    (is (true? (requires-execution? "seat-1")))
    (is (false? (claude-stale-session? "seat-1" stale)))))
