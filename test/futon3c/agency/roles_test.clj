(ns futon3c.agency.roles-test
  "M-futon-seams instance 4: routing follows the provider an agent declared,
   not its id. The redirect test (the mission's IDENTIFY exit): change the
   binding and confirm behaviour follows it, with the id held fixed."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.roles :as roles]
            [futon3c.transport.http]
            [futon3c.evidence.store :as estore]))

(use-fixtures :each (fn [t] (reg/reset-registry!) (estore/reset-store!) (t) (reg/reset-registry!)))

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

;; ------------------------------------------------------------------ roles

(def table {:roles {:reviewer {} :implementer {}}
            :bindings [{:role :reviewer :seat "claude-1"} {:role :implementer :seat "codex-1"}]})

(deftest seat-for-resolves-a-bound-role
  (binding [roles/*role-table* table]
    (is (= "claude-1" (roles/seat-for :reviewer)))
    (is (= "codex-1" (roles/seat-for :implementer)))))

(deftest seat-for-refuses-typed
  (let [refusal (fn [t role] (try (binding [roles/*role-table* t] (roles/seat-for role) nil)
                                  (catch clojure.lang.ExceptionInfo e (:refusal (ex-data e)))))]
    (is (= :unknown-role (refusal table :mentor)))
    (is (= :role-unbound (refusal (assoc table :bindings []) :reviewer)))
    (testing "two bindings for one role are refused, not resolved by order"
      (is (= :conflicting-bindings
             (refusal (update table :bindings conj {:role :reviewer :seat "codex-2"}) :reviewer))))))

(deftest the-shipped-role-table-resolves
  (testing "resources/roles.edn binds every role it names exactly once"
    (let [{:keys [roles]} (roles/load-role-table)]
      (is (seq roles))
      (doseq [r (keys roles)] (is (string? (roles/seat-for r)))))))

(deftest redirect-a-role-to-another-provider
  (testing "rebinding :reviewer to a Codex seat changes which agent reviews, and nothing else"
    ;; Capture the seat request-review! invokes. Agents of a real provider type
    ;; go through the clock gate, which needs a durable evidence backend; the
    ;; routing decision under test is which seat is asked, so that is what is
    ;; observed.
    (register! "claude-1" :claude)
    (register! "oxf-codex-7" :codex)
    (let [invoked (atom [])]
      (with-redefs [reg/invoke-agent! (fn [seat _prompt & _]
                                        (swap! invoked conj seat)
                                        {:ok true :result "APPROVE — fine." :session-id "s"})]
        (let [review (fn [] ((requiring-resolve 'futon3c.agents.tickle-orchestrate/request-review!)
                             {:number 1 :title "t" :body "b"} {:ok true :agent-id "codex-1" :result "done"}
                             {:evidence-store (atom {:entries {} :order []}) :repo-dir "/tmp"
                              :timeout-ms 5000 :session-id "roles-redirect"}))]
          (binding [roles/*role-table* table] (review))
          (binding [roles/*role-table* (assoc table :bindings [{:role :reviewer :seat "oxf-codex-7"}])] (review))))
      (is (= ["claude-1" "oxf-codex-7"] @invoked))
      (is (= [:claude :codex] (mapv roles/provider @invoked))))))
