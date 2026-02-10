(ns futon3c.social.authenticate-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.social.authenticate :as auth]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest happy-path-known-agent-presence
  (testing "known agent presence resolves to a valid AgentIdentity with registry enrichment"
    (let [registry (fix/mock-registry)
          ;; Presence agent-id is transport-typed at this boundary; value matches registry key.
          presence (fix/make-presence {:presence/agent-id (fix/make-agent-id "claude-1" :transport)})
          result (auth/resolve-identity presence registry)]
      (fix/assert-valid! shapes/AgentIdentity result)
      (is (= "claude-1" (get-in result [:identity/agent-id :id/value])))
      (is (= :continuity (get-in result [:identity/agent-id :id/type])) "R6: transport id resolved to continuity")
      (is (= :claude (:identity/type result)))
      (is (= [:explore :edit :test] (:identity/capabilities result))))))

(deftest agent-not-in-registry
  (testing "presence for unknown agent returns SocialError :agent-not-found"
    (let [registry (fix/mock-registry)
          presence (fix/make-presence {:presence/agent-id (fix/make-agent-id "ghost" :transport)})
          result (auth/resolve-identity presence registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-authenticate (:error/component result)))
      (is (= :agent-not-found (:error/code result))))))

(deftest invalid-presence-input
  (testing "malformed presence returns SocialError :invalid-presence"
    (let [registry (fix/mock-registry)
          result (auth/resolve-identity {:presence/agent-id (fix/make-agent-id "claude-1")} registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-authenticate (:error/component result)))
      (is (= :invalid-presence (:error/code result))))))

(deftest capabilities-come-from-registry
  (testing "capabilities in AgentIdentity are taken from registry"
    (let [registry (fix/mock-registry)
          presence (fix/make-presence {:presence/agent-id (fix/make-agent-id "codex-1" :transport)})
          result (auth/resolve-identity presence registry)]
      (fix/assert-valid! shapes/AgentIdentity result)
      (is (= [:edit] (:identity/capabilities result))))))

(deftest agent-type-comes-from-registry
  (testing ":identity/type in AgentIdentity is taken from registry"
    (let [registry (fix/mock-registry)
          presence (fix/make-presence {:presence/agent-id (fix/make-agent-id "mock-1" :transport)})
          result (auth/resolve-identity presence registry)]
      (fix/assert-valid! shapes/AgentIdentity result)
      (is (= :mock (:identity/type result))))))

(deftest output-shape-validation
  (testing "every resolve-identity return is either AgentIdentity or SocialError"
    (let [registry (fix/mock-registry)
          cases [(fix/make-presence {:presence/agent-id (fix/make-agent-id "claude-1" :transport)})
                 (fix/make-presence {:presence/agent-id (fix/make-agent-id "ghost" :transport)})
                 {:presence/agent-id (fix/make-agent-id "claude-1")}]]
      (doseq [p cases]
        (let [result (auth/resolve-identity p registry)]
          (is (or (shapes/valid? shapes/AgentIdentity result)
                  (shapes/valid? shapes/SocialError result))
              (str "Unexpected result shape: " result)))))))

