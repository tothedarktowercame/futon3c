(ns futon3c.social.presence-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.social.presence :as presence]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest happy-path-known-agent-ready
  (testing "known agent + ready handshake yields a valid PresenceRecord"
    (let [registry (fix/mock-registry)
          conn (fix/make-connection {:conn/agent-id (fix/make-agent-id "claude-1" :continuity)
                                     :conn/metadata {:ready true}})
          result (presence/verify conn registry)]
      (fix/assert-valid! shapes/PresenceRecord result)
      (is (= (:conn/id conn) (:presence/conn-id result)))
      (is (= (:conn/agent-id conn) (:presence/agent-id result)))
      (is (= (:conn/transport conn) (:presence/transport result)))
      (is (true? (:presence/ready? result))))))

(deftest unknown-agent-returns-social-error
  (testing "agent not in registry returns SocialError with :agent-not-found"
    (let [registry (fix/mock-registry)
          conn (fix/make-connection {:conn/agent-id (fix/make-agent-id "ghost" :continuity)
                                     :conn/metadata {:ready true}})
          result (presence/verify conn registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-presence (:error/component result)))
      (is (= :agent-not-found (:error/code result))))))

(deftest no-readiness-handshake-returns-social-error
  (testing "known agent without ready signal returns SocialError"
    (let [registry (fix/mock-registry)
          conn (fix/make-connection {:conn/agent-id (fix/make-agent-id "claude-1" :continuity)})
          result (presence/verify conn registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-presence (:error/component result)))
      (is (= :not-ready (:error/code result))))))

(deftest invalid-connection-input-returns-social-error
  (testing "malformed connection input returns SocialError (not nil)"
    (let [registry (fix/mock-registry)
          result (presence/verify {:conn/id "only-id"} registry)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :S-presence (:error/component result)))
      (is (= :invalid-connection (:error/code result))))))

(deftest verify-returns-are-shape-valid
  (testing "every verify result conforms to PresenceRecord or SocialError"
    (let [registry (fix/mock-registry)
          cases [(fix/make-connection {:conn/agent-id (fix/make-agent-id "claude-1" :continuity)
                                       :conn/metadata {:ready true}})
                 (fix/make-connection {:conn/agent-id (fix/make-agent-id "ghost" :continuity)
                                       :conn/metadata {:ready true}})
                 (fix/make-connection {:conn/agent-id (fix/make-agent-id "claude-1" :continuity)
                                       :conn/metadata {:ready false}})
                 {:conn/id "only-id"}]]
      (doseq [c cases]
        (let [result (presence/verify c registry)]
          (is (or (shapes/valid? shapes/PresenceRecord result)
                  (shapes/valid? shapes/SocialError result))
              (str "Unexpected result shape: " result)))))))

(deftest verify-does-not-modify-registry
  (testing "verify reads registry but does not modify it"
    (let [registry (fix/mock-registry)
          conn (fix/make-connection {:conn/agent-id (fix/make-agent-id "claude-1" :continuity)
                                     :conn/metadata {:ready true}})]
      (presence/verify conn registry)
      (is (= registry (fix/mock-registry registry)))))) ; registry is immutable; equality check guards accidental assoc

