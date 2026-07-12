(ns futon3c.agency.federation-registration-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.federation :as fed]
            [futon3c.agency.logic :as logic]
            [futon3c.agency.registry :as reg]
            [futon3c.transport.http :as http]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (fed/configure! {:peers [] :self-url nil :peer-sites []})
    (fed/remove-hook!)
    (f)))

(defn- handler
  []
  (http/make-handler {}))

(defn- post-json
  [handler uri body]
  (handler {:request-method :post
            :uri uri
            :body (json/generate-string body)}))

(defn- parsed-body
  [response]
  (json/parse-string (:body response) true))

(defn- logic-db-from-registry
  []
  (let [status (reg/registry-status)
        routing (into {}
                      (map (fn [[aid info]]
                             [aid (select-keys info
                                               [:invoke-route
                                                :invoke-ready?
                                                :invoke-diagnostic])])
                           (:agents status)))]
    (logic/snapshot->db {:registry @reg/!registry
                         :routing routing
                         :site :laptop
                         :local-point :laptop})))

(deftest remote-announce-registers-federation-proxy
  (testing "origin-url registrations create federation proxies, not local phantoms"
    (fed/configure! {:peers [] :self-url "http://laptop:7070" :peer-sites ["lon"]})
    (let [response (post-json (handler)
                              "/api/alpha/agents"
                              {"agent-id" "lon-claude-1"
                               "type" "claude"
                               "capabilities" ["coordination/execute"]
                               "origin-url" "http://lon:7070"
                               "proxy" true})
          parsed (parsed-body response)
          agent (reg/get-agent "lon-claude-1")]
      (is (= 201 (:status response)))
      (is (true? (:ok parsed)))
      (is (= "registered" (:action parsed)))
      (is (= true (get-in agent [:agent/metadata :proxy?])))
      (is (= true (get-in agent [:agent/metadata :remote?])))
      (is (= :lon (get-in agent [:agent/metadata :home-site])))
      (is (= "http://lon:7070" (get-in agent [:agent/metadata :origin-url])))
      (is (fn? (:agent/invoke-fn agent)))
      (is (empty? (logic/find-phantoms (logic-db-from-registry)))))))

(deftest remote-announce-replaces-existing-local-phantom
  (testing "an origin-url announce heals an existing site-qualified local phantom"
    (fed/configure! {:peers [] :self-url "http://laptop:7070" :peer-sites ["lon"]})
    (reg/register-agent!
     {:agent-id {:id/value "lon-claude-1" :id/type :continuity}
      :type :claude
      :invoke-fn (fn [_ _] {:result "wrong-local"})
      :capabilities [:coordination/execute]
      :metadata {:home-site :lon :auto-registered? true}})
    (is (seq (logic/find-phantoms (logic-db-from-registry)))
        "the seeded pre-fix state is a CP-A AG-2 violation")
    (let [response (post-json (handler)
                              "/api/alpha/agents"
                              {"agent-id" "lon-claude-1"
                               "type" "claude"
                               "origin-url" "http://lon:7070"
                               "proxy" true})
          parsed (parsed-body response)
          agent (reg/get-agent "lon-claude-1")]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= "updated" (:action parsed)))
      (is (= true (get-in agent [:agent/metadata :proxy?])))
      (is (= "http://lon:7070" (get-in agent [:agent/metadata :origin-url])))
      (is (empty? (logic/find-phantoms (logic-db-from-registry)))))))

(deftest local-registration-for-remote-home-is-refused
  (testing "server-side restore path refuses to mint a remote-homed local phantom"
    (fed/configure! {:peers [] :self-url "http://laptop:7070" :peer-sites ["lon" "chi"]})
    (let [restore-response (post-json (handler)
                                      "/api/alpha/agents/restore"
                                      {"agent-id" "lon-claude-1"
                                       "type" "claude"
                                       "session-id" "sess-lon"
                                       "cwd" "/home/joe/code/futon3c"})
          register-response (post-json (handler)
                                       "/api/alpha/agents"
                                       {"agent-id" "chi-claude-1"
                                        "type" "claude"})
          restore-parsed (parsed-body restore-response)
          register-parsed (parsed-body register-response)]
      (is (= 409 (:status restore-response)))
      (is (= "remote-home-local-registration-refused" (:err restore-parsed)))
      (is (= 409 (:status register-response)))
      (is (= "remote-home-local-registration-refused" (:err register-parsed)))
      (is (nil? (reg/get-agent "lon-claude-1")))
      (is (nil? (reg/get-agent "chi-claude-1")))
      (is (empty? (logic/find-phantoms (logic-db-from-registry)))))))

(deftest restore-existing-proxy-is-refused-not-localized
  (testing "restore cannot strip proxy metadata from an existing remote proxy"
    (fed/configure! {:peers [] :self-url "http://laptop:7070" :peer-sites ["lon"]})
    (post-json (handler)
               "/api/alpha/agents"
               {"agent-id" "lon-claude-1"
                "type" "claude"
                "origin-url" "http://lon:7070"
                "proxy" true})
    (let [response (post-json (handler)
                              "/api/alpha/agents/restore"
                              {"agent-id" "lon-claude-1"
                               "type" "claude"
                               "session-id" "sess-local-would-be-wrong"
                               "cwd" "/home/joe/code/futon3c"})
          parsed (parsed-body response)
          agent (reg/get-agent "lon-claude-1")]
      (is (= 409 (:status response)))
      (is (= "remote-home-local-registration-refused" (:err parsed)))
      (is (= true (get-in agent [:agent/metadata :proxy?])))
      (is (= "http://lon:7070" (get-in agent [:agent/metadata :origin-url])))
      (is (empty? (logic/find-phantoms (logic-db-from-registry)))))))

(deftest local-ws-bridge-registration-declares-home-site
  ;; codex-3 case (2026-07-12): a laptop-homed agent registering a ws-bridge
  ;; presence on the hub arrived bare (no site prefix, no metadata), so the
  ;; site-grouped roster filed it under the hub's own site instead of oxf.
  ;; A local (non-proxy) registration may declare its home federation point.
  (testing "home-site in the registration body lands in agent metadata"
    (let [h (handler)
          response (post-json h "/api/alpha/agents"
                              {"agent-id" "codex-3"
                               "type" "codex"
                               "ws-bridge" true
                               "home-site" "oxf"})
          agent (reg/get-agent "codex-3")]
      (is (= 201 (:status response)))
      (is (= :oxf (get-in agent [:agent/metadata :home-site])))
      (is (= true (get-in agent [:agent/metadata :ws-bridge?])))))
  (testing "registration without home-site stays untagged (renderer falls back)"
    (let [h (handler)
          _ (post-json h "/api/alpha/agents"
                       {"agent-id" "codex-9" "type" "codex" "ws-bridge" true})]
      (is (nil? (get-in (reg/get-agent "codex-9") [:agent/metadata :home-site]))))))
