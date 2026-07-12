(ns futon3c.agency.roster-store-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.roster-store :as roster]
            [futon3c.transport.http :as http]
            [futon3c.social.persist :as persist]
            [futon3c.social.test-fixtures :as fix]
            [cheshire.core :as json]))

(defn- temp-path []
  (.getAbsolutePath (java.io.File/createTempFile "futon3c-agent-roster-" ".edn")))

(defn- with-temp-roster [f]
  (let [path (temp-path)
        old-path (System/getProperty "FUTON3C_AGENT_ROSTER_FILE")
        old-restore (System/getProperty "FUTON3C_AGENT_RESTORE")
        old-allow-drop (System/getProperty "FUTON3C_ROSTER_ALLOW_DROP")]
    (try
      (System/setProperty "FUTON3C_AGENT_ROSTER_FILE" path)
      (System/clearProperty "FUTON3C_AGENT_RESTORE")
      (System/clearProperty "FUTON3C_ROSTER_ALLOW_DROP")
      (reg/reset-registry!)
      (persist/reset-sessions!)
      ;; Persistence is now installed explicitly (bootstrap does this AFTER
      ;; restore-on-boot!, not at registry ns-load). Mirror that here so the
      ;; continuous-persist tests exercise a watched registry; remove it in
      ;; teardown so the watch never leaks into other test namespaces.
      (roster/install-registry-watch! reg/!registry)
      (f path)
      (finally
        (remove-watch reg/!registry ::roster/persist-roster)
        (reg/reset-registry!)
        (if old-path
          (System/setProperty "FUTON3C_AGENT_ROSTER_FILE" old-path)
          (System/clearProperty "FUTON3C_AGENT_ROSTER_FILE"))
        (if old-restore
          (System/setProperty "FUTON3C_AGENT_RESTORE" old-restore)
          (System/clearProperty "FUTON3C_AGENT_RESTORE"))
        (if old-allow-drop
          (System/setProperty "FUTON3C_ROSTER_ALLOW_DROP" old-allow-drop)
          (System/clearProperty "FUTON3C_ROSTER_ALLOW_DROP"))
        (when (.exists (io/file path))
          (.delete (io/file path)))))))

(use-fixtures :each
  (fn [f]
    (with-temp-roster (fn [_] (f)))))

(defn- make-handler []
  (http/make-handler {:registry (fix/mock-registry)
                      :patterns (fix/mock-patterns)}))

(defn- post [handler uri body]
  (handler {:request-method :post :uri uri :body body}))

(defn- parse-body [response]
  (json/parse-string (:body response) true))

(defn- registry-with-agents
  [ids]
  (into {}
        (map (fn [id]
               [id {:agent/id {:id/value id :id/type :continuity}
                    :agent/type :codex}]))
        ids))

(defn- persisted-agent-ids
  []
  (->> (:agents (edn/read-string (slurp (roster/roster-store-path))))
       (map :agent-id)
       vec))

(deftest persist-load-round-trip-restorable-fields
  (let [path (roster/roster-store-path)]
    (reg/register-agent!
     {:agent-id {:id/value "codex-save" :id/type :continuity}
      :type :codex
      :invoke-fn (fn [_ _] {:result "ok"})
      :capabilities [:edit :test]
      :session-id "sess-save"
      :metadata {:session-file "/tmp/futon-codex-session-id-codex-save"
                 :cwd "/home/joe/code/futon3c"
                 :emacs-socket "server"
                 :model "claude-test"
                 :campaign-id "C-test"
                 :agency/contracts {:bell-on-complete? true}}})
    (let [loaded (edn/read-string (slurp path))
          [payload] (:agents loaded)]
      (is (= 1 (:version loaded)))
      (is (= "codex-save" (:agent-id payload)))
      (is (= :codex (:type payload)))
      (is (= "sess-save" (:session-id payload)))
      (is (= "/tmp/futon-codex-session-id-codex-save" (:session-file payload)))
      (is (= "/home/joe/code/futon3c" (:cwd payload)))
      (is (= "server" (:emacs-socket payload)))
      (is (= "claude-test" (:model payload)))
      (is (= {:bell-on-complete? true} (:agency/contracts payload)))
      (is (not (contains? payload :agent/invoke-fn)))
      (is (not (contains? payload :invoke-fn))))))

(deftest persistence-updates-on-deregister
  (let [path (roster/roster-store-path)]
    (reg/register-agent!
     {:agent-id {:id/value "claude-gone" :id/type :continuity}
      :type :claude
      :invoke-fn (fn [_ _] {:result "ok"})
      :capabilities []})
    (is (= 1 (count (:agents (edn/read-string (slurp path))))))
    (reg/unregister-agent! "claude-gone")
    (is (= [] (:agents (edn/read-string (slurp path)))))))

(deftest counter-ratchet-refuses-unexpected-bulk-roster-drop
  (roster/persist-registry! (registry-with-agents ["a" "b" "c" "d"]))
  (let [before (slurp (roster/roster-store-path))]
    (roster/persist-registry! (registry-with-agents ["a" "b"]))
    (is (= before (slurp (roster/roster-store-path))))
    (is (= ["a" "b" "c" "d"] (persisted-agent-ids)))))

(deftest counter-ratchet-allows-single-agent-deregistration
  (roster/persist-registry! (registry-with-agents ["a" "b" "c"]))
  (roster/persist-registry! (registry-with-agents ["a" "b"]))
  (is (= ["a" "b"] (persisted-agent-ids))))

(deftest counter-ratchet-allow-drop-escape-permits-bulk-drop
  (roster/persist-registry! (registry-with-agents ["a" "b" "c" "d"]))
  (System/setProperty "FUTON3C_ROSTER_ALLOW_DROP" "true")
  (roster/persist-registry! (registry-with-agents ["a"]))
  (is (= ["a"] (persisted-agent-ids))))

(deftest restore-on-boot-flag-off-does-not-replay
  (spit (roster/roster-store-path)
        (pr-str {:version 1
                 :agents [{:agent-id "codex-off" :type :codex}]}))
  (System/setProperty "FUTON3C_AGENT_RESTORE" "false")
  (let [called? (atom false)
        report (roster/restore-on-boot! (fn [_]
                                          (reset! called? true)
                                          {:ok true}))]
    (is (= {:enabled? false :attempted 0 :restored 0 :results []} report))
    (is (false? @called?))))

(deftest restore-on-boot-replays-seeded-roster-idempotently
  (spit (roster/roster-store-path)
        (pr-str {:version 1
                 :agents [{:agent-id "codex-replay" :type :codex :session-id "sess-1"}]}))
  (System/setProperty "FUTON3C_AGENT_RESTORE" "true")
  (let [registered (atom {})
        restore-fn (fn [{:keys [agent-id] :as payload}]
                     (swap! registered assoc agent-id payload)
                     {:ok true :agent-id agent-id})
        r1 (roster/restore-on-boot! restore-fn)
        r2 (roster/restore-on-boot! restore-fn)]
    (is (= 1 (:attempted r1)))
    (is (= 1 (:restored r1)))
    (is (= 1 (:restored r2)))
    (is (= ["codex-replay"] (sort (keys @registered))))
    (is (true? (get-in @registered ["codex-replay" :restored-detached?])))))

(deftest restored-agent-is-detached-not-falsely-idle
  (let [handler (make-handler)
        session-file (java.io.File/createTempFile "futon3c-roster-restore-" ".sid")
        body (json/generate-string {"agent-id" "codex-restored"
                                    "type" "codex"
                                    "session-id" "sess-restored"
                                    "session-file" (.getPath session-file)
                                    "cwd" "/home/joe/code/futon3c"
                                    "restored-detached?" true})]
    (try
      (let [response (post handler "/api/alpha/agents/restore" body)
            parsed (parse-body response)
            agent (reg/get-agent "codex-restored")
            status (get-in (reg/registry-status) [:agents "codex-restored" :status])]
        (is (= 201 (:status response)))
        (is (true? (:ok parsed)))
        (is (= :restored (:agent/status agent)))
        (is (= :restored status))
        (is (= :restored/detached (get-in agent [:agent/metadata :restore/state]))))
      (finally
        (when (.exists session-file)
          (.delete session-file))))))

(deftest proxies-are-not-persisted
  ;; AG-2 regression (M-federated-agency-hardening, 2026-07-12): restore-payload
  ;; strips :proxy?/:origin-url, so a persisted federation proxy replayed on
  ;; boot as a LOCAL phantom (the chi-claude-1/chi-codex-1 phantoms on lucy).
  ;; Proxies must be absent from the durable snapshot; the sync daemon
  ;; re-imports them from the live peer after boot.
  (let [registry {"claude-1" {:agent/id {:id/value "claude-1" :id/type :continuity}
                              :agent/type :claude
                              :agent/session-id "sess-local"}
                  "chi-claude-1" {:agent/id {:id/value "chi-claude-1" :id/type :continuity}
                                  :agent/type :claude
                                  :agent/session-id "sess-chi"
                                  :agent/metadata {:proxy? true
                                                   :remote? true
                                                   :home-site :chi
                                                   :origin-url "http://chi:7070"}}
                  "oxf-zai-1" {:agent/id {:id/value "oxf-zai-1" :id/type :continuity}
                               :agent/type :zai
                               :agent/metadata {"remote-proxy?" true}}}
        snapshot (roster/roster-snapshot registry)]
    (is (= ["claude-1"] (mapv :agent-id (:agents snapshot))))))
