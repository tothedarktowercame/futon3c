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
        old-restore (System/getProperty "FUTON3C_AGENT_RESTORE")]
    (try
      (System/setProperty "FUTON3C_AGENT_ROSTER_FILE" path)
      (System/clearProperty "FUTON3C_AGENT_RESTORE")
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
