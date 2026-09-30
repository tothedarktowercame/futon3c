(ns futon3c.agency.agent-context-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.agent-context :as context]
            [futon3c.agency.agent-pouch :as pouch]
            [futon3c.agents.codex-cli :as codex]
            [futon3c.peripheral.real-backend :as backend]
            [futon3c.peripheral.tools :as tools]))

(defn- rm-tree! [f]
  (when (.exists (io/file f))
    (doseq [x (reverse (file-seq (io/file f)))] (.delete x))))

(deftest context-is-an-atomic-current-job-file
  (let [dir (str (System/getProperty "java.io.tmpdir") "/agent-context-" (random-uuid))
        old {"agent" "codex-4" "session" "s1" "job" "j1"
             "caller" "joe" "at" "2026-09-30T00:00:00Z"}
        new {"agent" "codex-4" "session" "s1" "job" "j2"
             "caller" "claude-17" "caller_session" "cs17"
             "at" "2026-09-30T00:01:00Z"}]
    (try
      (binding [context/*context-root* dir] (context/write-context! old))
      (binding [context/*context-root* dir
                context/*before-rename-hook*
                (fn [_ target]
                  (is (= old (json/parse-string (slurp (str target))))))]
        (context/write-context! new))
      (is (= new (json/parse-string (slurp (io/file dir "codex-4.json")))))
      (finally (rm-tree! dir)))))

(deftest job-context-resolves-only-a-registered-caller-session
  (let [lookup #(when (= "claude-17" %) {:agent/session-id "caller-session"})]
    (is (= "caller-session"
           (get (context/job-context {:agent-id "codex-4" :session-id "s"
                                      :job-id "j" :caller "claude-17"} lookup)
                "caller_session")))
    (is (not (contains? (context/job-context {:agent-id "codex-4" :session-id "s"
                                              :job-id "j" :caller "http-caller"} lookup)
                        "caller_session")))))

(deftest process-environment-shape
  (is (= {"FUTON_AGENT_ID" "codex-4" "FUTON_AGENT_SESSION_ID" "s4"}
         (context/agent-env "codex-4" "s4")))
  (is (= {"FUTON_AGENT_ID" "kimi-1"} (context/agent-env "kimi-1" nil))))

(defn- executable! [body]
  (let [f (java.io.File/createTempFile "agent-env-" ".sh")]
    (spit f body)
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(deftest codex-process-receives-agent-environment
  (let [script (executable!
                (str "#!/bin/sh\n"
                     "cat >/dev/null\n"
                     "printf '%s\\n' '{\"type\":\"thread.started\",\"thread_id\":\"s\"}'\n"
                     "printf '{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"%s/%s\"}}\\n' \"$FUTON_AGENT_ID\" \"$FUTON_AGENT_SESSION_ID\"\n"))
        r (codex/run-codex-stream! [script] "prompt"
                                   {:timeout-ms 2000 :agent-id "codex-4" :session-id "cs4"})]
    (is (= "codex-4/cs4" (:text r)))))

(deftest pouch-process-receives-agent-environment
  (let [script (executable!
                (str "#!/bin/sh\n"
                     "while IFS= read -r line; do\n"
                     " printf '%s\\n' '{\"type\":\"system\",\"session_id\":\"ps\"}'\n"
                     " printf '{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"%s/%s\"}]}}\\n' \"$FUTON_AGENT_ID\" \"$FUTON_AGENT_SESSION_ID\"\n"
                     " printf '%s\\n' '{\"type\":\"result\",\"session_id\":\"ps\",\"is_error\":false}'\n"
                     "done\n"))]
    (pouch/clear!)
    (try
      (is (= "claude-env/pouch-session"
             (:result (pouch/feed-turn! "claude-env" "prompt"
                                        {:claude-bin script :session-id "pouch-session"
                                         :timeout-ms 2000}))))
      (finally (pouch/clear!)))))

(deftest real-backend-passes-agent-environment-to-bash
  (let [b (backend/make-real-backend {:cwd "/tmp" :agent-id "kimi-1"
                                      :session-id-fn (constantly "ks1")})
        r (tools/execute-tool b :bash
                              ["printf '%s/%s' \"$FUTON_AGENT_ID\" \"$FUTON_AGENT_SESSION_ID\""])]
    (is (= "kimi-1/ks1" (get-in r [:result :out])))))
