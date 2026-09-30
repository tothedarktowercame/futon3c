(ns futon3c.agency.agent-context
  "Write the job identity inherited by commands an agent launches.

   The file is replaced atomically at the start of each Agency job.  It is a
   current-job handoff, not history; the invoke ledger remains the history."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files Path StandardCopyOption]
           [java.time Instant]))

(def ^:dynamic *context-root*
  (str (System/getProperty "user.home") "/.futon/agent-context"))

(def ^:dynamic *before-rename-hook* nil)

(defn job-context
  "Build a context document. LOOKUP-AGENT returns a registry record or nil."
  [{:keys [agent-id session-id job-id caller]} lookup-agent]
  (let [caller-record (when (and caller lookup-agent) (lookup-agent (str caller)))
        caller-session (:agent/session-id caller-record)]
    (cond-> {"agent" (str agent-id)
             "session" (when session-id (str session-id))
             "job" (str job-id)
             "caller" (str (or caller "http-caller"))
             "at" (str (Instant/now))}
      caller-session (assoc "caller_session" (str caller-session)))))

(defn write-context!
  "Atomically replace AGENT's current context JSON. Returns the target path."
  [context]
  (let [agent (get context "agent")]
    (when-not (and (string? agent) (re-matches #"[A-Za-z0-9._-]+" agent))
      (throw (ex-info "agent id is not safe for a context filename" {:agent agent})))
    (let [dir (Path/of (str *context-root*) (make-array String 0))
          _ (Files/createDirectories dir (make-array java.nio.file.attribute.FileAttribute 0))
          target (.resolve dir (str agent ".json"))
          tmp (Files/createTempFile dir (str "." agent "-") ".tmp"
                                    (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (Files/writeString tmp (str (json/generate-string context) "\n")
                           StandardCharsets/UTF_8
                           (into-array java.nio.file.OpenOption []))
        (when *before-rename-hook* (*before-rename-hook* tmp target))
        (Files/move tmp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (str target)
        (finally
          (Files/deleteIfExists tmp))))))

(defn write-context-safely!
  "Write CONTEXT without allowing telemetry failure to fail an agent job."
  [context]
  (try
    (write-context! context)
    (catch Throwable t
      (binding [*out* *err*]
        (println "[agent-context] write failed for" (get context "agent") ":" (.getMessage t)))
      nil)))

(defn write-job-context-safely!
  "Build and write one job context. Registry lookup and persistence failures
   are both telemetry failures and never fail execution of the job."
  [job lookup-agent]
  (try
    (let [job (if (contains? job :session-id)
                job
                (assoc job :session-id
                       (some-> (lookup-agent (str (:agent-id job))) :agent/session-id)))]
      (write-context! (job-context job lookup-agent)))
    (catch Throwable t
      (binding [*out* *err*]
        (println "[agent-context] write failed for" (:agent-id job) ":" (.getMessage t)))
      nil)))

(defn agent-env
  "Environment additions for a process owned by an agent."
  [agent-id session-id]
  (cond-> {"FUTON_AGENT_ID" (str agent-id)}
    (some-> session-id str str/trim not-empty)
    (assoc "FUTON_AGENT_SESSION_ID" (str session-id))))
