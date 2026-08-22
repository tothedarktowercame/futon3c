(ns futon3c.apm.live-preflight-runtime
  "Live adapters for the durable f19 preflight state machine."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.apm.live-preflight :as preflight])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files OpenOption Path StandardCopyOption
            StandardOpenOption]
           [java.nio.file.attribute FileAttribute]))

(defn parse-report [result]
  (try
    (let [text (str/trim (or result ""))
          fences (map second
                      (re-seq #"(?s)```(?:clojure|edn)?\s*(.*?)\s*```" text))
          text (cond
                 (= 1 (count fences)) (first fences)
                 (seq fences) nil
                 :else text)
          report (edn/read-string text)]
      (when (map? report) report))
    (catch Throwable _ nil)))

(defn job->terminal [response]
  (let [job (:job response)]
    {:job-id (:job-id job) :agent-id (:agent-id job)
     :session-id (:session-id job)
     :state (some-> (:state job) keyword)
     :report (parse-report (:result job))}))

(defn prompt [request]
  (str "F19 PREFLIGHT — follow the pinned Proctor role card at "
       (:role-card-path request) " (blob " (:role-card-blob request) ").\n"
       (:instructions request) "\n"
       "Authority (do not substitute conversational state):\n"
       (pr-str (select-keys request
                            [:dispatch/id :frame-id :problem-id
                             :problem-repository :problem-revision
                             :problem-path :problem-blob :timeouts]))
       "\nReturn exactly one EDN map with keys "
       (pr-str preflight/required-report-fields)
       ". :lean must contain :exit, :warnings, :sorry-warnings, :errors, and :output."))

(defn atomic-persist! [path value]
  (let [target (.toAbsolutePath ^Path path)
        directory (.getParent target)]
    (Files/createDirectories directory (make-array FileAttribute 0))
    (let [temporary (Files/createTempFile directory ".preflight-" ".edn"
                                          (make-array FileAttribute 0))]
      (Files/writeString temporary (str (pr-str value) "\n") StandardCharsets/UTF_8
                         (into-array OpenOption [StandardOpenOption/WRITE
                                                 StandardOpenOption/TRUNCATE_EXISTING
                                                 StandardOpenOption/SYNC]))
      (Files/move temporary target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                           StandardCopyOption/REPLACE_EXISTING]))
      {:ok true :path (str target)})))

(defn read-state [path]
  (when (Files/isRegularFile ^Path path (make-array java.nio.file.LinkOption 0))
    (edn/read-string (slurp (str path)))))

(defn http-json
  ([method url] (http-json method url nil))
  ([method url payload]
   (let [connection ^java.net.HttpURLConnection
         (.openConnection (java.net.URL. url))]
     (.setRequestMethod connection method)
     (.setConnectTimeout connection 2000)
     (.setReadTimeout connection 30000)
     (when payload
       (.setRequestProperty connection "Content-Type" "application/json")
       (.setDoOutput connection true)
       (with-open [writer (java.io.OutputStreamWriter. (.getOutputStream connection))]
         (.write writer (json/generate-string payload))))
     (let [status (.getResponseCode connection)
           stream (if (< status 400) (.getInputStream connection)
                      (.getErrorStream connection))]
       (assoc (json/parse-string (slurp stream) true) :http/status status)))))

(def ^:private activation-terminal-states
  #{"running" "done" "failed" "timeout" "cancelled"})

(defn- job-state [agency-base job-id]
  (try (some-> (http-json "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id))
               :job :state str)
       (catch Throwable _ nil)))

(defn activate-job!
  "Second half of a frame dispatch. POST /api/alpha/invoke/announce only RESERVES
   a ledger row (state queued) — nothing drains it; see
   futon3c/holes/excursions/E-drainer-stall-announced-jobs.md. The row is run by a
   follow-up POST /api/alpha/invoke carrying the same job-id: create-invoke-job!
   reuses a non-terminal requested id, and the turn runs on the agent's drainer.
   That is the only activation the serving master implements:
   /api/alpha/invoke/activate exists only on feature/lane-effects, and
   /api/alpha/bell with an existing job-id is a no-op under FUTON3C_TYPED_BELLS
   (it answers 202 reused? and never enqueues).
   /invoke blocks for the whole turn, which a driver cannot afford, so the POST is
   fired on a daemon thread (the server runs the turn whether or not the socket
   stays open) and activation is CONFIRMED by polling the job until it leaves
   `queued`. Idempotent: a job already running/terminal is not re-posted, since a
   second /invoke with a running job-id would start a second turn under it."
  [agency-base {:keys [agent-id prompt surface caller mode job-id timeout-ms
                       confirm-attempts confirm-interval-ms]
                :or {confirm-attempts 30 confirm-interval-ms 500}}]
  (let [before (job-state agency-base job-id)]
    (if (contains? activation-terminal-states before)
      {:ok true :job-id job-id :state before :already-active? true}
      (let [payload (cond-> {:agent-id agent-id :prompt prompt
                             :surface (or surface "emacs-repl")
                             :caller (or caller "countdown-control")
                             :job-id job-id}
                      mode (assoc :mode mode)
                      timeout-ms (assoc :timeout-ms timeout-ms))
            worker (doto (Thread.
                          ^Runnable
                          (fn []
                            (try (http-json "POST" (str agency-base "/api/alpha/invoke") payload)
                                 (catch Throwable _ nil)))
                          (str "activate-" job-id))
                     (.setDaemon true)
                     (.start))]
        (loop [n 0]
          (let [state (job-state agency-base job-id)]
            (cond
              (contains? activation-terminal-states state)
              {:ok true :job-id job-id :state state}

              (< n confirm-attempts)
              (do (Thread/sleep (long confirm-interval-ms))
                  (recur (inc n)))

              :else
              {:ok false :job-id job-id :state state
               :error/code :live-job-activation-not-observed
               :posting? (.isAlive worker)})))))))

(defn run-live!
  [{:keys [contract inputs state-path agency-base]
    :or {agency-base "http://localhost:7070"}}]
  (preflight/drive!
   {:contract contract :inputs inputs :state (read-state state-path)
    :dispatch-fn
    (fn [request]
      (let [response (http-json "POST" (str agency-base "/api/alpha/invoke/announce")
                                {:agent-id (:agent-id request)
                                 :prompt (prompt request)
                                 :surface "emacs-repl"
                                 :caller "countdown-control"})]
        {:ok (and (= 202 (:http/status response)) (:ok response))
         :job-id (:job-id response)}))
    :activate-fn
    (fn [request ticket]
      (activate-job! agency-base
                     {:agent-id (:agent-id request) :prompt (prompt request)
                      :job-id (:job-id ticket)
                      :timeout-ms (get-in request [:timeouts :turn-timeout-ms])}))
    :job-fn
    (fn [job-id]
      (job->terminal
       (http-json "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id))))
    :persist-fn #(atomic-persist! state-path %)}))
