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
      (let [response (http-json "POST" (str agency-base "/api/alpha/invoke/activate")
                                {:agent-id (:agent-id request)
                                 :prompt (prompt request)
                                 :surface "emacs-repl"
                                 :caller "countdown-control"
                                 :job-id (:job-id ticket)})]
        {:ok (and (= 202 (:http/status response)) (:ok response)
                  (:accepted response))}))
    :job-fn
    (fn [job-id]
      (job->terminal
       (http-json "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id))))
    :persist-fn #(atomic-persist! state-path %)}))
