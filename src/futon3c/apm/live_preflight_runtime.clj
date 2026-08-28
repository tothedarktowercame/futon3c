(ns futon3c.apm.live-preflight-runtime
  "Live adapters for the durable f19 preflight state machine."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.live-preflight :as preflight])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files OpenOption Path StandardCopyOption
            StandardOpenOption]
           [java.nio.file.attribute FileAttribute]))

(defn lint-edn-text [text]
  (let [temporary (Files/createTempFile "apm-role-report-" ".edn"
                                        (make-array FileAttribute 0))]
    (try
      (Files/writeString temporary text StandardCharsets/UTF_8
                         (into-array OpenOption [StandardOpenOption/WRITE
                                                 StandardOpenOption/TRUNCATE_EXISTING]))
      (let [{:keys [exit out err]}
            (shell/sh "clj-kondo" "--lint" (str temporary))
            output (str/trim (str out err))]
        (if (zero? exit)
          {:ok true}
          {:ok false :error/code :report-edn-lint-failed
           :error/message output :linter/exit exit}))
      (catch java.io.IOException t
        {:ok false :error/code :report-edn-linter-unavailable
         :error/message (.getMessage t)})
      (finally
        (Files/deleteIfExists temporary)))))

(defn parse-report-diagnostic [result]
  (try
    (let [text (str/trim (or result ""))
          fences (map second
                      (re-seq #"(?s)```(?:clojure|edn)?\s*(.*?)\s*```" text))
          text (cond
                 (= 1 (count fences)) (first fences)
                 (seq fences) nil
                 :else text)
          lint-result (when text (lint-edn-text text))]
      (if-not (:ok lint-result)
        lint-result
        (let [report (edn/read-string text)]
          (if (map? report)
            {:ok true :report report}
            {:ok false :error/code :report-not-map}))))
    (catch Throwable t
      {:ok false :error/code :report-edn-invalid
       :error/message (.getMessage t)})))

(defn parse-report [result]
  (:report (parse-report-diagnostic result)))

(defn job->terminal [response]
  (let [job (:job response)
        parsed (parse-report-diagnostic (:result job))]
    {:job-id (:job-id job) :agent-id (:agent-id job)
     :session-id (:session-id job)
     :invocation/model (:invocation/model job)
     :state (some-> (:state job) keyword)
     :trace/delivery-observation (:trace/delivery-observation job)
     :report (:report parsed)
     :report/error (when-not (:ok parsed) (dissoc parsed :ok))}))

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

(defn normalize-preflight-state
  "Rehydrate the canonical preflight machine from the short-lived generic job
   driver representation. The immutable request and ticket are preserved; the
   terminal result is re-observed and certified by the preflight contract."
  [state]
  (if (= :live-job-dispatched (:state/type state))
    {:state/type :preflight-dispatched
     :request (:request state)
     :ticket (:ticket state)}
    state))

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
   {:contract contract :inputs inputs
    :state (normalize-preflight-state (read-state state-path))
    :dispatch-fn
    (fn [request]
      ((requiring-resolve 'futon3c.apm.job-port/announce!)
       agency-base {:agent-id (:agent-id request) :prompt (prompt request)}))
    :activate-fn
    (fn [request ticket]
      ((requiring-resolve 'futon3c.apm.job-port/activate!)
       agency-base {:agent-id (:agent-id request) :prompt (prompt request)
                    :job-id (:job-id ticket)
                    :timeout-ms (get-in request [:timeouts :turn-timeout-ms])}))
    :job-fn
    (fn [job-id]
      ((requiring-resolve 'futon3c.apm.job-port/observe) agency-base job-id))
    :persist-fn #(atomic-persist! state-path %)}))
