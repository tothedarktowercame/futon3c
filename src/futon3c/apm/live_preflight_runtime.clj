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

(defn- report-shaped?
  "A candidate only counts as a report if at least one key is a keyword:
  real reports use keyword fields throughout, while Lean/markdown debris
  that happens to parse ({M f N g} from prose, for example) has symbol or
  string keys and must not be mistaken for evidence."
  [value]
  (and (map? value) (seq value) (some keyword? (keys value))))

(defn- try-edn-map [text]
  (try
    (let [value (edn/read-string {:default (fn [_tag value] value)} text)]
      (when (report-shaped? value) value))
    (catch Throwable _ nil)))

(defn- try-json-map [text]
  (try
    (let [value (json/parse-string text keyword)]
      (when (report-shaped? value) value))
    (catch Throwable _ nil)))

(defn- balanced-brace-spans
  "Top-level {...} spans in TEXT, ignoring braces inside double-quoted
  strings. A span that fails to parse is simply skipped by the caller, so
  the scan errs toward offering candidates rather than judging them."
  [text]
  (let [length (count text)]
    (loop [i 0 depth 0 start nil in-string? false escaped? false spans []]
      (if (>= i length)
        spans
        (let [c (.charAt ^String text i)]
          (cond
            escaped? (recur (inc i) depth start in-string? false spans)
            (and in-string? (= c \\)) (recur (inc i) depth start true true spans)
            (= c \") (recur (inc i) depth start (not in-string?) false spans)
            in-string? (recur (inc i) depth start true false spans)
            (= c \{) (recur (inc i) (inc depth)
                            (if (zero? depth) i start) false false spans)
            (= c \}) (if (and (= 1 depth) (some? start))
                       (recur (inc i) 0 nil false false
                              (conj spans (subs text start (inc i))))
                       (recur (inc i) (max 0 (dec depth)) start false false spans))
            :else (recur (inc i) depth start false false spans)))))))

(defn- strict-parse-report-diagnostic
  "The historical single-fence EDN path, kept verbatim as the source of
  failure diagnostics: when nothing normalizes, the caller sees the same
  error codes, clj-kondo output and linter exit it always saw."
  [result]
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
        (or lint-result {:ok false :error/code :report-not-map})
        (let [report (edn/read-string text)]
          (if (map? report)
            {:ok true :report report}
            {:ok false :error/code :report-not-map}))))
    (catch Throwable t
      {:ok false :error/code :report-edn-invalid
       :error/message (.getMessage t)})))

(defn parse-report-diagnostic
  "Normalize a role's terminal reply into its report map.

  Role replies come from several model families; demanding one clean EDN
  fence turned formatting into frame deaths — f172's student reported a
  finished proof inside markdown prose and the frame voided on
  :report-edn-lint-failed (Joe, 2026-09-06: accept JSON and convert, stop
  losing frames to technology). Acceptance ladder, first non-empty map
  wins, latest candidate first within each rung (a role's final answer
  ends its reply):
    1. fenced blocks — ```edn/```clojure, then ```json, then untagged;
    2. balanced top-level {...} spans in the raw text;
    3. the whole trimmed text.
  Every candidate is tried as EDN, then as JSON with keys keywordized.
  clojure.edn and cheshire evaluate nothing, and the report still faces
  the phase's own field validation downstream; what this ladder removes
  is only the demand that the envelope be pristine. The winning route is
  recorded under :report/normalization so drift in role output stays
  measurable. When nothing parses, the strict path's diagnostic (kondo
  message, linter exit, historical error codes) is returned unchanged."
  [result]
  (try
    (let [text (str/trim (or result ""))
          fence-pairs (re-seq #"(?s)```([A-Za-z0-9_-]*)[ \t]*\r?\n?(.*?)\s*```"
                              text)
          tagged (fn [tags]
                   (->> fence-pairs
                        (filter #(contains? tags (str/lower-case (nth % 1))))
                        (map #(nth % 2))
                        reverse))
          candidates
          (concat (map vector (repeat :edn-fence) (tagged #{"edn" "clojure"}))
                  (map vector (repeat :json-fence) (tagged #{"json"}))
                  (map vector (repeat :bare-fence) (tagged #{""}))
                  (map vector (repeat :embedded-map)
                       (reverse (balanced-brace-spans text)))
                  [[:whole-text text]])
          hit (some (fn [[route candidate]]
                      (or (when-let [report (try-edn-map candidate)]
                            {:report report :route route :syntax :edn})
                          (when-let [report (try-json-map candidate)]
                            {:report report :route route :syntax :json})))
                    candidates)]
      (if hit
        {:ok true :report (:report hit)
         :report/normalization {:route (:route hit) :syntax (:syntax hit)}}
        (strict-parse-report-diagnostic result)))
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
     :terminal-code (some-> (:terminal-code job) keyword)
     :terminal-message (:terminal-message job)
     :trace/delivery-observation (:trace/delivery-observation job)
     :report (:report parsed)
     :report/normalization (:report/normalization parsed)
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
