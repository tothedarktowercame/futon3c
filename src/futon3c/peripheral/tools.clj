(ns futon3c.peripheral.tools
  "Tool dispatch + backend protocol for peripheral execution.

   Tool dispatch is the enforcement layer: before any tool reaches the backend,
   it must pass through allowed? (is this tool in the peripheral's tool set?)
   and in-scope? (do the args respect scope boundaries?).

   Scope enforcement is structural, not conventional — an explore peripheral
   physically cannot call :edit because dispatch rejects it before the backend
   ever sees it."
  (:require [clojure.string :as str]
            [futon3c.peripheral.runner :as runner]))

;; =============================================================================
;; ToolBackend protocol — pluggable execution
;; =============================================================================

(defprotocol ToolBackend
  (execute-tool [this tool-id args]
    "Execute a tool with the given args.
     Returns {:ok true :result <value>} | {:ok false :error <message>}"))

;; =============================================================================
;; Tool dispatch — allowed? + in-scope? + dispatch
;; =============================================================================

(defn allowed?
  "Check if a tool is in the peripheral spec's tool set.
   Returns true if tool-id is in (:peripheral/tools spec)."
  [tool-id peripheral-spec]
  (contains? (:peripheral/tools peripheral-spec) tool-id))

(defn- path-in-scope?
  "Check if a file path is within the allowed scope paths."
  [path scope-paths]
  (some (fn [allowed]
          (str/starts-with? (str path) (str allowed)))
        scope-paths))

(defn in-scope?
  "Check if tool args respect the peripheral's scope boundaries.
   Scope types:
     :full-codebase — no restrictions
     :test-commands-only — no restrictions on test commands
     :git-push-only — no restrictions on git commands
     :session-log-only — no restrictions on session log access
     {:paths [...]} — file paths must start with one of the allowed paths"
  [_tool-id args peripheral-spec]
  (let [scope (:peripheral/scope peripheral-spec)]
    (cond
      ;; Keyword scopes are unrestricted for their allowed tools
      (keyword? scope) true

      ;; Path-scoped: check the first arg (file path) against allowed paths.
      ;; File tools always take the path as the first arg; subsequent string
      ;; args are content (e.g. old-string/new-string for :edit).
      (and (map? scope) (:paths scope))
      (let [scope-paths (:paths scope)
            path-arg (first args)]
        (if (string? path-arg)
          (boolean (path-in-scope? path-arg scope-paths))
          true))

      :else true)))

(defn dispatch-tool
  "Dispatch a tool action through the enforcement layer.
   Checks allowed? and in-scope? before delegating to backend.
   Returns {:ok true :result <value>} | SocialError."
  [tool-id args peripheral-spec backend]
  (let [pid (:peripheral/id peripheral-spec)]
    (cond
      (not (allowed? tool-id peripheral-spec))
      (runner/runner-error pid :tool-not-allowed
                           (str "Tool " tool-id " is not in this peripheral's tool set")
                           :tool tool-id
                           :allowed (:peripheral/tools peripheral-spec))

      (not (in-scope? tool-id args peripheral-spec))
      (runner/runner-error pid :out-of-scope
                           (str "Tool " tool-id " args are outside this peripheral's scope")
                           :tool tool-id
                           :args (vec args)
                           :scope (:peripheral/scope peripheral-spec))

      :else
      (try
        (execute-tool backend tool-id args)
        (catch Exception e
          (runner/runner-error pid :tool-execution-failed
                               (str "Tool " tool-id " failed: " (.getMessage e))
                               :tool tool-id
                               :exception-class (.getName (class e))))))))

;; =============================================================================
;; MockBackend — canned results + call recording for tests
;; =============================================================================

(defrecord MockBackend [results calls]
  ;; results: atom of {tool-id -> result-value} or fn
  ;; calls: atom of [{:tool tool-id :args args}]
  ToolBackend
  (execute-tool [_ tool-id args]
    (swap! calls conj {:tool tool-id :args (vec args)})
    (let [r @results
          result (get r tool-id)]
      (cond
        (fn? result)  (result tool-id args)
        (some? result) {:ok true :result result}
        :else          {:ok true :result nil}))))

(defn make-mock-backend
  "Create a mock backend with optional canned results.
   results-map: {tool-id -> result-value-or-fn}
   Returns MockBackend. Access recorded calls via (:calls backend)."
  ([]
   (make-mock-backend {}))
  ([results-map]
   (->MockBackend (atom results-map) (atom []))))

(defn recorded-calls
  "Get the list of recorded tool calls from a MockBackend."
  [mock-backend]
  @(:calls mock-backend))
