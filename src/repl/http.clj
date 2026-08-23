(ns repl.http
  "Drawbridge (nREPL over HTTP) helper for mission control and admin access.

   Three endpoints on the same port:
   - /repl  — nREPL over HTTP (Drawbridge protocol, for Emacs/CIDER)
   - /eval  — dev-serve Clojure eval (POST code as body, get EDN back)
   - /admin/eval — explicitly attributed dev-admin eval

   The /eval endpoint is the tool bridge for CLI agents. An agent running
   via `claude -p` can call any Clojure function in the running JVM:

     curl -s -H 'x-admin-token: TOKEN' \\
       -d '(require (quote [futon3c.peripheral.proof-backend :as pb]))
            (pb/tool-proof-mode-get state {})' \\
       http://localhost:6768/eval

   Same auth (token + IP allowlist) as the nREPL endpoint."
  (:require [cemerick.drawbridge :as db]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [ring.middleware.keyword-params :as ring-keyword]
            [ring.middleware.nested-params :as ring-nested]
            [ring.middleware.params :as ring-params]
            [ring.middleware.session :as ring-session]
            [ring.util.codec :as codec]))

(defonce server (atom nil))

(defn- normalize-allow
  [allow]
  (cond
    (nil? allow) nil
    (set? allow) allow
    (sequential? allow) (set allow)
    :else #{allow}))

(defn- wrap-token
  [handler token allow]
  (fn [request]
    (let [remote (:remote-addr request)
          allow-set (normalize-allow allow)
          allowed? (if (seq allow-set)
                     (contains? allow-set remote)
                     true)
          supplied (or (get-in request [:headers "x-admin-token"])
                       (some-> (:query-string request)
                               (codec/form-decode "UTF-8")
                               (get "token")))
          supplied (some-> supplied str/trim)
          token (some-> token str/trim)]
      (if (and allowed? (= supplied token))
        (handler request)
        {:status 403
         :headers {"content-type" "text/plain"}
         :body "forbidden"}))))

;; =============================================================================
;; /eval — plain Clojure eval for CLI agent tool bridge
;; =============================================================================

(def ^:private eval-timeout-ms
  "Maximum time for a single eval request (5 minutes)."
  300000)

(def ^:private eval-log-file "/tmp/futon3c-eval.log")
(def ^:private eval-log-max-bytes 5000000)
(def ^:private eval-log-summary-chars 500)

(defn- truncate-str [s n]
  (when (some? s)
    (let [s (str s)]
      (if (> (count s) n) (str (subs s 0 n) "…<truncated>") s))))

(defn- eval-log!
  "Append one EDN line to the eval forensic log. Rotates to .1 at max bytes.
   Written before eval (:type :request) and after (:type :response) so a body
   that wedges or kills the JVM still leaves a trail."
  [entry]
  (try
    (let [f (java.io.File. eval-log-file)]
      (when (and (.exists f) (> (.length f) eval-log-max-bytes))
        (.renameTo f (java.io.File. (str eval-log-file ".1"))))
      (spit eval-log-file
            (str (pr-str (assoc entry :at (str (java.time.Instant/now)))) \newline)
            :append true))
    (catch Throwable _ nil)))

(defn- read-body
  "Read the request body as a string."
  [request]
  (when-let [body (:body request)]
    (cond
      (string? body) body
      (instance? java.io.InputStream body) (slurp body)
      :else (str body))))

(defonce ^{:doc "Sandbox ns for /eval. `load-string` evaluates with *ns* = its
   root binding (clojure.core) when *ns* is unbound on a Jetty worker thread, so
   a bare top-level `(def foo ...)` in an eval payload would clobber
   clojure.core/foo JVM-wide. This bit us 2026-07-05: a lab eval did
   `(def int (:intensity mm))`, so clojure.core/int returned nil for ~3 days and
   crashed the WM snapshot. Binding *ns* to this scratch ns confines agent defs
   here instead of clojure.core (refer-clojure so unqualified core fns still
   resolve; persistent so cross-eval defs/aliases still carry)."}
  eval-sandbox-ns
  (let [n (create-ns 'repl.eval-sandbox)]
    (binding [*ns* n] (clojure.core/refer-clojure))
    n))

(def ^:private refresh-refusal
  (str "REFUSED: clojure.tools.namespace refresh/reload is not permitted here.\n\n"
       "It is not a scoped reload in this JVM and cannot be made into one.\n\n"
       "1. BLAST RADIUS. deps.edn pulls futon3b, futon1b, futon0, futon2 and\n"
       "   futon3a as :local/root, so their source AND test directories are on\n"
       "   the classpath as directories. set-refresh-dirs is never called, so\n"
       "   refresh scans all of them: it unloads and reloads every namespace\n"
       "   across the constellation, not the one you edited.\n\n"
       "2. IT DESTROYS defonce STATE, so scoping it would not help. refresh\n"
       "   REMOVES a namespace and recreates it; defonce only protects against\n"
       "   re-evaluation, not removal. !registry, the agent pouch and the\n"
       "   parked-on store are all defonce, and all come back empty.\n\n"
       "3. IT SPLITS THE RUNNING IMAGE. Live handlers retain the old Vars and\n"
       "   keep serving from them, while anything newly resolved reaches the\n"
       "   recreated ones. The server then disagrees with itself and every\n"
       "   reading taken through this endpoint describes a phantom.\n\n"
       "This happened on 2026-08-19 at 19:37:49Z: a refresh intended to load one\n"
       "commit aborted on futon2.aif.mission-control-graph-test, left\n"
       "futon3c.agents.zai-api unloaded, emptied the agent registry as seen from\n"
       "here while :7070 still reported 80 agents, and broke frames/mint-seats.\n"
       "A frame was opened into that image 107 seconds later.\n\n"
       "Supported live evolution redefines Vars in place: use a reviewed,\n"
       "targeted load-file through the dev-admin surface when README-drawbridge\n"
       "classifies the change as reload-safe. Route topology, captured callback,\n"
       "protocol, state-layout, and lifecycle changes require an external\n"
       "restart. Namespace removal/recreation is supported by neither profile."))

(defn- request-profile [request]
  (if (= "/admin/eval" (:uri request)) :dev-admin :dev-serve))

(defn- refresh-attempt?
  "True when CODE tries to drive clojure.tools.namespace's refresh/reload.

   Deliberately a blunt textual guard: this runs before any parsing, must catch
   an aliased require (`[... :as ctnr]` then `(ctnr/refresh)`), and a false
   positive costs a restart while a false negative costs the running image."
  [code]
  (let [c (str code)]
    (boolean
     (or (str/includes? c "tools.namespace")
         (re-find #"\((?:[\w.-]+/)?refresh(?:-all|-dirs)?\s*[\)\s]" c)
         (re-find #"\((?:[\w.-]+/)?clear\s*\)" c)))))

(defn- eval-handler
  "Ring handler for /eval. Accepts POST with Clojure code as body.
   Returns EDN-encoded result. Timeout after eval-timeout-ms.
   Every non-blank request is written to eval-log-file before and after eval.

   Refuses tools.namespace refresh outright — see refresh-refusal."
  [request]
  (if (not= :post (:request-method request))
    {:status 405
     :headers {"content-type" "text/plain"}
     :body "POST only"}
    (let [code (read-body request)
          remote (:remote-addr request)
          profile (request-profile request)]
      (if (str/blank? code)
        {:status 400
         :headers {"content-type" "text/plain"}
         :body "empty code"}
        (if (refresh-attempt? code)
          (do
            (eval-log! {:type :refused :profile profile :remote remote
                        :bytes (count code) :code code})
            {:status 403
             :headers {"content-type" "text/plain"}
             :body refresh-refusal})
        (let [start-ns (System/nanoTime)]
          (eval-log! {:type :request :profile profile :remote remote
                      :bytes (count code) :code code})
          (try
            (let [f (future
                      (try
                        {:ok true :value (binding [*ns* eval-sandbox-ns]
                                           (load-string code))}
                        (catch Throwable t
                          {:ok false
                           :error (.getMessage t)
                           :type (.getName (class t))})))
                  result (deref f eval-timeout-ms ::timeout)
                  elapsed-ms (long (/ (- (System/nanoTime) start-ns) 1000000))]
              (if (= result ::timeout)
                (do (future-cancel f)
                    (eval-log! {:type :response :profile profile :remote remote
                                :elapsed-ms elapsed-ms
                                :ok false :error "eval timeout"})
                    {:status 504
                     :headers {"content-type" "application/edn"}
                     :body (pr-str {:ok false :error "eval timeout" :timeout-ms eval-timeout-ms})})
                (do (eval-log! {:type :response :profile profile :remote remote
                                :elapsed-ms elapsed-ms
                                :ok (boolean (:ok result))
                                :summary (if (:ok result)
                                           (truncate-str (pr-str (:value result)) eval-log-summary-chars)
                                           (truncate-str (str (:type result) ": " (:error result))
                                                         eval-log-summary-chars))})
                    {:status (if (:ok result) 200 500)
                     :headers {"content-type" "application/edn"}
                     :body (pr-str result)})))
            (catch Throwable t
              (eval-log! {:type :response :profile profile :remote remote :ok false
                          :error (.getMessage t) :exception-type (.getName (class t))})
              {:status 500
               :headers {"content-type" "application/edn"}
               :body (pr-str {:ok false :error (.getMessage t) :type (.getName (class t))})}))))))))

(defn- nrepl-handler-with-refresh-guard
  "Refuse tools.namespace refresh before a Drawbridge request reaches nREPL.

   Drawbridge carries the code inside a bencoded request body. The existing
   textual guard therefore sees the dangerous form without decoding bencode.
   Reading an InputStream consumes it, so safe requests receive a replayable
   stream containing the same bytes."
  [nrepl-handler request]
  (let [body (:body request)
        [encoded replay-body]
        (if (instance? java.io.InputStream body)
          (let [bytes (.readAllBytes ^java.io.InputStream body)]
            [(String. bytes "UTF-8") (java.io.ByteArrayInputStream. bytes)])
          [(read-body request) body])]
    (if (refresh-attempt? encoded)
      {:status 403
       :headers {"content-type" "text/plain"}
       :body refresh-refusal}
      (nrepl-handler
       (if (instance? java.io.InputStream body)
         (assoc request :body replay-body)
         request)))))

;; =============================================================================
;; Server startup with route dispatch
;; =============================================================================

(defn- route-handler
  "Dispatch dev-serve eval, attributed dev-admin eval, and nREPL/Drawbridge.

   /admin/eval is an operational/audit boundary rather than a language sandbox:
   it uses the same authenticated evaluator, while marking deliberate risky
   administration separately.  The coherence guard applies to both profiles."
  [nrepl-handler]
  (fn [request]
    (case (:uri request)
      "/eval" (eval-handler request)
      "/admin/eval"
      (if (= "dev-admin" (get-in request [:headers "x-drawbridge-profile"]))
        (eval-handler request)
        {:status 403
         :headers {"content-type" "text/plain"}
         :body "dev-admin profile header required"})
      (nrepl-handler-with-refresh-guard nrepl-handler request))))

(defn start!
  "Start a Drawbridge endpoint with /eval bridge.

   Options:
   {:port 6768
    :bind \"127.0.0.1\"
    :token \"secret\"
    :allow [\"127.0.0.1\" \"::1\"]}"
  [{:keys [port token bind allow]
    :or {port 6768
         bind "127.0.0.1"
         token "change-me"
         allow ["127.0.0.1" "::1"]}}]
  (when-let [stop-fn @server]
    (stop-fn))
  (let [handler (-> (route-handler
                     (-> (db/ring-handler)
                         ring-keyword/wrap-keyword-params
                         ring-nested/wrap-nested-params
                         ring-params/wrap-params
                         ring-session/wrap-session))
                    (wrap-token token allow))
        stop-fn (http/run-server handler {:ip bind :port port})]
    (reset! server stop-fn)
    (println (format "[dev] drawbridge: http://%s:%s/repl + /eval + /admin/eval (allow: %s)"
                     bind port (pr-str allow)))
    stop-fn))

(defn stop! []
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil)
    (println "[dev] drawbridge stopped")))
