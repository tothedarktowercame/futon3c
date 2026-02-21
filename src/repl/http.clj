(ns repl.http
  "Drawbridge (nREPL over HTTP) helper for mission control and admin access.

   Two endpoints on the same port:
   - /repl  — nREPL over HTTP (Drawbridge protocol, for Emacs/CIDER)
   - /eval  — plain Clojure eval (POST code as body, get EDN back)

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

(defn- read-body
  "Read the request body as a string."
  [request]
  (when-let [body (:body request)]
    (cond
      (string? body) body
      (instance? java.io.InputStream body) (slurp body)
      :else (str body))))

(defn- eval-handler
  "Ring handler for /eval. Accepts POST with Clojure code as body.
   Returns EDN-encoded result. Timeout after eval-timeout-ms."
  [request]
  (if (not= :post (:request-method request))
    {:status 405
     :headers {"content-type" "text/plain"}
     :body "POST only"}
    (let [code (read-body request)]
      (if (str/blank? code)
        {:status 400
         :headers {"content-type" "text/plain"}
         :body "empty code"}
        (try
          (let [f (future
                    (try
                      {:ok true :value (load-string code)}
                      (catch Throwable t
                        {:ok false
                         :error (.getMessage t)
                         :type (.getName (class t))})))
                result (deref f eval-timeout-ms ::timeout)]
            (if (= result ::timeout)
              (do (future-cancel f)
                  {:status 504
                   :headers {"content-type" "application/edn"}
                   :body (pr-str {:ok false :error "eval timeout" :timeout-ms eval-timeout-ms})})
              {:status (if (:ok result) 200 500)
               :headers {"content-type" "application/edn"}
               :body (pr-str result)}))
          (catch Throwable t
            {:status 500
             :headers {"content-type" "application/edn"}
             :body (pr-str {:ok false :error (.getMessage t) :type (.getName (class t))})}))))))

;; =============================================================================
;; Server startup with route dispatch
;; =============================================================================

(defn- route-handler
  "Dispatch between /eval (plain eval) and everything else (nREPL/Drawbridge)."
  [nrepl-handler]
  (fn [request]
    (if (= "/eval" (:uri request))
      (eval-handler request)
      (nrepl-handler request))))

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
    (println (format "[dev] drawbridge: http://%s:%s/repl + /eval (allow: %s)"
                     bind port (pr-str allow)))
    stop-fn))

(defn stop! []
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil)
    (println "[dev] drawbridge stopped")))
