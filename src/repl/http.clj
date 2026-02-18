(ns repl.http
  "Drawbridge (nREPL over HTTP) helper for mission control and admin access."
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

(defn start!
  "Start a Drawbridge endpoint.

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
  (let [handler (-> (db/ring-handler)
                    ring-keyword/wrap-keyword-params
                    ring-nested/wrap-nested-params
                    ring-params/wrap-params
                    ring-session/wrap-session
                    (wrap-token token allow))
        stop-fn (http/run-server handler {:ip bind :port port})]
    (reset! server stop-fn)
    (println (format "[dev] drawbridge: http://%s:%s/repl (allow: %s)"
                     bind port (pr-str allow)))
    stop-fn))

(defn stop! []
  (when-let [stop-fn @server]
    (stop-fn)
    (reset! server nil)
    (println "[dev] drawbridge stopped")))
