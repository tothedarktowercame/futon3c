(ns futon3c.transport.bootstrap-handler-migration
  "Explicit operator migration of the pre-composition bootstrap closure.

  This reads the two retained handlers from the qualified legacy closure;
  it never reconstructs runtime options or creates WebSocket callbacks."
  (:require [futon3c.transport.http :as http]))

(defn- state [name]
  (var-get (ns-resolve 'futon3c.transport.http name)))

(defn- refuse [reason]
  (throw (ex-info "Bootstrap handler migration refused" {:reason reason})))

(defn- retained [app field]
  (try
    (let [f (.getDeclaredField (class app) field)]
      (.setAccessible f true)
      (.get f app))
    (catch java.lang.ReflectiveOperationException _ (refuse :legacy-handler-fields-unavailable))))

(defn migrate!
  "Upgrade the original bootstrap composition under the installation lock.

  Only the historical start-futon3c! app layout is admitted. A missing
  retained rebuild function refuses. All validation and construction precede
  installation; no requests, connections, jobs or stores are created."
  []
  (let [lock (state 'handler-reconfiguration-lock)]
    (locking lock
    (let [app @(state '!installed-handler)]
      (when-not (and (fn? app)
                     (re-matches #"futon3c\.dev\.bootstrap\$start_futon3c_BANG_\$app__\d+"
                                 (.getName (class app))))
        (refuse :not-legacy-bootstrap-handler))
      (let [http-handler (retained app "http_handler")
            ws-handler (retained app "handler")
            rebuild (:futon3c.transport.http/rebuild-fn (meta http-handler))]
        (when-not (and (fn? http-handler) (fn? ws-handler) (fn? rebuild))
          (refuse :retained-handlers-unqualified))
        (let [rebuilt (rebuild)
              original-config (:futon3c.transport.http/handler-config (meta rebuilt))]
          (when-not (map? original-config)
            (refuse :retained-rebuild-lacks-config))
          ;; Keep the original HTTP closure for this migration. Its factory is
          ;; now explicit, so later reconfiguration can build from exact config.
          (let [captured (with-meta http-handler
                           (assoc (meta rebuilt)
                                  :futon3c.transport.http/handler-config original-config))
                composed (http/compose-http-websocket-handler captured ws-handler)]
            (http/rebuild-handler! composed)
            {:ok true :status :bootstrap-composition-migrated
             :http-handler-retained? true :websocket-handler-retained? true})))))))
