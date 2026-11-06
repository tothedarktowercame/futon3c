(ns futon3c.arxana.http
  "Loopback HTTP surface for the arxana.v1 envelope (INSTANTIATE increment 6).

  One POST endpoint accepting and returning arxana.v1 envelopes encoded with
  the futon3c.arxana.json tagging rules, plus a GET capabilities route. The
  surface binds 127.0.0.1 only, refuses to start when its port is occupied
  (preflight), and never simulates success: transport failures are typed
  D3 errors inside a well-formed response envelope whenever the request
  envelope itself decoded."
  (:require [futon3c.arxana.envelope :as env]
            [futon3c.arxana.json :as ajson]
            [futon3c.arxana.service :as service]
            [org.httpkit.server :as httpkit])
  (:import [java.io IOException]
           [java.net InetSocketAddress Socket]))

(def default-port 7084)

(defn port-open?
  "True when something is already listening on host:port."
  [host port]
  (try
    (with-open [^Socket s (Socket.)]
      (.connect s (InetSocketAddress. ^String host ^int port) 200)
      true)
    (catch IOException _ false)))

(defn preflight-ports!
  "Refuse (ex-info :arxana.http/port-in-use) when any [label port] is already
  bound on loopback. The demo must never displace or shadow another service."
  [host ports]
  (doseq [[label port] ports]
    (when (port-open? host port)
      (throw (ex-info (str "REFUSED port-in-use: " port " (" label ")")
                      {:type :arxana.http/port-in-use
                       :port port :label label :host host}))))
  true)

(defn- envelope-error-response
  "Best-effort typed response when the request envelope could not be
  validated/decoded: message/operation echo what we have, or nil."
  [msg err]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (ajson/encode
          {:protocol/version env/protocol-version
           :message/id (get msg :message/id)
           :operation (get msg :operation)
           :error err})})

(defn handler
  "Ring handler for the loopback surface. service-cfg is the dispatch config
  map; see futon3c.arxana.service/dispatch."
  [service-cfg]
  (fn [req]
    (let [path (:uri req)
          method (:request-method req)]
      (cond
        (and (= method :post) (= path "/arxana/v1"))
        (let [body-str (slurp (:body req))]
          (try
            (let [msg (ajson/decode body-str)
                  resp (service/dispatch service-cfg msg)]
              {:status 200
               :headers {"Content-Type" "application/json"}
               :body (ajson/encode resp)})
            (catch clojure.lang.ExceptionInfo e
              (if (= :arxana.json/malformed (:type (ex-data e)))
                (envelope-error-response
                 {}
                 (env/error :invalid :arxana.http/malformed-json
                            {:message (ex-message e)} nil))
                (envelope-error-response
                 {}
                 (env/error :failed :arxana.http/internal
                            {:message (ex-message e)} nil))))))

        (and (= method :get) (= path "/arxana/v1/capabilities"))
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body (ajson/encode
                (service/handshake (:graph-store service-cfg)
                                   (some? (:subscriptions service-cfg))))}

        :else
        {:status 404
         :headers {"Content-Type" "application/json"}
         :body (ajson/encode
                {:error (env/error :missing :arxana.http/unknown-route
                                   {:uri path :method method}
                                   [:capabilities/handshake])})}))))

(defn start!
  "Start the loopback server. Preflights :port (default 7084) and any
  :preflight-ports [[label port] ...] before binding. Returns a server handle
  (call it with no args to stop)."
  [{:keys [port host service-cfg preflight-ports]
    :or {port default-port host "127.0.0.1"}}]
  (preflight-ports! host (into [["arxana" port]] preflight-ports))
  (httpkit/run-server (handler service-cfg) {:ip host :port port}))
