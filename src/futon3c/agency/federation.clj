(ns futon3c.agency.federation
  "Inter-Agency federation — peer announcement and proxy agent registration.

   When an agent registers locally, federation announces the registration to
   configured peer Agencies. The peer creates a proxy agent whose invoke-fn
   forwards dispatch back to the originating Agency via HTTP.

   Configuration:
     FUTON3C_PEERS    — comma-separated peer Agency URLs (e.g. http://linode:7070)
     FUTON3C_SELF_URL — this Agency's externally reachable URL

   Architecture:
     Local Agency ──announce──→ Peer Agency
       (real agent)               (proxy agent, invoke-fn = HTTP POST back)

   The proxy invoke-fn POSTs to the origin Agency's /dispatch endpoint.
   The origin Agency has the real agent registered and dispatches normally."
  (:require [futon3c.agency.registry :as reg]
            [clojure.string :as str]
            [cheshire.core :as json]
            [org.httpkit.client :as http]))

(def ^:private default-proxy-timeout-ms 600000)

;; =============================================================================
;; Configuration state
;; =============================================================================

(defonce ^{:doc "Federation config atom.
   {:peers [url ...], :self-url string}"}
  !config (atom {:peers [] :self-url nil}))

(defn configure!
  "Configure federation with peer URLs and self URL.
   peers: vector of peer Agency base URLs (no trailing slash).
   self-url: this Agency's externally reachable base URL."
  [{:keys [peers self-url]}]
  (reset! !config {:peers (vec (remove str/blank? (or peers [])))
                   :self-url self-url}))

(defn configure-from-env!
  "Configure federation from environment variables.
   FUTON3C_PEERS: comma-separated peer URLs.
   FUTON3C_SELF_URL: this Agency's URL."
  []
  (let [peers-str (System/getenv "FUTON3C_PEERS")
        self-url (System/getenv "FUTON3C_SELF_URL")
        peers (when (and peers-str (not (str/blank? peers-str)))
                (->> (str/split peers-str #",")
                     (map str/trim)
                     (remove str/blank?)
                     vec))]
    (configure! {:peers (or peers [])
                 :self-url self-url})))

(defn peers
  "Return configured peer URLs."
  []
  (:peers @!config))

(defn self-url
  "Return this Agency's self URL."
  []
  (:self-url @!config))

(defn- parse-agent-type
  [x]
  (cond
    (keyword? x) x
    (string? x) (when-not (str/blank? x) (keyword x))
    :else nil))

(defn- parse-capability
  [x]
  (cond
    (keyword? x) x
    (string? x) (when-not (str/blank? x) (keyword x))
    :else nil))

(defn- parse-agent-id
  [x]
  (cond
    (keyword? x) (name x)
    (string? x) (when-not (str/blank? x) (str/trim x))
    :else nil))

;; =============================================================================
;; Proxy invoke-fn — forwards dispatch to origin Agency
;; =============================================================================

(defn make-proxy-invoke-fn
  "Create an invoke-fn that forwards dispatch to a remote Agency.
   origin-url: the Agency where the real agent lives.
   agent-id: the agent's string ID.

   Returns (fn [prompt session-id] result-map) that POSTs to
   origin-url/api/alpha/invoke."
  [origin-url agent-id]
  (fn [prompt session-id]
    (try
      (let [body (json/generate-string
                  {"agent-id" agent-id
                   "prompt" prompt
                   "caller" "federation-proxy"
                   "timeout-ms" default-proxy-timeout-ms})
            ;; Use org.httpkit.client for non-blocking HTTP
            resp @(http/post
                   (str origin-url "/api/alpha/invoke")
                   {:headers {"Content-Type" "application/json"}
                    :body body
                    :timeout default-proxy-timeout-ms})
            status (:status resp)
            parsed (try
                     (json/parse-string (or (:body resp) "{}") true)
                     (catch Exception _
                       {}))]
        (if (and status (< status 400) (= true (:ok parsed)))
          {:result (:result parsed)
           :session-id (or (:session-id parsed) session-id)}
          {:error (str "Proxy invoke failed: HTTP " status
                       (when-let [msg (:message parsed)]
                         (str " (" msg ")")))
           :exit-code -1}))
      (catch Exception e
        {:error (str "Proxy invoke exception: " (.getMessage e))
         :exit-code -1}))))

;; =============================================================================
;; Peer announcement
;; =============================================================================

(defn announce-to-peer!
  "Announce a local agent registration to a single peer Agency.
   Posts to peer-url/api/alpha/agents with origin-url so the peer creates a proxy.
   Returns {:ok bool :peer peer-url :status int} or {:ok false :error ...}."
  [peer-url agent-record self-url]
  (try
    (let [agent-id (get-in agent-record [:agent/id :id/value])
          agent-type (name (:agent/type agent-record))
          capabilities (mapv (fn [cap]
                               (if (keyword? cap)
                                 (subs (str cap) 1)
                                 (str cap)))
                             (:agent/capabilities agent-record))
          body (json/generate-string
                {"agent-id" agent-id
                 "type" agent-type
                 "capabilities" capabilities
                 "origin-url" self-url
                 "proxy" true})
          resp @(http/post
                 (str peer-url "/api/alpha/agents")
                 {:headers {"Content-Type" "application/json"}
                  :body body
                  :timeout 5000})]
      {:ok (and (:status resp) (< (:status resp) 400))
       :peer peer-url
       :status (:status resp)})
    (catch Exception e
      {:ok false
       :peer peer-url
       :error (.getMessage e)})))

(defn announce!
  "Announce a local agent registration to all configured peers.
   Skips announcement if no peers or no self-url configured.
   Skips proxy agents (to prevent announcement loops).
   Returns vector of per-peer results."
  [agent-record]
  (let [{:keys [peers self-url]} @!config]
    (when (and (seq peers) self-url
              ;; Don't re-announce proxy agents (prevents loops)
              (not (get-in agent-record [:agent/metadata :proxy?]))
              ;; Some local bridge agents intentionally connect directly to a
              ;; remote Agency over WS and should not be mirrored as proxies.
              (not (get-in agent-record [:agent/metadata :skip-federation-proxy?])))
      (mapv #(announce-to-peer! % agent-record self-url) peers))))

;; =============================================================================
;; Hook wiring — connect to registry's on-register callback
;; =============================================================================

(defn install-hook!
  "Install the federation announcement hook on the agent registry.
   After this, every successful local registration triggers peer announcement."
  []
  (reg/set-on-register! announce!))

(defn remove-hook!
  "Remove the federation announcement hook."
  []
  (reg/set-on-register! nil))

;; =============================================================================
;; Peer import / sync
;; =============================================================================

(defn- proxy-metadata
  [origin-url]
  {:proxy? true
   :remote? true
   :origin-url origin-url})

(defn- upsert-proxy-agent!
  [origin-url agent-id agent-info]
  (let [aid (parse-agent-id agent-id)
        agent-type (parse-agent-type (:type agent-info))
        capabilities (->> (:capabilities agent-info)
                          (keep parse-capability)
                          vec)
        existing (when (seq aid) (reg/get-agent aid))]
    (cond
      (str/blank? aid)
      {:ok false :action :invalid-agent-id}

      (nil? agent-type)
      {:ok false :agent-id aid :action :invalid-type}

      ;; Never overwrite a real local agent with a proxy import.
      (and existing (not (get-in existing [:agent/metadata :proxy?])))
      {:ok true :agent-id aid :action :skipped-local}

      ;; Existing proxy from another origin is a real conflict; keep the current one.
      (and existing
           (get-in existing [:agent/metadata :proxy?])
           (not= origin-url (get-in existing [:agent/metadata :origin-url])))
      {:ok false
       :agent-id aid
       :action :origin-conflict
       :origin-url (get-in existing [:agent/metadata :origin-url])}

      existing
      (do
        (reg/update-agent! aid
                           :agent/type agent-type
                           :agent/invoke-fn (make-proxy-invoke-fn origin-url aid)
                           :agent/capabilities capabilities
                           :agent/metadata (proxy-metadata origin-url))
        {:ok true :agent-id aid :action :updated})

      :else
      (let [result (reg/register-agent!
                    {:agent-id {:id/value aid :id/type :continuity}
                     :type agent-type
                     :invoke-fn (make-proxy-invoke-fn origin-url aid)
                     :capabilities capabilities
                     :metadata (proxy-metadata origin-url)})]
        (if (and (map? result) (= false (:ok result)))
          {:ok false
           :agent-id aid
           :action :register-failed
           :error result}
          {:ok true :agent-id aid :action :registered})))))

(defn sync-peer!
  "Fetch currently registered agents from PEER-URL and mirror them locally as proxies.
   Real local agents always win over imported proxy state."
  [peer-url]
  (try
    (let [resp @(http/get (str peer-url "/api/alpha/agents")
                          {:headers {"Accept" "application/json"}
                           :timeout 5000})
          status (:status resp)
          parsed (try
                   (json/parse-string (or (:body resp) "{}") true)
                   (catch Exception _
                     {}))]
      (if (and status (< status 400) (= true (:ok parsed)))
        (let [results (->> (:agents parsed)
                           (map (fn [[agent-id agent-info]]
                                  (upsert-proxy-agent! peer-url agent-id agent-info)))
                           vec)]
          {:ok true
           :peer peer-url
           :count (count results)
           :results results})
        {:ok false
         :peer peer-url
         :status status
         :error (or (:error parsed)
                    (:message parsed)
                    "peer-sync-failed")}))
    (catch Exception e
      {:ok false
       :peer peer-url
       :error (.getMessage e)})))

(defn sync-peers!
  "Mirror currently registered agents from all configured peers into the local registry."
  []
  (mapv sync-peer! (peers)))
