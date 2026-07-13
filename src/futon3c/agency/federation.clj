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
            [clojure.set :as set]
            [clojure.string :as str]
            [cheshire.core :as json]
            [org.httpkit.client :as http])
  (:import [java.time Instant]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(def ^:private default-proxy-timeout-ms 600000)
(def ^:private default-sync-timeout-ms 5000)
(def ^:private default-max-backoff-ms 60000)

(declare parse-agent-id)

;; =============================================================================
;; Configuration state
;; =============================================================================

(defonce ^{:doc "Federation config atom.
   {:peers [url ...], :self-url string, :peer-sites #{site-code ...}}"}
  !config (atom {:peers [] :self-url nil}))

(defonce ^:private !sync-daemon
  (atom {:executor nil
         :started-at nil
         :interval-ms 0
         :tick-count 0
         :last-tick-at nil
         :last-results []}))

(defonce ^:private !peer-liveness
  (atom {}))

(defn configure!
  "Configure federation with peer URLs and self URL.
   peers: vector of peer Agency base URLs (no trailing slash).
   self-url: this Agency's externally reachable base URL.
   peer-sites: optional site codes whose qualified agent ids are remote here."
  [{:keys [peers self-url peer-sites]}]
  (let [peer-entries (or peers [])
        peer-urls (->> peer-entries
                       (map (fn [peer]
                              (if (map? peer)
                                (or (:url peer) (:peer-url peer))
                                peer)))
                       (remove str/blank?)
                       vec)
        sites-from-peers (keep (fn [peer]
                                 (when (map? peer)
                                   (or (:site peer) (:peer-site peer) (:id peer))))
                               peer-entries)
        normalized-sites (->> (concat peer-sites sites-from-peers)
                              (map #(some-> % str str/trim str/lower-case))
                              (remove str/blank?)
                              set)
        ;; Retain the url→site association from map-shaped peers: it is the
        ;; sync-side fallback for :home-site on imported proxies whose bare
        ;; ids carry no site prefix (roster-completeness gap #1, 2026-07-12 —
        ;; without it 7 of lucy's 8 proxies on Chicago grouped under nil).
        site-by-url (into {}
                          (keep (fn [peer]
                                  (when (map? peer)
                                    (let [url (or (:url peer) (:peer-url peer))
                                          site (some-> (or (:site peer)
                                                           (:peer-site peer)
                                                           (:id peer))
                                                       str str/trim str/lower-case
                                                       not-empty)]
                                      (when (and url site)
                                        [url site])))))
                          peer-entries)]
    (reset! !config {:peers peer-urls
                     :self-url (when-not (str/blank? self-url)
                                 (str/trim self-url))
                     :peer-sites normalized-sites
                     :peer-site-by-url site-by-url})))

(defn configure-from-env!
  "Configure federation from environment variables.
   FUTON3C_PEERS: comma-separated peer URLs; an entry may declare the peer's
   site as site=url (e.g. chi=http://172.236.108.82:7070), which feeds the
   url→site home-site fallback for bare-id proxy imports.
   FUTON3C_SELF_URL: this Agency's URL."
  []
  (let [peers-str (System/getenv "FUTON3C_PEERS")
        self-url (System/getenv "FUTON3C_SELF_URL")
        peer-sites-str (System/getenv "FUTON3C_PEER_SITES")
        peers (when (and peers-str (not (str/blank? peers-str)))
                (->> (str/split peers-str #",")
                     (map str/trim)
                     (remove str/blank?)
                     (mapv (fn [entry]
                             (if-let [[_ site url] (re-matches #"([A-Za-z][A-Za-z0-9]*)=(.+)" entry)]
                               {:site site :url url}
                               entry)))))
        peer-sites (when (and peer-sites-str (not (str/blank? peer-sites-str)))
                     (->> (str/split peer-sites-str #",")
                          (map str/trim)
                          (remove str/blank?)
                          vec))]
    (configure! {:peers (or peers [])
                 :self-url self-url
                 :peer-sites peer-sites})))

(defn peers
  "Return configured peer URLs."
  []
  (:peers @!config))

(defn self-url
  "Return this Agency's self URL."
  []
  (:self-url @!config))

(defn- parse-long*
  [x]
  (try
    (when (some? x)
      (Long/parseLong (str/trim (str x))))
    (catch Exception _ nil)))

(defn sync-interval-ms
  "Configured continuous federation sync interval.
   Defaults to 0, which keeps the daemon disabled."
  []
  (long (or (parse-long* (System/getProperty "FUTON3C_FED_SYNC_INTERVAL_MS"))
            (parse-long* (System/getenv "FUTON3C_FED_SYNC_INTERVAL_MS"))
            0)))

(defn- max-backoff-ms
  []
  (long (or (parse-long* (System/getProperty "FUTON3C_FED_SYNC_MAX_BACKOFF_MS"))
            (parse-long* (System/getenv "FUTON3C_FED_SYNC_MAX_BACKOFF_MS"))
            default-max-backoff-ms)))

(defn- current-ms []
  (System/currentTimeMillis))

(defn peer-sites
  "Return configured peer site prefixes as lower-case strings."
  []
  (:peer-sites @!config))

(defn site-prefix
  "Return this Agency's site prefix, if configured."
  []
  (some-> (or (System/getProperty "FUTON3C_SITE")
              (System/getenv "FUTON3C_SITE"))
          str/trim
          str/lower-case
          not-empty))

(defn agent-id-home-site
  "Infer a site code from a site-qualified agent id like lon-claude-1."
  [agent-id]
  (when-let [[_ site] (re-matches #"(?i)^([a-z][a-z0-9]*)-(?:claude|codex|zai|tickle)-\d+$"
                                  (str agent-id))]
    (str/lower-case site)))

(defn- parse-home-site
  [x]
  (some-> (if (keyword? x) (name x) x)
          str str/trim str/lower-case not-empty))

(defn- known-remote-home-sites
  []
  (let [local-site (site-prefix)
        proxy-sites (->> (vals @reg/!registry)
                         (keep (fn [agent]
                                 (let [aid (get-in agent [:agent/id :id/value])
                                       metadata (:agent/metadata agent)]
                                   (when (:proxy? metadata)
                                     (or (some-> (:home-site metadata) str str/lower-case)
                                         (agent-id-home-site aid))))))
                         set)]
    (cond-> (set/union (or (peer-sites) #{}) proxy-sites)
      local-site (disj local-site))))

(defn remote-homed-agent-id?
  "True when AGENT-ID names a known remote home on this server.

   This is the server-side AG-2 guard used by local registration seams. It
   refuses site-qualified peer ids and existing proxy ids from being rebound
   as local agents."
  [agent-id]
  (let [aid (parse-agent-id agent-id)
        existing (when aid (reg/get-agent aid))
        home-site (agent-id-home-site aid)]
    (boolean
     (or (get-in existing [:agent/metadata :proxy?])
         (and home-site
              (contains? (known-remote-home-sites) home-site))))))

(defn- site-qualified-away-from-local?
  [agent-id]
  (let [home-site (agent-id-home-site agent-id)
        local-site (site-prefix)]
    (boolean
     (and home-site
          (or (nil? local-site)
              (not= home-site local-site))))))

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

(defn- present-entry
  [m k]
  (or (find m k)
      (find m (name k))))

(defn- present?
  [m k]
  (boolean (present-entry m k)))

(defn- field-value
  [m k]
  (val (present-entry m k)))

(defn- parse-status
  [x]
  (cond
    (keyword? x) x
    (string? x) (when-not (str/blank? x) (keyword x))
    :else nil))

(defn- parse-optional-string
  [x]
  (cond
    (nil? x) nil
    (string? x) (not-empty x)
    :else (not-empty (str x))))

(defn- parse-optional-instant
  [x]
  (cond
    (nil? x) nil
    (instance? Instant x) x
    (string? x) (try
                  (Instant/parse x)
                  (catch Exception _ nil))
    :else nil))

(def ^:private proxy-runtime-fields
  [[:status :agent/status parse-status]
   [:last-active :agent/last-active parse-optional-instant]
   [:session-id :agent/session-id parse-optional-string]
   [:invoke-started-at :agent/invoke-started-at parse-optional-instant]
   [:invoke-prompt-preview :agent/invoke-prompt-preview parse-optional-string]
   [:invoke-activity :agent/invoke-activity parse-optional-string]])

(def ^:private proxy-metadata-runtime-fields
  [:campaign-id :mission-id :excursion-id])

(defn- proxy-runtime-updates
  [agent-info]
  (into {}
        (keep (fn [[public-k registry-k parse-fn]]
                (when (present? agent-info public-k)
                  [registry-k (parse-fn (field-value agent-info public-k))])))
        proxy-runtime-fields))

(defn- proxy-runtime-metadata
  [agent-info]
  (into {}
        (keep (fn [k]
                (when (present? agent-info k)
                  [k (field-value agent-info k)])))
        proxy-metadata-runtime-fields))

(defn- apply-agent-updates!
  [aid updates]
  (when (seq updates)
    (apply reg/update-agent! aid (mapcat identity updates))))

(defn- env-list
  [k]
  (when-let [raw (System/getenv k)]
    (->> (str/split raw #",")
         (map str/trim)
         (remove str/blank?)
         vec)))

(defn- protected-local-agent-ids
  "Continuity ids that must not be claimed by imported proxy agents.

   These are operator-facing local lanes. Federation may mirror other peer
   agents normally, but it must not let a remote proxy silently occupy the
   continuity id that local REPLs treat as the default target."
  []
  (or (seq (env-list "FUTON3C_PROTECTED_LOCAL_AGENT_IDS"))
      (->> [(or (some-> (System/getenv "FUTON3C_CODEX_AGENT_ID") str/trim not-empty)
                "codex-1")
            (or (some-> (System/getenv "FUTON3C_VSCODE_AGENT_ID") str/trim not-empty)
                "codex-vscode")
            "claude-1"
            "claude-2"]
           (remove str/blank?)
           distinct
           vec)))

(defn- protected-local-agent-id?
  [agent-id]
  (contains? (set (protected-local-agent-ids)) agent-id))

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
          home-site (or (parse-home-site
                         (get-in agent-record [:agent/metadata :home-site]))
                        (site-prefix))
          body (json/generate-string
                (cond-> {"agent-id" agent-id
                         "type" agent-type
                         "capabilities" capabilities
                         "origin-url" self-url
                         "proxy" true}
                  (present? agent-record :agent/status)
                  (assoc "status" (some-> (:agent/status agent-record) name))
                  (present? agent-record :agent/session-id)
                  (assoc "session-id" (:agent/session-id agent-record))
                  (present? agent-record :agent/invoke-started-at)
                  (assoc "invoke-started-at"
                         (some-> (:agent/invoke-started-at agent-record) str))
                  (present? agent-record :agent/invoke-prompt-preview)
                  (assoc "invoke-prompt-preview"
                         (:agent/invoke-prompt-preview agent-record))
                  (present? agent-record :agent/invoke-activity)
                  (assoc "invoke-activity" (:agent/invoke-activity agent-record))
                  (get-in agent-record [:agent/metadata :campaign-id])
                  (assoc "campaign-id"
                         (get-in agent-record [:agent/metadata :campaign-id]))
                  (get-in agent-record [:agent/metadata :mission-id])
                  (assoc "mission-id"
                         (get-in agent-record [:agent/metadata :mission-id]))
                  (get-in agent-record [:agent/metadata :excursion-id])
                  (assoc "excursion-id"
                         (get-in agent-record [:agent/metadata :excursion-id]))
                  home-site (assoc "home-site" home-site)))
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
  (let [{:keys [peers self-url]} @!config
        agent-id (get-in agent-record [:agent/id :id/value])
        skip-reason (cond
                      (empty? peers) :no-peers
                      (str/blank? self-url) :no-self-url
                      (get-in agent-record [:agent/metadata :proxy?]) :proxy-agent
                      (get-in agent-record [:agent/metadata :skip-federation-proxy?])
                      :skip-federation-proxy)]
    (if skip-reason
      (do
        (println "[federation] announce skipped"
                 {:agent-id agent-id
                  :reason skip-reason
                  :peers peers
                  :self-url self-url})
        nil)
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

(defn proxy-home-site
  "Home site for an imported proxy, best-source-first: the agent id's own
   site prefix, then the origin's declaration (top-level :home-site in an
   announce body, or :metadata :home-site in a synced roster entry), then
   the configured url→site mapping for the origin peer."
  [origin-url agent-id agent-info]
  (or (agent-id-home-site agent-id)
      (parse-home-site (:home-site agent-info))
      (parse-home-site (get-in agent-info [:metadata :home-site]))
      (get (:peer-site-by-url @!config) origin-url)))

(defn proxy-local-id
  "Local registry id for an imported remote agent. Already-qualified ids are
   kept; a bare id gets its resolved home site as a prefix (laptop codex-1 →
   oxf-codex-1 here). Joe's addressing contract (2026-07-12): locals stay
   bare — 'tell codex-1' resolves to THIS box's codex-1 — and imports carry
   the site prefix, so same-named agents on different sites coexist instead
   of the import being refused (the claude-2/zai-1 collision gap)."
  [origin-url remote-id agent-info]
  (if (agent-id-home-site remote-id)
    remote-id
    (if-let [hs (proxy-home-site origin-url remote-id agent-info)]
      (str hs "-" remote-id)
      remote-id)))

(defn- proxy-metadata
  [origin-url remote-id local-id agent-info]
  (merge
   (cond-> {:proxy? true
            :remote? true
            :origin-url origin-url}
     (:federation/transport agent-info)
     (assoc :federation/transport (:federation/transport agent-info))
     (:federation/stale? agent-info)
     (assoc :federation/stale? (:federation/stale? agent-info))
     (:federation/uplink-site agent-info)
     (assoc :federation/uplink-site (:federation/uplink-site agent-info))
     (proxy-home-site origin-url remote-id agent-info)
     (assoc :home-site (keyword (proxy-home-site origin-url remote-id agent-info)))
     (not= remote-id local-id)
     (assoc :remote-agent-id remote-id))
   (proxy-runtime-metadata agent-info)))

(defn- own-site-reflection?
  "True when a peer's roster entry is really one of OUR agents reflected back
   (the peer's proxy of us). Importing it would create a loop-back proxy that
   routes our own agent through the peer. Detected by home-site = our site,
   or the entry's own origin-url = our self-url."
  [remote-id agent-info]
  (let [home-site (proxy-home-site nil remote-id agent-info)
        local-site (site-prefix)
        entry-origin (get-in agent-info [:metadata :origin-url])
        self (self-url)]
    (boolean
     (or (and home-site local-site (= home-site local-site))
         (and self entry-origin (= entry-origin self))))))

(defn register-proxy-agent!
  "Create or refresh a federation proxy for a remote agent.

   The proxy registers under `proxy-local-id` (bare remote ids are qualified
   with their home site) and forwards invokes using the REMOTE's own id.
   Existing real local agents are preserved unless the id itself proves the
   local record is a remote-home phantom on this server."
  [origin-url agent-id agent-info]
  (let [remote-id (parse-agent-id agent-id)
        agent-type (parse-agent-type (:type agent-info))
        capabilities (->> (:capabilities agent-info)
                          (keep parse-capability)
                          vec)
        aid (when (seq remote-id)
              (proxy-local-id origin-url remote-id agent-info))
        existing (when (seq aid) (reg/get-agent aid))
        invoke-fn (or (:invoke-fn agent-info)
                      (make-proxy-invoke-fn origin-url remote-id))]
    (cond
      (str/blank? remote-id)
      {:ok false :action :invalid-agent-id}

      (nil? agent-type)
      {:ok false :agent-id aid :action :invalid-type}

      ;; A peer's reflection of one of our own agents must not import: the
      ;; real record lives here, and a proxy would loop invokes through the
      ;; peer and back.
      (own-site-reflection? remote-id agent-info)
      {:ok true :agent-id remote-id :action :skipped-own-site}

      ;; A direct presence registration (e.g. codex-3's ws-bridge) already
      ;; represents this remote agent here under its bare id with the same
      ;; declared home-site — importing a proxy too would show the agent
      ;; twice in the roster. The direct channel wins.
      (and (not= remote-id aid)
           (when-let [direct (reg/get-agent remote-id)]
             (and (not (get-in direct [:agent/metadata :proxy?]))
                  (= (some-> (get-in direct [:agent/metadata :home-site]) name)
                     (proxy-home-site origin-url remote-id agent-info)))))
      {:ok true :agent-id remote-id :action :skipped-direct-presence}

      ;; Never let a remote proxy occupy an operator-facing local continuity id.
      ;; `cr new` and similar local REPL flows assume these ids are either owned
      ;; by a real local agent or unbound, never silently rebound to a peer.
      ;; If a protected id is already contaminated by proxy state, evict it.
      ;; (Only reachable for bare ids with no resolvable home site — a known
      ;; home site qualifies the id off the protected list.)
      (and (protected-local-agent-id? aid)
           (or (nil? existing)
               (get-in existing [:agent/metadata :proxy?])))
      (do
        (when existing
          (reg/unregister-agent! aid))
        {:ok true :agent-id aid :action :skipped-protected-id})

      ;; Never overwrite a real local agent with a proxy import unless the id
      ;; is site-qualified to a known remote home. In that case the local record
      ;; is exactly the AG-2 phantom this slice is meant to replace.
      (and existing
           (not (get-in existing [:agent/metadata :proxy?]))
           (not (site-qualified-away-from-local? aid)))
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
        (apply-agent-updates!
         aid
         (merge {:agent/type agent-type
                 :agent/invoke-fn invoke-fn
                 :agent/capabilities capabilities
                 :agent/metadata (proxy-metadata origin-url remote-id aid agent-info)}
                (proxy-runtime-updates agent-info)))
        {:ok true :agent-id aid :action :updated})

      :else
      (let [runtime-updates (proxy-runtime-updates agent-info)
            result (reg/register-agent!
                    {:agent-id {:id/value aid :id/type :continuity}
                     :type agent-type
                     :invoke-fn invoke-fn
                     :capabilities capabilities
                     :session-id (:agent/session-id runtime-updates)
                     :metadata (proxy-metadata origin-url remote-id aid agent-info)})]
        (if (and (map? result) (= false (:ok result)))
          {:ok false
           :agent-id aid
           :action :register-failed
           :error result}
          (do
            (apply-agent-updates! aid runtime-updates)
            {:ok true :agent-id aid :action :registered}))))))

(defn- capability-wire-name
  [cap]
  (cond
    (keyword? cap) (subs (str cap) 1)
    (string? cap) cap
    :else (str cap)))

(defn export-roster
  "Return exportable roster entries from the local registry.

   Options:
   - :exclude-site omits agents whose home-site is that site. Used by a hub
     replying to an uplink announce so the client does not import itself.
   - :include-proxies? defaults true so hubs can push local + other-site
     proxies back down the uplink."
  ([] (export-roster {}))
  ([{:keys [exclude-site include-proxies?]
     :or {include-proxies? true}}]
   (let [excluded (parse-home-site exclude-site)]
     (vec
      (for [agent (vals @reg/!registry)
            :let [metadata (:agent/metadata agent)
                  aid (get-in agent [:agent/id :id/value])
                  home-site (or (parse-home-site (:home-site metadata))
                                (agent-id-home-site aid)
                                (site-prefix))]
            :when (and aid
                       (or include-proxies?
                           (not (:proxy? metadata)))
                       (not= excluded home-site))]
        {:agent-id aid
         :type (some-> (:agent/type agent) name)
         :capabilities (mapv capability-wire-name
                             (:agent/capabilities agent))
         :last-active (some-> (:agent/last-active agent) str)
         :metadata (cond-> {}
                     home-site (assoc :home-site home-site)
                     (:proxy? metadata) (assoc :proxy? true)
                     (:remote-agent-id metadata)
                     (assoc :remote-agent-id (:remote-agent-id metadata))
                     (:origin-url metadata)
                     (assoc :origin-url (:origin-url metadata)))})))))

(def ^:private uplink-timeout-sentinel ::uplink-timeout)

(defn make-uplink-invoke-fn
  "Create a proxy invoke-fn that sends :fed-invoke frames over an existing
   uplink socket. SEND-FRAME! accepts a Clojure frame map. PENDING is an atom
   of invoke-id -> promise, resolved by resolve-uplink-invoke!."
  [send-frame! pending remote-agent-id]
  (fn [prompt session-id]
    (let [invoke-id (str (java.util.UUID/randomUUID))
          p (promise)
          timeout-ms default-proxy-timeout-ms]
      (swap! pending assoc invoke-id p)
      (try
        (send-frame! {:type "fed_invoke"
                      :invoke-id invoke-id
                      :agent-id remote-agent-id
                      :prompt prompt
                      :timeout-ms timeout-ms})
        (let [result (deref p timeout-ms uplink-timeout-sentinel)]
          (swap! pending dissoc invoke-id)
          (if (= uplink-timeout-sentinel result)
            {:error (str "Uplink invoke timed out: " remote-agent-id)
             :exit-code -1
             :error/code :invoke-timeout}
            (if (:ok result)
              {:result (:result result)
               :session-id (or (:session-id result) session-id)}
              {:error (or (:error result)
                          (str "Uplink invoke failed: " remote-agent-id))
               :exit-code -1
               :error/code :invoke-failed})))
        (catch Exception e
          (swap! pending dissoc invoke-id)
          {:error (str "Uplink invoke exception: " (.getMessage e))
           :exit-code -1
           :error/code :invoke-failed})))))

(defn resolve-uplink-invoke!
  "Resolve an invoke pending on a fed-uplink connection."
  [pending invoke-id result]
  (if-let [p (get @pending invoke-id)]
    (do
      (swap! pending dissoc invoke-id)
      (deliver p result)
      true)
    false))

(defn import-uplink-roster!
  "Import ROSTER entries through the existing proxy seam using WS-uplink
   metadata and optional per-entry invoke functions.

   ORIGIN-URL is a logical origin string for conflict detection. INVOKE-FN-FN,
   when supplied, receives the bare remote id and returns an invoke-fn."
  [origin-url roster {:keys [transport uplink-site invoke-fn-fn]}]
  (->> roster
       (mapv (fn [entry]
               (let [agent-id (or (:agent-id entry) (:agent_id entry))
                     invoke-fn (when invoke-fn-fn
                                 (invoke-fn-fn agent-id))
                     agent-info (cond-> {:type (or (:type entry) (:agent/type entry))
                                         :capabilities (:capabilities entry)
                                         :metadata (:metadata entry)
                                         :home-site (or (:home-site entry)
                                                        (get-in entry [:metadata :home-site])
                                                        uplink-site)}
                                  transport (assoc :federation/transport transport)
                                  uplink-site (assoc :federation/uplink-site uplink-site)
                                  invoke-fn (assoc :invoke-fn invoke-fn))]
                 (register-proxy-agent! origin-url agent-id agent-info))))))

(defn- uplink-proxy-for-site?
  [site agent]
  (let [metadata (:agent/metadata agent)
        home (parse-home-site (:home-site metadata))]
    (and (:proxy? metadata)
         (= :ws-uplink (:federation/transport metadata))
         (= (parse-home-site site)
            (or home (parse-home-site (:federation/uplink-site metadata)))))))

(defn mark-uplink-site-stale!
  "Mark WS-uplink proxies for SITE stale, without pruning."
  [site error]
  (let [site* (parse-home-site site)
        now-ms (current-ms)]
    (->> @reg/!registry
         (keep (fn [[aid agent]]
                 (when (uplink-proxy-for-site? site* agent)
                   (reg/update-agent!
                    aid
                    :agent/metadata
                    (-> (:agent/metadata agent)
                        (assoc :federation/stale? true
                               :federation/reachable? false
                               :federation/last-error error
                               :federation/last-failed-sync-at-ms now-ms)
                        (update :federation/missed-announces (fnil inc 0))))
                   aid)))
         vec)))

(defn prune-stale-uplink-site!
  "Prune SITE's WS-uplink proxies after MISSED-LIMIT missed announces."
  ([site] (prune-stale-uplink-site! site 3))
  ([site missed-limit]
   (let [site* (parse-home-site site)]
     (->> @reg/!registry
          (keep (fn [[aid agent]]
                  (let [missed (long (or (get-in agent [:agent/metadata :federation/missed-announces])
                                         0))]
                    (when (and (uplink-proxy-for-site? site* agent)
                               (true? (get-in agent [:agent/metadata :federation/stale?]))
                               (>= missed missed-limit))
                      (reg/unregister-agent! aid)
                      {:agent-id aid :action :pruned}))))
          vec))))

(defn- proxy-for-peer?
  [peer-url agent]
  (and (get-in agent [:agent/metadata :proxy?])
       (= peer-url (get-in agent [:agent/metadata :origin-url]))))

(defn- peer-proxy-ids
  [peer-url]
  (->> @reg/!registry
       (keep (fn [[aid agent]]
               (when (proxy-for-peer? peer-url agent)
                 aid)))
       set))

(defn- update-proxy-liveness!
  [peer-url reachable? extra]
  (doseq [[aid agent] @reg/!registry
          :when (proxy-for-peer? peer-url agent)]
    (reg/update-agent!
     aid
     ;; keep the mirrored idle time: a liveness stamp is not agent activity
     :agent/last-active (:agent/last-active agent)
     :agent/metadata
     (merge (:agent/metadata agent)
            {:federation/peer-url peer-url
             :federation/reachable? reachable?
             :federation/stale? (not reachable?)}
            extra))))

(defn- prune-departed-proxies!
  [peer-url roster-ids]
  (let [roster-set (set roster-ids)]
    (->> (peer-proxy-ids peer-url)
         (remove roster-set)
         (mapv (fn [aid]
                 (reg/unregister-agent! aid)
                 {:agent-id aid :action :pruned})))))

(defn- fetch-peer-roster!
  [peer-url]
  (let [resp @(http/get (str peer-url "/api/alpha/agents")
                        {:headers {"Accept" "application/json"}
                         :timeout default-sync-timeout-ms})
        status (:status resp)
        parsed (try
                 (json/parse-string (or (:body resp) "{}") true)
                 (catch Exception _
                   {}))]
    (if (and status (< status 400) (= true (:ok parsed)))
      parsed
      (throw (ex-info "peer-sync-failed"
                      {:peer peer-url
                       :status status
                       :error (or (:error parsed)
                                  (:message parsed)
                                  "peer-sync-failed")})))))

(defn- record-peer-success!
  [peer-url now results pruned]
  (swap! !peer-liveness
         assoc
         peer-url
         {:peer peer-url
          :reachable? true
          :failure-count 0
          :last-success-at-ms now
          :last-attempt-at-ms now
          :next-sync-at-ms now
          :last-results results
          :last-pruned pruned})
  (update-proxy-liveness!
   peer-url
   true
   {:federation/last-sync-at-ms now
    :federation/last-sync-at (str (Instant/ofEpochMilli now))
    :federation/last-error nil}))

(defn- backoff-delay-ms
  [interval-ms failure-count jitter-fn]
  (let [base (max 1 (long (or interval-ms (sync-interval-ms) 1)))
        multiplier (long (Math/pow 2 (max 0 (dec (long failure-count)))))
        capped (min (max-backoff-ms) (* base multiplier))
        jitter-bound (max 0 (long (/ capped 10)))
        jitter (long (or (when (pos? jitter-bound)
                           (jitter-fn jitter-bound))
                         0))]
    (+ capped jitter)))

(defn- record-peer-failure!
  [peer-url now interval-ms error jitter-fn]
  (let [failure-count (inc (long (get-in @!peer-liveness [peer-url :failure-count] 0)))
        delay (backoff-delay-ms interval-ms failure-count jitter-fn)
        next-sync-at (+ now delay)
        err-msg (or (:error (ex-data error))
                    (.getMessage ^Throwable error)
                    (str error))]
    (swap! !peer-liveness
           assoc
           peer-url
           {:peer peer-url
            :reachable? false
            :failure-count failure-count
            :last-attempt-at-ms now
            :last-error err-msg
            :next-sync-at-ms next-sync-at
            :backoff-ms delay})
    (update-proxy-liveness!
     peer-url
     false
     {:federation/last-error err-msg
      :federation/last-failed-sync-at-ms now
      :federation/last-failed-sync-at (str (Instant/ofEpochMilli now))})
    {:peer peer-url
     :ok false
     :error err-msg
     :failure-count failure-count
     :backoff-ms delay
     :next-sync-at-ms next-sync-at}))

(defn sync-peer!
  "Fetch currently registered agents from PEER-URL and mirror them locally as proxies.
   Real local agents always win over imported proxy state."
  ([peer-url]
   (sync-peer! peer-url {}))
  ([peer-url {:keys [fetch-fn now-ms interval-ms jitter-fn]
              :or {fetch-fn fetch-peer-roster!
                   jitter-fn (fn [bound] (rand-int (inc (long bound))))}}]
  (try
    (let [now (long (or now-ms (current-ms)))
          parsed (fetch-fn peer-url)
          agents (or (:agents parsed) {})
          ;; prune compares against the ids proxies are REGISTERED under —
          ;; qualified via proxy-local-id, same as register-proxy-agent!.
          ;; Side effects that make migration self-healing: leftover bare-id
          ;; proxies from the pre-qualification code fall out of this set and
          ;; are pruned on the first tick, replaced by their qualified twins;
          ;; own-site reflections are excluded, so loop-back proxies imported
          ;; by pre-guard code get pruned rather than kept alive.
          roster-ids (set (keep (fn [[k info]]
                                  (when-let [rid (parse-agent-id k)]
                                    (when-not (own-site-reflection? rid info)
                                      (proxy-local-id peer-url rid info))))
                                agents))
          results (->> agents
                       (map (fn [[agent-id agent-info]]
                              (register-proxy-agent! peer-url agent-id agent-info)))
                       vec)
          pruned (prune-departed-proxies! peer-url roster-ids)]
      (record-peer-success! peer-url now results pruned)
      {:ok true
       :peer peer-url
       :count (count results)
       :results results
       :pruned pruned})
    (catch Exception e
      (record-peer-failure! peer-url (long (or now-ms (current-ms))) interval-ms e jitter-fn)))))

(defn sync-peers!
  "Mirror currently registered agents from all configured peers into the local registry."
  ([] (sync-peers! {}))
  ([opts]
   (if (seq opts)
     (mapv #(sync-peer! % opts) (peers))
     (mapv sync-peer! (peers)))))

(defn sync-tick!
  "Run one liveness-aware federation sync tick.
   Tests pass :now-ms, :peers, :fetch-fn, and :jitter-fn to keep this deterministic."
  [{:keys [now-ms peers interval-ms fetch-fn jitter-fn]
    :or {jitter-fn (constantly 0)}}]
  (let [now (long (or now-ms (current-ms)))
        interval (long (or interval-ms (sync-interval-ms)))
        ;; NB: the destructured local `peers` shadows the `peers` fn, so the
        ;; original (or peers (peers)) called nil as a function on every
        ;; daemon tick — the sync daemon NPE'd and never synced (found live
        ;; on lucy 2026-07-12; tests always injected :peers, masking it).
        configured-peers (vec (or peers (:peers @!config)))
        results (mapv (fn [peer-url]
                        (let [next-at (long (get-in @!peer-liveness [peer-url :next-sync-at-ms] 0))]
                          (if (> next-at now)
                            {:ok false
                             :peer peer-url
                             :skipped? true
                             :reason :backoff
                             :next-sync-at-ms next-at}
                            (sync-peer! peer-url {:fetch-fn (or fetch-fn fetch-peer-roster!)
                                                  :now-ms now
                                                  :interval-ms interval
                                                  :jitter-fn jitter-fn}))))
                      configured-peers)]
    (swap! !sync-daemon
           (fn [s]
             (-> s
                 (update :tick-count (fnil inc 0))
                 (assoc :last-tick-at (str (Instant/ofEpochMilli now))
                        :last-results results))))
    results))

(declare stop-sync-daemon!)

(defn sync-daemon-status
  []
  (let [s @!sync-daemon
        ex (:executor s)]
    (-> s
        (dissoc :executor)
        (assoc :running? (and (some? ex)
                              (not (.isShutdown ^ScheduledExecutorService ex)))
               :enabled? (pos? (sync-interval-ms))
               :peers @!peer-liveness))))

(defn start-sync-daemon!
  "Start continuous federation peer sync when FUTON3C_FED_SYNC_INTERVAL_MS > 0.
   With the default 0 interval this is a no-op that preserves one-shot boot sync."
  ([] (start-sync-daemon! {}))
  ([{:keys [interval-ms]}]
   (let [interval (long (or interval-ms (sync-interval-ms)))]
     (if (not (pos? interval))
       (do
         (when (:executor @!sync-daemon)
           (stop-sync-daemon!))
         (sync-daemon-status))
       (let [existing (:executor @!sync-daemon)]
         (when (or (nil? existing)
                   (.isShutdown ^ScheduledExecutorService existing))
           (let [executor (Executors/newSingleThreadScheduledExecutor
                           (reify java.util.concurrent.ThreadFactory
                             (newThread [_ r]
                               (doto (Thread. r "futon3c-federation-sync")
                                 (.setDaemon true)))))
                 task #(try
                         (sync-tick! {:interval-ms interval})
                         (catch Throwable t
                           (binding [*out* *err*]
                             (println "[federation] sync daemon uncaught:"
                                      (.getMessage t)))))]
             (.scheduleWithFixedDelay executor task interval interval TimeUnit/MILLISECONDS)
             (swap! !sync-daemon assoc
                    :executor executor
                    :started-at (str (Instant/now))
                    :interval-ms interval)))
         (sync-daemon-status))))))

(defn stop-sync-daemon!
  []
  (when-let [^ScheduledExecutorService ex (:executor @!sync-daemon)]
    (.shutdownNow ex)
    (.awaitTermination ex 2 TimeUnit/SECONDS))
  (swap! !sync-daemon assoc :executor nil)
  (sync-daemon-status))

(defn reset-sync-state!
  "Reset scheduler and peer liveness state. Intended for deterministic tests."
  []
  (stop-sync-daemon!)
  (reset! !peer-liveness {})
  (swap! !sync-daemon assoc
         :started-at nil
         :interval-ms 0
         :tick-count 0
         :last-tick-at nil
         :last-results [])
  nil)

(defn remote-health-facts
  "Facts consumed by CP-A logic: proxy readiness must reflect current peer reachability."
  []
  (->> @reg/!registry
       (keep (fn [[aid agent]]
               (when (get-in agent [:agent/metadata :proxy?])
                 (let [metadata (:agent/metadata agent)
                       reachable? (not (true? (:federation/stale? metadata)))]
                   {:agent-id aid
                    :registered-ready? true
                    :current-ready? reachable?}))))
       vec))

(defn connection-facts
  "Peer-level connection facts consumed by CP-A logic."
  []
  (->> @!peer-liveness
       (mapv (fn [[peer-url {:keys [reachable?]}]]
               {:connection-id peer-url
                :declared-state :connected
                :channel-state (if reachable? :live :dead)}))))
