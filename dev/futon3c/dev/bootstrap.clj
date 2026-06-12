(ns futon3c.dev.bootstrap
  "Bootstrap helpers and top-level startup orchestration for futon3c.dev."
  (:require [cheshire.core :as json]
            [futon1a.system :as f1]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.roster-store :as roster-store]
            [futon3c.agency.turn-queue :as turn-queue]
            [futon3c.blackboard :as bb]
            [futon3c.cyder :as cyder]
            [futon3c.dev.config :as config]
            [futon3c.evidence.invariant :as evidence-invariant]
            [futon3c.logic.archaeology :as archaeology]
            [futon3c.logic.locus :as locus]
            [futon3c.logic.ratchet :as ratchet]
            [futon3c.logic.snapshot :as snapshot]
            [futon3c.logic.tracer :as tracer]
            [futon3c.mission-control.service :as mcs]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.transport.http :as http]
            [futon3c.transport.irc :as irc]
            [futon3c.watcher.multi :as multi-watcher]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.server :as hk]
            [repl.http :as drawbridge]))

(defn start-futon1a!
  "Start futon1a (XTDB + HTTP). Returns system map with :node, :store, :stop!, etc."
  [direct-xtdb?]
  (let [port (config/env-int "FUTON1A_PORT" 7071)
        data-dir (config/env "FUTON1A_DATA_DIR"
                             (str (System/getProperty "user.home")
                                  "/code/storage/futon1a/default"))
        static-dir (config/env "FUTON1A_STATIC_DIR" nil)
        allowed-penholders (->> (config/env-list "FUTON1A_ALLOWED_PENHOLDERS" ["api" "joe"])
                                (remove str/blank?)
                                set)]
    (println (str "[dev] Starting futon1a (XTDB: " data-dir ")..."))
    (let [sys (f1/start! (cond-> {:data-dir data-dir
                                  :port port
                                  :allowed-penholders allowed-penholders
                                  :expose-internals? direct-xtdb?}
                           static-dir (assoc :static-dir static-dir)))]
      (println (str "[dev] futon1a: http://localhost:" (:http/port sys)))
      (println (str "[dev] futon1a allowed penholders: "
                    (if (seq allowed-penholders)
                      (str/join "," (sort allowed-penholders))
                      "<none>")))
      (when static-dir
        (println (str "[dev] futon1a static: " static-dir)))
      sys)))

(defn start-futon5!
  "Start futon5 nonstarter heartbeat API. Returns system map or nil if disabled."
  [nonstarter-fn]
  (let [port (config/env-int "FUTON5_PORT" 7072)
        db (config/env "FUTON5_DB"
                       (str (System/getProperty "user.home")
                            "/code/futon5/data/nonstarter.db"))]
    (when (pos? port)
      (if-let [start! (nonstarter-fn 'start!)]
        (try
          (let [sys (start! {:port port :db db})]
            (println (str "[dev] futon5 heartbeat API: http://localhost:" port))
            sys)
          (catch Exception e
            (println (str "[dev] futon5 heartbeat API failed: " (.getMessage e)))
            nil))
        (do
          (println "[dev] futon5 heartbeat API disabled: nonstarter.api not on classpath")
          nil)))))


(defn- restore-agent-via-handler!
  [http-handler payload]
  (let [body (json/generate-string payload)
        response (http-handler {:request-method :post
                                :uri "/api/alpha/agents/restore"
                                :headers {"content-type" "application/json"}
                                :body (java.io.ByteArrayInputStream. (.getBytes body "UTF-8"))})
        parsed (try
                 (json/parse-string (str (:body response)) true)
                 (catch Throwable _
                   {:ok false :err "invalid-restore-response"}))]
    (assoc parsed :http/status (:status response))))

(defn start-futon3c!
  "Start futon3c transport HTTP+WS. Returns system map or nil when disabled."
  [{:keys [xtdb-node evidence-store irc-interceptor irc-send-fn irc-send-base make-http-handler make-ws-handler]}]
  (let [port (config/env-int "FUTON3C_PORT" 7070)]
    (when (pos? port)
      (let [pattern-ids (if-let [s (config/env "FUTON3C_PATTERNS")]
                          (mapv keyword (remove empty? (.split s ",")))
                          [])
            opts (cond-> {:patterns {:patterns/ids pattern-ids}
                          :irc-send-fn irc-send-fn
                          :irc-send-base irc-send-base}
                   xtdb-node (assoc :xtdb-node xtdb-node)
                   evidence-store (assoc :evidence-store evidence-store))
            http-handler (make-http-handler opts)
            ws-opts (cond-> opts
                      irc-interceptor (assoc :irc-interceptor irc-interceptor))
            {:keys [handler connections]} (make-ws-handler ws-opts)
            app (fn [request]
                  (if (:websocket? request)
                    (handler request)
                    (http-handler request)))
            result (http/start-server! app port)
            restore-report (roster-store/restore-on-boot!
                            #(restore-agent-via-handler! http-handler %))]
        ;; Install continuous roster persistence ONLY now — AFTER restore-on-boot!
        ;; has consumed the saved roster. Installing at registry ns-load fired the
        ;; watch's initial persist against the empty boot registry and clobbered
        ;; the saved roster before restore could read it. The initial persist here
        ;; captures the just-restored agents going forward.
        (roster-store/install-registry-watch! reg/!registry)
        ;; Restored turns sit in per-agent queues, but no bell arrives to trigger the
        ;; lazy drainer spawn — so spawn drainers for restored agents NOW, or their
        ;; queued turns (and the agents) stay stuck/un-drainable after a restart.
        ;; Pairs with turn-queue/load-state clearing the stale :draining lock.
        (turn-queue/resume-pending-drainers!)
        (println (str "[dev] futon3c: http://localhost:" (:port result)
                      " (patterns: " (if (seq pattern-ids) pattern-ids "none") ")"))
        (when (:enabled? restore-report)
          (println (str "[dev] agent roster restore: restored="
                        (:restored restore-report)
                        " attempted=" (:attempted restore-report))))
        (assoc result
               :ws-connections connections
               :agent-restore restore-report)))))

(defn start-irc!
  "Start IRC server + WS relay bridge. Returns system map or nil when disabled."
  ([evidence-store]
   (start-irc! evidence-store (config/deployment-role)))
  ([evidence-store role]
   (let [{:keys [irc-port irc-bind-host]} (config/role-defaults role)
         irc-port (config/env-int "FUTON3C_IRC_PORT" irc-port)]
     (when (pos? irc-port)
       (let [bind-host (config/env "FUTON3C_BIND_HOST" irc-bind-host)
             relay-bridge (irc/make-relay-bridge {:evidence-store evidence-store})
             server (irc/start-irc-server!
                     {:port irc-port
                      :bind-host bind-host
                      :relay-fn (:relay-fn relay-bridge)
                      :evidence-store evidence-store})]
         ((:set-irc-send-fn! relay-bridge) (:send-to-channel! server))
         (println (str "[dev] IRC: localhost:" irc-port
                       " (channel: #futon, auto-join on WS connect)"))
         {:server server
          :relay-bridge relay-bridge
          :port irc-port})))))

(defn install-irc-auto-join!
  "Watch WS connections atom; auto-join agents to #futon when they connect."
  [ws-connections relay-bridge irc-server]
  (add-watch ws-connections :irc-auto-join
             (fn [_ _ old-conns new-conns]
               (doseq [[ch conn] new-conns
                       :when (and (:connected? conn)
                                  (not (:connected? (get old-conns ch))))]
                 (let [agent-id (:agent-id conn)
                       agent-record (reg/get-agent agent-id)
                       agent-metadata (:agent/metadata agent-record)
                       irc-auto-join? (cond
                                        (contains? agent-metadata :irc-auto-join?)
                                        (boolean (:irc-auto-join? agent-metadata))
                                        (:ws-bridge? agent-metadata)
                                        false
                                        :else true)
                       nick (let [aid (str agent-id)]
                              (or (some-> aid
                                          (str/replace #"-\d+$" "")
                                          str/trim
                                          not-empty)
                                  aid))
                       send-fn (fn [msg] (hk/send! ch msg))]
                   (when irc-auto-join?
                     (try
                       ((:join-agent! relay-bridge) agent-id nick "#futon" send-fn {:overwrite? false})
                       (when-let [part-virtual-nick! (:part-virtual-nick! irc-server)]
                         (part-virtual-nick! "#futon" (str agent-id)))
                       ((:join-virtual-nick! irc-server) "#futon" nick)
                       (catch Exception e
                         (println (str "[dev] IRC auto-join failed for " agent-id ": "
                                       (.getMessage e)))))))))))

(defn start-drawbridge!
  "Start Drawbridge endpoint used by fubar/portal style tooling."
  []
  (let [port (config/env-int "FUTON3C_DRAWBRIDGE_PORT" 6768)]
    (when (pos? port)
      (let [bind (config/env "FUTON3C_DRAWBRIDGE_BIND" "127.0.0.1")
            allow (config/env-list "FUTON3C_DRAWBRIDGE_ALLOW" ["127.0.0.1" "::1"])
            token (config/read-admin-token)
            stop-fn (drawbridge/start! {:port port
                                        :bind bind
                                        :allow allow
                                        :token token})]
        {:stop stop-fn
         :port port
         :bind bind}))))

(defn start-webarxana!
  "Start WebArxana inside the futon3c JVM. Returns system map or nil when disabled."
  []
  (when (config/env-bool "FUTON3C_WEBARXANA_SERVER_AUTOSTART" true)
    (let [port (config/env-int "FUTON3C_WEBARXANA_PORT" 3100)
          futon1a-port (config/env-int "FUTON1A_PORT" 7071)
          futon1a-url (config/env "FUTON4_BASE_URL"
                                  (str "http://127.0.0.1:" futon1a-port))
          futon1a-url (str/replace futon1a-url #"/api/alpha/?$" "")
          emacs-socket (config/env "FUTON3C_EMACS_SOCKET" "server")
          start! (requiring-resolve 'webarxana.server.core/start!)
          status (requiring-resolve 'webarxana.server.core/status)
          stop! (requiring-resolve 'webarxana.server.core/stop!)
          system (start! {:port port
                          :futon1a-url futon1a-url
                          :emacs-socket emacs-socket})]
      (assoc system
             :state-fn #(status)
             :stop-fn #(stop!)))))

(defn start-shadow!
  "Start embedded shadow-cljs CLJS watches inside the futon3c JVM.
   Gated by FUTON3C_SHADOW_AUTOSTART (default false). Build ids come from
   FUTON3C_SHADOW_BUILDS (comma-separated, default \"war-machine,webarxana\").
   See dev/futon3c/dev/shadow.clj. CLAUDE.md I-0: lets pgrep java stay at
   one PID even while CLJS watches are active."
  []
  (when (config/env-bool "FUTON3C_SHADOW_AUTOSTART" false)
    (let [start!  (requiring-resolve 'futon3c.dev.shadow/start!)
          stop!   (requiring-resolve 'futon3c.dev.shadow/stop!)
          status  (requiring-resolve 'futon3c.dev.shadow/status)
          builds  (->> (str/split (str (config/env "FUTON3C_SHADOW_BUILDS"
                                                   "war-machine,webarxana"))
                                  #",")
                       (map str/trim)
                       (remove str/blank?)
                       (map keyword)
                       vec)
          result  (apply start! builds)]
      {:builds builds
       :result result
       :state-fn #(status)
       :stop-fn  #(stop!)})))

(defn- agent-availability-summary
  [registry-status]
  (let [route-counts (frequencies (map :invoke-route (vals (:agents registry-status))))
        local-count (long (or (get route-counts :local) 0))
        ws-count (long (or (get route-counts :ws) 0))
        unreachable-count (long (or (get route-counts :none) 0))]
    {:registered (long (or (:count registry-status) 0))
     :invocable (+ local-count ws-count)
     :local local-count
     :ws ws-count
     :unreachable unreachable-count}))

(defn run-main!
  [{:keys [!f1-sys !evidence-store !irc-sys
           direct-xtdb-enabled? make-evidence-store
           start-futon1a! start-futon5! start-irc! start-agents!
           start-tickle! start-process-watchdog! start-fm-conductor! start-drawbridge!
           start-agents-blackboard-ticker! nonstarter-fn stop-agents!
           on-agent-invoke-complete!]
    :as _deps}]
  (let [role-info (config/deployment-role-info)
        role (:role role-info)
        role-cfg (config/role-defaults role)
        _ (when-not (System/getenv "MEME_DB_PATH")
            (let [candidates ["/home/joe/code/futon3a/meme.db"
                              (str (System/getProperty "user.dir") "/meme.db")]]
              (when-let [found (first (filter #(.exists (io/file %)) candidates))]
                (alter-var-root (requiring-resolve 'meme.schema/db-path)
                                (constantly (fn [] found)))
                (println (str "  meme.db → " found " (auto-detected)")))))
        direct-xtdb? (direct-xtdb-enabled? role-cfg)
        f1-sys (start-futon1a! direct-xtdb?)
        evidence-store (make-evidence-store f1-sys direct-xtdb?)
        _ (reset! !f1-sys f1-sys)
        _ (reset! !evidence-store evidence-store)
        ;; I-evidence-per-turn boot-time check: refuse to proceed silently
        ;; with a non-durable evidence store. On failure we print loudly
        ;; and keep going — but the turn-level check will fail every turn
        ;; so the violation is impossible to miss.
        boot-check (evidence-invariant/check-store-backing evidence-store)
        _ (if (:ok boot-check)
            (println (str "[dev] I-evidence-per-turn boot check: OK ("
                          (name (:kind boot-check)) ")"))
            (do
              (println "================================================================")
              (println "[dev] I-evidence-per-turn BOOT CHECK FAILED")
              (println (str "      kind:   " (name (:kind boot-check))))
              (println (str "      reason: " (:reason boot-check)))
              (println (str "      invariant: " (:invariant boot-check)))
              (println "      Evidence writes will not persist. Fix and restart.")
              (println "================================================================")))
        ;; I-coverage-ratchet load-time check: compare working-tree inventory
        ;; against git HEAD; emit a :family-fired :coverage-ratchet entry
        ;; recording the outcome. Boot continues either way — the violation,
        ;; if any, is durably evidenced and visible in the operational-families
        ;; view. (Mission: M-invariant-queue-extend Track 4.1.)
        _ (try (ratchet/check-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] I-coverage-ratchet load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)
                               " — boot continues."))))
        _ (try (archaeology/check-autostash-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] archaeology/autostash load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (archaeology/check-deferred-stub-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] archaeology/deferred-stub load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (archaeology/check-pipeline-tracer-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] archaeology/pipeline-tracer load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (archaeology/check-stash-disposition-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] archaeology/stash-disposition load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (archaeology/check-branch-disposition-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] archaeology/branch-disposition load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (archaeology/check-mission-doc-disposition-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] archaeology/mission-doc-disposition load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        ;; metabolic-balance/working-tree (10th cognitive-faculty shape;
        ;; M-bounded-in-flight-state INSTANTIATE D-02). Lives behind
        ;; requiring-resolve so this site adds no namespace-level
        ;; require coupling to operator WIP in this file.
        _ (try ((requiring-resolve
                 'futon3c.logic.metabolic-balance/check-working-tree-pressure-on-load!)
                evidence-store)
               (catch Throwable t
                 (println (str "[dev] metabolic-balance/check-working-tree-pressure-on-load! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (locus/check-mission-home-locus-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] locus/mission-home load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        ;; Auto-register the family-check-fns at boot so the probe-loop
        ;; has a populated registry without operator-driven activation.
        ;; Mutating an in-memory atom across restarts was diagnosed as
        ;; an anti-pattern (Joe 2026-05-01); deterministic registration
        ;; from on-disk inventory + source code is the durable fix.
        _ (try (archaeology/register-archaeology-control-taps!)
               (println "[dev] registered archaeology-control probe-taps (6 family-ids)")
               (catch Throwable t
                 (println (str "[dev] register-archaeology-control-taps! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (locus/register-locus-taps!)
               (println "[dev] registered single-locus probe-taps (3 family-ids)")
               (catch Throwable t
                 (println (str "[dev] register-locus-taps! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try ((requiring-resolve
                 'futon3c.logic.metabolic-balance/register-metabolic-balance-taps!))
               (println "[dev] registered metabolic-balance probe-taps (1 family-id)")
               (catch Throwable t
                 (println (str "[dev] register-metabolic-balance-taps! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        ;; Ensure pipeline-tracer items exist in the durable store.
        ;; Idempotent: re-emits only those track-ids missing from the
        ;; persisted set. Reachable-from-boot discipline — tracer state
        ;; is reconstructible from `tracer/default-tracers` (on-disk
        ;; source) at every boot. M-reachable-from-boot 2026-05-01.
        _ (try (let [r (tracer/ensure-default-tracers! evidence-store)]
                 (println (str "[dev] tracer/ensure-default-tracers!: "
                               "present=" (:already-present r)
                               " emitted=" (:emitted r)
                               " attempted=" (:attempted r))))
               (catch Throwable t
                 (println (str "[dev] tracer/ensure-default-tracers! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        ;; State-snapshot-witness/inventory: emit one :inventory-snapshot
        ;; evidence entry per JVM boot, projecting the structural-law
        ;; inventory to a flat snapshot record. Mission:
        ;; M-state-snapshot-witness 2026-05-01.
        _ (try (snapshot/snapshot-inventory-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] snapshot/snapshot-inventory-on-load! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (snapshot/snapshot-registry-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] snapshot/snapshot-registry-on-load! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (snapshot/snapshot-repo-refs-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] snapshot/snapshot-repo-refs-on-load! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (snapshot/snapshot-hud-render-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] snapshot/snapshot-hud-render-on-load! threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (locus/check-agent-routing-locus-on-load!
                evidence-store
                {:state-source reg/!registry})
               (catch Throwable t
                 (println (str "[dev] locus/agent-routing load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (try (locus/check-artifact-live-copy-locus-on-load! evidence-store)
               (catch Throwable t
                 (println (str "[dev] locus/artifact-live-copy load-time check threw: "
                               (.getName (class t)) ": " (.getMessage t)))))
        _ (mcs/configure! {:evidence-store evidence-store
                           :repos mcb/default-repo-roots})
        _ (cyder/register!
           {:id "futon1a"
            :type :server
            :stop-fn (:stop! f1-sys)
            :state-fn #(let [s @!f1-sys]
                         {:port (:http/port s)
                          :direct-xtdb? direct-xtdb?})})
        ;; Multi-repo watcher (E-live-means-live Path B): in-JVM
        ;; replacement for the separate bb watcher process. Polls
        ;; the watched roots, dispatches per-file ingest into
        ;; substrate-2. Commit-vertex catch-up is currently disabled on
        ;; this path because the live futon1a hyperedge query can wedge
        ;; the entire watcher thread; keep file-event ingestion live and
        ;; treat commit catch-up as a separate lane until that query is
        ;; bounded.
        ;; Defaults to --no-cold-scan equivalent behaviour
        ;; (cold-scan? false). Wrapped defensively — boot continues
        ;; if watcher start throws.
        _ (try
            (let [roots [{:path "/home/joe/code/futon0"  :label "futon0-d"}
                         {:path "/home/joe/code/futon1"  :label "futon1-d"}
                         {:path "/home/joe/code/futon1a" :label "futon1a-d"}
                         {:path "/home/joe/code/futon2"  :label "futon2-d"}
                         {:path "/home/joe/code/futon3"  :label "futon3-d"}
                         {:path "/home/joe/code/futon3a" :label "futon3a-d"}
                         {:path "/home/joe/code/futon3b" :label "futon3b-d"}
                         {:path "/home/joe/code/futon3c" :label "futon3c-d"}
                         {:path "/home/joe/code/futon4"  :label "futon4-elisp-d"}
                         {:path "/home/joe/code/futon5"  :label "futon5-d2"}
                         {:path "/home/joe/code/futon5a" :label "futon5a-d"}
                         {:path "/home/joe/code/futon6"  :label "futon6-py-d"}
                         {:path "/home/joe/code/futon7"  :label "futon7-d"}
                         {:path "/home/joe/code/futon7a" :label "futon7a-d"}]
                  interval-ms (config/env-int "FUTON3C_MULTI_WATCHER_INTERVAL_MS" 5000)
                  commit-ingest? (config/env-bool "FUTON3C_MULTI_WATCHER_COMMIT_INGEST" false)]
              (multi-watcher/start! {:roots roots
                                     :interval-ms interval-ms
                                     :cold-scan? false
                                     :commit-ingest? commit-ingest?})
              (cyder/register!
               {:id "multi-watcher"
                :type :daemon
                :stop-fn multi-watcher/stop!
                :state-fn #(or (multi-watcher/status nil) {:running? false})
                :metadata {:health/heartbeat? true
                           :cross-refs ["M-live-geometric-stack"
                                        "M-self-documenting-stack"]}})
              (println (str "[dev] multi-watcher started in-JVM (interval-ms="
                            interval-ms ", roots=" (count roots) ")")))
            (catch Throwable t
              (println (str "[dev] multi-watcher start threw: "
                            (.getName (class t)) ": " (.getMessage t)
                            " — boot continues."))))
        f5-sys (start-futon5!)
        _ (when f5-sys
            (cyder/register!
             {:id "futon5"
              :type :server
              :stop-fn (or (nonstarter-fn 'stop!) (fn []))
              :state-fn #(do {:port (config/env-int "FUTON5_PORT" 7072)})}))
        irc-sys (start-irc! evidence-store role)
        _ (reset! !irc-sys irc-sys)
        _ (when irc-sys
            (cyder/register!
             {:id "irc-server"
              :type :server
              :stop-fn (or (get-in irc-sys [:server :stop-fn])
                           (fn []))
              :state-fn #(let [s @!irc-sys]
                           {:port (:port s)
                            :relay-bridge? (boolean (:relay-bridge s))})}))
        _ (reg/set-on-invoke-complete! on-agent-invoke-complete!)
        _ (start-agents!)
        webarxana-sys (try
                        (start-webarxana!)
                        (catch Throwable t
                          (println (str "[dev] webarxana start threw: "
                                        (.getName (class t)) ": " (.getMessage t)
                                        " — boot continues."))
                          nil))
        _ (when webarxana-sys
            (cyder/register!
             {:id "webarxana"
              :type :server
              :stop-fn (:stop-fn webarxana-sys)
              :state-fn (:state-fn webarxana-sys)
              :metadata {:cross-refs ["VSATARCS" "M-interest-network-coupling"]}}))
        shadow-sys (try
                     (start-shadow!)
                     (catch Throwable t
                       (println (str "[dev] shadow start threw: "
                                     (.getName (class t)) ": " (.getMessage t)
                                     " — boot continues."))
                       nil))
        _ (when shadow-sys
            (println (str "[dev] embedded shadow-cljs started: "
                          (:builds shadow-sys) " → " (:result shadow-sys)))
            (cyder/register!
             {:id "shadow-cljs"
              :type :daemon
              :stop-fn (:stop-fn shadow-sys)
              :state-fn (:state-fn shadow-sys)
              :metadata {:cross-refs ["M-repl-wins-over-cli" "CLAUDE.md:I-0"]}}))
        _ (when (config/env-bool "FUTON3C_PROCESS_WATCHDOG_AUTOSTART" true)
            (start-process-watchdog!))
        _ (when (config/env-bool "FUTON3C_TICKLE_AUTOSTART" false)
            (start-tickle! {:auto-restart? true}))
        _ (when (config/env-bool "FUTON3C_FM_CONDUCTOR_AUTOSTART" false)
            (start-fm-conductor!))
        bridge-sys (start-drawbridge!)
        _ (when bridge-sys
            (cyder/register!
             {:id "drawbridge"
              :type :server
              :stop-fn (or (:stop bridge-sys) (fn []))
              :state-fn #(do {:port (:port bridge-sys)
                              :bind (:bind bridge-sys)})}))
        _ (federation/configure-from-env!)
        _ (federation/install-hook!)
        _ (doseq [typed-id (reg/registered-agents)]
            (when-let [agent-record (reg/get-agent typed-id)]
              (federation/announce! agent-record)))
        fed-sync-results (federation/sync-peers!)
        fed-peers (federation/peers)
        fed-self (federation/self-url)
        mission-count (cyder/register-missions!)
        _ (add-watch cyder/!processes :blackboard
                     (fn [_ _ _ new-val]
                       (bb/project-processes!
                        (sort-by :process/id (vals new-val)))))
        _ (start-agents-blackboard-ticker! 5000)
        registry-status (reg/registry-status)
        agent-summary (agent-availability-summary registry-status)
        _ (bb/project-agents! registry-status)
        _ (bb/project-processes! (sort-by :process/id (vals @cyder/!processes)))]
    (println)
    (println (str "[dev] Role: " (name role)
                  " (" (:source role-info) ")"
                  " | agents: " (:registered agent-summary) " registered"
                  " (" (:invocable agent-summary) " invocable: "
                  (:local agent-summary) " local, "
                  (:ws agent-summary) " ws, "
                  (:unreachable agent-summary) " unreachable)"
                  " | CYDER: " (count (cyder/list-processes)) " processes"
                  " (" mission-count " missions)"))
    (println)
    (println "[dev] Evidence API (futon3c transport → XTDB backend)")
    (println "[dev]   POST /api/alpha/invoke             — invoke registered agent")
    (println "[dev]   POST /api/alpha/mission-control    — portfolio review, sessions, step")
    (println "[dev]   GET  /api/alpha/missions           — cross-repo mission inventory")
    (println "[dev]   GET  /api/alpha/missions/:id       — mission detail + wiring")
    (println "[dev]   GET  /api/alpha/missions/:id/wiring — per-mission wiring diagram")
    (println "[dev]   GET  /api/alpha/evidence          — query entries")
    (println "[dev]   GET  /api/alpha/evidence/:id       — single entry")
    (println "[dev]   GET  /api/alpha/evidence/:id/chain — reply chain")
    (println "[dev]   POST /api/alpha/evidence          — append entry")
    (println "[dev]   POST /api/alpha/agents            — register agent")
    (println "[dev]   GET  /api/alpha/agents            — list agents")
    (println "[dev]   GET  /api/alpha/processes         — CYDER: list processes")
    (println "[dev]   GET  /api/alpha/processes/:id     — CYDER: inspect process")
    (println "[dev]   DELETE /api/alpha/processes/:id   — CYDER: stop process")
    (println "[dev]   POST /api/alpha/portfolio/step    — AIF portfolio step")
    (println "[dev]   POST /api/alpha/portfolio/heartbeat — weekly heartbeat")
    (println "[dev]   GET  /api/alpha/portfolio/state   — portfolio belief state")
    (when f5-sys
      (println)
      (println "[dev] futon5 Heartbeat API (portfolio bid/clear persistence)")
      (println "[dev]   GET  /api/heartbeat              — current week heartbeat")
      (println "[dev]   POST /api/heartbeat/bid          — record intended actions")
      (println "[dev]   POST /api/heartbeat/clear        — record actual actions"))
    (println)
    (when irc-sys
      (println (str "[dev]   Connect IRC: irssi -c localhost -p " (:port irc-sys) " -n joe"))
      (println "[dev]   Agents auto-join #futon on WS connect")
      (println))
    (if (seq fed-peers)
      (do (println (str "[dev] Federation: self=" fed-self " peers=" fed-peers))
          (when (seq fed-sync-results)
            (println (str "[dev]   Peer sync results: " fed-sync-results)))
          (println "[dev]   Agents registered locally will be announced to peers."))
      (println "[dev] Federation: no peers configured (set FUTON3C_PEERS, FUTON3C_SELF_URL)"))
    (println)
    (when-let [port (config/env-int "FUTON3C_PORT" 7070)]
      (when (pos? port)
        (println
         (str "[dev] Invoke: curl --max-time "
              (config/direct-invoke-timeout-seconds)
              " -X POST http://localhost:"
              port
              "/api/alpha/invoke \\"))))
    (println "[dev]   -H 'Content-Type: application/json' \\")
    (println "[dev]   -d '{\"agent-id\":\"claude-1\",\"prompt\":\"hello\"}'")
    (println "[dev]   -d '{\"agent-id\":\"codex-1\",\"prompt\":\"hello\"}'")
    (println)
    (println "[dev] REPL helpers (Drawbridge or nREPL):")
    (println "[dev]   (require '[futon3c.dev :as dev])")
    (println "[dev]   (dev/restart-agents!)                  — restart WS+agents (IRC stays up)")
    (println "[dev]   (dev/stop-agents!)                     — stop WS+agents only")
    (println "[dev]   (dev/start-agents!)                    — start WS+agents only")
    (println "[dev]   (dev/start-tickle!)                    — start watchdog (auto-restart on)")
    (println "[dev]   (dev/start-tickle! {:auto-restart? false}) — watchdog without restart")
    (println "[dev]   (dev/stop-tickle!)                     — stop watchdog")
    (println "[dev]   (dev/start-process-watchdog!)          — start infra process watchdog")
    (println "[dev]   (dev/stop-process-watchdog!)           — stop infra process watchdog")
    (println "[dev]   (dev/status)                           — runtime summary")
    (println)
    (println "[dev] CT work queue (PlanetMath wiring extraction):")
    (println "[dev]   (dev/ct-progress!)                       — queue status (N/313)")
    (println "[dev]   (dev/run-ct-entry!)                      — process next entry")
    (println "[dev]   (dev/run-ct-entry! :entity-id \"pm-ct-FunctorCategory\")")
    (println "[dev]   (dev/run-ct-batch! :n 50)                — overnight batch (50 entries)")
    (println "[dev]   (dev/run-ct-batch! :n 313)               — full corpus")
    (println)
    (println "[dev] Mission control service:")
    (println "[dev]   (require '[futon3c.mission-control.service :as mcs])")
    (println "[dev]   (mcs/list-sessions)")
    (println "[dev]   (mcs/run-review! {:author \"joe\"})")
    (println)
    (when-let [port (config/env-int "FUTON3C_DRAWBRIDGE_PORT" 6768)]
      (when (pos? port)
        (println (str "[dev] drawbridge: http://"
                      (config/env "FUTON3C_DRAWBRIDGE_BIND" "127.0.0.1")
                      ":" port "/repl"
                      " (allow: " (config/env-list "FUTON3C_DRAWBRIDGE_ALLOW" ["127.0.0.1" "::1"]) ")"))))
    (println)
    (println "[dev] Press Ctrl-C to stop.")
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      ^Runnable
      (fn []
        (println "\n[dev] Shutting down...")
        (remove-watch cyder/!processes :blackboard)
        (reset! cyder/!processes {})
        (try (multi-watcher/stop!)
             (catch Throwable _))
        ;; Drain embedded shadow-cljs FIRST so its runtime-loop sees a
        ;; clean stop signal while the JVM thread pools are still healthy.
        ;; Without this, shadow.remote.runtime.clj.local/runtime_loop races
        ;; JVM-level executor shutdown and throws RejectedExecutionException
        ;; from async-thread-macro-3 after our shutdown sequence completes.
        ;; No-op when shadow was never started (requiring-resolve → nil).
        (try (when-let [stop-shadow (requiring-resolve 'futon3c.dev.shadow/stop!)]
               (stop-shadow))
             (catch Throwable _))
        (stop-agents!)
        (when-let [stop (:stop bridge-sys)] (stop))
        (when-let [irc @!irc-sys]
          (when-let [stop (:stop-fn (:server irc))] (stop)))
        (when f5-sys
          (when-let [stop! (nonstarter-fn 'stop!)]
            (stop! f5-sys)))
        (when-let [f1 @!f1-sys]
          ((:stop! f1)))
        (println "[dev] Stopped."))))
    @(promise)))
