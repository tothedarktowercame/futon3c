(ns futon3c.agency.roster-store
  "Durable agent roster snapshot for desktop-save restore.

   The store persists only the payload needed by /agents/restore. It never
   serializes invoke functions, live processes, or reset callbacks. Persistence
   is continuous: callers install a registry watch and each registry mutation
   rewrites the EDN snapshot so kill -9 still leaves the latest roster on disk."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Instant]))

(def ^:private roster-version 1)
(def ^:private registry-watch-key ::persist-roster)
(def ^:private roster-drop-tolerance 1)

(declare load-roster)

(defn roster-store-path []
  (or (System/getProperty "FUTON3C_AGENT_ROSTER_FILE")
      (System/getenv "FUTON3C_AGENT_ROSTER_FILE")
      "/tmp/futon3c-agent-roster.edn"))

(defn restore-enabled?
  "Boot-time restore flag.  Default TRUE (2026-06-14): the roster restores on boot
   unless explicitly disabled.  Robust against the launch env not propagating
   FUTON3C_AGENT_RESTORE (e.g. an OOM-resume) — restore still happens.  Use a
   `--fresh` boot (which sets FUTON3C_AGENT_RESTORE=false) for a deliberate reset."
  []
  (let [prop (System/getProperty "FUTON3C_AGENT_RESTORE")]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (if-let [s (System/getenv "FUTON3C_AGENT_RESTORE")]
        (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim s))))
        true))))

(defn- now [] (str (Instant/now)))

(defn- blank->nil [x]
  (some-> x str str/trim not-empty))

(defn- truthy-config?
  [k]
  (let [prop (System/getProperty k)]
    (if (some? prop)
      (#{"1" "true" "yes" "on"} (str/lower-case (str/trim prop)))
      (when-let [s (System/getenv k)]
        (#{"1" "true" "yes" "on"} (str/lower-case (str/trim s)))))))

(defn allow-drop?
  "Explicit escape hatch for sanctioned bulk roster shrinkage."
  []
  (boolean (truthy-config? "FUTON3C_ROSTER_ALLOW_DROP")))

(defn- agent-id-value [agent]
  (let [id (:agent/id agent)]
    (cond
      (map? id) (blank->nil (:id/value id))
      (some? id) (blank->nil id)
      :else nil)))

(defn- metadata-value [metadata k]
  (or (get metadata k)
      (get metadata (name k))))

(defn restore-payload
  "Project one registry record to its durable /agents/restore payload."
  [agent]
  (let [metadata (or (:agent/metadata agent) {})
        agent-id (agent-id-value agent)
        agent-type (:agent/type agent)
        contracts (or (metadata-value metadata :agency/contracts) {})]
    (when (and agent-id agent-type)
      (cond-> {:agent-id agent-id
               :type agent-type}
        (:agent/session-id agent)
        (assoc :session-id (:agent/session-id agent))
        (blank->nil (metadata-value metadata :session-file))
        (assoc :session-file (blank->nil (metadata-value metadata :session-file)))
        (blank->nil (metadata-value metadata :cwd))
        (assoc :cwd (blank->nil (metadata-value metadata :cwd)))
        (blank->nil (metadata-value metadata :emacs-socket))
        (assoc :emacs-socket (blank->nil (metadata-value metadata :emacs-socket)))
        (blank->nil (metadata-value metadata :model))
        (assoc :model (blank->nil (metadata-value metadata :model)))
        (blank->nil (metadata-value metadata :campaign-id))
        (assoc :campaign-id (blank->nil (metadata-value metadata :campaign-id)))
        (blank->nil (metadata-value metadata :mission-id))
        (assoc :mission-id (blank->nil (metadata-value metadata :mission-id)))
        (blank->nil (metadata-value metadata :excursion-id))
        (assoc :excursion-id (blank->nil (metadata-value metadata :excursion-id)))
        (seq contracts)
        (assoc :agency/contracts contracts)
        (seq metadata)
        (assoc :metadata (dissoc metadata
                                 :remote? "remote?"
                                 :proxy? "proxy?"
                                 :remote-proxy? "remote-proxy?"
                                 :origin-url "origin-url"
                                 :ws-bridge? "ws-bridge?"
                                 :note "note"))))))

(defn roster-snapshot [registry]
  {:version roster-version
   :generated-at (now)
   :agents (->> (vals registry)
                (keep restore-payload)
                (sort-by :agent-id)
                vec)})

(defn- agent-count
  [snapshot]
  (count (or (:agents snapshot) [])))

(defn- on-disk-agent-count
  [path]
  (when (.exists (io/file path))
    (agent-count (load-roster path))))

(defn- unexpected-roster-drop?
  [prev-count next-count]
  (> (- prev-count next-count) roster-drop-tolerance))

(defn- refuse-roster-clobber?
  [path snapshot]
  (let [prev-count (or (on-disk-agent-count path) 0)
        next-count (agent-count snapshot)
        drop (- prev-count next-count)]
    (and (pos? drop)
         (unexpected-roster-drop? prev-count next-count)
         (not (allow-drop?))
         {:prev-count prev-count
          :next-count next-count
          :drop drop
          :tolerance roster-drop-tolerance})))

(defn persist-registry!
  "Persist REGISTRY as a durable roster EDN snapshot. Returns the snapshot."
  [registry]
  (let [snapshot (roster-snapshot registry)
        path (roster-store-path)
        file (io/file path)]
    (try
      (when-let [parent (.getParentFile file)]
        (.mkdirs parent))
      (if-let [{:keys [prev-count next-count drop tolerance]} (refuse-roster-clobber? path snapshot)]
        (do
          (println (str "[agent-roster] COUNTER-RATCHET REFUSED roster persist: "
                        "on-disk agents=" prev-count
                        " new agents=" next-count
                        " drop=" drop
                        " tolerance=" tolerance
                        " path=" path
                        " — durable snapshot preserved; set "
                        "FUTON3C_ROSTER_ALLOW_DROP=true only for intentional bulk removal"))
          (flush))
        (do
          ;; Rotate one backup before overwriting, so a clobber (e.g. a clean boot
          ;; that came up with fewer agents) leaves the prior snapshot recoverable at
          ;; <path>.bak. (M-agency-hardening roster safety, 2026-06-13.)
          (when (.exists file)
            (try (io/copy file (io/file (str path ".bak"))) (catch Throwable _)))
          (spit file (pr-str snapshot))))
      (catch Throwable t
        (println (str "[agent-roster] persist failed: " (.getMessage t)))
        (flush)))
    snapshot))

(defn load-roster
  ([] (load-roster (roster-store-path)))
  ([path]
   (let [file (io/file path)]
     (if (.exists file)
       (try
         (let [parsed (edn/read-string (slurp file))]
           (if (map? parsed)
             (update parsed :agents #(vec (or % [])))
             {:version roster-version :agents []}))
         (catch Throwable t
           (println (str "[agent-roster] load failed: " (.getMessage t)))
           (flush)
           {:version roster-version :agents []}))
       {:version roster-version :agents []}))))

(defn persist-enabled?
  "Whether continuous roster persistence is on.  Default TRUE.
   A clean/--fresh boot sets FUTON3C_ROSTER_PERSIST=false so the empty/minimal
   registry does NOT overwrite (clobber) the durable snapshot — the snapshot is
   preserved for a later non-fresh boot to restore.  (Distinct from
   FUTON3C_AGENT_RESTORE, which gates replay-on-boot.)"
  []
  (let [prop (System/getProperty "FUTON3C_ROSTER_PERSIST")]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (if-let [s (System/getenv "FUTON3C_ROSTER_PERSIST")]
        (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim s))))
        true))))

(defn install-registry-watch!
  "Install continuous roster persistence on REGISTRY-ATOM. Idempotent.
   No-op when persistence is disabled (FUTON3C_ROSTER_PERSIST=false, e.g. a
   --fresh boot) — so a clean session cannot clobber the durable snapshot, and a
   later non-fresh boot restores the preserved roster."
  [registry-atom]
  (if-not (persist-enabled?)
    (do (println "[agent-roster] persistence DISABLED (FUTON3C_ROSTER_PERSIST=false) — durable snapshot preserved, not overwritten")
        (flush)
        false)
    (do
      (remove-watch registry-atom registry-watch-key)
      (add-watch registry-atom registry-watch-key
                 (fn [_ _ _ new-state]
                   (persist-registry! new-state)))
      (persist-registry! @registry-atom)
      true)))

(defn restore-on-boot!
  "Replay the durable roster through RESTORE-FN when FUTON3C_AGENT_RESTORE is on.

   RESTORE-FN receives each restore payload plus :restored-detached? true. It
   should be idempotent for existing agent IDs. Returns a report map."
  [restore-fn]
  (if-not (restore-enabled?)
    {:enabled? false :attempted 0 :restored 0 :results []}
    (let [agents (:agents (load-roster))
          results (mapv (fn [payload]
                          (try
                            (restore-fn (assoc payload :restored-detached? true))
                            (catch Throwable t
                              {:ok false
                               :agent-id (:agent-id payload)
                               :err "restore-exception"
                               :message (.getMessage t)})))
                        agents)]
      {:enabled? true
       :attempted (count agents)
       :restored (count (filter :ok results))
       :results results})))
