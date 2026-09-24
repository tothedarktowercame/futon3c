(ns futon3c.agency.clock-decision
  "Durable decisions at turn admission and on Claude editing activity.
   Decisions are append-only; an activity decision supplements the admission
   decision for the same turn. Publish RAM state only after verified persistence."
  (:require [clojure.java.io :as io]
            [futon3c.agency.clock-store :as clock]
            [futon3c.agency.clock-lineage :as lineage]
            [futon3c.evidence.backend]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as store])
  (:import [java.time Instant]
           [java.util UUID]
           [futon3c.evidence.backend AtomBackend]
           [futon3c.evidence.futon1b_backend Futon1bBackend]))

(def ^:dynamic *test-store* nil)
(def ^:dynamic *repo-roots* nil)
(def ^:dynamic *turn* nil)
(defonce ^:private !catalog (atom nil))
(defonce ^:private !active-turns (atom {}))

(defn end!
  "Release an invocation's callback context without changing its clock."
  [context]
  (swap! !active-turns dissoc [(:agent-id context) (:turn-id context)]))

(defn evidence-store
  [supplied]
  (let [backend (or supplied *test-store*
                    (when-let [n (find-ns 'futon3c.dev)]
                      (when-let [v (ns-resolve n '!evidence-store)]
                        @(var-get v))))]
    (if (or (instance? Futon1bBackend backend)
            (and *test-store*
                 (or (instance? clojure.lang.IAtom backend)
                     (instance? AtomBackend backend))))
      backend
      (throw (ex-info "Clock decisions require a durable evidence backend"
                      {:error/code :clock/non-durable-backend})))))

(defn- recovery-rows
  [backend query]
  ;; Count the SQL-pushed predicates WITHOUT tags: tags are post-filtered in
  ;; futon1b. Bound the entire sorted input, not just the matching decisions.
  ;; 10k rows also fit the backend's 20-page request budget. Split wider ranges
  ;; before any cursor walk; no page can encounter XTDB's 102,400-row spill.
  (letfn [(read-window [q lo hi]
            (if (> (store/count* backend (assoc (dissoc q :query/tags)
                                                    :query/include-ephemeral? true)) 10000)
              (let [mid (quot (+ lo hi) 2)]
                (when (or (= mid lo) (= mid hi))
                  (throw (ex-info "Clock recovery window cannot be safely subdivided"
                                  {:error/code :clock/recovery-dense-window :query q})))
                (let [at (str (Instant/ofEpochMilli mid))]
                  (into (read-window (assoc q :query/before at) lo mid)
                        (read-window (assoc q :query/since at) mid hi))))
              (let [rows (store/query* backend q)]
                (when (f1b/partial-result? rows)
                  (throw (ex-info "Incomplete clock recovery evidence"
                                  {:error/code :clock/recovery-incomplete :query q})))
                rows)))]
    ;; Uncached: a reconstructed client must observe external writers too.
    (binding [f1b/*query-cache-enabled* false]
      (read-window query 0 (.toEpochMilli (Instant/parse "9999-12-31T23:59:59Z"))))))

(defn restore!
  "Restore latest durable decision per exact (agent, session), including none.
   With two arguments restore all sessions of one agent; three narrow to a
   non-nil session. Nil session decisions use turn IDs in the envelope, so they
   must be found by author and then selected by their body session identity.
   Gather and validate every result before publishing any in-memory state."
  ([supplied agent-id] (restore! supplied agent-id ::all))
  ([supplied agent-id session-id]
   (when-not (seq agent-id)
     (throw (ex-info "Clock recovery requires an agent"
                     {:error/code :clock/missing-identity})))
   (let [backend (evidence-store supplied)
         query (cond-> {:query/author agent-id :query/tags [:clock-decision]}
                 (and session-id (not= ::all session-id))
                 (assoc :query/session-id session-id))
         rows (recovery-rows backend query)
         decisions (->> rows (map :evidence/body)
                        (filter #(and (= agent-id (:agent-id %))
                                      (or (= ::all session-id)
                                          (= session-id (:session-id %))))))
         latest (reduce (fn [acc d]
                          (when-not (and (contains? #{:clocked :unclocked} (:status d))
                                         (string? (:decision-id d)))
                            (throw (ex-info "Invalid durable clock decision"
                                            {:error/code :clock/recovery-invalid-decision
                                             :decision d})))
                          (let [order (clock/decision-order d)
                                key [(:agent-id d) (:session-id d)]
                                prior (get acc key)]
                            (if (or (nil? prior)
                                    (pos? (compare order (clock/decision-order prior))))
                              (assoc acc key d) acc)))
                        {} decisions)]
     (doseq [[[aid sid] d] latest]
       (clock/set-decision! aid sid
                            (if (= :unclocked (:status d))
                              (assoc d :clock (clock/empty-clock)) d)))
     {:restored (count latest)})))

(defn restore-registered!
  "Rebuild all saved sessions for currently registered agents. Called after
   roster restoration and when constructing an HTTP client/handler."
  [supplied]
  (let [names ((requiring-resolve 'futon3c.agency.registry/addressable-names))]
    (reduce (fn [n aid] (+ n (:restored (restore! supplied aid)))) 0 names)))

(defn dispatch-inheritance
  "Capture the registered caller's positive decision at dispatch time.
   Completion replies never carry a clock back to the original caller."
  [backend caller surface]
  (when (and (seq caller) (not= "auto-bellback" surface))
    (when-let [agent ((requiring-resolve 'futon3c.agency.registry/get-agent) caller)]
      (let [aid (get-in agent [:agent/id :id/value])
            sid (:agent/session-id agent)
            _ (when-not (clock/stored-state aid sid) (restore! backend aid sid))
            d (:decision (clock/current-state aid sid))]
        (when (and (= :clocked (:status d)) (some val (:clock d)))
          {:clock (:clock d)
           :evidence {:caller-id aid :caller-decision-id (:decision-id d)}})))))

(defn- canonical [path] (.getCanonicalPath (io/file path)))

(defn- catalog
  []
  ;; Share the mission parser and configured roots with mission control. Resolve
  ;; at call time: that namespace also uses registry, which calls this boundary.
  (let [parse! (requiring-resolve 'futon3c.peripheral.mission-control-backend/parse-mission-path)
        roots (or *repo-roots*
                  (var-get (requiring-resolve
                            'futon3c.peripheral.mission-control-backend/default-repo-roots)))
        ;; Mission control reads watcher-ingested substrate documents. Reuse
        ;; that intake rule for top-level holes/ documents (no recursive walk).
        top-level-doc? (apply some-fn
                              (map requiring-resolve
                                   ['futon3c.watcher.file-ingest/mission-doc-path?
                                    'futon3c.watcher.file-ingest/excursion-doc-path?
                                    'futon3c.watcher.file-ingest/campaign-doc-path?]))
        top-level (for [[_ root] roots
                        f (.listFiles (io/file root "holes"))
                        :when (and (.isFile f)
                                   (or (top-level-doc? (.getPath f))
                                       (re-matches #"T-.+\.md" (.getName f))))] f)
        nested (for [[_ root] roots
                    dir ["missions" "campaigns" "excursions" "tickets"]
                    :let [folder (io/file root "holes" dir)]
                    :when (.isDirectory folder)
                    f (file-seq folder)
                    :when (and (.isFile f) (re-matches #"[CMET]-.+\.md" (.getName f)))] f)
        files (concat top-level nested)
        signature (mapv (fn [f] [(canonical f) (.lastModified ^java.io.File f)
                                 (.length ^java.io.File f)]) files)]
    (if (= [roots signature] (:key @!catalog))
      (:targets @!catalog)
      (let [targets (mapv (fn [f]
                            (let [target (clock/resolve-clock-target-file (str f))
                                  parsed (parse! roots (str f) nil)]
                              (assoc target :paths (:mission/code-paths parsed)))) files)]
        (reset! !catalog {:key [roots signature] :targets targets})
        targets))))

(defn- unclocked [reason evidence]
  {:status :unclocked :reason reason :source 4
   :clock (clock/empty-clock) :evidence evidence})

(defn- choose-target [targets source evidence]
  (case (count targets)
    0 (unclocked :unresolvable-target evidence)
    1 {:status :clocked :source source :clock (:clock (first targets))
       :evidence evidence}
    (unclocked :ambiguous (assoc evidence :candidates (mapv :id targets)))))

(defn- mentioned-ids [text]
  (distinct (re-seq #"(?<![A-Za-z0-9_-])[MCET]-[A-Za-z0-9][A-Za-z0-9_-]*" (or text ""))))

(defn decide
  "Precedence: explicit target/mention, current session, attributed activity,
   typed absence. Target ambiguity never chooses the first filesystem hit."
  [{:keys [agent-id session-id mission-id text edited-path surface inherited-clock phase]}]
  (let [mentions (mentioned-ids text)
        ;; REPL payloads forward the buffer's OLD clock. A newly named target
        ;; in the operator text must supersede that carried value. For dispatch,
        ;; the explicit --mission option remains authoritative.
        operator? (contains? #{"emacs-repl" "emacs-claude-repl" "emacs-codex-repl"} surface)
        current (clock/current-clock agent-id session-id)
        clocked? (some val current)
        ;; Bells and bellbacks mention mission names in passing, so there a
        ;; mention only fills an empty clock; operator turns (Joe's, and the
        ;; followup reminders posted as Joe) switch it (Joe, 2026-09-24).
        ids (cond (and operator? (seq mentions)) mentions
                  (seq mission-id) [mission-id]
                  clocked? nil
                  :else mentions)]
    (cond
      (seq ids)
      (let [targets (filterv #(some #{(:id %)} ids) (catalog))
            decided (if (some #(not-any? #{%} (map :id targets)) ids)
                      (unclocked :unresolvable-target {:targets (vec ids)})
                      (choose-target targets 1 {:targets (vec ids)}))]
        ;; A name that does not resolve, or several that do, keeps a current
        ;; clock instead of emptying it; the refusal stays in the evidence.
        ;; (claude-8, 2026-09-24: "M-f11" shorthand in a bell wiped a correct
        ;; E-cascade-real clock, so every requisition drew a reminder.)
        (if (and clocked? (= :unclocked (:status decided)))
          {:status :clocked :source 2 :clock current
           :evidence {:session-id session-id
                      :kept-despite (assoc (:evidence decided)
                                           :reason (:reason decided))}}
          decided))

      (and inherited-clock (not= :activity phase)
           (not= "auto-bellback" surface))
      {:status :clocked :source :inherited :clock (:clock inherited-clock)
       :evidence (:evidence inherited-clock)}

      (some val current)
      {:status :clocked :source 2 :clock current :evidence {:session-id session-id}}

      edited-path
      (let [path (canonical edited-path)
            targets (filterv
                     (fn [t]
                       (or (= path (:file t))
                           (some (fn [declared]
                                   (let [f (io/file declared)]
                                     ;; Canonical parser emits absolute paths.
                                     (and (.isAbsolute f)
                                          (.startsWith (.toPath (io/file path))
                                                       (.toPath (io/file (canonical f)))))))
                                 (:paths t))))
                     (catalog))]
        (if (seq targets)
          (choose-target targets 3 {:path path})
          (unclocked :no-source {:path path})))

      :else (unclocked :no-source {}))))

(defn- decision-id [{:keys [agent-id session-id turn-id phase event-id]}]
  (str "clock-decision-"
       (UUID/nameUUIDFromBytes
        (.getBytes (pr-str [agent-id session-id turn-id phase event-id]) "UTF-8"))))

(defn record!
  "Compute and durably append a decision. Stable event identity makes retries
   idempotent. Returns the persisted decision, then projects it to clock-store."
  [{:keys [agent-id turn-id session-id surface] :as context}]
  (when-not (and (seq agent-id) (seq turn-id) (seq surface))
    (throw (ex-info "Clock decision lacks turn identity"
                    {:error/code :clock/missing-identity
                     :agent-id agent-id :turn-id turn-id :surface surface})))
  (let [backend (evidence-store (:evidence-store context))
        _ (when-not (clock/stored-state agent-id session-id)
            (restore! backend agent-id session-id))
        eid (decision-id context)
        existing (store/get-entry* backend eid)
        decision (or (:evidence/body existing)
                     (merge (select-keys context [:agent-id :session-id :turn-id
                                                  :job-id :surface :phase :event-id])
                            {:decided-at (str (Instant/now)) :decision-id eid}
                            (decide context)))
        result (when-not existing
                 (boundary/append!
                  backend {:evidence/id eid
                           :evidence/subject {:ref/type :agent :ref/id agent-id}
                           :evidence/type :coordination :evidence/claim-type :step
                           :evidence/author agent-id :evidence/at (:decided-at decision)
                           :evidence/session-id (or session-id turn-id)
                           :evidence/tags [:clock-decision]
                           :evidence/body decision}))
        duplicate (when (= :duplicate-id (:error/code result))
                      (store/get-entry* backend eid))
        persisted (update (or (:evidence/body duplicate) decision)
                          :clock #(merge (clock/empty-clock) %))
        old-clock (clock/current-clock agent-id session-id)]
      (when (and (not existing) (not duplicate) (not (:ok result)))
        (throw (ex-info "Clock decision persistence failed"
                        {:error/code :clock/persistence-failed :receipt result})))
      (clock/set-decision! agent-id session-id persisted)
      ;; Preserve the existing mission graph projection. The decision evidence
      ;; above is authoritative; graph projection retains its canonical-node
      ;; guard and asynchronous behavior, using this backend's URL.
      (when (and (instance? Futon1bBackend backend)
                 (= :clocked (:status persisted))
                 (not= old-clock (:clock persisted))
                 (= (:decision-id persisted)
                    (get-in (clock/current-state agent-id session-id) [:decision :decision-id])))
        (binding [lineage/*substrate-url* (:base-url backend)]
          (lineage/persist-clock! {:agent-id agent-id :session-id session-id
                                  :new-clock (:clock persisted)
                                  :witness {:rule "clock-decision"
                                            :decision-id (:decision-id persisted)
                                            :source (:source persisted)
                                            :evidence (:evidence persisted)}})))
      persisted))

(defn start!
  [agent-id session-id prompt options]
  (let [text (if (map? prompt)
               (str (or (:prompt prompt) (get prompt "prompt")
                        (:text prompt) (get prompt "text") ""))
               (str prompt))
        surface (or (:surface options) (second (re-find #"(?m)^Surface: ([^\r\n]+)" text)) "invoke")
        id (or (:dispatch-id options) (:turn-id options)
               (second (re-find #"(?m)^Edge: ([^\r\n]+)" text))
               (str (UUID/randomUUID)))
        context {:agent-id agent-id :session-id session-id :turn-id id
                 :clock-error (atom nil)
                 :job-id (:dispatch-id options) :surface surface
                 :phase :accepted :text text
                 :inherited-clock (:inherited-clock options)
                 :mission-id (or (:mission-id options)
                                 (when (map? prompt)
                                   (or (:mission-id prompt) (get prompt "mission-id"))))
                 :evidence-store (:evidence-store options)}]
    (record! context)
    (swap! !active-turns assoc [agent-id id] context)
    context))

(defn- callback-context
  [agent-id session-id]
  (or *turn*
      ;; Pouch demultiplexers can call an existing (pre-reload) callback on a
      ;; reader thread. Recover only a unique matching invocation; never guess
      ;; between overlapping turns. New callbacks convey the exact binding.
      (let [contexts (filter #(and (= agent-id (:agent-id %))
                                   (or (nil? (:session-id %))
                                       (= session-id (:session-id %))))
                             (vals @!active-turns))]
        (when (> (count contexts) 1)
          (throw (ex-info "Ambiguous clock activity turn"
                          {:error/code :clock/ambiguous-turn-context})))
        (first contexts))))

(defn record-tool-use!
  "Consume the existing Claude tool-detail feed. A qualifying activity decision
   is durable even when it occurs after the last operator input of a session."
  [agent-id session-id detail]
  (let [input (or (:input detail) (get detail "input"))
        path (or (:file_path input) (get input "file_path"))
        tool (or (:name detail) (get detail "name"))]
    (when (and (clock/editable-tool? tool) path)
      (when-let [context (callback-context agent-id session-id)]
        (when (= agent-id (:agent-id context))
          (try
            (record! (assoc context :session-id session-id :phase :activity
                            :event-id (or (:id detail) (get detail "id") (str (UUID/randomUUID)))
                            :edited-path path))
            (catch Exception e
              ;; Pouch callbacks intentionally swallow consumer exceptions.
              ;; Keep this failure on the invocation so finish! cannot report
              ;; a successful session after losing its activity decision.
              (when-let [error (:clock-error context)] (reset! error e))
              (throw e))))))))

(defn finish!
  "Attach the runtime's resolved session identity when admission preceded it."
  [session-id]
  (when *turn*
    (try
      (when-let [error (some-> (:clock-error *turn*) deref)]
        (throw (ex-info "Clock activity decision failed"
                        {:error/code :clock/activity-decision-failed} error)))
      (when (and session-id (not= session-id (:session-id *turn*)))
        (record! (assoc *turn* :session-id session-id :phase :session-resolved)))
      (finally (end! *turn*)))))
