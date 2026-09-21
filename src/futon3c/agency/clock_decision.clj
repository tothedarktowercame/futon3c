(ns futon3c.agency.clock-decision
  "Durable decisions at turn admission and on Claude editing activity.
   Decisions are append-only; an activity decision supplements the admission
   decision for the same turn. Publish RAM state only after verified persistence."
  (:require [clojure.java.io :as io]
            [futon3c.agency.clock-store :as clock]
            [futon3c.agency.clock-lineage :as lineage]
            [futon3c.evidence.backend]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend]
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

(defn- canonical [path] (.getCanonicalPath (io/file path)))

(defn- catalog
  []
  ;; Share the mission parser and configured roots with mission control. Resolve
  ;; at call time: that namespace also uses registry, which calls this boundary.
  (let [parse! (requiring-resolve 'futon3c.peripheral.mission-control-backend/parse-mission-path)
        roots (or *repo-roots*
                  (var-get (requiring-resolve
                            'futon3c.peripheral.mission-control-backend/default-repo-roots)))
        files (for [[_ root] roots
                    dir ["missions" "campaigns" "excursions"]
                    :let [folder (io/file root "holes" dir)]
                    :when (.isDirectory folder)
                    f (file-seq folder)
                    :when (and (.isFile f) (re-matches #"[CME]-.+\.md" (.getName f)))] f)
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
  (distinct (re-seq #"(?<![A-Za-z0-9_-])[MCE]-[A-Za-z0-9][A-Za-z0-9_-]*" (or text ""))))

(defn decide
  "Precedence: explicit target/mention, current session, attributed activity,
   typed absence. Target ambiguity never chooses the first filesystem hit."
  [{:keys [agent-id session-id mission-id text edited-path surface]}]
  (let [mentions (mentioned-ids text)
        ;; REPL payloads forward the buffer's OLD clock. A newly named target
        ;; in the operator text must supersede that carried value. For dispatch,
        ;; the explicit --mission option remains authoritative.
        operator? (contains? #{"emacs-repl" "emacs-claude-repl" "emacs-codex-repl"} surface)
        ids (cond (and operator? (seq mentions)) mentions
                  (seq mission-id) [mission-id]
                  :else mentions)
        current (clock/current-clock agent-id session-id)]
    (cond
      (seq ids)
      (let [targets (filterv #(some #{(:id %)} ids) (catalog))]
        (if (some #(not-any? #{%} (map :id targets)) ids)
          (unclocked :unresolvable-target {:targets (vec ids)})
          (choose-target targets 1 {:targets (vec ids)})))

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
