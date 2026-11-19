(ns futon3c.agency.clock-decision
  "Durable decisions at turn admission and on Claude editing activity.
   Decisions are append-only; an activity decision supplements the admission
   decision for the same turn. Publish RAM state only after verified persistence:
   in futon1b, or, while futon1b is down, in the local outbox (see `spool!`)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.agency.clock-store :as clock]
            [futon3c.agency.clock-lineage :as lineage]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.item-inventory :as inventory]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.circus.operator-focus :as operator-focus]
            [futon3c.evidence.backend]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as store])
  (:import [java.time Instant]
           [java.util UUID]
           [futon3c.evidence.backend AtomBackend]))

(def ^:dynamic *test-store* nil)
(def ^:dynamic *repo-roots* nil)
(def ^:dynamic *turn* nil)
(defonce ^:private !catalog (atom nil))
(defonce ^:private !active-turns (atom {}))
(defonce ^:private !supersession-cache (atom {}))

(def ^:dynamic *successor-resolver* nil)

(defn reset-supersession-cache! [] (reset! !supersession-cache {}))

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
    (if (or (f1b/futon1b-backend? backend)
            (and *test-store*
                 (or (instance? clojure.lang.IAtom backend)
                     (instance? AtomBackend backend))))
      backend
      (throw (ex-info "Clock decisions require a durable evidence backend"
                      {:error/code :clock/non-durable-backend})))))

;; futon1b being busy is not a clock-decision problem, and until 2026-09-28
;; it surfaced in agents' buffers as "Clock decision persistence failed" or
;; "futon1b read did not obtain evidence". Name the cause and what it means for
;; the turn; keep the clock-decision message for a write the store refuses.
(def ^:private busy-read-codes
  #{:futon1b-read-timeout :futon1b-unreachable :futon1b-read-unavailable})

(def ^:private busy-write-codes
  #{:store-unavailable :store-timeout :store-unreachable})

(defn- busy-write?
  "A write receipt that failed because futon1b was busy. The boundary turns an
  exception thrown inside an append (its read-back included) into
  :error/code :exception and keeps the thrown code under :invariant/violation,
  so a busy read there arrives as a nested :futon1b-read-unavailable."
  [result]
  (or (busy-write-codes (:error/code result))
      (busy-read-codes (get-in result [:invariant/violation :error/code]))))

(defn- store-down?
  "True when futon1b refused the connection, as opposed to answering slowly.
  A read that could not connect is :futon1b-unreachable with acquired outcome
  :unavailable (a timeout is :timeout); a write is :store-unreachable."
  [data]
  (or (= :unavailable (get-in data [:read :transport/acquired-outcome]))
      (= :store-unreachable (get-in data [:receipt :error/code]))
      (= :unavailable (get-in data [:receipt :invariant/violation
                                     :transport/acquired-outcome]))))

(defn- store-busy-error [what cause-message data cause]
  (ex-info (str "Turn not started: futon1b was busy while " what
                " (" cause-message "). Nothing was recorded, so sending the"
                " message again is safe.")
           (assoc data :error/code :clock/store-busy
                  :clock/store-down? (store-down? data))
           cause))

(def ^:dynamic *fail-fast-when-down*
  "Bound true by `record!` when the decision can be spooled to the outbox: a
  futon1b that refuses connections is then reported at once instead of being
  waited out on the busy schedule (2026-11-08: futon1b was down 08:09-15:18;
  waiting ten minutes per turn and then refusing lost a finished Codex job's
  completion and a park wake)."
  false)

(def ^:dynamic *store-busy-waits-ms*
  "Pauses between attempts to record a turn's admission decision while futon1b
  is busy; about ten minutes in all. On 2026-10-03 a 504 burst refused a
  codex-18 turn outright (\"Turn not started: futon1b was busy\"): the operator's
  message was dropped, though a turn is meant to wait its place, not give up
  because the store is momentarily saturated. `record!` is idempotent on a
  stable decision id, so retrying is safe; only after the whole schedule is the
  turn refused."
  [2000 5000 10000 20000 30000 60000 60000 60000 60000 60000 60000 60000 60000 60000])

(def ^:dynamic *busy-budget* nil)

(defn with-store-busy-budget
  "One retry allowance across nested clock reads/writes. Ingress supplies a
   short deadline and no retries; internal operations retain the wait schedule."
  ([f] (with-store-busy-budget {} f))
  ([{:keys [waits timeout-ms on-wait] :or {waits *store-busy-waits-ms*} :as options} f]
   (let [requested-deadline (when timeout-ms (+ (System/nanoTime) (* 1000000 timeout-ms)))
         inherited-deadline (or (:deadline *busy-budget*) f1b/*read-deadline-ns*)
         deadline (when-let [ds (seq (keep identity [requested-deadline inherited-deadline]))]
                    (reduce min ds))
         budget (assoc (or *busy-budget* {:waits (atom (seq waits))}) :deadline deadline)
         ;; An ingress nested inside a longer operation may tighten, never
         ;; renew, the shared retry allowance.
         _ (when (and *busy-budget* (contains? options :waits))
             (swap! (:waits budget) #(seq (take (count waits) %))))]
     (binding [*busy-budget* budget
               f1b/*read-deadline-ns* (or (:deadline budget) f1b/*read-deadline-ns*)]
       (letfn [(check-deadline! []
                 (when (and (:deadline budget) (>= (System/nanoTime) (:deadline budget)))
                   (throw (ex-info "Clock store operation deadline exhausted"
                                   {:error/code :clock/store-busy :clock/budget-exhausted? true}))))]
         (loop []
           (check-deadline!)
           (let [outcome (try
                           (let [v (f)] (check-deadline!) {:value v})
                           (catch clojure.lang.ExceptionInfo e
                             (let [code (:error/code (ex-data e))]
                               (cond
                                 (busy-read-codes code)
                                 {:busy (store-busy-error "reading this turn's clock decision"
                                                         (ex-message e) {:read (ex-data e)} e)}
                                 (= :clock/store-busy code) {:busy e}
                                 :else (throw e)))))]
             (if-let [e (:busy outcome)]
               (if-let [wait-ms (when-not (and *fail-fast-when-down*
                                               (:clock/store-down? (ex-data e)))
                                  (first @(:waits budget)))]
                 (do
                   (swap! (:waits budget) next)
                   (check-deadline!)
                   (when on-wait (on-wait wait-ms e))
                   (Thread/sleep (long (if-let [deadline (:deadline budget)]
                                        (min wait-ms (max 0 (quot (- deadline (System/nanoTime)) 1000000)))
                                        wait-ms)))
                   (recur))
                 (throw e))
               (:value outcome)))))))))

(defn connection-refused-read?
  "True only for an unreachable backend read caused by ConnectException.
   Inspect the original cause chain: store-down? also includes HTTP overload."
  [error]
  (boolean
   (some (fn [cause]
           (and (= :futon1b-unreachable (:error/code (ex-data cause)))
                (some #(instance? java.net.ConnectException %)
                      (take-while some? (iterate ex-cause (ex-cause cause))))))
         (take-while some? (iterate ex-cause error)))))

(def ^:dynamic *ingress-restart-wait-ms*
  "Total ingress restart deadline; never renewed by probes."
  (Long/parseLong (or (System/getenv "FUTON3C_INGRESS_RESTART_WAIT_MS") "90000")))

(defn with-ingress-restart-budget
  "Keep normal reads at five seconds/no retries. Only connection refusal
   permits another probe, under one outer deadline (and any inherited deadline).
   Nested clock operations share an empty retry allowance; this loop alone
   schedules restart probes. Checks after reads prevent late job creation."
  [f]
  (with-store-busy-budget
    {:waits [] :timeout-ms *ingress-restart-wait-ms*}
    (fn []
      (loop [pause-ms 1000]
        (let [result (try
                       {:value (with-store-busy-budget {:waits [] :timeout-ms 5000} f)}
                       (catch clojure.lang.ExceptionInfo e {:error e}))]
          (if-let [e (:error result)]
            (if (connection-refused-read? e)
              (let [remaining-ms (quot (- (:deadline *busy-budget*) (System/nanoTime)) 1000000)]
                (if (pos? remaining-ms)
                  (do (Thread/sleep (long (min pause-ms remaining-ms)))
                      ;; The budget checks before the next read as well as after it.
                      (recur (min 5000 (* 2 pause-ms))))
                  (throw e)))
              (throw e))
            (:value result)))))))

(defn- busy-read [f] (with-store-busy-budget f))

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
   (with-store-busy-budget
    (fn []
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
     {:restored (count latest)})))))

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

(defn- operator-words
  "Return the operator-authored suffix of a harness turn envelope. Older
   callers pass bare text, so an absent marker deliberately preserves the
   whole value."
  [text]
  (let [text (or text "")]
    (or (second (re-find #"(?s)(?:^|\r?\n)User message:[ \t]*\r?\n(.*)\z" text))
        text)))

(defn- requisition-target [text]
  (let [targets (->> (re-seq #"(?m)^Requisition:[ \\t]*([MCET]-[A-Za-z0-9][A-Za-z0-9_-]*)[ \\t]*(?:—|--)[ \\t]*\\S.*$"
                              (or text ""))
                     (map second)
                     distinct)]
    (when (= 1 (count targets)) (first targets))))

(defn- requisition-line?
  "True for the ordinary human-written Requisition line.  This is provenance
   only; `requisition-target` remains the selection parser for compatibility."
  [text]
  (boolean (re-find #"(?m)^Requisition:[ \t]*[MCET]-[A-Za-z0-9][A-Za-z0-9_-]*[ \t]*(?:—|--)[ \t]*\S.*$"
                    (or text ""))))

(defn- target-sha [target]
  (or (get-in target [:source :sha256])
      (when-let [file (:file target)]
        (digest/sha256-hex (java.nio.file.Files/readAllBytes (.toPath (io/file file)))))))

(defn- clock-for-id [id]
  (case (first (str id))
    \M {:campaign-id nil :mission-id id :excursion-id nil}
    \E {:campaign-id nil :mission-id nil :excursion-id id}
    \T {:campaign-id nil :mission-id nil :excursion-id id}
    nil))

(defn- inventory-targets []
  (->> (inventory/current-items)
       (keep (fn [item]
               (when-let [clock (clock-for-id (:id item))]
                 {:id (:id item) :file (:path item) :source (:source item)
                  :clock clock})))
       vec))

(defn- cache-current? [{:keys [shas]}]
  (every? (fn [[path sha]]
            (and (.isFile (io/file path))
                 (= sha (digest/sha256-hex
                         (java.nio.file.Files/readAllBytes (.toPath (io/file path)))))))
          shas))

(defn- read-supersession [target targets]
  (let [by-id (into {} (map (juxt :id identity)) targets)
        cached (get @!supersession-cache (:id target))]
    (if (and cached (cache-current? cached))
      cached
      (let [items (mapv (fn [t] {:id (:id t) :source {:sha256 (target-sha t)}}) targets)
            enriched (operator-focus/with-gcc-successors
                      (mentions/default-config) items [(:id target)])
            resolved (operator-focus/resolve-tag
                      (into {} (map (juxt :id identity)) enriched) (:id target))
            path (vec (or (:path resolved)
                          (get-in resolved [:notice :path])
                          [(:id target)]))
            path-targets (keep by-id path)
            entry {:shas (into {} (map (juxt :file target-sha)) path-targets)
                   :clocks (into {} (map (juxt :id :clock)) path-targets)
                   :result resolved}]
        (swap! !supersession-cache assoc (:id target) entry)
        entry))))

(defn- repoint-superseded [decision]
  (if-not (= :clocked (:status decision))
    decision
    (let [target-id (some val (:clock decision))]
      (try
        (let [cached (when-not *successor-resolver*
                       (let [entry (get @!supersession-cache target-id)]
                         (when (and entry (cache-current? entry)) entry)))
              targets (when-not cached
                        (if *successor-resolver* (catalog) (inventory-targets)))
              target (when targets (some #(when (= target-id (:id %)) %) targets))]
          (if (and (nil? cached) (nil? target))
            decision
            (let [entry (or cached
                            (if *successor-resolver*
                              {:result (*successor-resolver* target targets)
                               :clocks (into {} (map (juxt :id :clock)) targets)}
                              (read-supersession target targets)))
                resolved (:result entry)
                successor-id (:item resolved)
                successor-clock (get-in entry [:clocks successor-id])
                notice (:notice resolved)]
              (cond
                notice
                (update decision :evidence merge
                        (cond-> {:supersession-reason (:reason notice)}
                          (:path notice) (assoc :supersession-path (:path notice))))

                (and successor-clock (not= target-id successor-id))
                (-> decision
                    (assoc :clock successor-clock)
                    (update :evidence merge
                            {:superseded-from target-id
                             :supersession-path (:path resolved)}))

                :else decision))))
        (catch Throwable t
          (update decision :evidence merge
                  {:supersession-reason :supersession-unread
                   :supersession-error (.getMessage t)}))))))

(defn decide
  "Precedence: operator target, dispatched requisition, explicit mission,
   current session, attributed activity, typed absence. Target ambiguity never
   chooses the first filesystem hit."
  [{:keys [agent-id session-id mission-id text edited-path surface inherited-clock phase
           clock-manual]}]
  (let [operator? (contains? #{"emacs-repl" "emacs-claude-repl" "emacs-codex-repl"} surface)
        ;; The harness enriches an operator turn with retrieval and routing
        ;; headers. Those are context, not words the operator used.
        mentions (mentioned-ids (if operator? (operator-words text) text))
        requisition (requisition-target text)
        requisition-line (requisition-line? text)
        ;; REPL payloads forward the buffer's OLD clock. A newly named target
        ;; in the operator text must supersede that carried value. For dispatch,
        ;; an explicit requisition names the work before --mission or inheritance.
        current (clock/current-clock agent-id session-id)
        clocked? (some val current)
        ;; Bells and bellbacks mention mission names in passing, so there a
        ;; mention only fills an empty clock; operator turns (Joe's, and the
        ;; followup reminders posted as Joe) switch it (Joe, 2026-09-24).
        selection (cond (and operator? (seq mentions))
                        {:ids mentions :provenance :operator-words}
                        requisition {:ids [requisition] :provenance :requisition}
                  ;; On a REPL turn mission-id is the buffer's displayed OLD
                  ;; clock: it fills an empty session but cannot re-clock one.
                  ;; On non-operator surfaces it is explicit dispatch metadata
                  ;; (for example --mission), whose override contract remains.
                  ;; A manual clock-in (C-c C-m) marks its turn :clock-manual,
                  ;; and that turn's forwarded mission does re-clock.
                  (and (seq mission-id)
                       (or (not operator?) (not clocked?) clock-manual))
                  {:ids [mission-id]
                   :provenance (cond clock-manual :manual-clock-in
                                     operator? :forwarded-fill
                                     :else :dispatch-mission)}
                  clocked? {:ids nil :provenance :current}
                  ;; A bell or bellback mention filling an empty clock: not
                  ;; the operator's words (CL-89 review).
                  :else {:ids mentions :provenance (if requisition-line
                                                     :requisition
                                                     :message-mention)})
        ids (:ids selection)
        provenance (:provenance selection)]
    (repoint-superseded
     (cond
      (seq ids)
      (let [targets (filterv #(some #{(:id %)} ids) (catalog))
            decided (if (some #(not-any? #{%} (map :id targets)) ids)
                      (unclocked :unresolvable-target {:targets (vec ids)
                                                       :provenance provenance})
                      (choose-target targets 1 {:targets (vec ids)
                                                :provenance provenance}))]
        ;; A name that does not resolve, or several that do, keeps a current
        ;; clock instead of emptying it; the refusal stays in the evidence.
        ;; (claude-8, 2026-09-24: "M-f11" shorthand in a bell wiped a correct
        ;; E-cascade-real clock, so every requisition drew a reminder.)
        (if (and clocked? (= :unclocked (:status decided)))
          {:status :clocked :source 2 :clock current
           :evidence {:session-id session-id
                      :provenance :current
                      :kept-despite (assoc (:evidence decided)
                                           :reason (:reason decided))}}
          decided))

      (and inherited-clock (not= :activity phase)
           (not= "auto-bellback" surface))
      {:status :clocked :source :inherited :clock (:clock inherited-clock)
       :evidence (assoc (:evidence inherited-clock) :provenance :inherited)}

      (some val current)
      {:status :clocked :source 2 :clock current
       :evidence {:session-id session-id :provenance :current}}

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
          (choose-target targets 3 {:path path :provenance :edited-path})
          (unclocked :no-source {:path path})))

      :else (unclocked :no-source {})))))

(defn- decision-id [{:keys [agent-id session-id turn-id phase event-id]}]
  (str "clock-decision-"
       (UUID/nameUUIDFromBytes
        (.getBytes (pr-str [agent-id session-id turn-id phase event-id]) "UTF-8"))))

;; ---------------------------------------------------------------------------
;; Outbox: decisions made while futon1b is down
;;
;; A decision is bookkeeping about a turn, not the turn. When futon1b refuses
;; connections and this JVM already holds the session's clock, the decision is
;; written to a local file (write to temp, then atomic rename) and the turn
;; goes ahead. `replay-outbox!` appends the files to futon1b under their
;; original stable ids once it answers again; a duplicate id means an earlier
;; replay landed. Without a clock in RAM there is nothing safe to decide from,
;; so admission still refuses as before.

(def ^:dynamic *outbox-dir*
  (or (System/getenv "FUTON3C_CLOCK_OUTBOX")
      (str (System/getProperty "user.home") "/code/storage/clock-outbox")))

(defonce ^:private outbox-lock (Object.))

(defn- outbox-file [eid] (io/file *outbox-dir* (str eid ".edn")))

(defn- decision-envelope [agent-id session-id turn-id eid decision]
  {:evidence/id eid
   :evidence/subject {:ref/type :agent :ref/id agent-id}
   :evidence/type :coordination :evidence/claim-type :step
   :evidence/author agent-id :evidence/at (:decided-at decision)
   :evidence/session-id (or session-id turn-id)
   :evidence/tags [:clock-decision]
   :evidence/body decision})

(defn- write-outbox! [envelope]
  (locking outbox-lock
    (let [f (outbox-file (:evidence/id envelope))]
      (when-not (.exists f)
        (io/make-parents f)
        (let [tmp (io/file (str (.getPath f) ".tmp"))]
          (spit tmp (pr-str envelope))
          (java.nio.file.Files/move
           (.toPath tmp) (.toPath f)
           (into-array java.nio.file.CopyOption
                       [java.nio.file.StandardCopyOption/ATOMIC_MOVE]))))
      (edn/read-string (slurp f)))))

(defn outbox-entries
  "Envelopes waiting in the outbox, oldest decision first."
  []
  (locking outbox-lock
    (->> (.listFiles (io/file *outbox-dir*))
         (filter #(.endsWith (.getName ^java.io.File %) ".edn"))
         (map #(edn/read-string (slurp %)))
         (sort-by #(clock/decision-order (:evidence/body %)))
         vec)))

(defn replay-outbox!
  "Append outboxed decisions to BACKEND under their stable ids, deleting each
  file once futon1b holds it. Stops at the first failure and leaves the rest.
  Returns {:replayed n :remaining m}."
  [backend]
  (locking outbox-lock
    (let [entries (outbox-entries)
          replayed (reduce (fn [n envelope]
                             (let [r (boundary/append! backend envelope)]
                               (if (or (:ok r) (= :duplicate-id (:error/code r)))
                                 (do (io/delete-file (outbox-file (:evidence/id envelope)) true)
                                     (inc n))
                                 (reduced n))))
                           0 entries)]
      {:replayed replayed :remaining (- (count entries) replayed)})))

(defn- replay-outbox-quietly! [backend]
  (when (seq (.listFiles (io/file *outbox-dir*)))
    (try (let [{:keys [replayed remaining]} (replay-outbox! backend)]
           (when (pos? replayed)
             (println (str "[clock-decision] replayed " replayed
                           " outboxed decision(s) to futon1b; " remaining " left"))
             (flush)))
         (catch Throwable t
           (println (str "[clock-decision] outbox replay deferred: " (ex-message t)))
           (flush)))))

(defn- spool!
  "Decide from the clock this JVM holds and write the decision to the outbox."
  [{:keys [agent-id session-id turn-id] :as context} cause]
  (let [eid (decision-id context)
        decision (merge (select-keys context [:agent-id :session-id :turn-id
                                              :job-id :surface :phase :event-id])
                        {:decided-at (str (Instant/now)) :decision-id eid
                         :persisted-to :outbox}
                        (decide context))
        envelope (write-outbox! (decision-envelope agent-id session-id turn-id eid decision))
        persisted (update (:evidence/body envelope) :clock #(merge (clock/empty-clock) %))]
    (println (str "[clock-decision] futon1b down; decision " eid " for agent="
                  agent-id " turn=" turn-id " written to the outbox ("
                  (ex-message cause) ")"))
    (flush)
    (clock/set-decision! agent-id session-id persisted)
    persisted))

(declare record-durably!)

(defn record!
  "Compute and durably append a decision. Stable event identity makes retries
   idempotent. Returns the persisted decision, then projects it to clock-store.
   If futon1b is down and this JVM holds the session's clock, the decision goes
   to the outbox instead and the turn is not refused."
  [{:keys [agent-id turn-id session-id surface] :as context}]
  (when-not (and (seq agent-id) (seq turn-id) (seq surface))
    (throw (ex-info "Clock decision lacks turn identity"
                    {:error/code :clock/missing-identity
                     :agent-id agent-id :turn-id turn-id :surface surface})))
  (let [spoolable? (some? (clock/stored-state agent-id session-id))]
    (try
      (binding [*fail-fast-when-down* spoolable?]
        (record-durably! context))
      (catch clojure.lang.ExceptionInfo e
        (if (and spoolable?
                 (= :clock/store-busy (:error/code (ex-data e)))
                 (:clock/store-down? (ex-data e)))
          (spool! context e)
          (throw e))))))

(defn- record-durably!
  [{:keys [agent-id turn-id session-id] :as context}]
  (with-store-busy-budget
   (fn []
    (let [backend (evidence-store (:evidence-store context))
        _ (when-not (clock/stored-state agent-id session-id)
            (busy-read #(restore! backend agent-id session-id)))
        eid (decision-id context)
        existing (busy-read #(store/get-entry* backend eid))
        _ (replay-outbox-quietly! backend)
        decision (or (:evidence/body existing)
                     (merge (select-keys context [:agent-id :session-id :turn-id
                                                  :job-id :surface :phase :event-id])
                            {:decided-at (str (Instant/now)) :decision-id eid}
                            (decide context)))
        result (when-not existing
                 (boundary/append!
                  backend (decision-envelope agent-id session-id turn-id eid decision)))
        duplicate (when (= :duplicate-id (:error/code result))
                      (busy-read #(store/get-entry* backend eid)))
        persisted (update (or (:evidence/body duplicate) decision)
                          :clock #(merge (clock/empty-clock) %))
        old-clock (clock/current-clock agent-id session-id)]
      (when (and (not existing) (not duplicate) (not (:ok result)))
        (if (busy-write? result)
          (throw (store-busy-error "recording this turn's clock decision"
                                   (:error/message result) {:receipt result} nil))
          (throw (ex-info (str "Clock decision not recorded: the evidence store refused it ("
                               (some-> (:error/code result) name) ": "
                               (:error/message result) ")")
                          {:error/code :clock/persistence-failed :receipt result}))))
      (clock/set-decision! agent-id session-id persisted)
      ;; Preserve the existing mission graph projection. The decision evidence
      ;; above is authoritative; graph projection retains its canonical-node
      ;; guard and asynchronous behavior, using this backend's URL.
      (when (and (f1b/futon1b-backend? backend)
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
      persisted))))

(defn- note-waiting! [agent-id activity]
  (try
    (when-let [update! (requiring-resolve 'futon3c.agency.registry/update-invoke-activity!)]
      (update! agent-id activity))
    (catch Throwable _ nil)))

(defn- record-waiting-out-busy-store! [context]
  (with-store-busy-budget
    {:on-wait (fn [wait-ms _]
                (note-waiting! (:agent-id context)
                               (str "waiting for futon1b (busy), retry in "
                                    (quot wait-ms 1000) "s")))}
    #(record! context)))

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
                 :pending-activity (atom [])
                 :job-id (:dispatch-id options) :surface surface
                 :phase :accepted :text text
                 :inherited-clock (:inherited-clock options)
                 :mission-id (or (:mission-id options)
                                 (when (map? prompt)
                                   (or (:mission-id prompt) (get prompt "mission-id"))))
                 :evidence-store (:evidence-store options)}]
    ;; The turn's process is not started until this record lands, and a slow
    ;; futon1b read blocks here with no busy error, so the on-wait note never
    ;; fires. Say what the turn is waiting on before the first read: on
    ;; 2026-11-14 a claude-1 bell sat here 7+ minutes (futon1b at 7.9/8 GB)
    ;; while follow mode showed only "turn started" and "…".
    (note-waiting! agent-id "before the turn starts: recording its clock in futon1b")
    (record-waiting-out-busy-store! context)
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
          (let [activity (assoc context :session-id session-id :phase :activity
                                :event-id (or (:id detail) (get detail "id") (str (UUID/randomUUID)))
                                :edited-path path)]
            (try
              (record! activity)
              (catch Exception e
                (if (and (= :clock/store-busy (:error/code (ex-data e)))
                         (:pending-activity context))
                  ;; A busy futon1b has not lost the decision. Until 2026-10-04
                  ;; one 504 during an edit failed the whole job and dropped
                  ;; the agent's reply (7 Codex jobs, 10-02..10-04). finish!
                  ;; retries it on the admission schedule.
                  (do (println (str "[clock-decision] futon1b busy recording activity agent="
                                    agent-id " turn=" (:turn-id context) " path=" path
                                    "; deferred to turn finish"))
                      (flush)
                      (swap! (:pending-activity context) conj activity))
                  (do
                    ;; Pouch callbacks intentionally swallow consumer exceptions.
                    ;; Keep this failure on the invocation so finish! cannot report
                    ;; a successful session after losing its activity decision.
                    (when-let [error (:clock-error context)] (compare-and-set! error nil e))
                    (println (str "[clock-decision] activity decision failed agent=" agent-id
                                  " turn=" (:turn-id context) " path=" path ": "
                                  (ex-message e) " " (pr-str (ex-data e))))
                    (flush)
                    (throw e)))))))))))

(defn finish!
  "Attach the runtime's resolved session identity when admission preceded it."
  [session-id]
  (when *turn*
    (try
      (doseq [activity (some-> (:pending-activity *turn*) deref)]
        (try
          (record-waiting-out-busy-store! activity)
          (catch Exception e
            (compare-and-set! (:clock-error *turn*) nil e))))
      (when-let [error (some-> (:clock-error *turn*) deref)]
        (throw (ex-info (str "Clock activity decision failed: " (ex-message error))
                        {:error/code :clock/activity-decision-failed} error)))
      (when (and session-id (not= session-id (:session-id *turn*)))
        ;; Same bounded busy-wait as admission (3617cc30c): a busy store at the
        ;; session-resolved decision refused META diagnosis outright
        ;; (repair iteration 1, 2026-10-28).
        (record-waiting-out-busy-store!
         (assoc *turn* :session-id session-id :phase :session-resolved)))
      (finally (end! *turn*)))))
