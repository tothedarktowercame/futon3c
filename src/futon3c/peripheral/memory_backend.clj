(ns futon3c.peripheral.memory-backend
  "Memory-and-orientation backend seam for agent harnesses.

   M-custom-harness (futon2/holes/M-custom-harness.md) §12, decision D-11.i:
   every store or registry access a harness's memory/orientation tools need
   goes through this namespace, and callers see only plain result maps (the
   §12.3 envelope) — no Datalog, no store types, no store-specific query
   shapes. Port cost to another store = re-implement these fns; the harness
   loop, prompt, and client change zero.

   Slice 1 (this file): boot-context, repo-contract, boot-packet-string.
   Slices 2–3 add the evidence / graph / mission / coordination read fns.

   Registry and clock-store are reached via requiring-resolve so this
   namespace never creates a load-order dependency on the Agency."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.substrate.client :as substrate])
  (:import [java.time Instant]))

(def ^:private boot-packet-max-chars 8192) ; INV-4: boot packet hard cap

(defn- safe-call [sym & args]
  (when-let [f (try (requiring-resolve sym) (catch Throwable _ nil))]
    (try (apply f args) (catch Throwable _ nil))))

(defn- sh [cwd & cmd]
  (try
    (let [{:keys [exit out]} (apply shell/sh (concat cmd [:dir cwd]))]
      (when (zero? exit) (str/trimr out)))
    (catch Throwable _ nil)))

(defn- git-info [cwd]
  (when-let [branch (sh cwd "git" "rev-parse" "--abbrev-ref" "HEAD")]
    (let [dirty (->> (or (some-> (sh cwd "git" "status" "--porcelain")
                                 str/split-lines)
                         [])
                     (remove str/blank?)
                     (mapv str/trim))]
      {:branch branch
       :toplevel (sh cwd "git" "rev-parse" "--show-toplevel")
       :dirty-count (count dirty)
       :dirty-files (vec (take 40 dirty))})))

(defn- find-agents-md
  "AGENTS.md in dir, else walk up to the git toplevel (inclusive).
   Outside a git repo, only dir itself is checked."
  [dir]
  (let [dir (io/file dir)
        top (some-> (sh (.getPath dir) "git" "rev-parse" "--show-toplevel")
                    io/file
                    .getCanonicalPath)]
    (if-not top
      (let [f (io/file dir "AGENTS.md")]
        (when (.exists f) f))
      (loop [d dir]
        (when d
          (let [f (io/file d "AGENTS.md")]
            (cond
              (.exists f) f
              (= (.getCanonicalPath d) top) nil
              :else (recur (.getParentFile d)))))))))

(defn boot-context
  "Snapshot of the agent's situation: identity, clock target, git state,
   AGENTS.md presence. ctx: {:agent-id :session-id :cwd}. Live state at
   call time — this is an orientation tool, not a memory tool."
  [{:keys [agent-id session-id cwd]}]
  (let [cwd (or cwd (System/getProperty "user.dir"))
        reg (when agent-id
              (safe-call 'futon3c.agency.registry/get-agent agent-id))
        clock (when agent-id
                (safe-call 'futon3c.agency.clock-store/current-clock
                           agent-id session-id))
        git (git-info cwd)
        agents-md (find-agents-md cwd)]
    {:ok true
     :result (cond-> {:agent-id agent-id
                      :session-id session-id
                      :cwd cwd
                      :as-of (str (Instant/now))
                      :note "snapshot at call time"}
               reg (assoc :registry {:type (:agent/type reg)
                                     :status (:agent/status reg)
                                     :capabilities (:agent/capabilities reg)
                                     :current-peripheral (:agent/current-peripheral reg)})
               clock (assoc :clock clock)
               git (assoc :git git)
               agents-md (assoc :agents-md {:path (.getPath agents-md)
                                            :size (.length agents-md)}))}))

(defn repo-contract
  "Read AGENTS.md verbatim with provenance (path, size, mtime).
   ctx: {:cwd}; args: {:repo optional directory, defaults to cwd}."
  [{:keys [cwd]} {:keys [repo]}]
  (let [base (or repo cwd (System/getProperty "user.dir"))]
    (if-let [f (find-agents-md base)]
      {:ok true
       :result {:path (.getPath f)
                :size (.length f)
                :mtime (str (Instant/ofEpochMilli (.lastModified f)))
                :content (slurp f)}}
      {:ok false :error (str "No AGENTS.md found at or above " base)})))

;; --- Slice 2: memory family, read side (M-custom-harness §12.3/§12.5) -----
;;
;; Every memory tool returns the §12.3 envelope:
;;   {:frame "recorded, not necessarily current"
;;    :query <map echoing the filters/mode used>
;;    :items [...]}
;; Each item carries :id :at and either :author (evidence sources) or
;; :file :mtime (markdown sources). Limit: default 20, max 100, clamped
;; server-side by `clamp-limit`. Memory tools are strictly READ-ONLY.

(def ^:private default-limit 20)
(def ^:private max-limit 100)

(defn- clamp-limit
  "Coerce a limit to a safe int in [1, max-limit], defaulting to 20."
  [n]
  (let [n (cond (int? n) n
                (string? n) (try (Long/parseLong n) (catch Throwable _ default-limit))
                :else default-limit)]
    (max 1 (min max-limit (int n)))))

(defn- envelope
  "Wrap items in the §12.3 envelope, echoing the query map. Truncates items
   to the (clamped) limit. query echoes the filters/mode the caller used."
  [query items limit]
  (let [limit (clamp-limit limit)]
    {:frame "recorded, not necessarily current"
     :query query
     :items (vec (take limit items))}))

(defn- durable-evidence-store
  "The durable evidence store configured in futon3c.dev, when present.
   Discipline records (PSR/PUR/PAR proof paths) land there via the backend's
   evidence bridge; ad-hoc in-process evidence lands in store.clj's default
   !store. Memory reads must cover both (found live 2026-07-04: a PAR
   proof-path was invisible to pattern_memory because only the default
   store was queried)."
  []
  (try (some-> (requiring-resolve 'futon3c.dev/!evidence-store) deref deref)
       (catch Throwable _ nil)))

(defn- evidence-query
  "Query the durable AND default evidence stores, deduped by :evidence/id
   (durable wins)."
  [q]
  (let [durable (durable-evidence-store)
        durable-hits (when durable
                       (or (safe-call 'futon3c.evidence.store/query* durable q) []))
        default-hits (or (safe-call 'futon3c.evidence.store/query q) [])
        seen (volatile! #{})]
    (filterv (fn [e]
               (let [id (:evidence/id e)]
                 (when-not (@seen id)
                   (vswap! seen conj id)
                   true)))
             (concat durable-hits default-hits))))

(defn- graph-call
  "Call a store-taking graph fn against the durable store, falling back to
   the default store when the durable store is absent or returns nothing."
  [sym & args]
  (let [durable (durable-evidence-store)
        r (when durable (apply safe-call sym durable args))]
    (if (seq r) r (apply safe-call sym nil args))))

(defn- discipline-info-from-body
  "Extract discipline identity from an evidence body when it carries a
   proof-path-persisted event (PAR/PUR/PSR records bridged from the gate
   pipeline). Returns nil for non-discipline entries.

   The body shape (from real_backend.clj persist-proof-path!):
     {:event :proof-path-persisted
      :path/id ...
      :path/file ...
      :gate-events [{:gate/id :g0
                     :gate/record {:par/id ... :par/session-ref ...}
                     :gate/at ...}]}
   The gate records carry :par/, :pur/, or :psr/ namespaced keys."
  [body]
  (when (and (map? body) (= :proof-path-persisted (:event body)))
    (let [gate-events (if (map? (:gate-events body))
                        ;; Single gate-event map (not wrapped in vector)
                        [(:gate-events body)]
                        (:gate-events body))
          records (keep (fn [ge]
                          (let [r (:gate/record ge)]
                            (when (map? r) r)))
                        gate-events)
          ;; Session-ref: bridged bodies carry it directly (write-side fix,
          ;; same day); gate records are the fallback for rich bodies.
          session-ref (or (:session-ref body)
                          (some #(or (:par/session-ref %)
                                     (:session-ref %))
                                records))
          ;; Kind: prefer the bridged body's explicit :discipline-kind.
          kind (or (:discipline-kind body)
                   (cond
                     (some :par/id records) :par
                     (some :pur/id records) :pur
                     (some :psr/id records) :psr
                     :else                  :proof-path))]
      (cond-> {}
        session-ref (assoc :session-ref session-ref)
        true        (assoc :discipline-kind kind)))))

(defn- evidence-item
  "Project an EvidenceEntry into the §12.3 item shape. For discipline records
   (PAR/PUR/PAR proof-paths), surfaces :session-ref and :discipline-kind so an
   agent can recognize its own discipline entries without the full body."
  [e]
  (let [body (:evidence/body e)
        disc (when body (discipline-info-from-body body))]
    (cond-> {:id (:evidence/id e)
             :at (:evidence/at e)}
      (:evidence/author e) (assoc :author (:evidence/author e))
      (:evidence/type e) (assoc :type (:evidence/type e))
      (:evidence/claim-type e) (assoc :claim-type (:evidence/claim-type e))
      (:evidence/tags e) (assoc :tags (:evidence/tags e))
      (:session-ref disc) (assoc :session-ref (:session-ref disc))
      (:discipline-kind disc) (assoc :discipline-kind (:discipline-kind disc)))))

(defn- thread-item
  "Project a ThreadProjection into the §12.3 item shape."
  [t]
  {:id (:thread/id t)
   :at (:evidence/at (:thread/goal t))
   :author (:evidence/author (:thread/goal t))
   :status (:thread/status t)
   :entry-count (:thread/entry-count t)
   :subject (:thread/subject t)})

(defn memory-search
  "Search the evidence store via futon3c.evidence.store/query.
   ctx unused; args: {:type :claim-type :author :since :tags :limit
   :include-ephemeral?} (all optional). Returns the §12.3 envelope."
  [_ctx {:keys [subject type claim-type author since tags limit include-ephemeral?]}]
  (let [limit (clamp-limit limit)
        ;; Review fix (claude-16): §12.3 lists :subject among the filters;
        ;; coerce JSON string :ref/type to keyword as in evidence-graph.
        subject (when (and (map? subject) (:ref/type subject) (:ref/id subject))
                  (update subject :ref/type
                          #(if (keyword? %) % (keyword (str %)))))
        q (cond-> {}
            subject (assoc :query/subject subject)
            type (assoc :query/type (if (keyword? type) type (keyword type)))
            claim-type (assoc :query/claim-type
                              (if (keyword? claim-type) claim-type (keyword claim-type)))
            author (assoc :query/author author)
            since (assoc :query/since since)
            true (assoc :query/limit limit)
            (true? include-ephemeral?) (assoc :query/include-ephemeral? true)
            (seq tags) (assoc :query/tags (mapv (fn [t]
                                                  (if (keyword? t) t (keyword t)))
                                                tags)))
        entries (evidence-query q)]
    {:ok true
     :result (envelope {:store :evidence
                        :filters (dissoc q :query/limit)}
                       (mapv evidence-item entries)
                       limit)}))

(defn tool-history
  "Read the clock-store session state for an agent/session.
   ctx: {:agent-id :session-id}; args unused. Returns the §12.3 envelope with
   a single item describing the current session-state and clock target."
  [{:keys [agent-id session-id]} _args]
  (let [state (when agent-id
                (safe-call 'futon3c.agency.clock-store/current-state
                           agent-id session-id))
        item (when state
               {:id (str "clock/" agent-id "/" (or session-id "default"))
                :at (str (Instant/now))
                :clock (:clock state)
                :last-edit-activity (:last-edit-activity state)
                :last-auto-clock-witness (:last-auto-clock-witness state)
                :edit-activity-count (count (or (:edit-activity state) []))})]
    {:ok true
     :result (envelope {:store :clock-store
                        :agent-id agent-id
                        :session-id session-id
                        :mode :current-state}
                       (if item [item] [])
                       max-limit)}))

(defn evidence-graph
  "Project evidence into graphs.
   args: {:mode (thread|reply-chain|forks|neighborhood) :subject-ref
          :evidence-id :end-id :limit}. thread mode uses
          futon3c.evidence.threads/project-thread (and thread-forks).
          reply-chain uses store/get-reply-chain. forks uses store/get-forks.
          neighborhood uses futon1a.api.routes/hyperedges-by-end (needs :end-id).
   Returns the §12.3 envelope."
  [_ctx {:keys [mode subject-ref evidence-id end-id limit]}]
  (let [limit (clamp-limit limit)
        mode (or (when mode (keyword mode)) :thread)
        ;; Review fix (claude-16): JSON-sourced refs carry string :ref/type;
        ;; store subject-refs are keyword-typed — coerce or nothing matches.
        subject-ref (if (and (map? subject-ref) (:ref/type subject-ref))
                      (update subject-ref :ref/type
                              #(if (keyword? %) % (keyword (str %))))
                      subject-ref)]
    (case mode
      :thread
      (let [proj (when subject-ref
                   ;; durable store first, default fallback (see graph-call).
                   (graph-call 'futon3c.evidence.threads/project-thread
                               subject-ref))
            forks (when proj
                    (safe-call 'futon3c.evidence.threads/thread-forks proj))
            items (cond-> []
                    proj (conj (thread-item proj))
                    (seq forks) (into (mapv thread-item forks)))]
        {:ok true
         :result (envelope {:mode :thread :subject-ref subject-ref} items limit)})

      :reply-chain
      (let [entries (if evidence-id
                      (or (graph-call 'futon3c.evidence.store/get-reply-chain*
                                      evidence-id) [])
                      [])]
        {:ok true
         :result (envelope {:mode :reply-chain :evidence-id evidence-id}
                           (mapv evidence-item entries) limit)})

      :forks
      (let [entries (if evidence-id
                      (or (graph-call 'futon3c.evidence.store/get-forks*
                                      evidence-id) [])
                      [])]
        {:ok true
         :result (envelope {:mode :forks :evidence-id evidence-id}
                           (mapv evidence-item entries) limit)})

      :neighborhood
      (let [edges (when end-id
                    (substrate/hyperedges-by-end end-id {:limit limit}))]
        {:ok true
         :result (envelope {:mode :neighborhood
                            :end-id end-id
                            :count (count edges)}
                           (mapv (fn [h]
                   {:id (:hx/id h)
                    :at (:hx/at h)
                    :type (:hx/type h)
                    :endpoints (:hx/endpoints h)})
                 edges)
                           limit)})

      {:ok false :error (str "Unknown evidence-graph mode: " mode)})))

(defn pattern-memory
  "Query the evidence store for pattern-family tags (PSR/PUR/PAR/proof-path).
   args: {:tags optional override (default :psr :pur :par :proof-path)
           :limit}. Returns the §12.3 envelope."
  [_ctx {:keys [tags limit]}]
  (let [limit (clamp-limit limit)
        tags (if (seq tags)
               (mapv (fn [t] (if (keyword? t) t (keyword t))) tags)
               [:psr :pur :par :proof-path])
        ;; :query/tags is AND semantics (verified live 2026-07-04); pattern
        ;; retrieval wants ANY-of — query per tag and dedupe by id.
        entries (let [seen (volatile! #{})]
                  (into []
                        (comp (mapcat (fn [t]
                                        (evidence-query {:query/tags [t]
                                                         :query/limit limit})))
                              (filter (fn [e]
                                        (let [id (:evidence/id e)]
                                          (when-not (@seen id)
                                            (vswap! seen conj id)
                                            true)))))
                        tags))]
    {:ok true
     ;; Review fix (claude-16): project via evidence-item like memory-search —
     ;; raw entries violate INV-1's item shape and leak full :evidence/body.
     :result (envelope {:store :evidence :filters {:tags tags}}
                       (mapv evidence-item entries)
                       limit)}))

(defn- compact-job-item
  "Project an invoke job into the §12.3 item shape, EXCLUDING bulky fields
   (:events, :result-summary). Compact per M-custom-harness §12.5."
  [job]
  (cond-> {:id (:job-id job)
           :at (or (:started-at job) (:created-at job))
           :author (:caller job)}
    (:agent-id job) (assoc :agent-id (:agent-id job))
    (:state job) (assoc :state (:state job))
    (:bell-type job) (assoc :bell-type (:bell-type job))
    (:surface job) (assoc :surface (:surface job))))

(defn- edge-item
  "Project a coordination mesh edge (edge-public-view) into the §12.3 item shape."
  [edge]
  (cond-> {:id (:edge-id edge)
           :at (:at edge)
           :author (:from edge)}
    (:to edge) (assoc :to (:to edge))
    (:surface edge) (assoc :surface (:surface edge))
    (:kind edge) (assoc :kind (:kind edge))
    (some? (:ok? edge)) (assoc :ok? (:ok? edge))
    (:error edge) (assoc :error (:error edge))))

(defn recent-coordination
  "Read recent coordination activity (invoke jobs and/or mesh edges).
   args: {:limit, :scope \"jobs\"|\"edges\"|\"both\" (default \"both\")}.
   Returns the §12.3 envelope. Read-only."
  [_ctx {:keys [limit scope]}]
  (let [limit (clamp-limit limit)
        scope (or (some-> scope str/lower-case) "both")
        jobs (when (contains? #{"jobs" "both"} scope)
               ;; futon3c.transport.http/recent-invoke-jobs is a private fn
               ;; that derefs a ledger var; resolve and call it directly
               ;; (mirrors mesh_qa/current-invoke-jobs).
               (or (safe-call 'futon3c.transport.http/recent-invoke-jobs limit) []))
        edges (when (contains? #{"edges" "both"} scope)
                (or (safe-call 'futon3c.social.coordination-ledger/recent-mesh-edges
                               limit) []))]
    {:ok true
     :result (envelope {:store :coordination
                        :scope scope}
                       (vec (concat (mapv compact-job-item jobs)
                                    (mapv edge-item edges)))
                       limit)}))

;; --- Slice 3a: mission_context -------------------------------------------

(defn- find-mission-file
  "Locate a C-/M-/E- target doc via shell glob. Bare targets default to M-*.
   Tries nested and flat holes layouts."
  [target]
  (let [target (str target)
        stem (if (re-find #"^[MEC]-" target) target (str "M-" target))
        globs [(str "/home/joe/code/*/holes/**/" stem ".md")
               (str "/home/joe/code/*/holes/" stem ".md")
               (str "/home/joe/code/**/" stem ".md")]]
    (some (fn [g]
            ;; not-empty: `ls | head -1` exits 0 with "" on no match, and ""
            ;; is truthy to `some`. globstar: `**` is a no-op in plain sh.
            (some-> (sh "/home/joe/code" "bash" "-c"
                        (str "shopt -s globstar nullglob; "
                             "ls -1 " g " 2>/dev/null | head -1"))
                    str/trim
                    not-empty))
          globs)))

(defn- extract-status-banner
  "Pull the first **Status paragraph from mission markdown."
  [content]
  (some (fn [line]
          (when (str/includes? line "**Status")
            (str/trim line)))
        (str/split-lines content)))

(defn- extract-last-checkpoint
  "Find the last heading containing 'Checkpoint' and return a map with
   :text (the checkpoint block, ~40 lines) and :content-after? (true when
   non-blank content exists after the extracted block — later material may
   supersede the checkpoint). Returns nil when no checkpoint heading exists."
  [content]
  (let [lines (vec (str/split-lines content))
        idxs (keep-indexed (fn [i line]
                             (when (and (re-find #"^#{2,3}\s.*Checkpoint" line)
                                        (not (str/includes? line "Checkpoints")))
                               i))
                           lines)]
    (when (seq idxs)
      (let [start (last idxs)
            block-end (min (count lines) (+ start 40))
            block (subvec lines start block-end)
            ;; Content after the extracted block: any non-blank line beyond
            ;; block-end means later material was added that may supersede.
            after (subvec lines block-end)
            content-after? (some (fn [line]
                                   (not (str/blank? line)))
                                 after)]
        {:text (str/trimr (str/join "\n" block))
         :content-after? (boolean content-after?)}))))

(defn- mission-obligations
  "Read mission obligations via the MissionBackend safe-call seam.
   Constructs a transient MissionBackend and calls :obligation-query."
  [target cwd]
  (when-let [make-fn (try (requiring-resolve
                           'futon3c.peripheral.mission-backend/make-mission-backend)
                          (catch Throwable _ nil))]
    (when-let [exec-fn (try (requiring-resolve
                             'futon3c.peripheral.tools/execute-tool)
                            (catch Throwable _ nil))]
      (when-let [backend (try (make-fn {:cwd cwd}) (catch Throwable _ nil))]
        (try (exec-fn backend :obligation-query [target])
             (catch Throwable _ nil))))))

(defn mission-context
  "Compose mission orientation: markdown (status banner + last checkpoint),
   obligations, and related evidence. ctx: {:agent-id :session-id :cwd};
   args: {:target optional mission id, :limit}. Target may arrive with or
   without the M-/E-/C- prefix; defaults to the clocked target.
   Returns the §12.3 envelope."
  [{:keys [agent-id session-id cwd]} {:keys [target limit]}]
  (let [limit (clamp-limit limit)
        cwd (or cwd (System/getProperty "user.dir"))
        target (or target
                   (when agent-id
                     (let [clock (safe-call 'futon3c.agency.clock-store/current-clock
                                            agent-id session-id)]
                       (or (:mission-id clock)
                           (:excursion-id clock)
                           (:campaign-id clock)))))
        target (when target
                 (let [t (str/trim (str target))]
                   (if (re-find #"^[MEC]-" t) t (str "M-" t))))]
    (if-not target
      {:ok false :error "no mission target"}
      (let [file (find-mission-file target)
            content (when file (try (slurp file) (catch Throwable _ nil)))
            status-banner (when content (extract-status-banner content))
            cp (when content (extract-last-checkpoint content))
            last-checkpoint (:text cp)
            content-after? (:content-after? cp)
            ;; Honest marker: prepend note when later content may supersede.
            last-checkpoint (when last-checkpoint
                              (if content-after?
                                (let [lines (str/split-lines last-checkpoint)
                                      first-line (first lines)
                                      rest-lines (rest lines)]
                                  (str/trimr
                                   (str/join "\n"
                                             (cons (str first-line
                                                        " [note: later content exists below this checkpoint — it may be superseded]")
                                                   rest-lines))))
                                last-checkpoint))
            ;; Obligations via the mission backend seam.
            obl (mission-obligations target cwd)
            obligations (when (:ok obl) (:result obl))
            ;; Related evidence: reuse evidence-query.
            subject {:ref/type :mission :ref/id target}
            entries (evidence-query {:query/subject subject :query/limit limit})
            items (cond-> []
                    (or file content) (conj (cond-> {:id (str "mission/" target)
                                                     :file file
                                                     :mtime (when file
                                                              (str (Instant/ofEpochMilli
                                                                    (.lastModified
                                                                     (java.io.File. file)))))}
                                              status-banner (assoc :status-banner status-banner)
                                              last-checkpoint (assoc :last-checkpoint last-checkpoint)
                                              content-after? (assoc :content-after-checkpoint? true)))
                    (map? obligations) (conj {:id (str "obligations/" target)
                                              :at (str (Instant/now))
                                              :obligations obligations})
                    (seq entries) (into (mapv evidence-item entries)))]
        {:ok true
         :result (envelope {:store :mission :target target} items limit)}))))

;; --- Slice 3b: D-7 session rehydration (from the invoke-jobs ledger) -----

(defn session-turns
  "Condensed prior turns for a session, from the invoke-jobs ledger.
   ctx: {:session-id}; args: {:limit}. Terminal jobs only (the currently
   running turn is excluded). Items: {:id :at :caller :surface :prompt
   :outcome} — :prompt only for jobs recorded after prompt capture landed
   (2026-07-04); :outcome is the ledger's result summary. Returns the
   §12.3 envelope. This is commitment summary, not conversation replay:
   pre-capture prompts were never persisted, and tool detail is elided by
   design (live facts should be re-fetched, D-5)."
  [{:keys [agent-id session-id]} {:keys [limit]}]
  (let [limit (clamp-limit limit)
        jobs (or (safe-call 'futon3c.transport.http/recent-invoke-jobs 200) [])
        mine (->> jobs
                  (filter #(and (= (str session-id) (str (:session-id %)))
                                ;; bellback echo jobs can share the session id;
                                ;; when agent-id is known, keep only this
                                ;; agent's own turns.
                                (or (nil? agent-id)
                                    (= (str agent-id) (str (:agent-id %))))
                                (contains? #{"done" "failed"} (str (:state %)))))
                  (sort-by #(str (or (:started-at %) (:created-at %))))
                  (take-last limit))
        prompt-of (fn [job]
                    (some #(when (= "prompt" (str (:type %))) (:text %))
                          (:events job)))
        items (mapv (fn [job]
                      (cond-> {:id (:job-id job)
                               :at (or (:started-at job) (:created-at job))
                               :caller (:caller job)
                               :surface (:surface job)
                               :outcome (:result-summary job)}
                        (prompt-of job) (assoc :prompt (prompt-of job))))
                    mine)]
    {:ok true
     :result (envelope {:store :invoke-jobs :session-id session-id} items limit)}))

(def ^:private rehydration-max-chars 6000)

(defn rehydration-string
  "Render prior-session turns for system-prompt injection (D-7), or nil when
   the ledger has none. Marked as reconstruction per the DERIVE spec."
  [{:keys [agent-id session-id limit]}]
  (let [{:keys [result]} (session-turns {:agent-id agent-id :session-id session-id}
                                        {:limit (or limit 10)})
        items (:items result)]
    (when (seq items)
      (let [turn-str (fn [{:keys [at caller surface prompt outcome]}]
                       (str "· " at " from " caller " via " surface ":\n"
                            "  asked: " (or prompt "[prompt not recorded]") "\n"
                            "  outcome: " (or outcome "[none]")))
            s (str "--- REHYDRATED SESSION MEMORY "
                   "(reconstructed from the invoke-jobs ledger; tool detail "
                   "elided — re-fetch live facts with tools; prompts exist "
                   "only for turns after prompt-capture landed) ---\n"
                   (str/join "\n" (map turn-str items)))]
        (if (> (count s) rehydration-max-chars)
          (str (subs s 0 (- rehydration-max-chars 40)) "\n…[rehydration truncated]")
          s)))))

(defn boot-packet-string
  "Render the D-1 boot packet for system-prompt injection: identity, clock,
   git state, AGENTS.md verbatim. Hard-capped at 8 KB (INV-4)."
  [{:keys [agent-id session-id] :as ctx}]
  (let [{:keys [result]} (boot-context ctx)
        {:keys [clock git agents-md]} result
        contract (when agents-md
                   (try (slurp (:path agents-md)) (catch Throwable _ nil)))
        packet (str "--- BOOT CONTEXT (snapshot at session start; refresh with boot_context) ---\n"
                    "agent: " agent-id "  session: " session-id "\n"
                    "cwd: " (:cwd result) "\n"
                    (when clock
                      (str "clock: " (pr-str clock) "\n"))
                    (when git
                      (str "git: branch " (:branch git)
                           ", " (:dirty-count git) " dirty file(s)"
                           (when (pos? (:dirty-count git))
                             (str " — " (str/join ", " (take 8 (:dirty-files git)))
                                  (when (> (:dirty-count git) 8) " …")))
                           "\n"))
                    (if contract
                      (str "--- AGENTS.md (" (:path agents-md) ", verbatim) ---\n"
                           contract)
                      "No AGENTS.md found at or above cwd.\n"))]
    (if (> (count packet) boot-packet-max-chars)
      (str (subs packet 0 (- boot-packet-max-chars 60))
           "\n...[boot packet truncated at 8 KB (INV-4)]")
      packet)))
