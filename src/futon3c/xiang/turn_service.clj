(ns futon3c.xiang.turn-service
  "The 象 turn pipeline, server-side: record, dispatch, reap, publish.

   This is the orchestration half of `emacs/session-turn-analysis.el` moved
   into the JVM so that a client with no filesystem and no python3 can drive
   it over HTTP. The states and their transitions are the Emacs ones:

     requested ──dispatch!──▶ requested + job id ──reap!──▶ analyzed
                                                       ├──▶ refused / failed
                                                       ├──▶ (quota) bench the seat,
                                                       │     retry on another
                                                       └──▶ (store busy) retry
                                                             on a bounded schedule

   Every effect that reaches the rest of the stack is injected, so the
   pipeline is testable with no JVM services running and the HTTP layer can
   bind it to the in-process bell and job ledger:

     :bell!          (fn [{:keys [agent-id prompt caller surface mode type]}]
                      -> {:ok true :job-id ..} | {:ok false :status .. :error ..})
     :job-status     (fn [job-id] -> public job view, nil when unknown)
                      (throw = unreachable)
     :post-withdrawal (fn [payload] -> {:status .. :json ..})   POST /withdrawal/provisional
     :post-negation   (fn [payload] -> {:status .. :json ..})   POST /interpretation/negation
     :deliver-notice  (fn [payload] -> {:status .. :json ..})   POST /turn-notice
     :reset-seat!     (fn [agent]) optional; clears the seat's conversation
     :draft           (fn [source-text] -> xiaoxiang_preview.py fragments or nil)
                      optional; run at record time, about 0.1 s
     :pattern-candidates (fn [queries] -> {query [{:id :score :title ..}]} or nil)
                      optional; BM25 per fragment before dispatch, so the seat
                      reads candidates instead of searching
     :skip-routine?   true to record a routine turn (tr/routine-draft?) as
                      \"drafted\" and never queue it; default false until
                      the agreement numbers justify it
     :schedule!       (fn [delay-seconds thunk]) default: a daemon scheduler
     :now-ms          (fn [] ms) default System/currentTimeMillis

   Delivery is not accomplishment: a bell that was accepted leaves the record
   `requested` with its job id, and only a reap or a publication moves it.
   The REPL-side notice (Emacs inserted a system line into the buffer) is not
   delivered here: a web client reads `withdrawal_effects` off the record and
   shows the notice itself, so the record carries no `repl_notice_*` fields
   written by the server."
  (:require [clojure.string :as str]
            [futon3c.xiang.turn-acts :as turn-acts]
            [futon3c.xiang.turn-record :as tr]
            [futon3c.xiang.turn-store :as ts])
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit ThreadFactory]))

;; ---------------------------------------------------------------------------
;; Construction

(def defaults
  "The Emacs defcustoms, by their Emacs names."
  {:seat "象"                                   ; session-mode-analysis-agent
   :alternate "象-sonnet"                       ; session-mode-analysis-alternate
   :pool ["象-1" "象-2" "象-3" "象-4"]           ; session-mode-analysis-pool
   :bench-minutes 60                            ; session-mode-analysis-bench-minutes
   :caller "turn-capture"                       ; session-mode-analysis-caller
   :requisition "M-futon-seams"                 ; session-mode-analysis-requisition
   :reset-every 20                              ; session-mode-analysis-reset-every
   :reap-after 180                              ; session-mode-analysis-reap-after
   :reap-late-after 600                         ; session-mode-analysis-reap-late-after
   :reap-late-tries 6                           ; session-mode-analysis-reap-late-tries
   :store-busy-delays [60 180 600]              ; session-mode-analysis-store-busy-retry-delays
   :notice-max-attempts 5                       ; session-mode-withdrawal-notice-max-attempts
   :settle-evidence-delay 1800                  ; a settled turn waits this long for happened
   :surface "emacs-repl"                        ; agency_send.py --surface default
   :vocabulary tr/default-vocabulary
   :brief-paths tr/default-brief-paths
   ;; M-象-2000 step 2: records and readings are also appended to the
   ;; evidence store. Default no-op so tests and existing callers are
   ;; unaffected; the real append is wired in http.clj's xiang-turn-service.
   :evidence! (fn [_] {:ok true})
   ;; How a settled turn's append is run off the caller's path (fn [thunk]); nil
   ;; means on the scheduler at delay 0.
   :evidence-async! nil
   ;; Likewise for the 小象 draft of a turn not dispatched at once.
   :draft-async! nil})

(defn- daemon-scheduler ^ScheduledExecutorService []
  (Executors/newSingleThreadScheduledExecutor
   (reify ThreadFactory
     (newThread [_ r]
       (doto (Thread. r "xiang-turn-service") (.setDaemon true))))))

(defn service
  "Build a service over OPTS (see the namespace doc). :store is required."
  [{:keys [store] :as opts}]
  (when-not store (throw (ex-info "turn-service needs a :store" {:reason :no-store})))
  (let [config (merge defaults opts)
        scheduler (when-not (:schedule! opts) (daemon-scheduler))
        config (cond-> config
                 (not (:now-ms opts)) (assoc :now-ms #(System/currentTimeMillis))
                 scheduler (assoc :schedule!
                                  (fn [delay-s thunk]
                                    (.schedule scheduler ^Runnable thunk (long delay-s) TimeUnit/SECONDS))))]
    {:config config
     :scheduler scheduler
     :state (atom {:dispatch-count 0      ; session-mode--analysis-dispatch-count
                   :benched {}            ; seat -> until-ms
                   :outstanding {}        ; record-id -> [seat at-ms]
                   :health {:state nil :detail nil}
                   :notified-no-grant false})}))

(defn stop!
  "Stop the default scheduler, if this service made one."
  [{:keys [scheduler]}]
  (when scheduler (.shutdownNow ^ScheduledExecutorService scheduler))
  nil)

(defn- cfg [svc k] (get-in svc [:config k]))
(defn- now-ms [svc] ((cfg svc :now-ms)))
(defn- iso [ms] (str (.truncatedTo (java.time.Instant/ofEpochMilli (long ms)) java.time.temporal.ChronoUnit/SECONDS)))

(defn- set-health! [svc state detail]
  (swap! (:state svc) assoc :health {:state state :detail detail :at (now-ms svc)})
  nil)

(defn health [svc] (:health @(:state svc)))

;; ---------------------------------------------------------------------------
;; Seats (session-mode--analysis-seat and friends)

(defn benched? [svc agent]
  (let [until (get-in @(:state svc) [:benched agent])]
    (boolean (and until (< (now-ms svc) until)))))

(defn bench-seat!
  "A pool seat benches the whole pool: the seats share one provider's limit."
  [svc agent]
  (let [pool (cfg svc :pool)
        until (+ (now-ms svc) (* 60000 (cfg svc :bench-minutes)))
        seats (if (some #{agent} pool) pool [agent])]
    (swap! (:state svc) update :benched merge (zipmap seats (repeat until)))
    nil))

(defn- note-dispatch! [svc id seat]
  (swap! (:state svc) assoc-in [:outstanding id] [seat (now-ms svc)]))

(defn- note-done! [svc id]
  (swap! (:state svc) update :outstanding dissoc id))

(defn- seat-load [svc seat]
  (let [cutoff (- (now-ms svc) 3600000)]
    (count (filter (fn [[s at]] (and (= s seat) (> at cutoff)))
                   (vals (:outstanding @(:state svc)))))))

(defn- pool-seat [svc]
  (->> (cfg svc :pool)
       (remove #(benched? svc %))
       (map (fn [seat] [(seat-load svc seat) seat]))
       (sort-by first)
       first
       second))

(defn other-seat
  "The seat to use when AGENT is out of usage."
  [svc agent]
  (let [pool (cfg svc :pool) primary (cfg svc :seat) alternate (cfg svc :alternate)]
    (cond (some #{agent} pool) (or (pool-seat svc) alternate)
          (= agent primary) alternate
          :else (or (pool-seat svc) primary))))

(defn analysis-seat
  "With a pool: its least-loaded unbenched seat, else the alternate.
   Without: the analysis agent unless it is benched."
  [svc]
  (let [pool (cfg svc :pool) primary (cfg svc :seat) alternate (cfg svc :alternate)]
    (cond
      (seq pool) (or (pool-seat svc)
                     (and alternate (not (benched? svc alternate)) alternate)
                     (first pool))
      (and alternate (benched? svc primary) (not (benched? svc alternate))) alternate
      :else primary)))

;; ---------------------------------------------------------------------------
;; The happened summary (session-mode--turn-happened-summary)

(def happened-commit-cap 20)

(defn summarize-numstat
  "git numstat TEXT as [added removed files]; binary lines count as files only."
  [text]
  (reduce (fn [[a r f] line]
            (if-let [[_ add rem] (re-matches #"([0-9-]+)\t([0-9-]+)\t.*" line)]
              [(if (= add "-") a (+ a (Long/parseLong add)))
               (if (= rem "-") r (+ r (Long/parseLong rem)))
               (inc f)]
              [a r f]))
          [0 0 0]
          (remove str/blank? (str/split (str text) #"\n"))))

(defn git-numstat
  "`git show --numstat --format= SHA` in REPO-PATH, or nil. Line counts only."
  [repo-path sha]
  (try
    (let [p (.start (doto (ProcessBuilder. ["git" "show" "--numstat" "--format=" (str sha)])
                      (.directory (java.io.File. (str repo-path)))
                      (.redirectErrorStream false)))
          out (slurp (.getInputStream p))]
      (when (zero? (.waitFor p)) out))
    (catch Exception _ nil)))

(defn commit-summary-line
  "One line for COMMIT {:repo :sha :subject :numstat?}: repo, short sha,
   subject, and line counts when :numstat text is present. Never the diff."
  [{:keys [repo sha subject numstat]}]
  (let [sha (str sha)
        short (subs sha 0 (min 8 (count sha)))
        head (str "- " (or repo "?") " " short " " (or subject ""))]
    (if numstat
      (let [[a r f] (summarize-numstat numstat)]
        (str head " (+" a " -" r " over " f " files)"))
      head)))

(defn happened-summary
  "The \"What the agent did\" block: the first five reply lines and one line
   per commit (capped). COMMITS may carry :numstat already, or :repo-path so
   it is read here with git."
  [reply commits]
  (let [lines (take 5 (str/split (str (or reply "")) #"\n" -1))
        commits (map (fn [c] (if (and (:repo-path c) (not (:numstat c)) (not (str/blank? (str (:sha c)))))
                               (assoc c :numstat (git-numstat (:repo-path c) (:sha c)))
                               c))
                     commits)
        shown (take happened-commit-cap commits)
        more (- (count commits) (count shown))]
    (str "What the agent did (machine-added context for reading the turn):\n"
         "Reply begins:\n"
         (str/join "\n" lines)
         "\nCommits during the turn (any repo; not necessarily the agent's own):\n"
         (if (seq shown)
           (str (str/join "\n" (map commit-summary-line shown))
                (when (pos? more) (str "\n…and " more " more")))
           "(none)"))))

;; ---------------------------------------------------------------------------
;; Evidence (M-象-2000 step 2: a settled turn is evidence)
;;
;; One entry per turn, written once the turn has settled: the record with
;; its reading (analysed) or its draft (routine). Nothing is written while a
;; turn is in flight, so no reader has to look in the database for a turn
;; that is still being worked on (Joe, 2026-10-04). Turns that settle with
;; no reading -- declared, not-requested (象-off: gone dark) -- are not
;; written. The files stay the source of truth: a failed append never fails
;; or blocks a turn, and the deterministic id makes a retry a quiet
;; duplicate.

(defn turn-evidence-entry
  "The evidence entry for settled turn ID: RECORD with the READING that
   settled it (an analysis, or a routine draft) and HOW (:analyzed or
   :drafted). Cites the operator turn's evidence_id when the record has one."
  [id record reading how]
  (cond-> {:evidence-id (str "e-xiang-turn-" id)
           :subject {:ref/type :thread :ref/id id}
           :type :memory
           :claim-type :assert
           :author "xiang-turn-service"
           :body {:record record :reading reading :settled (name how)}
           :tags (cond-> [:xiang-turn (keyword (str "xiang-turn-" (name how)))]
                   (:session_id record) (conj (:session_id record))
                   (:agent_id record) (conj (:agent_id record)))}
    (:session_id record) (assoc :session-id (:session_id record))
    (:evidence_id record) (assoc :in-reply-to (:evidence_id record))))

(declare append-evidence! compute-ports!)

(defn- settle-evidence!
  "Write settled turn ID as evidence, off the caller's path. The packet
   should carry both the reading and the happened summary, so a turn whose
   reply has not ended yet waits for it, bounded: after
   :settle-evidence-delay it is written without the summary. The record is
   re-read at write time, so a summary that landed in the meantime rides
   along; the deterministic id keeps a second write a quiet duplicate.
   When the packet is complete (reading and happened both present, in
   either arrival order) the turn's ports are (re)computed alongside it."
  [svc id reading how]
  (let [store (cfg svc :store)
        write! (fn []
                 ((or (cfg svc :evidence-async!) #((cfg svc :schedule!) 0 %))
                  #(append-evidence! svc (turn-evidence-entry id (ts/read-record store id) reading how)
                                     (cfg svc :store-busy-delays))))]
    (if (:happened_summary (ts/read-record store id))
      (do (write!)
          ((or (cfg svc :evidence-async!) #((cfg svc :schedule!) 0 %))
           #(compute-ports! svc id)))
      ((cfg svc :schedule!) (cfg svc :settle-evidence-delay) write!))))

(defn append-evidence!
  "Append ENTRY through the :evidence! effect; never throw, never block the
   turn. A failed append is logged in health and retried on the bounded
   DELAYS schedule (the store-busy one); an append that reports itself
   idempotent (a duplicate of an earlier success) is accepted quietly."
  [svc entry delays]
  ;; A service built before step 2 (the JVM's cached one) has no :evidence!:
  ;; skip quietly rather than fail every turn's health.
  (let [result (try ((or (cfg svc :evidence!) (constantly {:ok true})) entry)
                    (catch Exception e {:ok false :error/message (.getMessage e)}))]
    (cond
      (:ok result) result

      (or (:idempotent? result) (= :duplicate-id (:error/code result)))
      result

      ;; The boundary refuses an in-reply-to whose parent it cannot find (an
      ;; operator turn with no evidence row, or whose row has not landed).
      ;; Keep the entry, drop the link: the body still carries the id.
      (and (= :reply-not-found (:error/code result)) (:in-reply-to entry))
      (append-evidence! svc (-> entry
                                (dissoc :in-reply-to)
                                (update :tags conj :xiang-reply-parent-missing))
                        delays)

      (seq delays)
      (do (set-health! svc nil (str (:evidence-id entry) ": evidence append failed ("
                                    (or (:error/message result) (:error/code result) "unknown")
                                    "); retrying in " (first delays) "s"))
          ((cfg svc :schedule!) (first delays)
           (fn [] (append-evidence! svc entry (rest delays))))
          result)

      :else
      (do (set-health! svc :failing
                       (str (:evidence-id entry)
                            ": evidence append failed; the file record is the source of truth"))
          result))))

;; ---------------------------------------------------------------------------
;; Ports (the HAPPENED / DIDN'T HAPPEN heads-up display)

(def ^:private ports-session-cap 50)

(defn- trim-act-text
  [s]
  (let [s (str/trim (str (or s "")))]
    (if (> (count s) 120) (str (subs s 0 119) "…") s)))

(defn compute-ports!
  "The settled turn's HAPPENED / DIDN'T HAPPEN: run the turn-acts adapter
   over the session's settled turns (the last `ports-session-cap`, ordered
   by created_at) and store the kernel's answer on ID's record as :ports
   {:closed_this_turn [{:act :kind :text}] :still_open [{:act :kind :text
   :since}]}, each act carrying its paragraph/fragment text trimmed to 120
   characters so the HUD needs no lookup. Drafted turns contribute their
   reply and commit acts but no operator fragments: 小象's parse is
   provisional. Run off the caller's path (the caller schedules this);
   never fails the turn — a failure sets health detail only."
  [svc id]
  (try
    (let [store (cfg svc :store)
          record (ts/read-record store id)
          settled (->> (ts/list-records store :session-id (:session_id record)
                                        :limit ports-session-cap)
                       (keep (fn [{rid :id r :record}]
                               (let [status (:analysis_status r)]
                                 (when (contains? #{"analyzed" "drafted"} status)
                                   {:record r
                                    :reading (if (= status "analyzed")
                                               (or (ts/read-analysis store rid) {})
                                               {:sentences []})
                                    :reply (or (:reply_text r) "")
                                    :commits (turn-acts/happened-commits (:happened_summary r))}))))
                       (sort-by #(get-in % [:record :created_at]))
                       vec)
          acts (turn-acts/session-acts settled)
          by-id (into {} (map (juxt :id identity) acts))
          ports (turn-acts/turn-ports acts (:turn_id record))
          entry (fn [act-id]
                  (let [a (by-id act-id)]
                    {:act act-id
                     :kind (some-> (:kind a) name)
                     :text (trim-act-text (:text a))}))]
      (ts/update-record! store id
                         #(assoc % :ports
                                 {:closed_this_turn (mapv entry (:closed-this-turn ports))
                                  :still_open (mapv (fn [{:keys [act since]}]
                                                      (assoc (entry act) :since since))
                                                    (:still-open ports))})))
    (catch Exception e
      (set-health! svc nil (str id ": ports not computed (" (.getMessage e) ")")))))

;; ---------------------------------------------------------------------------
;; Record (session-mode--record-turn via the store)

(declare dispatch! draft!)

(def ^:private inline-draft-timeout-ms
  "How long record-turn! waits for a :soon/:later turn's 小象 draft before
   answering without it and letting the off-path draft finish the job."
  2000)

(defn record-turn!
  "Record one operator turn. OPTS are `tr/make-record`'s, plus :dispatch
   (:later, the default, waits for `attach-happened!`; :soon dispatches on
   the scheduler at delay 0, so the caller is answered at once and the
   reading still starts at send; :now dispatches synchronously, as an
   externally captured turn does; :none records only and marks
   the record \"declared\" — settled without a reading, never dispatched).
   Returns {:id :path :record :dispatch :draft} where :dispatch is the dispatch result or
   :pending/:skipped/:declared/:scheduled, and :draft is true exactly when
   小象's draft was written before this call returned (inline drafts are
   bounded at `inline-draft-timeout-ms`; a slow one continues off-path).
   A turn addressed to the analysis seat itself is recorded, never dispatched."
  [svc {:keys [dispatch agent-id] :or {dispatch :later} :as opts}]
  (let [{:keys [record redacted]} (tr/make-record (merge {:vocabulary (cfg svc :vocabulary)
                                                          :now-ms (now-ms svc)}
                                                         (dissoc opts :dispatch)))
        {:keys [id path]} (ts/write-record! (cfg svc :store) record)
        ;; Off the request path: the caller (Emacs, synchronously) is waiting
        ;; on this response, and a busy futon1b must not hold it up.
        to-seat? (= (cfg svc :seat) agent-id)
        ;; The 小象 draft took 11 s on the live JVM (2026-10-03), past Emacs's
        ;; 10 s wait on this request. A turn dispatched now needs its draft
        ;; first; :soon/:later turns try the draft inline, bounded (M-象-2000:
        ;; it is usually ~0.1 s, and the caller wants it in the response),
        ;; falling back to the off-path draft when the bound is hit. dispatch!
        ;; drafts any record still without one.
        async-draft! (fn [] ((or (cfg svc :draft-async!) #((cfg svc :schedule!) 0 %))
                             #(draft! svc id)))
        [draft timed-out? draft-future]
        (cond
          (= dispatch :now) [(draft! svc id) false nil]
          (contains? #{:soon :later} dispatch)
          (let [f (future (draft! svc id))
                d (deref f inline-draft-timeout-ms ::timeout)]
            (if (identical? ::timeout d)
              ;; Do not cancel: this same future is the async fallback and
              ;; will write the draft when the slow effect completes.
              [nil true f]
              [d false nil]))
          :else [nil false nil])
        ;; On a timeout the off-path draft still happens: for :soon the
        ;; dispatch below drafts the record itself; :later needs the
        ;; scheduled draft, as does any record-only dispatch (:none).
        _ (when (not (contains? #{:now :soon :later} dispatch))
            (async-draft!))
        result (cond
                 to-seat? :skipped
                 (= dispatch :none) (do (ts/update-record! (cfg svc :store) id
                                                           #(assoc % :analysis_status "declared"))
                                        :declared)
                 (= dispatch :now) (dispatch! svc id {})
                 ;; :soon (the Emacs jvm recorder): the reading starts at
                 ;; send, off the request path — POST still answers at once;
                 ;; dispatch! drafts the record itself.
                 (= dispatch :soon) (do (ts/update-record! (cfg svc :store) id
                                                           #(assoc-in % [:analysis_dispatch :scheduled_at] (now-ms svc)))
                                        (if timed-out?
                                          ;; Do not race a second draft through dispatch!.
                                          ;; Once the timed-out draft lands, queue the reading.
                                          (future @draft-future
                                                  ((cfg svc :schedule!) 0 #(dispatch! svc id {})))
                                          ((cfg svc :schedule!) 0 #(dispatch! svc id {})))
                                        :scheduled)
                 :else :pending)]
    {:id id :path path :record (ts/read-record (cfg svc :store) id) :redacted redacted
     :dispatch result :draft (boolean (or draft (ts/read-draft (cfg svc :store) id)))}))

(defn draft!
  "Run the :draft effect over ID's source text and store the draft; nil when
   there is no effect, it returned nothing, or its output did not validate
   (a bad draft is logged in health, never stored)."
  [svc id]
  (when-let [draft-fn (cfg svc :draft)]
    (let [store (cfg svc :store)]
      (when-let [record (ts/read-record store id)]
        (try
          (when-let [raw (draft-fn (:source_text record))]
            (let [draft (tr/validate-draft record raw {:now-ms (now-ms svc)})]
              (ts/write-draft! store id draft)
              draft))
          (catch Exception e
            (set-health! svc nil (str id ": draft rejected: " (.getMessage e)))
            nil))))))

(defn store-draft!
  "Store a draft submitted from outside (the bridge, a widget) for ID."
  [svc id raw]
  (let [store (cfg svc :store)
        record (or (ts/read-record store id)
                   (throw (ex-info "Record not found" {:reason :record-not-found :id id})))
        draft (tr/validate-draft record raw {:now-ms (now-ms svc)})]
    (ts/write-draft! store id draft)
    draft))

(defn attach-happened!
  "Attach what the agent did while answering turn ID. The summary is
   classical (no LLM) and lands at turn finalisation, like the \"Cooked for
   Ns\" line: it never triggers or waits for a reading. Only a turn still
   `requested' with no job — a `files'-era record or a bridge's — is
   dispatched here, as before. On an already settled turn the summary joins
   the reading in the settled evidence packet (idempotent id, off-path).
   HAPPENED is a ready summary string or {:reply TEXT :commits [...]}; the
   reply text is also stored as the record's :reply_text (the input 大象
   reads, and the adapter's source for the reply's marked acts)."
  [svc id happened]
  (let [store (cfg svc :store)
        summary (try (cond (string? happened) happened
                           (map? happened) (happened-summary (:reply happened) (:commits happened))
                           :else nil)
                     (catch Exception _ nil))
        reply (when (map? happened) (:reply happened))]
    (when summary
      (try (ts/update-record! store id #(cond-> (assoc % :happened_summary summary)
                                          reply (assoc :reply_text reply)))
           (catch Exception _ nil)))
    (let [record (ts/read-record store id)
          status (:analysis_status record)]
      (cond
        (nil? record) {:dispatched false :reason :record-not-found}
        (= (cfg svc :seat) (:agent_id record)) {:dispatched false :reason :addressed-to-seat}
        (contains? #{"analyzed" "drafted"} status)
        (do (settle-evidence! svc id
                              (if (= status "analyzed")
                                (or (ts/read-analysis store id) {})
                                (or (ts/read-draft store id) {}))
                              (keyword status))
            {:dispatched false :reason (keyword status)})
        (contains? #{"declared" "not-requested"} status)
        {:dispatched false :reason (keyword status)}
        (get-in record [:analysis_dispatch :job_id])
        {:dispatched false :reason :already-dispatched}

        ;; A "soon" dispatch is drafting (11 s live) and has no job id yet: a
        ;; short reply ending inside that window must not dispatch it twice.
        ;; Bounded, so a mark stranded by a JVM restart still lets the turn go.
        (when-let [at (get-in record [:analysis_dispatch :scheduled_at])]
          (< (- (now-ms svc) at) (* 5 60 1000)))
        {:dispatched false :reason :dispatch-scheduled}
        :else (dispatch! svc id {})))))

;; ---------------------------------------------------------------------------
;; Dispatch (session-mode--dispatch-analysis)

(declare reap! dispatch-to!)

(defn- schedule-reap! [svc id agent tries store-busy-delays delay-s]
  ((cfg svc :schedule!) delay-s
   (fn [] (try (reap! svc id {:agent agent :tries tries :store-busy-delays store-busy-delays})
               (catch Throwable t
                 (set-health! svc :failing (str id ": reap threw " (.getMessage t)))
                 {:outcome :error :reason (.getMessage t)})))))

(defn dispatch!
  "Ask AGENT (default `analysis-seat`) to interpret the turn recorded as ID.
   Fire and forget: the bell is sent, its job id written onto the record, and
   a reap scheduled. A failed send leaves the record `requested`, which is the
   honest state. Returns {:dispatched bool :agent :job-id :reason}."
  [svc id {:keys [agent store-busy-delays]}]
  (let [store (cfg svc :store)
        agent (or agent (analysis-seat svc))
        path (ts/record-path store id)]
    (if-not (ts/read-record store id)
      {:dispatched false :reason :record-not-found}
      (let [status (:analysis_status (ts/read-record store id))]
        (if (#{"declared" "not-requested"} status)
          ;; Declared (dispatch :none) records are settled without a reading;
          ;; not-requested records were taken while the analysis policy was
          ;; `never' (e.g. 象-off): recorded, but never to be read. Neither may
          ;; be queued by dispatch, retry or sweep.
          {:dispatched false :reason (keyword status)}
      (let [record (ts/read-record store id)
            draft (or (ts/read-draft store id) (draft! svc id))]
        (if (and draft (cfg svc :skip-routine?) (tr/routine-draft? record draft)
                 (contains? #{nil "requested"} (:analysis_status record)))
          ;; Routine: the draft is the reading. Recorded as such, never queued.
          (do (ts/update-record! store id #(assoc % :analysis_status "drafted"))
              (settle-evidence! svc id draft :drafted)
              (set-health! svc nil (str id ": routine, settled by the draft"))
              {:dispatched false :reason :drafted :agent nil})
          (dispatch-to! svc id agent store-busy-delays record draft path))))))))

(defn candidate-queries
  "What to search the library for before dispatch: the draft's fragments
   when there is a draft, else the record's sentences. Short tokens such as
   \"Yes.\" are skipped; BM25 over them returns noise."
  [record draft]
  (->> (if (seq (:fragments draft)) (map :text (:fragments draft)) (map :text (:sentences record)))
       (map str)
       (remove #(< (tr/word-count %) 2))
       distinct
       vec))

(defn find-pattern-candidates!
  "Run the :pattern-candidates effect for ID and store the result; nil when
   there is no effect, nothing to ask, or it failed (logged in health)."
  [svc id record draft]
  (when-let [find (cfg svc :pattern-candidates)]
    (let [queries (candidate-queries record draft)]
      (when (seq queries)
        (try
          (when-let [found (find queries)]
            (when (map? found)
              (ts/write-pattern-candidates! (cfg svc :store) id found)
              found))
          (catch Exception e
            (set-health! svc nil (str id ": pattern candidates failed: " (.getMessage e)))
            nil))))))

(defn- dispatch-to!
  [svc id agent store-busy-delays record draft path]
  (let [store (cfg svc :store)]
      (let [_ (note-dispatch! svc id agent)
            candidates (find-pattern-candidates! svc id record draft)
            brief-fn (if (= "agent" (:origin record)) tr/agent-brief tr/analysis-brief)
            brief (brief-fn id path (cond-> {:requisition (cfg svc :requisition)
                                             :paths (cfg svc :brief-paths)
                                             :vocabulary (cfg svc :vocabulary)}
                                      draft (assoc :draft draft :draft-path (ts/draft-path store id))
                                      candidates (assoc :candidates candidates
                                                        :candidates-path (ts/pattern-candidates-path store id))))
            reset-every (cfg svc :reset-every)
            count-before (:dispatch-count @(:state svc))
            reset? (and reset-every (>= count-before reset-every) (cfg svc :reset-seat!))
            _ (when reset?
                (try ((cfg svc :reset-seat!) agent) (catch Exception _ nil)))
            response (try ((cfg svc :bell!) {:agent-id agent :prompt brief
                                             :caller (cfg svc :caller)
                                             :surface (cfg svc :surface)
                                             :mode "work" :type "request"})
                          (catch Exception e {:ok false :error (.getMessage e)}))]
        (if (and (:ok response) (:job-id response))
          (let [job-id (str (:job-id response))]
            (swap! (:state svc) assoc :dispatch-count (if reset? 1 (inc count-before)))
            (try (ts/update-record! store id #(assoc-in % [:analysis_dispatch :job_id] job-id))
                 (catch Exception _ nil))
            (schedule-reap! svc id agent nil store-busy-delays (cfg svc :reap-after))
            {:dispatched true :agent agent :job-id job-id})
          (let [reason (str "dispatch to " agent " failed"
                            (when-let [s (:status response)] (str " (http " s ")"))
                            (when-let [e (:error response)] (str ": " e)))]
            (set-health! svc :failing reason)
            {:dispatched false :agent agent :reason reason :response response})))))

;; ---------------------------------------------------------------------------
;; Retry (turn_dispatch_reap.py --retry)

(defn retry!
  "Put ID back to `requested` before re-dispatching it; the old attempt is
   kept under analysis_dispatch.attempts. A \"declared\" record (dispatch
   :none) is settled without a reading and is left alone."
  [svc id]
  (ts/update-record! (cfg svc :store) id
                     (fn [record]
                       (if (= "declared" (:analysis_status record))
                         record
                         (let [disp (or (:analysis_dispatch record) {})
                               old (select-keys disp [:job_id :outcome])
                               disp (cond-> (dissoc disp :job_id :outcome)
                                      (seq old) (update :attempts (fnil conj []) old))]
                           (assoc record :analysis_dispatch disp :analysis_status "requested")))))
  nil)

;; ---------------------------------------------------------------------------
;; Withdrawals (session-mode--process-withdrawals)

(defn- field [record k]
  (let [v (get record k)] (when-not (nil? v) v)))

(defn- post-inferred-withdrawal [svc record-id record fragment-id fragment]
  (let [target (tr/g fragment :target)
        idempotency-key (str record-id ":" fragment-id)]
    (if (nil? target)
      {:fragment_id fragment-id :status 422 :reason "target-unresolved"
       :idempotency_key idempotency-key}
      (let [payload (cond-> {:caller "xiang"
                             :agent (field record :agent_id)
                             :session (field record :session_id)
                             :interpretation-id (or (field record :turn_id) record-id)
                             :interpretation-version (field record :interpretation_version)
                             :idempotency-key idempotency-key}
                      (not= target "seat-active-card") (assoc :target target))
            post (cfg svc :post-withdrawal)
            response (try (if post (post payload) {:status 0 :error "no withdrawal route bound"})
                          (catch Exception e {:status 0 :error (.getMessage e)}))
            body (:json response)
            reason (or (some-> (tr/g body :reason) name) (:error response))
            effect-id (or (tr/g (tr/g body :record) :id) (tr/g body :effect-id))]
        {:fragment_id fragment-id :status (or (:status response) 0)
         :reason reason :effect_id effect-id :idempotency_key idempotency-key}))))

(defn- post-negation-interpretation [svc record fragment-id fragment]
  (let [operator-evidence-id (field record :evidence_id)]
    (if-not (and (string? operator-evidence-id) (not (str/blank? operator-evidence-id)))
      {:fragment_id fragment-id :status 0 :evidence_id nil :resolution nil :reason "no-evidence-id"}
      (let [target (tr/g fragment :target)
            payload (cond-> {:caller "xiang"
                             :operator-evidence-id operator-evidence-id
                             :fragment-id fragment-id
                             :fragment-text (tr/g fragment :text)
                             :analysis-version (field record :interpretation_version)}
                      (and (string? target) (str/starts-with? target "act:")) (assoc :target target))
            post (cfg svc :post-negation)
            response (try (if post (post payload) {:status 0 :error "no negation route bound"})
                          (catch Exception e {:status 0 :error (.getMessage e)}))
            body (:json response)
            entry (tr/g body :entry)
            evidence-id (tr/g entry :evidence/id)
            resolution (tr/g (tr/g entry :evidence/body) :resolution)
            reason (or (some-> (tr/g body :reason) name) (:error response))]
        {:fragment_id fragment-id :status (or (:status response) 0)
         :evidence_id evidence-id :resolution resolution :reason reason}))))

(defn- deliver-notice
  "Publish OUTCOME's header notice once; returns the outcome, changed or not."
  [svc record outcome]
  (if-let [notice (tr/withdrawal-notice outcome)]
    (if (or (:header_notice_published_at outcome) (:header_notice_give_up_reason outcome))
      outcome
      (let [payload (cond-> {:caller "xiang"
                             :agent (:agent_id record) :session (:session_id record)
                             :notice-id (:idempotency_key outcome)
                             :kind (:kind notice)}
                      (:effect_id notice) (assoc :effect-id (:effect_id notice)))
            deliver (cfg svc :deliver-notice)
            response (try (if deliver (deliver payload) {:status 0 :error "no notice route bound"})
                          (catch Exception e {:status 0 :error (.getMessage e)}))
            status (or (:status response) 0)]
        (if (<= 200 status 299)
          (assoc outcome :header_notice_published_at (iso (now-ms svc)))
          (let [attempts (inc (or (:header_notice_attempts outcome) 0))]
            (cond-> (assoc outcome :header_notice_attempts attempts)
              (>= attempts (cfg svc :notice-max-attempts))
              (assoc :header_notice_give_up_reason
                     (str "http-" status (when-let [e (:error response)] (str ":" e)))))))))
    outcome))

(defn process-withdrawals!
  "Apply newly analysed withdraw interpretations for record ID: post each
   as a provisional withdrawal and a negation interpretation (once, by
   fragment id), publish its header notice, and write every outcome onto the
   record. Returns the outcomes written."
  [svc id]
  (let [store (cfg svc :store)
        record (ts/read-record store id)
        analysis (ts/read-analysis store id)]
    (when (and record analysis (= "operator" (:origin record)))
      (let [existing (vec (or (:withdrawal_effects record) []))
            done (set (map :fragment_id existing))
            negations (vec (or (:negation_interpretations record) []))
            negation-done (set (map :fragment_id negations))
            pairs (tr/withdrawal-fragments analysis)
            new-outcomes (vec (for [[fid fragment] pairs :when (not (done fid))]
                                (post-inferred-withdrawal svc id record fid fragment)))
            new-negations (vec (for [[fid fragment] pairs :when (not (negation-done fid))]
                                 (post-negation-interpretation svc record fid fragment)))
            _ (when (and (some #(and (= 403 (:status %)) (= "no-grant" (:reason %))) new-outcomes)
                         (not (:notified-no-grant @(:state svc))))
                (swap! (:state svc) assoc :notified-no-grant true)
                (set-health! svc nil "象: inferred withdrawals are off until Joe's grant exists"))
            all (mapv #(deliver-notice svc record %) (into existing new-outcomes))
            all-negations (into negations new-negations)]
        (when (or (seq new-outcomes) (seq new-negations) (not= all existing))
          (ts/update-record! store id #(assoc % :withdrawal_effects all
                                              :negation_interpretations all-negations)))
        all))))

(defn- handle-analyzed! [svc id]
  (note-done! svc id)
  (let [failure (try (process-withdrawals! svc id) nil (catch Throwable t t))]
    (if failure
      (do (try (ts/update-record! (cfg svc :store) id
                                  #(assoc % :withdrawal_processing_error
                                          {:at (iso (now-ms svc))
                                           :error (.getName (class failure))
                                           :message (subs (str (.getMessage ^Throwable failure))
                                                          0 (min 500 (count (str (.getMessage ^Throwable failure)))))}))
               (catch Exception _ nil))
          (set-health! svc :failing (str id ": withdrawal processing failed")))
      (do (try (ts/update-record! (cfg svc :store) id #(assoc % :withdrawal_processing_error nil))
               (catch Exception _ nil))
          (set-health! svc :ok (str id ": analysed"))))))

;; ---------------------------------------------------------------------------
;; Reap (session-mode--reap-dispatch + turn_dispatch_reap.py --apply)

(defn reap!
  "Ask what became of ID's dispatch and write the answer onto the record.
   OPTS: :agent (the seat it went to), :tries (reaps left; nil = the full
   schedule), :store-busy-delays (remaining redispatch schedule, or
   :exhausted). Returns {:outcome KEYWORD ...} where KEYWORD is one of
   :analyzed :running :refused :failed :unreachable :benched :store-busy
   :no-job-id :record-not-found."
  [svc id {:keys [agent tries store-busy-delays]}]
  (let [store (cfg svc :store)
        record (ts/read-record store id)]
    (cond
      (nil? record) {:outcome :record-not-found}

      ;; The published analysis is the authority, not the flag.
      (or (ts/analysis-published? store id)
          (not (contains? #{nil "requested"} (:analysis_status record))))
      (do (when (ts/analysis-published? store id) (handle-analyzed! svc id))
          {:outcome :analyzed :status (:analysis_status record)})

      :else
      (let [job-id (get-in record [:analysis_dispatch :job_id])]
        (if-not job-id
          {:outcome :no-job-id}
          (let [job (try ((cfg svc :job-status) job-id)
                         (catch Exception e {:unreachable (.getMessage e)}))
                {:keys [outcome state reason]} (tr/job-outcome job)
                late-tries (cfg svc :reap-late-tries)
                tries (or tries (+ 3 late-tries))]
            (case outcome
              :unreachable
              (do (set-health! svc :failing (str id ": job status unreachable"))
                  {:outcome :unreachable :reason reason})

              :running
              (if (> tries 1)
                (let [left (dec tries)
                      delay (if (> left late-tries) (cfg svc :reap-after) (cfg svc :reap-late-after))]
                  (schedule-reap! svc id agent left store-busy-delays delay)
                  {:outcome :running :state state :next-reap-s delay :tries-left left})
                {:outcome :running :state state :tries-left 0})

              ;; :refused or :failed
              (cond
                ;; The seat ran out of usage: bench it and send the turn elsewhere.
                ;; Bench BEFORE choosing the other seat. Emacs chose first and
                ;; benched second, which with a pool picked another pool seat
                ;; and then benched it along with the rest, so the failover the
                ;; pool docstring promises (the alternate takes over) never ran.
                (and agent (tr/quota-failure? reason)
                     (let [_ (bench-seat! svc agent)
                           other (other-seat svc agent)]
                       (when (and other (not (benched? svc other)))
                         (retry! svc id)
                         (set-health! svc nil (str agent " out of usage; " other " took " id))
                         (dispatch! svc id {:agent other})
                         true)))
                {:outcome :benched :agent agent :reason reason}

                ;; Transient futon1b admission load: retry on the bounded schedule.
                (and (tr/store-busy-failure? reason)
                     (not= store-busy-delays :exhausted)
                     (seq (or store-busy-delays (cfg svc :store-busy-delays))))
                (let [delays (or store-busy-delays (cfg svc :store-busy-delays))
                      delay (first delays)
                      remaining (or (seq (rest delays)) :exhausted)]
                  (set-health! svc nil (str id ": futon1b busy; retrying in " delay "s"))
                  ((cfg svc :schedule!) delay
                   (fn [] (try (retry! svc id)
                               (dispatch! svc id {:agent agent :store-busy-delays remaining})
                               (catch Throwable t
                                 (set-health! svc :failing (str id ": could not prepare store-busy retry: " (.getMessage t)))))))
                  {:outcome :store-busy :retry-in-s delay})

                :else
                (do (ts/update-record! store id
                                       #(-> % (assoc :analysis_status (name outcome))
                                            (assoc-in [:analysis_dispatch :outcome] {:state state :reason reason})))
                    (note-done! svc id)
                    (set-health! svc :failing (str id ": job refused or failed"))
                    {:outcome outcome :state state :reason reason})))))))))

;; ---------------------------------------------------------------------------
;; Publish (session_turn_analysis.py complete)

(defn publish-analysis!
  "Validate ANALYSIS against record ID and publish it beside the record,
   then process its withdrawals. OPTS :pattern-source and :rnode-contract as
   for `tr/validate-analysis`. Throws ex-info {:reason :invalid-analysis |
   :analysis-exists | :record-not-found}."
  [svc id analysis opts]
  (let [store (cfg svc :store)
        record (or (ts/read-record store id)
                   (throw (ex-info "Record not found" {:reason :record-not-found :id id})))
        canonical (-> (tr/validate-analysis record analysis (assoc opts :now-ms (now-ms svc)))
                      (tr/annotate-with-draft (ts/read-draft store id))
                      (assoc :request_file (ts/record-path store id)
                             :request_sha256 (tr/sha256 (slurp (ts/record-path store id) :encoding "UTF-8"))))
        published (ts/publish-analysis! store id canonical)]
    (settle-evidence! svc id canonical :analyzed)
    (handle-analyzed! svc id)
    (assoc published :analysis canonical)))

;; ---------------------------------------------------------------------------
;; Reading

(defn turn-view
  "Record ID with its analysis and candidates, for a client: {:id :record
   :analysis :candidates :notices}. :notices are the fixed withdrawal notice
   lines a REPL would have shown, derived from withdrawal_effects."
  [svc id]
  (let [store (cfg svc :store)]
    (when-let [record (ts/read-record store id)]
      {:id id
       :record record
       :draft (ts/read-draft store id)
       :pattern_candidates (ts/read-pattern-candidates store id)
       :analysis (ts/read-analysis store id)
       :candidates (ts/read-candidates store id)
       :notices (vec (keep (fn [o] (when-let [n (tr/withdrawal-notice o)]
                                    (assoc n :fragment_id (:fragment_id o))))
                           (:withdrawal_effects record)))})))

(defn draft-agreement
  "How often 象's reading changed 小象's draft, summed over the store's
   analysed records (optionally one :session-id / :agent-id): the
   :draft_agreement counts, plus :drafted (settled without a reading),
   :with-draft and :without-draft record counts. This is the number that
   says whether the LLM pass still earns its cost for a given intent."
  [svc & {:keys [session-id agent-id limit] :or {limit 1000}}]
  (let [store (cfg svc :store)
        records (ts/list-records store :session-id session-id :agent-id agent-id :limit limit)
        analyses (keep (fn [{:keys [id record]}]
                         (when (= "analyzed" (:analysis_status record))
                           [(:draft_agreement (ts/read-analysis store id)) record]))
                       records)]
    {:records (count records)
     :drafted (count (filter #(= "drafted" (get-in % [:record :analysis_status])) records))
     :analysed (count analyses)
     :with-draft (count (filter first analyses))
     :without-draft (count (remove first analyses))
     :totals (reduce (fn [acc [da _]] (merge-with + acc (or da {})))
                     {:agreed 0 :relabelled 0 :resegmented 0 :new 0 :dropped 0 :unsure 0
                      :draft_fragments 0 :published_fragments 0}
                     analyses)}))
