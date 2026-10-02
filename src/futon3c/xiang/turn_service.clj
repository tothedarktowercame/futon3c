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
     :schedule!       (fn [delay-seconds thunk]) default: a daemon scheduler
     :now-ms          (fn [] ms) default System/currentTimeMillis

   Delivery is not accomplishment: a bell that was accepted leaves the record
   `requested` with its job id, and only a reap or a publication moves it.
   The REPL-side notice (Emacs inserted a system line into the buffer) is not
   delivered here: a web client reads `withdrawal_effects` off the record and
   shows the notice itself, so the record carries no `repl_notice_*` fields
   written by the server."
  (:require [clojure.string :as str]
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
   :surface "emacs-repl"                        ; agency_send.py --surface default
   :vocabulary tr/default-vocabulary
   :brief-paths tr/default-brief-paths})

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
;; Record (session-mode--record-turn via the store)

(declare dispatch!)

(defn record-turn!
  "Record one operator turn. OPTS are `tr/make-record`'s, plus :dispatch
   (:later, the default, waits for `attach-happened!`; :now dispatches at
   once, as an externally captured turn does). Returns {:id :record
   :dispatch} where :dispatch is the dispatch result or :pending/:skipped.
   A turn addressed to the analysis seat itself is recorded, never dispatched."
  [svc {:keys [dispatch agent-id] :or {dispatch :later} :as opts}]
  (let [{:keys [record redacted]} (tr/make-record (merge {:vocabulary (cfg svc :vocabulary)
                                                          :now-ms (now-ms svc)}
                                                         (dissoc opts :dispatch)))
        {:keys [id]} (ts/write-record! (cfg svc :store) record)
        to-seat? (= (cfg svc :seat) agent-id)
        result (cond
                 to-seat? :skipped
                 (= dispatch :now) (dispatch! svc id {})
                 :else :pending)]
    {:id id :record record :redacted redacted :dispatch result}))

(defn attach-happened!
  "Attach what the agent did while answering turn ID, then dispatch it: 象
   reads the turn together with the reply. HAPPENED is a ready summary string
   or {:reply TEXT :commits [...]}. A failure to build or store the summary
   still dispatches, as in Emacs."
  [svc id happened]
  (let [summary (try (cond (string? happened) happened
                           (map? happened) (happened-summary (:reply happened) (:commits happened))
                           :else nil)
                     (catch Exception _ nil))]
    (when summary
      (try (ts/update-record! (cfg svc :store) id #(assoc % :happened_summary summary))
           (catch Exception _ nil)))
    (let [record (ts/read-record (cfg svc :store) id)]
      (cond
        (nil? record) {:dispatched false :reason :record-not-found}
        (= (cfg svc :seat) (:agent_id record)) {:dispatched false :reason :addressed-to-seat}
        :else (dispatch! svc id {})))))

;; ---------------------------------------------------------------------------
;; Dispatch (session-mode--dispatch-analysis)

(declare reap!)

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
      (let [_ (note-dispatch! svc id agent)
            brief (tr/analysis-brief id path {:requisition (cfg svc :requisition)
                                              :paths (cfg svc :brief-paths)
                                              :vocabulary (cfg svc :vocabulary)})
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
            {:dispatched false :agent agent :reason reason :response response}))))))

;; ---------------------------------------------------------------------------
;; Retry (turn_dispatch_reap.py --retry)

(defn retry!
  "Put ID back to `requested` before re-dispatching it; the old attempt is
   kept under analysis_dispatch.attempts."
  [svc id]
  (ts/update-record! (cfg svc :store) id
                     (fn [record]
                       (let [disp (or (:analysis_dispatch record) {})
                             old (select-keys disp [:job_id :outcome])
                             disp (cond-> (dissoc disp :job_id :outcome)
                                    (seq old) (update :attempts (fnil conj []) old))]
                         (assoc record :analysis_dispatch disp :analysis_status "requested"))))
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
                      (assoc :request_file (ts/record-path store id)
                             :request_sha256 (tr/sha256 (slurp (ts/record-path store id) :encoding "UTF-8"))))
        published (ts/publish-analysis! store id canonical)]
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
       :analysis (ts/read-analysis store id)
       :candidates (ts/read-candidates store id)
       :notices (vec (keep (fn [o] (when-let [n (tr/withdrawal-notice o)]
                                    (assoc n :fragment_id (:fragment_id o))))
                           (:withdrawal_effects record)))})))
