(ns futon3c.inbox-zero.board-consumer
  "Consume replay-verified board proposals through the existing promotion
  planner, sensitivity screen, gates and Git executor. Never invent claims.
  The watcher remains the sole state writer. Fresh observations here are
  read-only overlays, retained in the cycle evidence."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3.inbox-zero.escalation :as escalation]
            [futon3.inbox-zero.gates :as gates]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.promotion :as promotion]
            [futon3.inbox-zero.promote-exec :as executor]
            [futon3.inbox-zero.watcher :as watcher]
            [futon3c.agents.inbox-zero-board :as board]
            [futon3c.agents.inbox-zero-board-live :as live]
            [futon3c.inbox-zero.turn-promotion :as turn])
  (:import [java.time Instant]
           [java.util Date]
           [java.nio.channels FileChannel]
           [java.nio.file StandardOpenOption]))

(defn read-state
  "Read the same atomic watcher snapshot as the live board. Missing state is
  an error here: no evidence is not a clean sweep. Never rewrite the snapshot."
  [path]
  (let [state (edn/read-string (slurp path))]
    (when-not (and (= 0 (:schema/version state)) (map? (:records state)))
      (throw (ex-info "Invalid watcher snapshot" {:reason :invalid-state})))
    state))

(defn refresh-repo
  "Use the standing watcher's Git observation boundary, without becoming a
  second snapshot writer. New edits become recent activity, including edits
  the background watcher has not yet seen."
  [state root now]
  (let [observations (watcher/observe-repo
                      state {:path root :label (.getName (io/file root))}
                      (Date/from now))]
    {:state (update state :records into
                    (map (juxt :observation/id identity) observations))
     :observations observations}))

(defn proposal-run [state now]
  (let [records (:records state)
        inputs (board/observation-packet (live/sweep-from-records records now)
                                        (live/in-flight-from-records records now)
                                        false)]
    (assoc (board/run inputs (constantly nil)) :inputs inputs)))

(defn verified-proposal?
  "Replay in this process; a caller-supplied verified? boolean is insufficient.
  The registry digest is process-local, so foreign-process proposals refuse."
  [run]
  (try
    (let [replayed (board/run (:inputs run) (constantly nil))]
      (and (= (:certificate replayed) (:certificate run))
         (= (:trace replayed) (:trace run))
         (= (:end-reason replayed) (:end-reason run))
         (= (:verbs/digest replayed) (:verbs/digest run))))
    (catch Exception _ false)))

(defn commit-message [run plan]
  (str "inbox-zero: promote " (count (:include plan)) " path(s) for " (:seat/id plan)
       "\n\nInbox-zero-board-digest: " (get-in run [:certificate :board/digest])
       "\nInbox-zero-inputs-digest: " (get-in run [:certificate :inputs/digest])
       "\nInbox-zero-verbs-digest: " (:verbs/digest run)))

(defn eligible-plans
  "Use existing claim planning, intersecting with the dirty-set projection
  which invalidates claims separated from current dirt by a clean transition."
  [state root now]
  (let [current (projection/current-observations state)
        worktrees (set (map :worktree/id (filter #(= root (:repo/root %)) (vals current))))
        dirty (projection/project-dirty-sets state (Date/from now))
        sets (filter #(worktrees (:worktree/id %)) (:dirty-sets dirty))]
    {:unattributed (filterv #(worktrees (:worktree/id %)) (:unattributed dirty))
     :ambiguous (filterv #(worktrees (:worktree/id %)) (:ambiguous dirty))
     :plans (vec
             (for [dirty-set sets
                   plan (promotion/plan-promotion state (:seat/id dirty-set) (Date/from now))
                   :when (and (= (:repo/id dirty-set) (:repo/id plan))
                              (= (:worktree/id dirty-set) (:worktree/id plan)))
                   :let [claims (set (map :claim/id (:members dirty-set)))]]
               (update plan :include #(filterv (comp claims :claim/id) %))))}))

(defn consume!
  "Consume one board run. Record intent before Git and result after Git.
  All refusals retain the proposal certificate. Exceptions become refusals;
  ledger failures propagate, because an unrecorded action is not admissible.
  Options expose IO boundaries for focused tests. No transport sends or pushes."
  [run {:keys [state-path record! now-fn load-fn refresh-fn execute-fn gate-specs]
        :or {now-fn #(Instant/now) load-fn read-state refresh-fn refresh-repo
             execute-fn executor/execute-plan! gate-specs []}}]
  (let [records (atom [])
        emit! (fn [record]
                (let [record (assoc record :certificate (:certificate run)
                                          :verbs/digest (:verbs/digest run)
                                          :recorded-at (str (now-fn)))]
                  (record! record)
                  (swap! records conj record)
                  record))
        refuse! (fn [repo reason details]
                  (emit! {:record/type :inbox-zero/refusal :repo repo
                          :refusal/reason reason :details details}))
        fresh! (fn [root]
                 (let [now (now-fn)
                       refreshed (refresh-fn (load-fn state-path) root now)]
                   (when (contains? (live/in-flight-from-records
                                      (get-in refreshed [:state :records]) now)
                                    (.getName (io/file root)))
                     (throw (ex-info "Repository is in flight"
                                     {:reason :in-flight :observations (:observations refreshed)})))
                   (:state refreshed)))
        effects (mapcat :effects (:trace run))]
    (if-not (verified-proposal? run)
      (refuse! nil :invalid-certificate {})
      (doseq [[kind {:keys [repo] :as payload}] effects]
        (case kind
          :refusal (refuse! repo (:reason payload) payload)
          :commit
          (let [outcome
                (try
                  (let [state (load-fn state-path)
                        roots (->> (live/file-observations (:records state))
                                   (map :repo/root) distinct
                                   (filter #(= repo (.getName (io/file %))))
                                   vec)]
                    (when-not (= 1 (count roots))
                      (throw (ex-info "Proposal must resolve one repository root"
                                      {:reason :ambiguous-repo-root :roots roots})))
                    (let [root (first roots)
                          state (fresh! root)
                          {:keys [plans unattributed ambiguous]} (eligible-plans state root (now-fn))]
                      {:root root :plans plans :unattributed unattributed :ambiguous ambiguous}))
                  (catch Exception e {:error (merge {:reason :execution-error
                                                    :message (.getMessage e)} (ex-data e))}))]
            (if-let [error (:error outcome)]
              (refuse! repo (:reason error) error)
              (do
                (doseq [reason [:unattributed :ambiguous]
                        :let [paths (get outcome reason)] :when (seq paths)]
                  (refuse! repo reason {:paths paths :next-action :establish-attribution}))
                (when (and (empty? (:plans outcome))
                           (empty? (:unattributed outcome)) (empty? (:ambiguous outcome)))
                  (refuse! repo :nothing-promotable {}))
                (doseq [plan (:plans outcome)]
                  (let [result
                        (try
                          (let [root (:root outcome)
                                plan (update plan :include
                                             #(mapv (fn [entry]
                                                      (assoc entry :size (.length (io/file root (:path entry))))) %))
                                screened (escalation/screen-sensitivity plan escalation/default-rules)
                                _ (when (= :held (:verdict screened))
                                    (throw (ex-info "Promotion held" {:reason (:held/reason screened)
                                                                      :plan screened})))
                                _ (when (empty? gate-specs)
                                    (throw (ex-info "Explicit validation gates required"
                                                    {:reason :validation-required :plan plan})))
                                validation (gates/run-gates root (gates/validate-gates! gate-specs)
                                                            (mapv :path (:include plan)))
                                _ (when-not (:passed? validation)
                                    (throw (ex-info "Validation failed" {:reason :gate-failed
                                                                         :validation validation})))
                                ;; Re-check after potentially long gates, before staging.
                                fresh (fresh! root)
                                refreshed (eligible-plans fresh root (now-fn))
                                _ (when-not (some #(= (dissoc plan :computed-at :include)
                                                                     (dissoc % :computed-at :include))
                                                 (:plans refreshed))
                                    (throw (ex-info "Attribution changed" {:reason :stale-attribution})))
                                _ (when-not (some #(= (mapv (fn [p] (dissoc p :size)) (:include plan))
                                                      (:include %)) (:plans refreshed))
                                    (throw (ex-info "Path claims changed" {:reason :stale-attribution})))
                                intent {:record/type :inbox-zero/commit-intent :repo repo
                                        :plan plan :validation validation}]
                            ;; Keep ledger failure outside the exception-to-refusal boundary.
                            {:intent intent :root root :plan plan :validation validation})
                          (catch Exception e {:error (merge {:reason :execution-error
                                                            :message (.getMessage e)} (ex-data e))}))]
                    (if-let [error (:error result)]
                      (refuse! repo (:reason error) error)
                      (do
                        (emit! (:intent result))
                        (let [executed (try
                                         (fresh! (:root result))
                                         (execute-fn (:plan result)
                                                     {:repo-root (:root result) :gates []
                                                      :message (commit-message run (:plan result))})
                                         (catch Exception e
                                           {:verdict :held
                                            :held/reason (or (:reason (ex-data e)) :execution-error)
                                            :details (ex-data e) :message (.getMessage e)}))]
                          (if (= :committed (:verdict executed))
                            (emit! {:record/type :inbox-zero/commit-witness :repo repo
                                    :result executed :validation (:validation result)})
                            (refuse! repo (:held/reason executed) executed))))))))))
          nil)))
    ;; FEEL's safe arm reports without emitting a refusal; make that refusal durable.
    (when (and (verified-proposal? run) (not-any? #(= :commit (first %)) effects))
      (doseq [row (get-in run [:inputs :sweep]) :when (not (:clean? row))]
        (refuse! (:repo row) (if (some #{(:repo row)} (get-in run [:inputs :in-flight-repos]))
                              :in-flight :board-did-not-propose) {})))
    @records))

(defn run-cycle!
  "One bounded pass, one board invocation per flagged repository, then a fresh
  watcher observation and board run. Full packets and certificates are saved.
  A file lock excludes overlapping consumers sharing this watcher state path."
  [{:keys [state-path ledger-path] :as options}]
  (io/make-parents ledger-path)
  (with-open [channel (FileChannel/open (.toPath (io/file (str state-path ".consumer.lock")))
                                       (into-array StandardOpenOption
                                                   [StandardOpenOption/CREATE StandardOpenOption/WRITE]))]
    (with-open [lock (.tryLock channel)]
      (when-not lock (throw (ex-info "Consumer already running" {:reason :consumer-busy})))
      (let [now (Instant/now)
            before (proposal-run (read-state state-path) now)
            flagged (filterv (complement :clean?) (get-in before [:inputs :sweep]))
            record! #(turn/append-escalation! ledger-path %)
            _ (record! {:record/type :inbox-zero/cycle-start :run before :at (str now)})
            consumed (mapv (fn [row]
                             (let [inputs (assoc (:inputs before) :sweep [row])
                                   run (assoc (board/run inputs (constantly nil)) :inputs inputs)]
                               (record! {:record/type :inbox-zero/proposal :run run})
                               {:run run :records (consume! run (assoc options :record! record!))}))
                           flagged)
            state (read-state state-path)
            roots (sort (distinct (map :repo/root (live/file-observations (:records state)))))
            refreshed (reduce (fn [{:keys [state observations]} root]
                                (let [r (refresh-repo state root (Instant/now))]
                                  {:state (:state r) :observations (into observations (:observations r))}))
                              {:state state :observations []} roots)
            after (proposal-run (:state refreshed) (Instant/now))
            result {:record/type :inbox-zero/cycle-result
                    :before before :after after :consumed consumed
                    :after/observations (:observations refreshed)
                    :flags/before (count flagged)
                    :flags/after (count (remove :clean? (get-in after [:inputs :sweep])))}]
        (record! result)
        result))))

(defn -main [& [options-edn]]
  (let [result (run-cycle! (merge {:state-path turn/default-state-path
                                   :ledger-path "/home/joe/code/storage/inbox-zero/board-consumer.edn"}
                                  (when options-edn (edn/read-string options-edn))))]
    (prn (select-keys result [:flags/before :flags/after]))
    (shutdown-agents)))
