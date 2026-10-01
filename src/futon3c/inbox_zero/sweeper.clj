(ns futon3c.inbox-zero.sweeper
  "Dirty-repo pressure reporting: the lane that keeps inbox zero visible.

  One bounded pass per interval. For each watched repo, read `git status`
  and count the dirty paths. Repos over the threshold are reported as
  UNCERTAIN-OWNERSHIP pressure to the operator surfaces (operator backlog
  file and the uncertain-pressure feed merged into the mana snapshot),
  with temporal window overlaps attached as labeled diagnostics.

  No personal cleanup assignment is made from this lane. Temporal overlap
  and historical claim/confirmation records are not evidence of who wrote
  the current bytes (M-inbox-zero-claim-lifecycle, C8; the stale-claim
  incident), and no current-schema record establishes dirty-byte
  authorship. Until the claim lifecycle provides one, authorship is
  reported as unknown and stays visible repo-wide rather than being routed
  to a seat. Attribution is NOT by file mtime against the Agency's invoke windows — a file
  written at T belongs to whoever was invoking at T. The agent tool stream
  plays no part: on this stack the great majority of dirt is run output
  written by processes agents launch, not by their editor tools, so a lane
  keyed on Edit/Write witnesses observes almost nothing (2 of 238 paths on
  2026-09-17, and zero proposals in the three weeks to then).

  Three lanes, split by whether a person has to decide anything.
  Dirty files INFORM: deciding what is yours and whether it should be kept
  needs a person, so that lane never stages, commits or mints a claim.
  Unpushed commits ACT: pushing a commit already written decides nothing, so
  that lane pushes rather than asking anyone to (Joe, 2026-09-18).
  Merged worktrees ACT: every commit in them is already on the pushed branch,
  so removing one loses nothing there is any way to lose.

  Dirty-file pressure retains a threshold of ten because attribution and file
  disposition require work. The push lane acts at one commit: a commit that
  exists only on one machine is already exposed, and pushing it needs no batch.
  The worktree lane likewise has no count threshold.

  All three report their counts on every pass, so a silent pass cannot look
  like a working one."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.watcher.roots :as roots])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]
           [java.util Date]))

(def default-threshold 10)
(def default-push-threshold 1)
(def default-interval-ms 1800000)

(defonce ^:private !loop (atom nil))

(def ^:private sample-size 5)

;; A turn Joe drives from the REPL creates no invoke job — the ledger only
;; records dispatched work — so an agent in conversation with the operator is
;; invisible to window attribution, which is the case that matters most. An
;; agent the roster reports as invoking gets an open window back one interval.
(def ^:private operator-turn-grace-ms 1800000)
(def ^:private max-dir-walk 2000)

;; ---------- the filesystem side ----------

(defn- mtime-ms
  "Last-write time of PATH in ms, or nil when it has gone. An untracked
  directory reports the newest file beneath it: git names the directory,
  but the writes happened to its contents."
  [^java.io.File file]
  (cond
    (not (.exists file)) nil
    (.isDirectory file) (->> (file-seq file)
                             (filter #(.isFile ^java.io.File %))
                             (take max-dir-walk)
                             (map #(.lastModified ^java.io.File %))
                             (reduce max 0)
                             (#(when (pos? %) %)))
    :else (let [t (.lastModified file)] (when (pos? t) t))))

(defn- status-entry
  "Parse one NUL-terminated `git status --porcelain -z` record."
  [root record]
  (when (>= (count record) 4)
    (let [code (subs record 0 2)
          path (subs record 3)]
      (when-not (str/blank? path)
        {:path path
         :status code
         :untracked? (= "??" code)
         :mtime-ms (mtime-ms (io/file root path))}))))

(defn git-dirty
  "Dirty entries in the repo at ROOT, as parsed status records.

  Rename records carry the old path after a second NUL; `git status -z`
  emits it as its own record, which parses to nil and drops out."
  [root]
  (let [{:keys [exit out]} (shell/sh "git" "status" "--porcelain" "-z" :dir root)]
    (if-not (zero? exit)
      []
      (->> (str/split out #"\x00")
           (keep #(status-entry root %))
           vec))))

(defn git-unpushed
  "Commits on HEAD that its upstream does not have, newest first.

  Measured against HEAD's OWN upstream, not against a repo's default branch.
  The branch that ought to be pushed is the one being worked on: mathlib4
  works on `darktower` while origin/HEAD still says `master`, and measuring
  the default branch reported it clean for a week with 217 commits stranded
  (Joe, 2026-09-18).

  Entries are shaped like git-dirty's so attribute can name commits to the
  turns that produced them without knowing which lane it is reading. A repo
  with no upstream yields nothing here -- that is a different failure, and
  reporting it as zero pressure to push is the honest reading of it.

  Pressure is the COUNT, never the age. A commit that exists only on this box
  is not saved, and the box is bare metal that can break at any moment, so
  waiting out a staleness window is not a safety margin — it is exposure."
  [root]
  (let [{:keys [exit out]} (shell/sh "git" "log" "--format=%H%x1f%ct%x1f%s"
                                     "@{upstream}..HEAD" :dir root)]
    (if-not (zero? exit)
      []
      (->> (str/split-lines out)
           (keep (fn [line]
                   (let [[sha ct subject] (str/split line #"\x1f" 3)]
                     (when-let [ms (and sha (not (str/blank? (str ct)))
                                        (try (* 1000 (Long/parseLong (str/trim ct)))
                                             (catch Exception _ nil)))]
                       {:path (str (subs sha 0 (min 12 (count sha)))
                                   (when-not (str/blank? subject) (str " " subject)))
                        :sha sha
                        :mtime-ms ms}))))
           vec))))

;; ---------- the who-was-running side ----------

(defn- instant-ms [value]
  (when-not (str/blank? (str value))
    (try (.toEpochMilli (Instant/parse (str value)))
         (catch Exception _ nil))))

(defn- agency-url [path]
  (str "http://127.0.0.1:" (or (System/getenv "FUTON3C_PORT") "7070") path))

(defn- fetch-json [url]
  (let [response (http/get url {:headers {"Accept" "application/json"}
                                :throw false :timeout 10000})]
    (when (= 200 (:status response))
      (json/parse-string (:body response) true))))

(defn default-windows
  "Invoke windows as `[{:agent id :start ms :end ms}]`.

  Two sources. The job ledger covers dispatched work; a job still running has
  no finish time, so its window stays open. The roster covers operator-driven
  turns, which the ledger never sees at all: an agent reported as invoking
  gets a window reaching back one interval from now."
  []
  (let [now (System/currentTimeMillis)
        dispatched
        (->> (:jobs (fetch-json (agency-url "/api/alpha/invoke/jobs?limit=4000")))
             (keep (fn [job]
                     (when-let [start (or (instant-ms (:started-at job))
                                          (instant-ms (:created-at job)))]
                       (when-let [agent (:agent-id job)]
                         {:agent agent :start start
                          :end (or (instant-ms (:finished-at job)) now)})))))
        conversing
        (->> (:agents (fetch-json (agency-url "/api/alpha/agents")))
             (keep (fn [[agent-id agent]]
                     (when (= "invoking" (:status agent))
                       {:agent (name agent-id)
                        :start (- now operator-turn-grace-ms)
                        :end now}))))]
    (vec (concat dispatched conversing))))

(defn default-roster
  "Agent id → session id for every agent the Agency can reach."
  []
  (->> (:agents (fetch-json (agency-url "/api/alpha/agents")))
       (keep (fn [[agent-id agent]]
               (when-let [session (:session-id agent)]
                 [(name agent-id) session])))
       (into {})))

(defn- live-candidates
  "The live agents whose turns cover ENTRY's last write."
  [windows roster entry]
  (if-let [mtime (:mtime-ms entry)]
    (->> windows
         (keep (fn [{:keys [agent start end]}]
                 (when (and (<= start mtime end) (contains? roster agent))
                   agent)))
         set)
    #{}))

(defn diagnostic-overlaps
  "Entry path → sorted live agent ids whose windows contain its mtime.

  DIAGNOSTIC ONLY (C8): temporal overlap is not authorship. A sole overlap
  says only that the agent was running when the file's mtime last changed,
  not that it wrote the file, and the windows themselves are coarse (open
  job windows, synthetic invoking grace). Never route work from this."
  [windows roster entries]
  (into {}
        (keep (fn [entry]
                (let [candidates (live-candidates windows roster entry)]
                  (when (seq candidates)
                    [(:path entry) (vec (sort candidates))]))))
        entries))

(defn uncertain-row
  "One repo's dirty entries as an uncertain-ownership pressure row.

  ROOT is canonicalized so downstream joins (mana snapshot per-repo) match
  by real worktree identity, never by label: futon3c-d and futon3c are
  different roots. :paths carries the COMPLETE per-file drilldown (newest
  first); bounded displays take a prefix and use :remainder for the rest,
  so no file is ever silently dropped from the record."
  [windows roster {:keys [label root entries]}]
  (let [sorted (vec (sort-by :mtime-ms > entries))
        paths (mapv #(select-keys % [:path :mtime-ms]) sorted)]
    {:label label
     :root (.getCanonicalPath (io/file root))
     :dirty-count (count entries)
     :untracked (count (filter :untracked? entries))
     :paths paths
     :remainder (max 0 (- (count paths) sample-size))
     :diagnostic-overlaps (diagnostic-overlaps windows roster entries)}))

(defn- atomic-write! [path value]
  (let [target (.toPath (io/file path))
        parent (or (.getParent target) (.toPath (io/file ".")))
        attrs (make-array FileAttribute 0)]
    (Files/createDirectories parent attrs)
    (let [tmp (Files/createTempFile parent ".commit-notices-" ".edn" attrs)]
      (try
        (spit (.toFile tmp) (str (pr-str value) "\n"))
        (Files/move tmp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists tmp))))))

;; ---------- the pass ----------

(defn- empty-counts []
  {:repos 0 :over-threshold 0 :uncertain 0 :errored 0})

(def ^:private default-backlog-path
  "/home/joe/code/storage/inbox-zero/operator-backlog.edn")

(defn- write-backlog!
  "Overwrite the operator backlog with the repos that are over threshold and
  attributable to nobody alive.

  Rewritten in full every pass, never appended to. That is the whole design:
  escalate-by-who-can-act forbids a queue that waits, and an append-only
  ledger of unowned dirt becomes exactly that — it accumulates entries that
  were resolved hours ago and nobody trusts it by the end of the week. A
  current-state file cannot rot in that direction, because cleaning the repo
  removes its entry on the next pass without anyone acknowledging anything.

  Before 2026-09-18 this case only reached print-fn, i.e. the serving JVM's
  stdout, which is the same unwatched-lamp failure gate-fails-loudly was
  written about — one layer up from the gate it was written about."
  [path rows now print-fn]
  (try
    (atomic-write! path {:at now
                         :generated-by "futon3c.inbox-zero.sweeper"
                         :note (str "Repos over the dirty-file threshold; ownership of "
                                    "the current bytes is unknown (C8). Current state, "
                                    "not a queue: rewritten every pass.")
                         :repos (vec (sort-by :label rows))})
    true
    (catch Throwable error
      (print-fn (str "[inbox-zero] operator backlog unwritable: "
                     (.getMessage error)))
      false)))

(def ^:private default-uncertain-pressure-path
  "/home/joe/code/storage/inbox-zero/uncertain-pressure.edn")

(defn- write-uncertain-pressure!
  "Atomically publish the uncertain-ownership pressure feed that the mana
  snapshot (futon0) merges into the War Machine commit-hygiene queues.
  Current state, rewritten every pass; :interval-ms lets consumers mark
  staleness instead of trusting a silent file."
  [path rows now backlog-path interval-ms diagnostics-available?
   collection-complete? row-failures backlog-written? print-fn]
  (try
    (atomic-write! path {:at now
                         :generated-by "futon3c.inbox-zero.sweeper"
                         :interval-ms interval-ms
                         :drilldown backlog-path
                         :diagnostics-available? (boolean diagnostics-available?)
                         :collection {:complete? (boolean collection-complete?)
                                      :row-failures (long (or row-failures 0))}
                         :publication {:backlog-written? (boolean backlog-written?)}
                         :repos (vec (sort-by :label rows))})
    true
    (catch Throwable error
      (print-fn (str "[inbox-zero] uncertain-pressure feed unwritable: "
                     (.getMessage error)))
      false)))

(defn- finish-pass
  "Publish both outputs and return typed completeness. A pass is :complete?
  only when BOTH the backlog and the pressure feed were written AND every
  over-threshold repo produced a row; failures are reported in counts AND
  propagated into the feed's :collection/:publication status, never
  silently counted as success."
  [counts backlog-path pressure-path now interval-ms aux-ok? print-fn]
  (let [backlog-ok? (write-backlog! backlog-path (:uncertain-rows counts)
                                    now print-fn)
        feed-ok? (write-uncertain-pressure! pressure-path
                                            (:uncertain-rows counts) now
                                            backlog-path interval-ms aux-ok?
                                            (zero? (:row-failures counts))
                                            (:row-failures counts)
                                            backlog-ok? print-fn)
        counts (-> counts
                   (dissoc :uncertain-rows)
                   (assoc :collection-complete? (zero? (:row-failures counts))
                          :backlog-written? (boolean backlog-ok?)
                          :feed-written? (boolean feed-ok?)
                          :diagnostics-available? (boolean aux-ok?)
                          :complete? (and (zero? (:row-failures counts))
                                          (boolean backlog-ok?)
                                          (boolean feed-ok?))))]
    (when-not (:complete? counts)
      (print-fn (str "[inbox-zero] uncertain-pressure pass INCOMPLETE: "
                     "backlog-written?=" (:backlog-written? counts)
                     " feed-written?=" (:feed-written? counts))))
    (print-fn (str "[inbox-zero] uncertain-pressure pass: " (pr-str counts)))
    counts))

(defn sweep-dirty-repos!
  "Run one bounded uncertain-pressure pass. Every collaborator is injectable.

  No personal cleanup notices are sent: no current-schema evidence
  establishes who wrote the current dirty bytes (C8). Every over-threshold
  repo becomes an uncertain-ownership row in BOTH the operator backlog and
  the uncertain-pressure feed, with temporal overlaps attached as labeled
  diagnostics only."
  [options]
  (let [print-fn (or (:print-fn options) println)]
    (try
      (let [threshold (max 1 (long (or (:threshold options)
                                       (some-> (System/getenv "FUTON3C_INBOX_ZERO_THRESHOLD")
                                               Long/parseLong)
                                       default-threshold)))
            watch-roots (or (:roots options) roots/sweep-roots)
            git-fn (or (:git-fn options) git-dirty)
            windows-fn (or (:windows-fn options) default-windows)
            roster-fn (or (:roster-fn options) default-roster)
            now-fn (or (:now-fn options) #(Date.))
            backlog-path (or (:backlog-path options) default-backlog-path)
            pressure-path (or (:pressure-path options)
                              (System/getenv "FUTON3C_INBOX_ZERO_PRESSURE_PATH")
                              default-uncertain-pressure-path)
            interval-ms (long (or (:interval-ms options) default-interval-ms))
            now (now-fn)
            ;; A single repo's git failure must not kill the pass: the repo
            ;; is recorded as a row failure and collection is incomplete,
            ;; but every other repo's pressure is still reported.
            scan (reduce (fn [acc {:keys [path label]}]
                           (try
                             (let [entries (git-fn path)]
                               (if (>= (count entries) threshold)
                                 (update acc :rows conj
                                         {:label label :root path :entries entries
                                          :threshold threshold})
                                 acc))
                             (catch Throwable error
                               (print-fn (str "[inbox-zero] git status failed for "
                                              label ": " (.getMessage error)))
                               (update acc :failures (fnil inc 0)))))
                         {:rows [] :failures 0}
                         watch-roots)
            over (:rows scan)
            scan-failures (:failures scan)
            ;; Only pay for the roster and the job ledger when something is
            ;; over the line; a clean stack costs one git status per repo.
            ;; Their failure degrades diagnostics, never the pressure rows.
            aux-ok? (atom true)
            aux-fn (fn [f fallback]
                     (try (f)
                          (catch Throwable error
                            (reset! aux-ok? false)
                            (print-fn (str "[inbox-zero] diagnostic input "
                                           "unavailable (overlaps omitted): "
                                           (.getMessage error)))
                            fallback)))
            windows (if (seq over) (aux-fn windows-fn []) [])
            roster (if (seq over) (aux-fn roster-fn {}) {})
            counts
            (reduce
             (fn [counts repo]
               (try
                 (let [row (uncertain-row windows roster repo)]
                   (print-fn (str "[inbox-zero] " (:label repo) " has "
                                  (:dirty-count row)
                                  " dirty file(s) over the threshold of "
                                  threshold " — ownership unknown,"
                                  " operator backlog + pressure feed"))
                   (-> counts
                       (update :uncertain inc)
                       (update :uncertain-rows conj row)))
                 (catch Throwable error
                   (print-fn (str "[inbox-zero] uncertain-pressure pass failed for "
                                  (:label repo) ": " (.getMessage error)))
                   (-> counts
                       (update :errored inc)
                       (update :row-failures (fnil inc 0))))))
             (assoc (empty-counts)
                    :repos (count watch-roots)
                    :over-threshold (count over)
                    :errored scan-failures
                    :row-failures scan-failures
                    :uncertain-rows [])
             over)]
        (finish-pass counts backlog-path pressure-path now interval-ms
                     @aux-ok? print-fn))
      (catch Throwable error
        (try
          (print-fn (str "[inbox-zero] uncertain-pressure pass failed: "
                         (.getMessage error)))
          (catch Throwable _))
        (empty-counts)))))

;; ---------- the push lane: act, never notify ----------

(def ^:private default-push-log-path
  "/home/joe/code/storage/inbox-zero/push-log.edn")

(def ^:private push-timeout-ms 120000)

(defn- mid-operation?
  "True when the repo is part-way through a rebase, merge, cherry-pick or
  bisect. HEAD is then a waypoint inside an operation somebody is still
  running, not a branch tip they finished, and pushing it is never what
  anyone meant."
  [root]
  (boolean (some #(.exists (io/file root ".git" %))
                 ["rebase-merge" "rebase-apply" "MERGE_HEAD"
                  "CHERRY_PICK_HEAD" "BISECT_LOG"])))

(defn git-push!
  "Run the plain `git push` in ROOT. Never forces, never names a refspec.

  Non-interactive and bounded: a prompt or a stalled network inside a
  background loop would wedge every later pass, and a wedged sweeper is
  indistinguishable from a clean stack."
  [root]
  (let [proc (.exec (Runtime/getRuntime)
                    (into-array String ["git" "push"])
                    (into-array String ["GIT_TERMINAL_PROMPT=0"
                                        "GIT_SSH_COMMAND=ssh -o BatchMode=yes"
                                        (str "HOME=" (System/getenv "HOME"))
                                        (str "PATH=" (System/getenv "PATH"))
                                        (str "SSH_AUTH_SOCK="
                                             (or (System/getenv "SSH_AUTH_SOCK") ""))])
                    (io/file root))]
    (if-not (.waitFor proc push-timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
      (do (.destroyForcibly proc)
          {:ok? false :output (str "push timed out after " push-timeout-ms "ms")})
      {:ok? (zero? (.exitValue proc))
       :output (str/trim (str (slurp (.getInputStream proc))
                              (slurp (.getErrorStream proc))))})))

(defn- non-fast-forward? [output]
  (boolean (re-find #"(?i)(non-fast-forward|fetch first|behind its remote)"
                    (str output))))

(defn- git-pull-merge! [root]
  (let [proc (.exec (Runtime/getRuntime)
                    (into-array String ["git" "pull" "--no-rebase" "--no-edit"])
                    (into-array String ["GIT_TERMINAL_PROMPT=0"
                                        "GIT_MERGE_AUTOEDIT=no"
                                        "GIT_SSH_COMMAND=ssh -o BatchMode=yes"
                                        (str "HOME=" (System/getenv "HOME"))
                                        (str "PATH=" (System/getenv "PATH"))
                                        (str "SSH_AUTH_SOCK="
                                             (or (System/getenv "SSH_AUTH_SOCK") ""))])
                    (io/file root))]
    (if-not (.waitFor proc push-timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
      (do (.destroyForcibly proc)
          {:ok? false :output (str "pull timed out after " push-timeout-ms "ms")})
      {:ok? (zero? (.exitValue proc))
       :output (str/trim (str (slurp (.getInputStream proc))
                              (slurp (.getErrorStream proc))))})))

(defn git-push-reconciled!
  "Push ROOT, mechanically merging upstream only after non-fast-forward.
  Dirty or mid-operation repositories refuse. A conflicted merge is aborted,
  restoring the original HEAD before the failure is returned."
  [root]
  (let [first-attempt (git-push! root)]
    (if (or (:ok? first-attempt)
            (not (non-fast-forward? (:output first-attempt))))
      first-attempt
      (cond
        (mid-operation? root)
        {:ok? false :output (str (:output first-attempt)
                                 "\nreconciliation refused: Git operation in progress")}

        (seq (git-dirty root))
        {:ok? false :output (str (:output first-attempt)
                                 "\nreconciliation refused: working tree is dirty")}

        :else
        (let [original (str/trim (:out (shell/sh "git" "rev-parse" "HEAD" :dir root)))
              pull (git-pull-merge! root)]
          (if (:ok? pull)
            (let [retry (git-push! root)]
              (assoc retry :reconciled? true :original-head original
                           :reconcile-output (:output pull)))
            (do
              (when (.exists (io/file root ".git" "MERGE_HEAD"))
                (shell/sh "git" "merge" "--abort" :dir root))
              {:ok? false :reconciled? false :original-head original
               :output (str (:output first-attempt) "\nreconciliation failed: "
                            (:output pull))})))))))

(defn push-stranded-commits!
  "Push every watched repo carrying one or more unpushed commits by default.

  An action, not a notice. Pushing a commit that has already been written
  involves no judgement at all: escalate-by-who-can-act puts ordinary
  accumulation at tier 0, which acts without asking anybody, and a notice for
  something needing no judgement is only a queue that waits (Joe, 2026-09-18).
  Ten dirty files means ask the street sweeper to decide their disposition.
  One unpushed commit means push, because nothing needs deciding.

  The threshold is a count and never an age. A commit that exists only on this
  box is not saved, and the box is bare metal that can break at any moment, so
  a staleness window is not a safety margin -- it is the length of time the
  work is allowed to be at risk.

  A FAILED push is the case that does need a person -- a rejected
  non-fast-forward is someone else's history to reconcile -- so failures are
  what this reports, into a current-state file that clears itself the pass
  after the repo is pushed."
  [options]
  (let [print-fn (or (:print-fn options) println)]
    (try
      (let [threshold (max 1 (long (or (:push-threshold options)
                                       (some-> (System/getenv "FUTON3C_INBOX_ZERO_PUSH_THRESHOLD")
                                               Long/parseLong)
                                       default-push-threshold)))
            watch-roots (or (:roots options) roots/sweep-roots)
            ahead-fn (or (:ahead-fn options) git-unpushed)
            push-fn (or (:push-fn options) git-push-reconciled!)
            busy-fn (or (:busy-fn options) mid-operation?)
            now ((or (:now-fn options) #(Date.)))
            log-path (or (:push-log-path options) default-push-log-path)
            over (->> watch-roots
                      (keep (fn [{:keys [path label]}]
                              (let [commits (ahead-fn path)]
                                (when (>= (count commits) threshold)
                                  {:label label :root path :commits commits}))))
                      vec)
            result
            (reduce
             (fn [acc {:keys [label root commits]}]
               (let [n (count commits)]
                 (if (busy-fn root)
                   (do (print-fn (str "[inbox-zero] " label " has " n
                                      " unpushed commit(s) but is mid-operation"
                                      " — leaving it alone"))
                       (-> acc (update :skipped inc)
                           (update :rows conj {:label label :root root
                                               :unpushed n :outcome :mid-operation})))
                   (let [{:keys [ok? output]} (push-fn root)]
                     (if ok?
                       (do (print-fn (str "[inbox-zero] pushed " label ": " n
                                          " commit(s) that existed only on this box"))
                           (update acc :pushed inc))
                       (do (print-fn (str "[inbox-zero] push of " label " FAILED ("
                                          n " commit(s) still local): " output))
                           (-> acc (update :failed inc)
                               (update :rows conj
                                       {:label label :root root :unpushed n
                                        :outcome :failed :error output}))))))))
             {:repos (count watch-roots) :over-threshold (count over)
              :pushed 0 :failed 0 :skipped 0 :rows []}
             over)]
        (try
          (atomic-write! log-path
                         {:at now
                          :generated-by "futon3c.inbox-zero.sweeper"
                          :note (str "Repos over the unpushed-commit threshold that "
                                     "could not be pushed. Current state, not a "
                                     "queue: rewritten every pass.")
                          :threshold threshold
                          :repos (vec (sort-by :label (:rows result)))})
          (catch Throwable error
            (print-fn (str "[inbox-zero] push log unwritable: " (.getMessage error)))))
        (let [counts (dissoc result :rows)]
          (print-fn (str "[inbox-zero] push pass: " (pr-str counts)))
          counts))
      (catch Throwable error
        (try (print-fn (str "[inbox-zero] push pass failed: " (.getMessage error)))
             (catch Throwable _))
        {:repos 0 :over-threshold 0 :pushed 0 :failed 0 :skipped 0}))))

;; ---------- the worktree lane: retire what is already saved ----------

(def ^:private default-worktree-log-path
  "/home/joe/code/storage/inbox-zero/worktree-log.edn")

(def ^:private default-worktree-idle-hours 24)

(def ^:private default-worktree-root
  (str (io/file (System/getProperty "user.home") "worktrees")))

(def ^:private lease-owned-prefixes
  "Worktree paths another lifecycle owns, which this lane never touches.

  APM frame workspaces are provisioned and retired by
  futon3c.apm.workspace-lifecycle, behind eight fail-closed preconditions
  (terminal frame, no job or ledger claim referencing it, a content-addressed
  receipt). Removing one from here would skip every one of those checks.
  classify-the-dirt says a worktree is retired through the owning lease path
  and never by raw removal; this list is where that rule is enforced."
  ["/home/joe/code/apm-frames/"])

(defn- worktree-records
  "Linked worktrees of the repo at ROOT, as {:path :head :branch :locked?}.
  The main checkout is the first record and is dropped: it is the repository."
  [root]
  (let [{:keys [exit out]} (shell/sh "git" "worktree" "list" "--porcelain" :dir root)]
    (if-not (zero? exit)
      []
      (->> (str/split out #"\n\n+")
           (map str/trim)
           (remove str/blank?)
           (map (fn [chunk]
                  (reduce (fn [m line]
                            (cond
                              (str/starts-with? line "worktree ") (assoc m :path (subs line 9))
                              (str/starts-with? line "HEAD ") (assoc m :head (subs line 5))
                              (str/starts-with? line "branch ") (assoc m :branch (subs line 7))
                              (= line "locked") (assoc m :locked? true)
                              (str/starts-with? line "locked ") (assoc m :locked? true)
                              :else m))
                          {:locked? false}
                          (str/split-lines chunk))))
           rest
           vec))))

(defn- worktree-idle-ms
  "Milliseconds since anything last happened in WORKTREE-PATH.

  Reads the worktree's own git admin files (index, HEAD, logs/HEAD) as well as
  the checkout directory: the admin files move on any git operation there, so
  a worktree somebody is actively switching branches in does not look idle
  merely because nothing was written to the tree."
  [worktree-path now-ms]
  (let [admin (let [{:keys [exit out]} (shell/sh "git" "rev-parse" "--absolute-git-dir"
                                                 :dir worktree-path)]
                (when (zero? exit) (str/trim out)))
        candidates (cond-> [(io/file worktree-path)]
                     admin (into (map #(io/file admin %) ["index" "HEAD" "logs/HEAD"])))
        newest (->> candidates
                    (filter #(.exists ^java.io.File %))
                    (map #(.lastModified ^java.io.File %))
                    (reduce max 0))]
    (if (pos? newest) (max 0 (- now-ms newest)) Long/MAX_VALUE)))

(defn- pushed-tip
  "The exact upstream ref this checkout can push normally, or nil.
  Retirement is forbidden without this witness: local HEAD is not evidence
  that a commit exists anywhere else."
  [root]
  (let [{:keys [exit out]} (shell/sh "git" "rev-parse" "--verify"
                                     "@{upstream}" :dir root)]
    (when (zero? exit) (str/trim out))))

(defn- ancestor-of? [root ancestor descendant]
  (zero? (:exit (shell/sh "git" "merge-base" "--is-ancestor"
                          (str ancestor) (str descendant) :dir root))))

(defn- process-cwd-under?
  "True when a live process has PATH, or a descendant, as its cwd."
  [path]
  (let [prefix (str (.getCanonicalPath (io/file path)) java.io.File/separator)]
    (boolean
     (some (fn [pid]
             (try
               (let [cwd (.getCanonicalPath (io/file pid "cwd"))]
                 (or (= cwd (subs prefix 0 (dec (count prefix))))
                     (str/starts-with? cwd prefix)))
               (catch Throwable _ false)))
           (filter #(re-matches #"[0-9]+" (.getName ^java.io.File %))
                   (or (seq (.listFiles (io/file "/proc"))) []))))))

(defn- relocation-destination [worktree-root repo-root worktree-path]
  (io/file worktree-root (.getName (io/file repo-root))
           (.getName (io/file worktree-path))))

(defn- retirement-refusal
  "Why WORKTREE may not be retired, or nil when every condition holds.

  Every commit in it must already be an ancestor of the branch this repo
  pushes, which is what makes removal a no-judgement act: the work is not in
  the worktree in any sense that matters, it is in the branch that went to the
  remote. The rest are refusals to act on something that is still somebody's."
  [root {:keys [path head locked?]} now-ms idle-hours upstream ancestor-fn]
  (cond
    locked? :locked
    (some #(str/starts-with? (str path) %) lease-owned-prefixes) :lease-owned
    (not (.isDirectory (io/file path))) :absent
    (seq (git-dirty path)) :dirty
    (nil? upstream) :no-upstream
    (not (ancestor-fn root head upstream)) :unmerged
    (< (worktree-idle-ms path now-ms) (* idle-hours 60 60 1000)) :in-use
    :else nil))

(defn retire-merged-worktrees!
  "Remove linked worktrees whose every commit is already on the pushed branch.

  A third pressure-free lane, and deliberately so: there is no count to wait
  for. Ten dirty files and ten unpushed commits are thresholds because the
  count IS the exposure. A merged worktree is residue the moment it merges,
  removing it loses nothing, and waiting for ten of them would only mean
  holding nine known-empty directories.

  Never forces. `git worktree remove` refuses on its own if the tree turns out
  dirty or busy, and that refusal is the last guard rather than the only one:
  see `retirable` for the conditions checked first, and lease-owned-prefixes
  for the worktrees this lane declines to touch at all."
  [options]
  (let [print-fn (or (:print-fn options) println)]
    (try
      (let [watch-roots (or (:roots options) roots/sweep-roots)
            records-fn (or (:worktrees-fn options) worktree-records)
            remove-fn (or (:remove-fn options)
                          (fn [root path]
                            (let [{:keys [exit out err]}
                                  (shell/sh "git" "worktree" "remove" (str path) :dir root)]
                              {:ok? (zero? exit)
                               :output (str/trim (str out " " err))})))
            idle-hours (or (:idle-hours options) default-worktree-idle-hours)
            upstream-fn (or (:upstream-fn options) pushed-tip)
            ancestor-fn (or (:ancestor-fn options) ancestor-of?)
            cwd-busy-fn (or (:cwd-busy-fn options) process-cwd-under?)
            worktree-root (or (:worktree-root options)
                              (System/getenv "FUTON3C_WORKTREE_ROOT")
                              default-worktree-root)
            move-fn (or (:move-fn options)
                        (fn [root from to]
                          (.mkdirs (.getParentFile ^java.io.File to))
                          (let [{:keys [exit out err]}
                                (shell/sh "git" "worktree" "move" (str from) (str to)
                                          :dir root)]
                            {:ok? (zero? exit)
                             :output (str/trim (str out " " err))})))
            now ((or (:now-fn options) #(Date.)))
            now-ms (.getTime ^Date now)
            log-path (or (:worktree-log-path options) default-worktree-log-path)
            result
            (reduce
             (fn [acc {:keys [path label]}]
               (let [upstream (upstream-fn path)]
                (reduce
                (fn [acc wt]
                  (let [refusal (retirement-refusal path wt now-ms idle-hours
                                                    upstream ancestor-fn)]
                    (cond
                      (nil? refusal)
                      (let [{:keys [ok? output]} (remove-fn path (:path wt))]
                        (if ok?
                          (do (print-fn (str "[inbox-zero] retired merged worktree "
                                             (:path wt) " (" label "): every commit "
                                             "already on the pushed branch"))
                              (update acc :retired inc))
                          (do (print-fn (str "[inbox-zero] worktree removal FAILED "
                                             (:path wt) ": " output))
                              (-> acc (update :failed inc)
                                  (update :rows conj
                                          {:label label :worktree (:path wt)
                                           :outcome :failed :error output})))))
                      (= :unmerged refusal)
                      (let [acc (update acc :unmerged inc)
                            destination (relocation-destination worktree-root path (:path wt))]
                        (cond
                          (seq (git-dirty (:path wt)))
                          (-> acc (update :skipped inc)
                              (update :rows conj {:label label :worktree (:path wt)
                                                  :outcome :dirty}))
                          (< (worktree-idle-ms (:path wt) now-ms)
                             (* idle-hours 60 60 1000))
                          (-> acc (update :skipped inc)
                              (update :rows conj {:label label :worktree (:path wt)
                                                  :outcome :in-use}))
                          (cwd-busy-fn (:path wt))
                          (-> acc (update :skipped inc)
                              (update :rows conj {:label label :worktree (:path wt)
                                                  :outcome :process-cwd}))
                          (.exists destination)
                          (-> acc (update :failed inc)
                              (update :rows conj {:label label :worktree (:path wt)
                                                  :outcome :destination-exists
                                                  :destination (str destination)}))
                          :else
                          (let [{:keys [ok? output]} (move-fn path (:path wt) destination)]
                            (if ok?
                              (do (print-fn (str "[inbox-zero] moved active worktree "
                                                 (:path wt) " -> " destination))
                                  (update acc :moved inc))
                              (-> acc (update :failed inc)
                                  (update :rows conj {:label label :worktree (:path wt)
                                                      :outcome :move-failed
                                                      :destination (str destination)
                                                      :error output}))))))
                      :else
                      (-> acc (update :skipped inc)
                          (update :rows conj {:label label :worktree (:path wt)
                                              :outcome refusal})))))
                acc
                (records-fn path))))
             {:repos (count watch-roots) :retired 0 :moved 0 :failed 0
              :skipped 0 :unmerged 0 :rows []}
             watch-roots)]
        (try
          (atomic-write! log-path
                         {:at now
                          :generated-by "futon3c.inbox-zero.sweeper"
                          :note (str "Linked worktrees this lane did not retire, and "
                                     "why. Clean idle unmerged worktrees move to the "
                                     "declared worktree root; blocked moves remain listed. "
                                     "Current state, not a queue.")
                          :idle-hours idle-hours
                          :worktrees (vec (sort-by :worktree (:rows result)))})
          (catch Throwable error
            (print-fn (str "[inbox-zero] worktree log unwritable: " (.getMessage error)))))
        (let [counts (dissoc result :rows)]
          (print-fn (str "[inbox-zero] worktree pass: " (pr-str counts)))
          counts))
      (catch Throwable error
        (try (print-fn (str "[inbox-zero] worktree pass failed: " (.getMessage error)))
             (catch Throwable _))
        {:repos 0 :retired 0 :moved 0 :failed 0 :skipped 0 :unmerged 0}))))

(defn run-pass!
  "One full pass: inform about dirty files, act on unpushed commits.

  The loop below calls this ONE var rather than each lane in turn, so a lane
  added or changed later reaches the running loop through a plain namespace
  reload. Calling the lanes directly from the loop body meant the opposite:
  the push lane was invisible to the already-running future until the loop
  itself was restarted, because a future holds the body it was compiled with
  and a reload only rebinds the vars that body calls (2026-09-18)."
  [options]
  (let [print-fn (or (:print-fn options) println)]
    (try (sweep-dirty-repos! options)
         (catch Throwable error
           (print-fn (str "[inbox-zero] commit-notice lane threw: "
                          (.getMessage error)))))
    (try (push-stranded-commits! options)
         (catch Throwable error
           (print-fn (str "[inbox-zero] push lane threw: "
                          (.getMessage error)))))
    (try (retire-merged-worktrees! options)
         (catch Throwable error
           (print-fn (str "[inbox-zero] worktree lane threw: "
                          (.getMessage error)))))))

(defn stop-loop!
  "Cancel the background pass loop, if one is running. Returns true if it
  cancelled something. Needed to swap the loop body itself; a reload alone
  cannot, for the reason in run-pass!."
  []
  (boolean (when-let [f @!loop]
             (when-not (future-done? f)
               (future-cancel f)))))

(defn start-loop!
  "Start one delayed background pass loop. Repeated starts are idempotent."
  [{:keys [interval-ms print-fn] :as options}]
  (let [interval-ms (long (or interval-ms default-interval-ms))
        print-fn (or print-fn println)]
    (when-not (and @!loop (not (future-done? @!loop)))
      (reset!
       !loop
       (future
         (try
           (loop []
             (Thread/sleep interval-ms)
             (try
               (run-pass! options)
               (catch Throwable error
                 (print-fn (str "[inbox-zero] pass loop threw: "
                                (.getMessage error)))))
             (recur))
           (catch InterruptedException _)
           (catch Throwable error
             (print-fn (str "[inbox-zero] commit-notice loop stopped: "
                            (.getMessage error)))))))
      @!loop)))
