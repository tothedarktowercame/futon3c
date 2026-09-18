(ns futon3c.inbox-zero.sweeper
  "Dirty-repo commit notices: the lane that makes inbox zero act.

  One bounded pass per interval. For each watched repo, read `git status`
  and count the dirty paths. When a repo carries more than the threshold,
  tell the agents whose turns wrote them that they need to commit.

  Attribution is by file mtime against the Agency's invoke windows — a file
  written at T belongs to whoever was invoking at T. The agent tool stream
  plays no part: on this stack the great majority of dirt is run output
  written by processes agents launch, not by their editor tools, so a lane
  keyed on Edit/Write witnesses observes almost nothing (2 of 238 paths on
  2026-09-17, and zero proposals in the three weeks to then).

  Two lanes, and the difference between them is whether judgement is needed.
  Dirty files INFORM: deciding what is yours and whether it should be kept
  needs a person, so that lane never stages, commits or mints a claim.
  Unpushed commits ACT: pushing a commit already written decides nothing, so
  that lane pushes rather than asking anyone to (Joe, 2026-09-18).

  Both report their counts on every pass, so a silent pass cannot look like a
  working one."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.watcher.roots :as roots])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]
           [java.util Date]))

(def default-threshold 10)
(def default-interval-ms 1800000)
(def default-max-recipients 3)

;; Re-notify only when the backlog has grown by a threshold's worth, or when
;; the last notice has gone stale. A notice an agent has already had is not
;; new information, and repeating it is how the earlier lane earned its
;; "seats were being spammed with stale notices" refusal.
(def default-renotify-ms (* 6 60 60 1000))

(defonce ^:private !loop (atom nil))
(defonce ^:private notices-monitor (Object.))

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

(defn attribute
  "Agent → `{:count n :entries [entry ...]}` over ENTRIES, live agents only.

  Every entry inside an agent's turns is named to that agent, newest first.
  Where several agents were running at once the entry goes to each of them
  carrying `:shared-with`, rather than to none of them: the file still has
  to be committed, and an entry named to nobody is the silent refusal this
  lane exists to end. The recipient is told who else was running and can
  see at a glance which files are not its own."
  [windows roster entries]
  (reduce
   (fn [acc entry]
     (let [candidates (live-candidates windows roster entry)]
       (reduce (fn [acc agent]
                 (-> acc
                     (update-in [agent :count] (fnil inc 0))
                     (update-in [agent :entries] (fnil conj [])
                                (assoc entry :shared-with
                                       (vec (sort (disj candidates agent)))))))
               acc
               candidates)))
   {}
   entries))

(defn recipients
  "Live agents to tell, most-responsible first, capped at MAX-RECIPIENTS.

  Responsibility is how many dirty files were written inside that agent's
  turns. Each recipient carries its own entries, newest first."
  [attributed max-recipients]
  (->> attributed
       (sort-by (fn [[agent {:keys [count]}]] [(- count) agent]))
       (take max-recipients)
       (mapv (fn [[agent {:keys [count entries]}]]
               {:agent agent
                :count count
                :entries (vec (sort-by :mtime-ms > entries))}))))

;; ---------- the notice ----------

(defn- notice-prompt
  [{:keys [label root entries]} recipient]
  (let [total (count entries)
        untracked (count (filter :untracked? entries))
        named (->> (:entries recipient)
                   (take sample-size)
                   (map (fn [entry]
                          (if-let [others (seq (:shared-with entry))]
                            (str (:path entry) " (also inside "
                                 (str/join ", " others) "'s turn)")
                            (:path entry)))))]
    (str "inbox-zero: " label " is carrying " total " dirty file(s) ("
         untracked " untracked); " (:count recipient) " of them were written "
         "during your turns. Commit or delete what is yours and leave what is "
         "not. Newest first: " (str/join ", " named)
         ". Full list: git -C " root " status --porcelain")))

(defn- followup-payload
  [repo recipient]
  (let [total (count (:entries repo))]
    {:agent (:agent recipient)
     :session (:session recipient)
     :type "inbox-zero"
     :dedupe-key ["commit-notice" (:label repo) (:agent recipient)
                  (quot total (max 1 (:threshold repo default-threshold)))]
     :prompt (notice-prompt repo recipient)
     :metadata {:proposal/type :inbox-zero/commit-notice
                :repo-id (:label repo)
                :dirty-count total
                :implicated-count (:count recipient)}}))

(defn- default-deliver [payload]
  (http/post (agency-url "/api/alpha/followups")
             {:headers {"Content-Type" "application/json"}
              :body (json/generate-string payload)
              :throw false}))

;; ---------- the "already told them" ledger ----------

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

(defn- load-notices [path print-fn]
  (locking notices-monitor
    (try
      (let [file (io/file path)]
        (if-not (.exists file)
          {}
          (let [value (edn/read-string (slurp file))]
            (if (map? value) value {}))))
      (catch Throwable error
        (print-fn (str "[inbox-zero] commit-notice ledger unreadable; "
                       "starting empty: " (.getMessage error)))
        {}))))

(defn- record-notice! [path notices key value]
  (locking notices-monitor
    (let [next-value (assoc @notices key value)]
      (atomic-write! path next-value)
      (reset! notices next-value))))

(defn due?
  "True when this agent has not already been told this, or the backlog has
  grown by a threshold's worth since, or the last notice has gone stale."
  [prior total now-ms {:keys [threshold renotify-ms]}]
  (or (nil? prior)
      (>= (- total (:count prior 0)) threshold)
      (>= (- now-ms (:at-ms prior 0)) renotify-ms)))

;; ---------- the pass ----------

(defn- empty-counts []
  {:repos 0 :over-threshold 0 :notified 0 :held 0 :unowned 0 :errored 0})

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
                         :note (str "Repos over the dirty-file threshold that no live "
                                    "agent wrote. Current state, not a queue: "
                                    "rewritten every pass.")
                         :repos (vec (sort-by :label rows))})
    (catch Throwable error
      (print-fn (str "[inbox-zero] operator backlog unwritable: "
                     (.getMessage error))))))

(defn sweep-dirty-repos!
  "Run one bounded commit-notice pass. Every collaborator is injectable."
  [options]
  (let [print-fn (or (:print-fn options) println)]
    (try
      (let [threshold (max 1 (long (or (:threshold options)
                                       (some-> (System/getenv "FUTON3C_INBOX_ZERO_THRESHOLD")
                                               Long/parseLong)
                                       default-threshold)))
            renotify-ms (long (or (:renotify-ms options) default-renotify-ms))
            max-recipients (long (or (:max-recipients options) default-max-recipients))
            watch-roots (or (:roots options) roots/sweep-roots)
            git-fn (or (:git-fn options) git-dirty)
            windows-fn (or (:windows-fn options) default-windows)
            roster-fn (or (:roster-fn options) default-roster)
            deliver! (or (:deliver! options) default-deliver)
            now-fn (or (:now-fn options) #(Date.))
            notices-path (or (:notices-path options)
                             (System/getenv "FUTON3C_INBOX_ZERO_NOTICES_PATH")
                             "/home/joe/code/storage/inbox-zero/commit-notices.edn")
            now (now-fn)
            now-ms (.getTime ^Date now)
            notices (atom (load-notices notices-path print-fn))
            over (->> watch-roots
                      (keep (fn [{:keys [path label]}]
                              (let [entries (git-fn path)]
                                (when (>= (count entries) threshold)
                                  {:label label :root path :entries entries
                                   :threshold threshold}))))
                      vec)
            ;; Only pay for the roster and the job ledger when something is
            ;; over the line; a clean stack costs one git status per repo.
            windows (if (seq over) (windows-fn) [])
            roster (if (seq over) (roster-fn) {})
            counts
            (reduce
             (fn [counts repo]
               (try
                 (let [attributed (attribute windows roster (:entries repo))
                       targets (mapv #(assoc % :session (get roster (:agent %)))
                                     (recipients attributed max-recipients))
                       total (count (:entries repo))]
                   (if (empty? targets)
                     (do
                       (print-fn (str "[inbox-zero] " (:label repo) " has " total
                                      " dirty file(s) over the threshold of "
                                      threshold " but no live agent wrote them "
                                      "— operator backlog"))
                       (-> counts
                           (update :unowned inc)
                           (update :unowned-rows conj
                                   {:label (:label repo) :root (:root repo)
                                    :dirty-count total :threshold threshold
                                    :newest (mapv :path (take sample-size
                                                              (:entries repo)))})))
                     (reduce
                      (fn [counts recipient]
                        (let [key [(:label repo) (:agent recipient)]
                              prior (get @notices key)]
                          (if-not (due? prior total now-ms
                                        {:threshold threshold
                                         :renotify-ms renotify-ms})
                            (update counts :held inc)
                            (let [response (deliver! (followup-payload repo recipient))]
                              (if (= 200 (:status response))
                                (do
                                  (record-notice! notices-path notices key
                                                  {:count total :at-ms now-ms
                                                   :at now
                                                   :implicated (:count recipient)})
                                  (print-fn (str "[inbox-zero] told " (:agent recipient)
                                                 " to commit: " (:label repo) " has "
                                                 total " dirty file(s), "
                                                 (:count recipient) " written in its turns"))
                                  (update counts :notified inc))
                                (do
                                  (print-fn (str "[inbox-zero] commit notice to "
                                                 (:agent recipient) " failed: status "
                                                 (:status response)))
                                  (update counts :errored inc)))))))
                      counts
                      targets)))
                 (catch Throwable error
                   (print-fn (str "[inbox-zero] commit-notice pass failed for "
                                  (:label repo) ": " (.getMessage error)))
                   (update counts :errored inc))))
             (assoc (empty-counts)
                    :repos (count watch-roots)
                    :over-threshold (count over)
                    :unowned-rows [])
             over)]
        (write-backlog! (or (:backlog-path options) default-backlog-path)
                        (:unowned-rows counts) now print-fn)
        (let [counts (dissoc counts :unowned-rows)]
          (print-fn (str "[inbox-zero] commit-notice pass: " (pr-str counts)))
          counts))
      (catch Throwable error
        (try
          (print-fn (str "[inbox-zero] commit-notice pass failed: "
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

(defn push-stranded-commits!
  "Push every watched repo carrying THRESHOLD or more unpushed commits.

  An action, not a notice. Pushing a commit that has already been written
  involves no judgement at all: escalate-by-who-can-act puts ordinary
  accumulation at tier 0, which acts without asking anybody, and a notice for
  something needing no judgement is only a queue that waits (Joe, 2026-09-18).
  Ten dirty files means commit, because deciding what is yours needs a person.
  Ten unpushed commits means push, because nothing needs deciding.

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
      (let [threshold (max 1 (long (or (:threshold options)
                                       (some-> (System/getenv "FUTON3C_INBOX_ZERO_THRESHOLD")
                                               Long/parseLong)
                                       default-threshold)))
            watch-roots (or (:roots options) roots/sweep-roots)
            ahead-fn (or (:ahead-fn options) git-unpushed)
            push-fn (or (:push-fn options) git-push!)
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
               (run-pass! (dissoc options :interval-ms))
               (catch Throwable error
                 (print-fn (str "[inbox-zero] pass loop threw: "
                                (.getMessage error)))))
             (recur))
           (catch InterruptedException _)
           (catch Throwable error
             (print-fn (str "[inbox-zero] commit-notice loop stopped: "
                            (.getMessage error)))))))
      @!loop)))
