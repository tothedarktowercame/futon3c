#!/usr/bin/env bb
;; APM invariant checker — the executing half of an already-specified system.
;;
;; Every invariant below is documented somewhere in this repo but had no
;; process evaluating it. This script observes and reports. It is STRICTLY
;; READ-ONLY: it never restarts, enables, kills, prunes, or repairs anything,
;; and it never loads code into the live JVMs it inspects.
;;
;; Sources of authority, per invariant:
;;  :apm/jvm-count          futon3c/CLAUDE.md "One JVM per repo" (~lines 98-135):
;;                          one futon3c JVM; the futon1b override note permits a
;;                          second. More than two long-lived JVMs at rest is a
;;                          violation of the documented invariant, not noise.
;;  :apm/coordinator-health durable coordinator registry (commits 99bb3f26 /
;;                          88d064d added explicit stop causes). A coordinator
;;                          disabled with :stop-cause/fault-class :substrate is
;;                          a restartable fault that nothing restarted — that is
;;                          the 42-minute cycle that ran unobserved for weeks.
;;  :apm/coordinator-churn  :coordinator/enabled-history. Mean enabled span
;;                          under 2h is a defect signal (claude-7's diagnosis:
;;                          173 stops / 7 days, mean 42 min). The threshold is
;;                          reported, not hidden.
;;  :apm/campaign-liveness  :regulator/ticks in coordinator.edn (mechanical
;;                          liveness only — ticks advancing is NOT semantic
;;                          progress) and :event/at of the last :frame/advanced
;;                          in the active frame's ledger.edn (semantic signal).
;;  :apm/worktrees          /home/joe/code/apm-lean worktree count. Retirement
;;                          keeps failing, so growth is unbounded; 60 is the
;;                          ceiling (≈ a dozen active lanes plus headroom —
;;                          189 today is 3x over, which is exactly the point).
;;  :apm/frames-disk        /home/joe/code/apm-frames. 30 GiB ceiling: today it
;;                          holds 17G and frames are dozens of MB each; 30G
;;                          means ~13G of unretired accumulation on top of the
;;                          current working set.
;;  :apm/disk-headroom      campaign data dir must keep >= 10% free (or 20G,
;;                          whichever is smaller) so a full disk cannot wedge
;;                          the ledger writes mid-campaign.
;;
;; Verdicts: :pass, :violated, :unknown. :unknown is emitted loudly whenever an
;; input is missing — a checker that goes quiet when its subject disappears is
;; the failure mode this script exists to fix. Exit code 1 iff any :violated,
;; 2 iff any :unknown (and none violated), 0 otherwise.

(ns apm-invariant-checker
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(def ^:private defaults
  {:registry-path    "data/apm-coordinators/registry.edn"
   :repo-root        "/home/joe/code/futon3c"
   :apm-lean-root    "/home/joe/code/apm-lean"
   :frames-root      "/home/joe/code/apm-frames"
   :campaigns-root   "data/apm-campaigns"
   :max-jvms         1                   ; per repo; other checkouts are not counted
   :churn-window-ms  (* 24 60 60 1000)   ; stop count window: 24h
   :churn-span-window-ms (* 7 24 60 60 1000) ; span statistics window: 7d
   :min-mean-span-ms (* 2 60 60 1000)    ; < 2h mean enabled span is a defect
   :min-span-count   3                   ; need 3+ completed spans to judge
   :max-worktrees    60
   :max-frames-gb    30
   :min-disk-free-frac 0.10
   :min-disk-free-gb 20
   :ticks-sample-ms  10000               ; second sample delay for tick advance
   :max-frame-staleness-ms (* 24 60 60 1000)}) ; no :frame/advanced in 24h = stale

(defn verdict
  [id source observed expected v reason]
  {:invariant/id id
   :invariant/source source
   :invariant/observed observed
   :invariant/expected expected
   :invariant/verdict v
   :invariant/reason reason})

;; -- pure helpers (unit-tested in apm_invariant_checker_test.bb) -----------

(defn mean [xs] (when (seq xs) (/ (reduce + xs) (count xs))))

(defn now-ms [] (System/currentTimeMillis))

(defn proc-cwd
  "Working directory of PID, or nil when unreadable (other user, or exited)."
  [pid]
  (try (let [r (p/shell {:out :string :err :string :continue true}
                        "readlink" "-f" (str "/proc/" pid "/cwd"))]
         (when (zero? (:exit r)) (not-empty (str/trim (:out r)))))
       (catch Exception _ nil)))

(defn with-cwds
  "Attach :cwd to each proc so the per-repo invariant can be evaluated."
  [procs]
  (mapv #(assoc % :cwd (proc-cwd (:pid %))) procs))

(defn jvm-check
  "Expected: at most :max-jvms java processes *for this repo*. CLAUDE.md states
   the invariant per repo, so JVMs belonging to other checkouts (futon1b,
   benchmarks) are reported but never counted -- a global count goes red
   whenever any other repo runs a JVM, and a permanently-red check trains
   readers to ignore the checker."
  [max-jvms repo procs]
  (let [repo* (str (fs/canonicalize repo))
        mine (filterv #(= repo* (:cwd %)) procs)
        foreign (filterv #(not= repo* (:cwd %)) procs)
        n (count mine)]
    (verdict :apm/jvm-count
             "futon3c/CLAUDE.md, One JVM per repo (counted for this repo only, by /proc/<pid>/cwd)"
             {:jvm/repo repo*
              :jvm/count n
              :jvm/processes (mapv #(select-keys % [:pid :age-days :rss-gb :cwd]) mine)
              :jvm/foreign (mapv #(select-keys % [:pid :cwd]) foreign)
              :jvm/foreign-count (count foreign)}
             {:jvm/count-max max-jvms}
             (if (and (pos? n) (<= n max-jvms)) :pass
               (if (zero? n) :unknown :violated))
             (cond (zero? n) (str "no JVM with cwd " repo*
                                  " -- live stack is down, or every candidate cwd was unreadable")
                   (<= n max-jvms) (str n " JVM(s) for this repo (" (count foreign)
                                        " other-repo JVMs ignored)")
                   :else (str n " JVMs share cwd " repo* "; limit is " max-jvms
                              ". Concurrent drivers can double-write durable APM state.")))))

(defn parse-jps
  "Parse `ps -eo pid,etime,rss,args` lines into maps for java processes."
  [lines]
  (->> lines
       (filter #(str/includes? (str/lower-case %) "java"))
       (map (fn [line]
              (let [[pid etime rss & args] (str/split (str/trim line) #"\s+")
                    age-days (when-let [[_ d] (re-find #"(\d+)-" (str etime))]
                               (/ (parse-long d) 1.0))]
                {:pid (parse-long pid)
                 :etime etime
                 :age-days age-days
                 :rss-gb (when rss (/ (parse-long rss) 1048576.0))
                 :cmd (str/join " " (take 3 args))})))
       (vec)))

(defn latest-stop-transition
  "Most recent transition with :enabled/new false, by timestamp."
  [history]
  (->> history
       (filter (comp false? :enabled/new))
       (sort-by :transition/timestamp-ms)
       (last)))

(defn transition-stop-cause
  "Mirrors durable-coordinator semantics: legacy stops without :stop/cause are
   :unknown, never invented."
  [t]
  (or (:stop/cause t) {:stop-cause/type :unknown}))

(defn read-edn-file [path]
  (when (fs/exists? path)
    (try (edn/read-string (slurp path))
         (catch Exception _ ::unparseable))))

(defn coordinator-health-check
  [entry]
  (let [cid (:coordinator/id entry "?")
        enabled (:coordinator/enabled? entry)
        lifecycle (get-in entry [:coordinator/lifecycle :coordinator/lifecycle])
        state-path (or (:coordinator/state-path entry)
                       (get-in entry [:coordinator/lifecycle :coordinator/state-path]))
        state (read-edn-file (str state-path))
        status (when (map? state) (:regulator/status state))
        stop-t (latest-stop-transition (:coordinator/enabled-history entry))
        cause (some-> stop-t transition-stop-cause)
        fault (:stop-cause/fault-class cause)
        substrate-stopped? (and (false? enabled) (= :substrate fault))]
    (verdict (keyword "apm.coordinator" cid)
             "data/apm-coordinators/registry.edn (:coordinator/enabled?, :coordinator/lifecycle, :stop/cause per commits 99bb3f26/88d064d) + coordinator.edn :regulator/status"
             {:coordinator/enabled? enabled
              :coordinator/lifecycle lifecycle
              :regulator/status status}
             {:coordinator/enabled? :any
              :stop/fault-class-not :substrate-while-disabled}
             (cond
               (nil? enabled) :unknown
               substrate-stopped? :violated
               ;; "Enabled" alone is not liveness: an enabled coordinator whose
               ;; own state says :complete is finished-but-never-disabled.
               (and enabled (= :complete status)) :completed-but-enabled
               (and enabled (nil? status)) :unknown
               :else :pass)
             (cond
               (nil? enabled) "registry entry lacks :coordinator/enabled? — cannot evaluate"
               substrate-stopped? (str "STOPPED BY SUBSTRATE FAULT AND NOT RESTARTED — restartable fault class with nobody restarting it (reason-code " (:stop-cause/reason-code cause) ")")
               (and enabled (= :complete status)) "registry enabled but campaign state :regulator/status :complete — finished, never disabled (bookkeeping debt; the liveness check carries the same finding)"
               (and enabled (nil? status)) "enabled but coordinator state unreadable — cannot confirm the loop is alive (see liveness check)"
               (false? enabled) "disabled (intentional or legacy stop; no substrate fault class recorded)"
               :else "enabled and state readable"))))

(defn enabled-spans
  "Completed enabled spans [start-ms end-ms] from an enabled-history, computed
  by pairing each :enabled/new true transition with the next false one."
  [history]
  (let [ts (sort-by :transition/timestamp-ms history)]
    (loop [ts ts spans [] open nil]
      (if-let [t (first ts)]
        (cond
          (:enabled/new t) (recur (rest ts) spans (:transition/timestamp-ms t))
          (and (not (:enabled/new t)) open)
          (recur (rest ts) (conj spans [open (:transition/timestamp-ms t)]) nil)
          :else (recur (rest ts) spans open))
        spans))))

(defn churn-check
  "Stops in the last 24h and mean enabled span over 7d. A mean span under the
   2h floor is reported as :violated only when there are at least :min-span-count
   completed spans in the window (fewer spans cannot support a mean claim).
   The numbers are always reported whatever the verdict."
  [history {:keys [churn-window-ms churn-span-window-ms min-mean-span-ms min-span-count]} now]
  (let [recent-stops (->> history
                          (filter (comp false? :enabled/new))
                          (filter #(> (:transition/timestamp-ms %) (- now churn-window-ms)))
                          count)
        spans (->> (enabled-spans history)
                   (filter (fn [[s _e]] (> s (- now churn-span-window-ms)))))
        span-ms (map (fn [[s e]] (- e s)) spans)
        m-ms (mean span-ms)
        m-min (when m-ms (/ m-ms 60000.0))]
    (verdict :apm.coordinator-churn/recent
             "data/apm-coordinators/registry.edn :coordinator/enabled-history (mean enabled span < 2h is a defect signal, not operation)"
             {:churn/stops-24h recent-stops
              :churn/completed-spans-7d (count spans)
              :churn/mean-enabled-span-minutes m-min}
             {:churn/min-mean-span-minutes (/ min-mean-span-ms 60000.0)
              :churn/min-span-count min-span-count}
             (cond
               (empty? history) :unknown
               (nil? m-ms) :unknown
               (< (count spans) min-span-count) :unknown
               (< m-ms min-mean-span-ms) :violated
               :else :pass)
             (cond
               (empty? history) "no enabled-history recorded"
               (nil? m-ms) "history has stop(s) but no completed enabled span in window — cannot compute mean span"
               (< (count spans) min-span-count) (str "only " (count spans) " completed span(s) in 7d — too few to judge the mean")
               (< m-ms min-mean-span-ms) (str "mean enabled span " (format "%.0f" m-min) " min over " (count spans) " spans — below the 2h defect floor; churn is the defect, each stop is not an isolated incident")
                :else "mean enabled span within floor"))))

(defn parse-ledger-events
  "ledger.edn is line-delimited EDN event maps."
  [path]
  (when (fs/exists? path)
    (->> (str/split-lines (slurp path))
         (keep #(try (edn/read-string %) (catch Exception _ nil)))
         (vec))))

(defn latest-frame-ledger
  "Newest frame directory (by name) under a campaign root."
  [campaign-root]
  (->> (fs/list-dir campaign-root)
       (filter #(and (fs/directory? %)
                     (re-find #"-f\d+$" (fs/file-name %))))
       (sort-by fs/file-name)
       (last)))

(defn frame-advanced-at
  "Latest :event/at (ISO string) among :frame/advanced events."
  [events]
  (->> events
       (filter #(= :frame/advanced (:event/type %)))
       (map :event/at)
       (sort)
       (last)))

(defn iso-to-ms [s]
  (try (-> (java.time.Instant/parse s) (.toEpochMilli))
       (catch Exception _ nil)))

(defn campaign-liveness-check
  "Two independent signals, deliberately not conflated:
   (1) :regulator/ticks advancing between two samples — mechanical liveness;
   (2) time since last :frame/advanced in the active frame ledger — the only
   semantic-progress proxy available cheaply. Ticks advancing while frames are
   stale means the loop is spinning without advancing work."
  [{:keys [state-path ticks-sample-ms max-frame-staleness-ms]} now]
  (let [state1 (read-edn-file state-path)
        ticks1 (:regulator/ticks state1)
        status1 (:regulator/status state1)]
    (if (or (nil? state1) (= ::unparseable state1) (nil? ticks1))
      (verdict :apm/campaign-liveness
               "coordinator.edn :regulator/ticks; frame ledger.edn :frame/advanced"
               {:liveness/state-path state-path :liveness/ticks ticks1 :liveness/regulator-status status1}
               {:liveness/ticks-advance true}
               :unknown
               (if (= ::unparseable state1)
                 "coordinator.edn present but unparseable"
                 "no readable :regulator/ticks — coordinator state missing"))
      (let [_ (Thread/sleep (long ticks-sample-ms))
            state2 (read-edn-file state-path)
            ticks2 (:regulator/ticks state2)
            ledger-path (when (fs/exists? state-path)
                          (let [root (fs/parent state-path)]
                            (some-> (latest-frame-ledger root)
                                    (fs/path "ledger.edn")
                                    str)))
            adv (some-> ledger-path parse-ledger-events frame-advanced-at)
            adv-ms (some-> adv iso-to-ms)
            stale-ms (when adv-ms (- now adv-ms))
            ticks-ok? (and ticks2 (> ticks2 ticks1))]
        (verdict :apm/campaign-liveness
                 "coordinator.edn :regulator/ticks (mechanical) + :regulator/status + active frame ledger.edn :frame/advanced (semantic)"
                 {:liveness/ticks-before ticks1
                  :liveness/ticks-after ticks2
                  :liveness/ticks-advanced ticks-ok?
                  :liveness/regulator-status status1
                  :liveness/last-frame-advanced-at adv
                  :liveness/hours-since-frame-advanced (when stale-ms (/ stale-ms 3600000.0))
                  :liveness/ledger-path ledger-path}
                 {:liveness/ticks-advance true
                  :liveness/max-frame-staleness-hours (/ max-frame-staleness-ms 3600000.0)}
                 (cond
                   (not ticks-ok?)
                   (case status1
                     :complete :completed-but-enabled
                     :running :violated
                     :unknown) ; status missing/unrecognized: static ticks unexplained
                   (nil? adv-ms) :unknown
                   (> stale-ms max-frame-staleness-ms) :violated
                   :else :pass)
                 (cond
                   (not ticks-ok?)
                   (case status1
                     :complete (str "ticks static and :regulator/status :complete — campaign FINISHED but never disabled in the registry (bookkeeping debt, not a zombie; will trip forever until unregistered)")
                     :running (str "ZOMBIE: registry says enabled, own state says :regulator/status :running, and ticks have not advanced across " ticks-sample-ms "ms — a running loop that is not running")
                     (str "ticks not advancing across " ticks-sample-ms "ms sample and :regulator/status is " (pr-str status1) " — cannot classify (see TN-apm-watcher.md on stale :running)"))
                   (nil? adv-ms) "ticks advance but no :frame/advanced event found in active frame ledger — semantic signal unavailable"
                   (> stale-ms max-frame-staleness-ms) (str "ticks advance (mechanical liveness) but last :frame/advanced was " (format "%.1f" (/ stale-ms 3600000.0)) "h ago — loop is spinning without semantic progress")
                   :else "ticks advancing and frames advancing recently"))))))

(defn worktree-count [apm-lean-root]
  (try (-> (p/shell {:out :string :continue true}
                    "git" "-C" apm-lean-root "worktree" "list")
           :out str/split-lines count)
       (catch Exception _ nil)))

(defn worktrees-check
  [apm-lean-root max-worktrees]
  (let [n (worktree-count apm-lean-root)]
    (verdict :apm/worktrees
             (str apm-lean-root " worktree count (workspace retirement keeps failing; growth is unbounded without this watch)")
             {:worktree/count n}
             {:worktree/count-max max-worktrees}
             (if (nil? n) :unknown (if (> n max-worktrees) :violated :pass))
             (if (nil? n)
               "git worktree list failed"
               (str n " worktrees vs ceiling " max-worktrees
                    " (ceiling ≈ a dozen active lanes + headroom; every excess tree is a retirement failure)")))))

(defn dir-size-gb [root]
  (try (-> (p/shell {:out :string} (str "du -sb " root)) :out
           (str/split #"\s+") first parse-long (/ 1073741824.0))
       (catch Exception _ nil)))

(defn frames-disk-check [frames-root max-frames-gb]
  (let [gb (dir-size-gb frames-root)]
    (verdict :apm/frames-disk
             (str "disk under " frames-root)
             {:disk/gb gb}
             {:disk/gb-max max-frames-gb}
             (if (nil? gb) :unknown (if (> gb max-frames-gb) :violated :pass))
             (if (nil? gb) "du failed" (str (format "%.1f" gb) " GiB vs " max-frames-gb " GiB ceiling")))))

(defn disk-headroom-check [path {:keys [min-disk-free-frac min-disk-free-gb]}]
  (try
    (let [f (fs/file path)
          total (.getTotalSpace f) free (.getUsableSpace f)
          free-frac (/ (double free) (double total))
          free-gb (/ (double free) 1073741824.0)
          need-gb (min (double min-disk-free-gb) (* (double min-disk-free-frac) (/ (double total) 1073741824.0)))]
      (verdict :apm/disk-headroom
               (str "headroom for campaign data on the filesystem of " path)
               {:disk/free-gb free-gb :disk/free-frac free-frac}
               {:disk/min-free-gb need-gb}
               (if (>= free-gb need-gb) :pass :violated)
               (str (format "%.1f" free-gb) " GiB free (" (format "%.0f" (* 100 free-frac)) "%); need >= " (format "%.1f" need-gb) " GiB so a full disk cannot wedge ledger writes")))
    (catch Exception e
      (verdict :apm/disk-headroom
               (str "headroom for campaign data on the filesystem of " path)
               {:error (str e)}
               {:disk/min-free-frac min-disk-free-frac}
               :unknown
               "could not stat filesystem"))))

;; -- orchestration ----------------------------------------------------------

(defn coordinator-checks
  "Health check per registry entry + churn check on the entry with the most
   history (churn is a property of a lifecycle, and the vN lineage shares one)."
  [registry {:keys [now] :as cfg}]
  (let [entries (vals (:entries registry))]
    (concat
     (map coordinator-health-check entries)
     (when-let [churniest (->> entries (sort-by (comp count :coordinator/enabled-history)) last)]
       (when-let [h (seq (:coordinator/enabled-history churniest))]
         [(assoc (churn-check h cfg now)
                 :invariant/id :apm.coordinator-churn/recent
                 :invariant/coordinator (:coordinator/id churniest))])))))

(defn run-all [cfg]
  (let [now (now-ms)
        cfg (merge cfg {:now now})
        repo (:repo-root cfg)
        procs (-> (p/shell {:out :string} "ps -eo pid,etime,rss,args") :out str/split-lines parse-jps with-cwds)
        registry (read-edn-file (str (fs/path repo (:registry-path cfg))))
        enabled-entries (when (map? registry)
                          (->> (:entries registry) vals (filter :coordinator/enabled?)))
        liveness (mapv (fn [e]
                         (let [sp (:coordinator/state-path e)]
                           (try (campaign-liveness-check
                                 {:state-path sp
                                  :ticks-sample-ms (:ticks-sample-ms cfg)
                                  :max-frame-staleness-ms (:max-frame-staleness-ms cfg)}
                                 now)
                                (catch Exception ex
                                  (verdict :apm/campaign-liveness
                                           "coordinator.edn :regulator/ticks"
                                           {:error (str ex) :state-path sp}
                                           {}
                                           :unknown "liveness probe threw")))))
                       enabled-entries)]
    {:check/at (str (java.time.Instant/now))
     :check/read-only true
     :invariants
     (vec (concat
           [(jvm-check (:max-jvms cfg) repo procs)
            (worktrees-check (:apm-lean-root cfg) (:max-worktrees cfg))
            (frames-disk-check (:frames-root cfg) (:max-frames-gb cfg))
            (disk-headroom-check (str repo "/data") cfg)]
           (if (map? registry)
             (coordinator-checks registry cfg)
             [(verdict :apm/coordinator-health (:registry-path cfg)
                       {:registry/present false}
                       {:registry/present true}
                       :unknown "registry.edn missing or unparseable")])
           (if (empty? liveness)
             [(verdict :apm/campaign-liveness
                       "coordinator.edn :regulator/ticks"
                       {:coordinator/enabled-count 0}
                       {:coordinator/min-enabled-count 1}
                       :unknown "no enabled coordinators in registry — nothing to sample for liveness")]
             liveness)))}))

(defn summarize [report]
  (let [ivs (:invariants report)
        counts (frequencies (map :invariant/verdict ivs))]
    (str "APM invariant check " (:check/at report) "\n"
         "  pass: " (get counts :pass 0)
         "  violated: " (get counts :violated 0)
         "  unknown: " (get counts :unknown 0) "\n"
         (str/join "\n"
                   (for [iv ivs]
                     (format "  [%s] %-40s %s"
                             (case (:invariant/verdict iv)
                               :pass "PASS" :violated "VIOLATED" :completed-but-enabled "DONE-ENABLED" :unknown "UNKNOWN")
                             (name (:invariant/id iv))
                             (:invariant/reason iv))))
         "\n")))

(defn exit-code [report]
  (let [vs (set (map :invariant/verdict (:invariants report)))]
    ;; :completed-but-enabled is low-severity bookkeeping debt, deliberately
    ;; non-failing: it would otherwise trip forever and train everyone to
    ;; ignore a permanent red light.
    (cond (vs :violated) 1 (vs :unknown) 2 :else 0)))

(defn usage []
  (binding [*out* *err*]
    (println "usage: bb scripts/apm_invariant_checker.bb [--check]")
    (println "  Runs all read-only APM invariant checks. EDN report on stdout,")
    (println "  human summary on stderr. Exit 0 = all pass, 1 = violation(s),")
    (println "  2 = unknown(s) only. There is no other mode: the checker only observes.")))

(defn -main [& args]
  (when-not (every? #(= "--check" %) args)
    (usage)
    (System/exit 64))
  (let [opts (merge defaults
                    {:registry-path (str (fs/path (:repo-root defaults) (:registry-path defaults)))}
                    (when-let [r (some-> (System/getenv "APM_CHECKER_REPO") not-empty)]
                      {:repo-root r}))
        report (run-all opts)]
    (binding [*out* *err*] (println (summarize report)))
    (pp/pprint report)
    (System/exit (exit-code report))))

;; Standard babashka main-detection: run on any direct invocation (with or
;; without --check); loading for tests (load-file) does not match and stays
;; silent. An unrecognised argument prints usage and exits non-zero — never
;; exit 0 having done nothing.
(when (= *file* (System/getProperty "babashka.file")) (apply -main (or *command-line-args* [])))
