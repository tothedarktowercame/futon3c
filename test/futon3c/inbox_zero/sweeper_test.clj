(ns futon3c.inbox-zero.sweeper-test
  (:require [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.inbox-zero.sweeper :as sweeper])
  (:import [java.util Date]))

(def now (Date. 1789000000000))
(def now-ms (.getTime now))

(defn entry
  ([path minutes-ago] (entry path minutes-ago true))
  ([path minutes-ago untracked?]
   {:path path :status (if untracked? "??" " M") :untracked? untracked?
    :mtime-ms (- now-ms (* minutes-ago 60000))}))

(defn dirty-repo [n]
  (mapv #(entry (str "runs/out-" % ".edn") (inc %)) (range n)))

(defn window [agent from-minutes to-minutes]
  {:agent agent
   :start (- now-ms (* from-minutes 60000))
   :end (- now-ms (* to-minutes 60000))})

(defn temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "commit-notice-test-"
            (make-array java.nio.file.attribute.FileAttribute 0))))

(defn temp-notices-path []
  (str (java.io.File. (temp-dir) "commit-notices.edn")))

;; Never let a pass write the real operator backlog: sweep-dirty-repos!
;; rewrites it unconditionally, so an un-redirected test would blank the
;; live file under /home/joe/code/storage/inbox-zero.
(defn temp-backlog-path []
  (str (java.io.File. (temp-dir) "operator-backlog.edn")))

(defn base-options [entries-by-label calls]
  {:roots (mapv (fn [label] {:path (str "/repo/" label) :label label})
                (keys entries-by-label))
   :git-fn (fn [path] (get entries-by-label (last (str/split path #"/")) []))
   :windows-fn (fn [] [(window "codex-10" 600 0)])
   :roster-fn (fn [] {"codex-10" "session-10"})
   :now-fn (constantly now)
   :backlog-path (temp-backlog-path)
   :pressure-path (str (java.io.File. (temp-dir) "uncertain-pressure.edn"))
   :print-fn (fn [line] (swap! calls conj [:print line]))})

(deftest a-repo-under-the-threshold-is-left-alone
  (let [calls (atom [])
        counts (sweeper/sweep-dirty-repos!
                (base-options {"futon2-d" (dirty-repo 9)} calls))]
    (is (= 0 (:over-threshold counts)))
    (is (= 0 (:uncertain counts)))
    (is (empty? (remove #(= :print (first %)) @calls)))))

;; Joe's live example (C8): ten dirty files overlapping THREE other agents'
;; windows must produce ZERO personal deliveries, with every file visible
;; as uncertain-ownership pressure.
(deftest joe-example-ten-files-three-overlaps-no-personal-delivery
  (let [calls (atom [])
        options (assoc (base-options {"futon3c-d" (dirty-repo 10)} calls)
                       :windows-fn (fn [] [(window "codex-4" 600 0)
                                           (window "kimi-9" 600 0)
                                           (window "xiang" 600 0)])
                       :roster-fn (fn [] {"codex-4" "s4" "kimi-9" "s9"
                                          "xiang" "sx"}))
        counts (sweeper/sweep-dirty-repos! options)
        backlog (read-string (slurp (:backlog-path options)))
        feed (read-string (slurp (:pressure-path options)))
        row (first (:repos feed))]
    ;; no delivery machinery exists anymore: nothing but prints in calls
    (is (empty? (remove #(= :print (first %)) @calls)))
    (is (= 1 (:uncertain counts)))
    ;; all ten files represented in the FULL drilldown: 10 paths recorded,
    ;; bounded display would take 5 and remainder reports the other 5
    (is (= 10 (:dirty-count row)))
    (is (= 10 (count (:paths row))))
    (is (= 5 (:remainder row)))
    (is (= "runs/out-0.edn" (:path (first (:paths row)))))
    ;; overlaps are diagnostics naming all three seats, never assignment
    (is (= 10 (count (:diagnostic-overlaps row))))
    (is (= ["codex-4" "kimi-9" "xiang"]
           (get (:diagnostic-overlaps row) "runs/out-0.edn")))
    ;; backlog carries the same repo WITH the complete per-file drilldown
    (is (= 1 (count (:repos backlog))))
    (is (= "futon3c-d" (:label (first (:repos backlog)))))
    (is (= 10 (count (:paths (first (:repos backlog))))))))

(deftest sole-overlap-is-still-not-authorship
  ;; Exactly one live agent overlapping every write: still no delivery.
  (let [calls (atom [])
        counts (sweeper/sweep-dirty-repos!
                (base-options {"futon2-d" (dirty-repo 11)} calls))]
    (is (= 1 (:uncertain counts)))
    (is (empty? (remove #(= :print (first %)) @calls)))))

(deftest historical-records-do-not-route-either
  ;; A seat that historically touched a repo (old claim/confirmation
  ;; analogue: it appears in the roster and windows) gets no cleanup
  ;; assignment — historical records are citations, never current
  ;; authorship, and the sweeper reads no claim state at all.
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 12)} calls)
                       :windows-fn (fn [] [(window "claude-10" 4000 0)])
                       :roster-fn (fn [] {"claude-10" "session-old"}))
        counts (sweeper/sweep-dirty-repos! options)]
    (is (= 1 (:uncertain counts)))
    (is (empty? (remove #(= :print (first %)) @calls)))))

(deftest session-rollover-cannot-route-to-current-seat
  ;; Whatever session a seat currently holds, nothing routes to sessions.
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :roster-fn (fn [] {"codex-10" "brand-new-session"}))
        counts (sweeper/sweep-dirty-repos! options)]
    (is (= 1 (:uncertain counts)))
    (is (empty? (remove #(= :print (first %)) @calls)))))

(deftest mixed-repos-are-all-represented
  ;; One repo with overlapping windows, one with none: both appear in the
  ;; backlog AND the pressure feed (the old empty?-targets gate dropped
  ;; the uncertain repo whenever any repo had a target).
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)
                                      "futon3c-d" (dirty-repo 12)}
                                     calls)
                       :windows-fn (fn [] [(window "codex-10" 600 0)])
                       :roster-fn (fn [] {"codex-10" "s10"}))
        counts (sweeper/sweep-dirty-repos! options)
        backlog (read-string (slurp (:backlog-path options)))
        feed (read-string (slurp (:pressure-path options)))]
    (is (= 2 (:uncertain counts)))
    (is (= ["futon2-d" "futon3c-d"] (map :label (:repos backlog))))
    (is (= ["futon2-d" "futon3c-d"] (map :label (:repos feed))))))

(deftest repeated-passes-are-bounded-and-idempotent
  (let [calls (atom [])
        options (base-options {"futon2-d" (dirty-repo 11)} calls)
        _ (sweeper/sweep-dirty-repos! options)
        first-feed (read-string (slurp (:pressure-path options)))
        _ (sweeper/sweep-dirty-repos! options)
        second-feed (read-string (slurp (:pressure-path options)))]
    (is (= (dissoc first-feed :at) (dissoc second-feed :at)))
    (is (= 11 (:dirty-count (first (:repos second-feed)))))
    (is (= 11 (count (:paths (first (:repos second-feed))))))
    (is (= 6 (:remainder (first (:repos second-feed)))))))

(deftest uncertain-row-canonicalizes-the-root
  (let [dir (temp-dir)
        link (str (temp-dir) "-link")]
    (.delete (java.io.File. link))
    (java.nio.file.Files/createSymbolicLink
     (.toPath (java.io.File. link)) (.toPath dir)
     (make-array java.nio.file.attribute.FileAttribute 0))
    (let [row (sweeper/uncertain-row [] {} {:label "x" :root link
                                            :entries (dirty-repo 1)})]
      (is (= (.getCanonicalPath dir) (:root row)))
      (is (not= link (:root row))))))

(deftest a-cleaned-repo-leaves-the-backlog-without-being-acknowledged
  ;; The backlog is current state, not a queue: escalate-by-who-can-act
  ;; forbids one that waits, so an entry must disappear on the pass after
  ;; the dirt does, with nobody having to close it.
  (let [calls (atom [])
        backlog-path (temp-backlog-path)
        dirty (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                     :backlog-path backlog-path)
        _ (sweeper/sweep-dirty-repos! dirty)
        _ (is (= 1 (count (:repos (read-string (slurp backlog-path))))))
        clean (assoc (base-options {"futon2-d" (dirty-repo 0)} calls)
                     :backlog-path backlog-path)]
    (sweeper/sweep-dirty-repos! clean)
    (is (= [] (:repos (read-string (slurp backlog-path)))))))

(deftest every-pass-reports-its-counts
  (let [calls (atom [])
        _ (sweeper/sweep-dirty-repos! (base-options {"futon2-d" []} calls))]
    (is (some (fn [call]
                (and (= :print (first call))
                     (str/includes? (second call) "uncertain-pressure pass:")))
              @calls))))

(deftest diagnostic-overlaps-are-by-write-time-and-labeled
  (let [entries [(entry "a.edn" 30) (entry "b.edn" 30) (entry "c.edn" 300)]
        windows [(window "codex-10" 60 10) (window "zai-5" 400 200)]
        roster {"codex-10" "s10" "zai-5" "s5"}
        overlaps (sweeper/diagnostic-overlaps windows roster entries)]
    (is (= ["codex-10"] (get overlaps "a.edn")))
    (is (= ["zai-5"] (get overlaps "c.edn")))))

(deftest a-file-outside-every-turn-has-no-diagnostic
  (is (= {} (sweeper/diagnostic-overlaps [(window "codex-10" 60 10)]
                                         {"codex-10" "s10"}
                                         [(entry "old.edn" 5000)]))))

(deftest a-dead-seat-is-never-a-diagnostic-candidate
  (is (= {} (sweeper/diagnostic-overlaps [(window "codex-16" 60 10)]
                                         {"codex-10" "s10"}
                                         [(entry "a.edn" 30)]))))

(deftest auxiliary-input-failure-never-suppresses-pressure
  ;; windows-fn/roster-fn throwing must degrade diagnostics, not rows:
  ;; the feed and backlog are still written, marked unavailable.
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :windows-fn (fn [] (throw (ex-info "agency down" {})))
                       :roster-fn (fn [] (throw (ex-info "agency down" {}))))
        counts (sweeper/sweep-dirty-repos! options)
        feed (read-string (slurp (:pressure-path options)))]
    (is (= 1 (:uncertain counts)))
    (is (false? (:diagnostics-available? counts)))
    (is (true? (:complete? counts)))
    (is (false? (:diagnostics-available? feed)))
    (is (= {} (:diagnostic-overlaps (first (:repos feed)))))
    (is (= 11 (count (:paths (first (:repos feed))))))))

(deftest a-write-failure-is-never-reported-as-complete
  ;; An unwritable feed path must surface typed incompleteness; the
  ;; backlog half still succeeds and the pass says so.
  (let [calls (atom [])
        blocker (java.io.File. (temp-dir) "a-file-not-a-dir")
        _ (spit blocker "occupied")
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :pressure-path (str (java.io.File. blocker "feed.edn")))
        counts (sweeper/sweep-dirty-repos! options)]
    (is (true? (:backlog-written? counts)))
    (is (false? (:feed-written? counts)))
    (is (false? (:complete? counts)))
    (is (some (fn [call]
                (and (= :print (first call))
                     (str/includes? (second call) "INCOMPLETE")))
              @calls))))

;; ---------- the push lane ----------

(defn commits [n]
  (mapv (fn [i] {:path (str "abc" i " subject " i) :sha (str "abc" i)
                 :mtime-ms (- now-ms (* (inc i) 60000))})
        (range n)))

(defn push-options [ahead-by calls & [extra]]
  (merge {:roots [{:path "/repo/futon2-d" :label "futon2-d"}]
          :ahead-fn (fn [_] (commits ahead-by))
          :push-fn (fn [root] (swap! calls conj [:push root]) {:ok? true :output ""})
          :busy-fn (constantly false)
          :now-fn (constantly now)
          :push-log-path (temp-backlog-path)
          :print-fn (fn [line] (swap! calls conj [:print line]))}
         extra))

(deftest nine-unpushed-commits-are-left-alone
  (let [calls (atom [])
        counts (sweeper/push-stranded-commits! (push-options 9 calls))]
    (is (= 0 (:over-threshold counts)))
    (is (= 0 (:pushed counts)))
    (is (empty? (filter #(= :push (first %)) @calls)))))

(deftest ten-unpushed-commits-are-pushed-without-asking-anyone
  ;; The whole point: no notice, no recipient, no judgement. It just pushes.
  (let [calls (atom [])
        counts (sweeper/push-stranded-commits! (push-options 10 calls))]
    (is (= 1 (:over-threshold counts)))
    (is (= 1 (:pushed counts)))
    (is (= [[:push "/repo/futon2-d"]] (filter #(= :push (first %)) @calls)))))

(deftest a-repo-mid-rebase-is-never-pushed
  (let [calls (atom [])
        counts (sweeper/push-stranded-commits!
                (push-options 40 calls {:busy-fn (constantly true)}))]
    (is (= 1 (:skipped counts)))
    (is (= 0 (:pushed counts)))
    (is (empty? (filter #(= :push (first %)) @calls)))))

(deftest a-rejected-push-is-recorded-because-it-needs-a-person
  (let [calls (atom [])
        options (push-options 12 calls
                              {:push-fn (fn [_] {:ok? false
                                                 :output "! [rejected] non-fast-forward"})})
        counts (sweeper/push-stranded-commits! options)
        logged (read-string (slurp (:push-log-path options)))
        row (first (:repos logged))]
    (is (= 1 (:failed counts)))
    (is (= 0 (:pushed counts)))
    (is (= "futon2-d" (:label row)))
    (is (= 12 (:unpushed row)))
    (is (= :failed (:outcome row)))
    (is (str/includes? (:error row) "non-fast-forward"))))

(deftest a-pushed-repo-leaves-the-push-log-without-being-acknowledged
  (let [calls (atom [])
        log (temp-backlog-path)
        failing (push-options 12 calls
                              {:push-log-path log
                               :push-fn (fn [_] {:ok? false :output "rejected"})})
        _ (sweeper/push-stranded-commits! failing)
        _ (is (= 1 (count (:repos (read-string (slurp log))))))
        fixed (push-options 0 calls {:push-log-path log})]
    (sweeper/push-stranded-commits! fixed)
    (is (= [] (:repos (read-string (slurp log)))))))


;; ---------- the worktree lane ----------

(defn wt-options [records calls & [extra]]
  (merge {:roots [{:path "/repo/mathlib4-d" :label "mathlib4-d"}]
          :worktrees-fn (fn [_] records)
          :upstream-fn (constantly "upstream")
          :ancestor-fn (fn [_ _ _] false)
          :remove-fn (fn [root path]
                       (swap! calls conj [:remove root path]) {:ok? true :output ""})
          :now-fn (constantly now)
          :worktree-log-path (temp-backlog-path)
          :print-fn (fn [line] (swap! calls conj [:print line]))}
         extra))

(defn removals [calls] (filterv #(= :remove (first %)) @calls))

(defn git! [dir & args]
  (let [{:keys [exit out err]} (apply shell/sh "git" (concat args [:dir (str dir)]))]
    (when-not (zero? exit)
      (throw (ex-info (str out err) {:dir (str dir) :args args})))
    (str/trim out)))

(defn real-worktree-fixture []
  (let [base (temp-dir)
        repo (java.io.File. base "repo")
        remote (java.io.File. base "remote.git")
        wt (java.io.File. base "feature")]
    (.mkdirs repo)
    (git! repo "init" "-b" "main")
    (git! repo "config" "user.name" "Inbox Zero Test")
    (git! repo "config" "user.email" "inbox-zero@example.invalid")
    (spit (java.io.File. repo "base.txt") "base\n")
    (git! repo "add" "base.txt")
    (git! repo "commit" "-m" "base")
    (git! base "init" "--bare" (str remote))
    (git! repo "remote" "add" "origin" (str remote))
    (git! repo "push" "-u" "origin" "main")
    (git! repo "worktree" "add" "-b" "feature" (str wt) "main")
    (git! wt "config" "user.name" "Inbox Zero Test")
    (git! wt "config" "user.email" "inbox-zero@example.invalid")
    (spit (java.io.File. wt "feature.txt") "feature\n")
    (git! wt "add" "feature.txt")
    (git! wt "commit" "-m" "feature")
    {:base base :repo repo :remote remote :wt wt
     :head (git! wt "rev-parse" "HEAD")}))

(deftest a-worktree-that-is-not-on-disk-is-never-removed
  ;; The path is gone; there is nothing to judge merged and nothing to remove.
  (let [calls (atom [])
        counts (sweeper/retire-merged-worktrees!
                (wt-options [{:path "/wt/absent" :head "abc" :locked? false}] calls))]
    (is (= 0 (:retired counts)))
    (is (= 1 (:skipped counts)))
    (is (empty? (removals calls)))))

(deftest a-locked-worktree-is-never-removed
  (let [calls (atom [])
        counts (sweeper/retire-merged-worktrees!
                (wt-options [{:path "/wt/locked" :head "abc" :locked? true}] calls))]
    (is (= 0 (:retired counts)))
    (is (= 1 (:skipped counts)))
    (is (empty? (removals calls)))))

(deftest an-apm-frame-workspace-is-never-touched-by-this-lane
  ;; classify-the-dirt: a worktree is retired through the owning lease path,
  ;; never by raw removal. APM owns everything under apm-frames.
  (let [calls (atom [])
        options (wt-options [{:path "/home/joe/code/apm-frames/batch-1-a01A01-mem"
                              :head "abc" :locked? false}] calls)
        counts (sweeper/retire-merged-worktrees! options)
        rows (:worktrees (read-string (slurp (:worktree-log-path options))))]
    (is (= 0 (:retired counts)))
    (is (= 1 (:skipped counts)))
    (is (empty? (removals calls)))
    (is (= [:lease-owned] (mapv :outcome rows)))))

(deftest the-refusal-reason-is-recorded-for-each-skipped-worktree
  (let [calls (atom [])
        options (wt-options [{:path "/wt/locked" :head "abc" :locked? true}
                             {:path "/home/joe/code/apm-frames/x" :head "d" :locked? false}]
                            calls)
        _ (sweeper/retire-merged-worktrees! options)
        rows (:worktrees (read-string (slurp (:worktree-log-path options))))]
    (is (= 2 (count rows)))
    (is (= #{:locked :lease-owned} (set (map :outcome rows))))))

(deftest local-main-containment-cannot-retire-work-missing-from-upstream
  (let [{:keys [repo wt head]} (real-worktree-fixture)
        _ (git! repo "merge" "--ff-only" "feature")
        log (temp-backlog-path)
        counts (sweeper/retire-merged-worktrees!
                {:roots [{:path (str repo) :label "repo"}]
                 :idle-hours 0
                 :cwd-busy-fn (constantly true)
                 :worktree-log-path log
                 :print-fn (constantly nil)})
        row (first (:worktrees (read-string (slurp log))))]
    (is (.isDirectory wt))
    (is (= head (git! wt "rev-parse" "HEAD")))
    (is (= 0 (:retired counts)))
    (is (= :process-cwd (:outcome row))
        "the commit is local-only, so the tree reaches relocation guards, not retirement")))

(deftest clean-unmerged-worktree-moves-out-of-code-style-root
  (let [{:keys [repo wt head base]} (real-worktree-fixture)
        destination-root (java.io.File. base "worktrees")
        expected (java.io.File. (java.io.File. destination-root "repo") "feature")
        counts (sweeper/retire-merged-worktrees!
                {:roots [{:path (str repo) :label "repo"}]
                 :idle-hours 0
                 :cwd-busy-fn (constantly false)
                 :worktree-root (str destination-root)
                 :worktree-log-path (temp-backlog-path)
                 :print-fn (constantly nil)})]
    (is (= 1 (:moved counts)))
    (is (not (.exists wt)))
    (is (.isDirectory expected))
    (is (= head (git! expected "rev-parse" "HEAD")))))

(deftest pushed-containment-retires-the-real-worktree
  (let [{:keys [repo wt]} (real-worktree-fixture)
        _ (git! repo "merge" "--ff-only" "feature")
        _ (git! repo "push")
        counts (sweeper/retire-merged-worktrees!
                {:roots [{:path (str repo) :label "repo"}]
                 :idle-hours 0
                 :worktree-log-path (temp-backlog-path)
                 :print-fn (constantly nil)})]
    (is (= 1 (:retired counts)))
    (is (not (.exists wt)))))

(deftest configured-interval-reaches-the-feed
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :interval-ms 42000)
        _ (sweeper/sweep-dirty-repos! options)
        feed (read-string (slurp (:pressure-path options)))]
    (is (= 42000 (:interval-ms feed))
        "nondefault interval is wired end-to-end, not the constant")))

(deftest row-failure-propagates-as-incomplete-collection
  ;; One repo's git-fn throws: its row is absent, and the feed must NOT
  ;; claim complete coverage.
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)
                                      "futon3c-d" (dirty-repo 12)}
                                     calls)
                       :git-fn (fn [path]
                                 (if (str/includes? path "futon3c-d")
                                   (throw (ex-info "git died" {}))
                                   (dirty-repo 11))))
        counts (sweeper/sweep-dirty-repos! options)
        feed (read-string (slurp (:pressure-path options)))]
    (is (= 1 (:errored counts)))
    (is (false? (:collection-complete? counts)))
    (is (false? (:complete? counts)))
    (is (= 1 (get-in feed [:collection :row-failures])))
    (is (false? (get-in feed [:collection :complete?])))
    (is (= ["futon2-d"] (map :label (:repos feed))))))

(deftest backlog-failure-is-published-inside-the-feed
  (let [calls (atom [])
        blocker (java.io.File. (temp-dir) "backlog-blocker")
        _ (spit blocker "occupied")
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :backlog-path (str (java.io.File. blocker "b.edn")))
        counts (sweeper/sweep-dirty-repos! options)
        feed (read-string (slurp (:pressure-path options)))]
    (is (false? (:backlog-written? counts)))
    (is (true? (:feed-written? counts)))
    (is (false? (:complete? counts)))
    (is (false? (get-in feed [:publication :backlog-written?]))
        "consumer can see the backlog half failed from the feed itself")))
