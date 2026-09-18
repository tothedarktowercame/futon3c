(ns futon3c.inbox-zero.sweeper-test
  (:require [clojure.string :as str]
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
   :notices-path (temp-notices-path)
   :backlog-path (temp-backlog-path)
   :deliver! (fn [payload] (swap! calls conj payload) {:status 200})
   :print-fn (fn [line] (swap! calls conj [:print line]))})

(deftest a-repo-under-the-threshold-is-left-alone
  (let [calls (atom [])
        counts (sweeper/sweep-dirty-repos!
                (base-options {"futon2-d" (dirty-repo 9)} calls))]
    (is (= 0 (:over-threshold counts)))
    (is (= 0 (:notified counts)))
    (is (empty? (remove #(= :print (first %)) @calls)))))

(deftest at-the-threshold-the-writing-agent-is-told-to-commit
  (let [calls (atom [])
        counts (sweeper/sweep-dirty-repos!
                (base-options {"futon2-d" (dirty-repo 10)} calls))
        payload (first (remove #(= :print (first %)) @calls))]
    (is (= 1 (:over-threshold counts)))
    (is (= 1 (:notified counts)))
    (is (= "codex-10" (:agent payload)))
    (is (= "session-10" (:session payload)))
    (is (= "inbox-zero" (:type payload)))
    (is (= 10 (get-in payload [:metadata :dirty-count])))
    (is (= 10 (get-in payload [:metadata :implicated-count])))
    (is (str/includes? (:prompt payload) "10 dirty file(s)"))
    (is (str/includes? (:prompt payload) "10 of them were written"))
    (is (str/includes? (:prompt payload) "git -C /repo/futon2-d status"))
    (is (str/includes? (:prompt payload) "Newest first: runs/out-0.edn"))))

(deftest a-told-agent-is-not-told-again-until-the-backlog-grows
  (let [calls (atom [])
        options (base-options {"futon2-d" (dirty-repo 11)} calls)
        first-counts (sweeper/sweep-dirty-repos! options)
        second-counts (sweeper/sweep-dirty-repos! options)
        grown (sweeper/sweep-dirty-repos!
               (assoc options :git-fn (constantly (dirty-repo 21))))]
    (is (= 1 (:notified first-counts)))
    (is (= 0 (:notified second-counts)))
    (is (= 1 (:held second-counts)))
    (is (= 1 (:notified grown)))))

(deftest a-stale-notice-is-repeated
  (let [calls (atom [])
        options (base-options {"futon2-d" (dirty-repo 11)} calls)
        _ (sweeper/sweep-dirty-repos! options)
        later (Date. (+ now-ms (* 7 60 60 1000)))
        repeated (sweeper/sweep-dirty-repos!
                  (assoc options :now-fn (constantly later)))]
    (is (= 1 (:notified repeated)))))

(deftest dirt-nobody-live-wrote-is-named-as-operator-backlog
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :roster-fn (constantly {}))
        counts (sweeper/sweep-dirty-repos! options)]
    (is (= 1 (:unowned counts)))
    (is (= 0 (:notified counts)))
    (is (some (fn [call]
                (and (= :print (first call))
                     (str/includes? (second call) "operator backlog")))
              @calls))))

(deftest unowned-dirt-is-written-to-the-operator-backlog
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :roster-fn (constantly {}))
        _ (sweeper/sweep-dirty-repos! options)
        backlog (read-string (slurp (:backlog-path options)))
        row (first (:repos backlog))]
    (is (= 1 (count (:repos backlog))))
    (is (= "futon2-d" (:label row)))
    (is (= 11 (:dirty-count row)))
    (is (seq (:newest row)))))

(deftest a-cleaned-repo-leaves-the-backlog-without-being-acknowledged
  ;; The backlog is current state, not a queue: escalate-by-who-can-act
  ;; forbids one that waits, so an entry must disappear on the pass after
  ;; the dirt does, with nobody having to close it.
  (let [calls (atom [])
        backlog-path (temp-backlog-path)
        dirty (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                     :roster-fn (constantly {})
                     :backlog-path backlog-path)
        _ (sweeper/sweep-dirty-repos! dirty)
        _ (is (= 1 (count (:repos (read-string (slurp backlog-path))))))
        clean (assoc (base-options {"futon2-d" (dirty-repo 0)} calls)
                     :roster-fn (constantly {})
                     :backlog-path backlog-path)]
    (sweeper/sweep-dirty-repos! clean)
    (is (= [] (:repos (read-string (slurp backlog-path)))))))

(deftest every-pass-reports-its-counts
  (let [calls (atom [])
        _ (sweeper/sweep-dirty-repos! (base-options {"futon2-d" []} calls))]
    (is (some (fn [call]
                (and (= :print (first call))
                     (str/includes? (second call) "commit-notice pass:")))
              @calls))))

(deftest attribution-is-by-write-time-not-by-tool
  (let [entries [(entry "a.edn" 30) (entry "b.edn" 30) (entry "c.edn" 300)]
        windows [(window "codex-10" 60 10) (window "zai-5" 400 200)]
        roster {"codex-10" "s10" "zai-5" "s5"}
        attributed (sweeper/attribute windows roster entries)]
    (is (= 2 (get-in attributed ["codex-10" :count])))
    (is (= 1 (get-in attributed ["zai-5" :count])))
    (is (= ["a.edn" "b.edn"]
           (sort (map :path (get-in attributed ["codex-10" :entries])))))))

(deftest a-file-written-outside-every-turn-implicates-nobody
  (is (= {} (sweeper/attribute [(window "codex-10" 60 10)]
                               {"codex-10" "s10"}
                               [(entry "old.edn" 5000)]))))

(deftest a-dead-seat-is-never-a-candidate
  (is (= {} (sweeper/attribute [(window "codex-16" 60 10)]
                               {"codex-10" "s10"}
                               [(entry "a.edn" 30)]))))

(deftest a-file-two-agents-could-have-written-goes-to-both-marked-shared
  (let [entries [(entry "shared.edn" 30)]
        windows [(window "codex-10" 60 10) (window "zai-5" 60 10)]
        roster {"codex-10" "s10" "zai-5" "s5"}
        attributed (sweeper/attribute windows roster entries)]
    (is (= 1 (get-in attributed ["codex-10" :count])))
    (is (= 1 (get-in attributed ["zai-5" :count])))
    (is (= ["zai-5"] (:shared-with (first (get-in attributed ["codex-10" :entries])))))
    (is (= ["codex-10"] (:shared-with (first (get-in attributed ["zai-5" :entries])))))))

(deftest a-shared-file-is-named-with-who-else-was-running
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :windows-fn (fn [] [(window "codex-10" 600 0)
                                           (window "zai-5" 600 0)])
                       :roster-fn (fn [] {"codex-10" "s10" "zai-5" "s5"})
                       :max-recipients 1)
        _ (sweeper/sweep-dirty-repos! options)
        payload (first (remove #(= :print (first %)) @calls))]
    (is (str/includes? (:prompt payload) "(also inside zai-5's turn)"))))

(deftest the-agent-with-the-most-files-is-told-first
  (let [attributed {"codex-10" {:count 130 :entries [{:path "a" :mtime-ms 2}]}
                    "codex-16" {:count 121 :entries [{:path "b" :mtime-ms 3}
                                                     {:path "c" :mtime-ms 1}]}}]
    (is (= [["codex-10" 130] ["codex-16" 121]]
           (mapv (juxt :agent :count) (sweeper/recipients attributed 2))))
    (is (= ["b" "c"]
           (mapv :path (:entries (second (sweeper/recipients attributed 2))))))))

(deftest a-failed-delivery-is-counted-and-not-recorded-as-told
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :deliver! (fn [_] {:status 503}))
        counts (sweeper/sweep-dirty-repos! options)
        retry (sweeper/sweep-dirty-repos! (assoc options :deliver!
                                                 (fn [_] {:status 200})))]
    (is (= 1 (:errored counts)))
    (is (= 0 (:notified counts)))
    (is (= 1 (:notified retry)))))

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
          :remove-fn (fn [root path]
                       (swap! calls conj [:remove root path]) {:ok? true :output ""})
          :now-fn (constantly now)
          :worktree-log-path (temp-backlog-path)
          :print-fn (fn [line] (swap! calls conj [:print line]))}
         extra))

(defn removals [calls] (filterv #(= :remove (first %)) @calls))

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
