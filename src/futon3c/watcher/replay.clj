(ns futon3c.watcher.replay
  "Substrate-2 D3 slice 2 — historical re-ingest of the structural code graph.

   Replays each repo's commits oldest→newest, carrying an incremental
   structural snapshot, and re-emits the structural graph
   (var / contains / calls / coverage) at each commit's XTDB **valid-time**
   via the shared `file-ingest/emit-structure!` — so `db-as-of(t)` recovers
   structure as it stood at commit t, across the whole history.

   Properties:
   - BLOB-ACCURATE: parses `git show <sha>:<path>`, NEVER the working tree.
   - O(changed files) per commit: the carried snapshot + derived by-ns mean a
     commit only re-parses what it touched; cross-file `calls`/`coverage`
     resolution sees repo state up to that commit.
   - REMOVAL-ACCURATE (HELD): a def/edge that disappears at a commit is
     retracted (end-valid-time) via a per-file manifest diff — but only when
     `:emit-removals?` is true (default false), pending Joe's greenlight.
   - RESUMABLE: a per-repo `code/v05/replay-cursor` marker hyperedge records
     the last replayed sha so a re-run skips completed history.

   Scope (slice 2): clj-family files (the entire var/contains/calls/coverage
   graph). Non-clj (elisp/python/flexiarg) are skipped and counted — their
   historical structure is a named follow-on (collect-from-string returns nil).
   `edits` (commit→var) is NOT re-emitted here — it is already populated by the
   commit spine and the live forward path; valid-time-versioning edits is a
   separate follow-on.

   PRODUCTION-RUN DISCIPLINE: do not fire a full 14-repo run without (a) a code
   review and (b) Joe's timing greenlight; it is a 30min–few-hour run that
   shares the live serving JVM. Verify on a small scope first."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [babashka.http-client :as http]
            [futon3c.watcher.file-ingest :as fi]
            [futon3c.watcher.commit-ingest :as ci]))

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))

;; ---------- git layer (blob-accurate) ----------

(defn- git [root & args]
  (let [{:keys [exit out err]} (apply sh/sh "git" "-C" root args)]
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "[replay git]" (pr-str args) (str/trim (or err "")))))
    {:exit exit :out out :err err}))

(defn show-blob
  "Content of PATH at SHA, or nil if it did not exist at that commit."
  [root sha path]
  (let [{:keys [exit out]} (git root "show" (str sha ":" path))]
    (when (zero? exit) out)))

(defn changed-with-status
  "[[status path]…] for SHA, A/M/D only. `--no-renames` so a rename surfaces as
   D(old)+A(new) — structurally correct (old file's structure removed, new
   added) and avoids the two-path rename line."
  [root sha]
  (let [{:keys [out]} (git root "show" "--name-status" "--no-renames" "--format=" sha)]
    (->> (str/split-lines (or out ""))
         (remove str/blank?)
         (keep (fn [line]
                 (let [[status path] (str/split line #"\t" 2)]
                   (when (and status path (#{"A" "M" "D"} status))
                     [status (str/trim path)])))))))

;; ---------- incremental repo model ----------

(defn- derive-by-ns
  "Build resolve-symbol's by-ns ({ns {var-name qname}}) from the snapshot's
   parsed structures — repo state as of the current commit."
  [snapshot]
  (reduce (fn [acc {:keys [structure]}]
            (reduce (fn [a v]
                      (assoc-in a [(:var/ns v) (:var/name v)] (:var/qname v)))
                    acc (:vars structure)))
          {} (vals snapshot)))

;; ---------- resume cursor (marker hyperedge) ----------

(def cursor-type "code/v05/replay-cursor")

(defn read-cursor-sha
  "Last sha this repo was replayed through, or nil."
  [label]
  (let [resp (try (http/get (str FUTON1A "/api/alpha/hyperedge/hx:" cursor-type ":" label)
                            {:throw false :timeout 5000})
                  (catch Exception _ nil))]
    ;; futon1a's /hyperedge/:id returns EDN (not JSON) — parse accordingly.
    ;; (Was json/parse-string, which threw JsonParseException on the EDN body
    ;; and broke :resume?; caught 2026-06-25 in claude-2's sweep-runbook test.)
    (when (= 200 (:status resp))
      (try (some-> (:body resp) edn/read-string :hx/props :sha)
           (catch Exception _ nil)))))

(defn write-cursor!
  "Advance the resume cursor (current valid-time — a control record, never
   stamped at a past commit time)."
  [label sha ts-ms n-commits]
  (fi/post-hyperedge! cursor-type [label] ["replay-cursor" label]
                      {"sha" sha "through-ts" ts-ms
                       "n-commits" n-commits
                       "updated" (System/currentTimeMillis)}))

;; ---------- the replay ----------

(defn replay-repo!
  "Replay one repo's commits, emitting structure at each commit's valid-time.

   Opts: {:root :label :limit :from-sha :emit-removals? :resume? :verbose?}
   - :limit       — replay at most N commits (small-scope verify).
   - :from-sha    — resume strictly AFTER this sha (skip it + everything before).
   - :resume?     — when true and no :from-sha, read the cursor marker.
   - :emit-removals? — emit retracts for vanished structure (default false; HELD).

   Returns {:label :n-commits :n-files :n-skipped-nonclj :through-sha}."
  [{:keys [root label limit from-sha emit-removals? resume? verbose?]}]
  (let [labels ["v05" "phase-3-replay" label]
        base-props {"repo" label "phase" 3 "replay" true}
        from-sha (or from-sha (when resume? (read-cursor-sha label)))
        all (ci/list-commits root nil)
        commits (cond->> all
                  from-sha (drop-while #(not= from-sha (:sha %)))
                  from-sha (drop 1)
                  limit   (take limit))
        result
        (reduce
         (fn [{:keys [snapshot] :as acc} c]
           (let [vt (ci/commit-vt-ms c)
                 changes (changed-with-status root (:sha c))
                 ;; parse the BLOB at this commit for each A/M clj file
                 parsed (into {} (for [[st path] changes
                                       :when (#{"A" "M"} st)
                                       :let [content (show-blob root (:sha c) path)
                                             struct (when content (fi/collect-from-string content path))]
                                       :when struct]
                                   [path struct]))
                 n-nonclj (count (for [[st path] changes
                                       :when (and (#{"A" "M"} st) (not (contains? parsed path)))]
                                   path))
                 ;; snapshot AFTER this commit: A/M replace structure, D drops
                 snapshot' (reduce
                            (fn [m [st path]]
                              (cond
                                (and (#{"A" "M"} st) (contains? parsed path))
                                (assoc m path {:structure (get parsed path)
                                               :manifest (get-in m [path :manifest])})
                                (= "D" st) (dissoc m path)
                                :else m))
                            snapshot changes)
                 root-ctx {:by-ns (derive-by-ns snapshot')}]
             (binding [fi/*valid-time-ms* vt]
               ;; emit changed-file structure at the commit's valid-time;
               ;; capture each file's manifest for future removal diffs
               (let [in-file-retracts (atom 0)
                     snapshot''
                     (reduce
                      (fn [m [st path]]
                        (if (and (#{"A" "M"} st) (contains? parsed path))
                          (let [{:keys [manifest stats]}
                                (fi/emit-structure!
                                 {:structure (get parsed path)
                                  :label label
                                  :base-props (assoc base-props "source-file" path)
                                  :root-ctx root-ctx
                                  :labels labels
                                  :prior-manifest (get-in snapshot [path :manifest])
                                  :emit-removals? emit-removals?})]
                            (swap! in-file-retracts + (long (:retracted stats 0)))
                            (assoc-in m [path :manifest] manifest))
                          m))
                      snapshot' changes)
                     ;; D files: retract their whole prior manifest (HELD)
                     deleted-retracts
                     (if emit-removals?
                       (reduce
                        (fn [n [st path]]
                          (if (= "D" st)
                            (reduce (fn [k [t eps]]
                                      (fi/retract-hyperedge! t eps labels)
                                      (inc k))
                                    n (or (get-in snapshot [path :manifest]) []))
                            n))
                        0 changes)
                       0)]
                 (when (and verbose? (or (seq parsed) (pos? n-nonclj)))
                   (println (format "[replay %s] %s  files=%d skipped-nonclj=%d  vt=%s"
                                    label (subs (:sha c) 0 7) (count parsed) n-nonclj vt)))
                 (-> acc
                     (assoc :snapshot snapshot'')
                     (update :n-commits inc)
                     (update :n-files + (count parsed))
                     (update :n-skipped-nonclj + n-nonclj)
                     (update :n-retracted + @in-file-retracts deleted-retracts))))))
         {:snapshot {} :n-commits 0 :n-files 0 :n-skipped-nonclj 0 :n-retracted 0}
         commits)
        through (some-> commits last :sha)]
    (when through
      (write-cursor! label through (ci/commit-vt-ms (last commits)) (:n-commits result)))
    {:label label
     :n-commits (:n-commits result)
     :n-files (:n-files result)
     :n-skipped-nonclj (:n-skipped-nonclj result)
     :n-retracted (:n-retracted result)
     :through-sha through}))

(defn replay-all!
  "Replay every repo in ROOTS ([{:path :label}…]) sequentially, resuming each
   from its cursor. PRODUCTION run — gated on code review + Joe's timing
   greenlight; do not call casually."
  [{:keys [roots emit-removals? verbose?] :or {verbose? true}}]
  (mapv (fn [{:keys [path label]}]
          (replay-repo! {:root path :label label
                         :resume? true :emit-removals? emit-removals?
                         :verbose? verbose?}))
        roots))
