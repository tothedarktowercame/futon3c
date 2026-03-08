(ns futon3c.peripheral.issue-holes
  "Project open GitHub issues into Holistic Argument-friendly EDN.

   The export treats each open issue as a candidate hole/tension with anchors
   back into local mission and claim vocabularies:
   - mission refs (M-... tags or explicit mission-id mentions)
   - claim refs (:namespace/claim-id mentions that match claim ledgers)

   Output shape is pure EDN so it can be consumed by the same s-exp tooling
   used for mission/devmap/tension analysis."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [futon3c.peripheral.mission-control-backend :as mcb])
  (:import [java.time Duration Instant]))

(def ^:dynamic *run-command*
  "Indirection over shell/sh for test seams."
  shell/sh)

(def ^:private claim-ledger-paths
  ["docs/wiring-claims.edn"
   "docs/mission-claims.edn"])

(defn- run-command!
  [& args]
  (let [{:keys [exit out err]} (apply *run-command* args)]
    (if (zero? exit)
      (str/trim (or out ""))
      (throw (ex-info "Command failed"
                      {:cmd args
                       :exit exit
                       :err (str/trim (or err ""))})))))

(defn- repo-slug-from-origin
  []
  (let [remote (run-command! "git" "remote" "get-url" "origin")]
    (or (second (re-find #"github\.com[:/]([^/\s]+/[^/.\s]+?)(?:\.git)?$" remote))
        (throw (ex-info "Cannot infer GitHub repo slug from origin"
                        {:remote remote})))))

(defn- parse-instant
  [s fallback]
  (if (string? s)
    (try
      (Instant/parse s)
      (catch Exception _ fallback))
    fallback))

(defn- days-since
  [then now]
  (max 0 (long (.toDays (Duration/between then now)))))

(defn- read-edn-file
  [path]
  (-> path slurp edn/read-string))

(defn load-claim-ids
  "Load known claim ids from mission + wiring claim ledgers.
   Returns a set of strings without leading ':', e.g. \"war-room/tri-agent-loop\"."
  [repo-root]
  (->> claim-ledger-paths
       (map #(io/file repo-root %))
       (filter #(.exists ^java.io.File %))
       (mapcat (fn [f]
                 (->> (read-edn-file f)
                      :claims
                      (keep :claim/id)
                      (map name))))
       set))

(defn load-mission-ids
  "Load local mission ids from holes/missions markdown files.
   Returns ids in canonical lowercase form, e.g. \"mission-control\"."
  [repo-root]
  (->> (mcb/scan-mission-files repo-root :local)
       (keep :mission/id)
       (map str/lower-case)
       set))

(defn extract-mission-refs
  "Extract mission-id anchors from issue text.

   Recognises:
   1) explicit `M-foo-bar` tokens
   2) direct mention of known mission ids"
  [text known-mission-ids]
  (let [text (or text "")
        ;; Avoid low-signal generic ids (e.g. \"test\") unless explicitly M- prefixed.
        eligible-mentions (->> known-mission-ids
                               (filter (fn [mid]
                                         (or (str/includes? mid "-")
                                             (>= (count mid) 8)))))
        explicit (->> (re-seq #"(?i)\bM-[A-Za-z0-9][A-Za-z0-9-]*\b" text)
                      (map (fn [token]
                             (-> token str/lower-case (str/replace #"^m-" ""))))
                      (filter #(>= (count %) 5)))
        mentioned (->> eligible-mentions
                       (filter (fn [mid]
                                 (boolean
                                  (re-find (re-pattern (str "(?i)\\b"
                                                            (java.util.regex.Pattern/quote mid)
                                                            "\\b"))
                                           text)))))]
    (->> (concat explicit mentioned)
         distinct
         sort
         vec)))

(defn extract-claim-refs
  "Extract claim-id anchors from issue text.

   Returns ids without leading ':', filtered to known claim ids."
  [text known-claim-ids]
  (let [tokens (->> (re-seq #"(?i):[a-z0-9][a-z0-9-]*/[a-z0-9][a-z0-9-]*" (or text ""))
                    (map (fn [token]
                           (-> token str/lower-case (subs 1))))
                    set)]
    (->> tokens
         (filter known-claim-ids)
         sort
         vec)))

(defn- issue-labels
  [issue]
  (->> (:labels issue)
       (keep :name)
       (map str/lower-case)
       distinct
       vec))

(defn- issue-assignees
  [issue]
  (->> (:assignees issue)
       (keep :login)
       (map str/lower-case)
       distinct
       vec))

(defn issue->hole
  "Project one GitHub issue into an issue-hole map.

   ctx keys:
   - :repo : repo slug (owner/name)
   - :now : java.time.Instant
   - :known-mission-ids : set of mission-id strings
   - :known-claim-ids : set of claim-id strings
   - :stale-days : threshold on updated-at
   - :old-days : threshold on created-at"
  [issue {:keys [repo now known-mission-ids known-claim-ids stale-days old-days]}]
  (let [created-at (parse-instant (:createdAt issue) now)
        updated-at (parse-instant (:updatedAt issue) now)
        age-days (days-since created-at now)
        updated-days (days-since updated-at now)
        labels (issue-labels issue)
        assignees (issue-assignees issue)
        comment-count (count (:comments issue))
        text (str (or (:title issue) "") "\n\n" (or (:body issue) ""))
        mission-refs (extract-mission-refs text known-mission-ids)
        claim-refs (extract-claim-refs text known-claim-ids)
        unanchored? (and (empty? mission-refs) (empty? claim-refs))
        no-label? (empty? labels)
        no-assignee? (empty? assignees)
        stale? (>= updated-days stale-days)
        old? (>= age-days old-days)
        no-discussion? (and (>= age-days 2) (zero? comment-count))
        flags (cond-> []
                no-label? (conj :no-label)
                no-assignee? (conj :no-assignee)
                stale? (conj :stale)
                old? (conj :old)
                no-discussion? (conj :no-discussion)
                unanchored? (conj :unanchored))
        hole-status (cond
                      (and unanchored? no-label? no-assignee? stale? no-discussion?) :spam-candidate
                      stale? :stale
                      (or no-label? no-assignee?) :untriaged
                      :else :active)
        suggested-actions (cond-> []
                            (= hole-status :spam-candidate) (conj :close-or-reframe)
                            no-label? (conj :add-label)
                            no-assignee? (conj :assign-owner)
                            stale? (conj :request-next-step)
                            unanchored? (conj :link-mission-or-claim))]
    {:issue/id (str repo "#" (:number issue))
     :issue/number (:number issue)
     :issue/title (:title issue)
     :issue/url (:url issue)
     :issue/state :open
     :issue/author (some-> issue :author :login)
     :issue/created-at (str created-at)
     :issue/updated-at (str updated-at)
     :issue/age-days age-days
     :issue/updated-days updated-days
     :issue/labels labels
     :issue/assignees assignees
     :issue/comment-count comment-count
     :hole/status hole-status
     :hole/flags flags
     :hole/anchors {:missions mission-refs
                    :claims claim-refs}
     :hole/suggested-actions suggested-actions}))

(defn hole->tensions
  "Derive one or more tension records from an issue-hole projection."
  [hole now]
  (let [base {:tension/type :issue-open
              :tension/source :github-issue
              :tension/issue-id (:issue/id hole)
              :tension/issue-number (:issue/number hole)
              :tension/detected-at (str now)
              :tension/summary (str "Issue #" (:issue/number hole)
                                    " -- " (:issue/title hole))}
        flags (set (:hole/flags hole))]
    (cond-> [base]
      (= :stale (:hole/status hole))
      (conj {:tension/type :issue-stale
             :tension/source :github-issue
             :tension/issue-id (:issue/id hole)
             :tension/detected-at (str now)
             :tension/summary (str "Issue #" (:issue/number hole)
                                   " stale for " (:issue/updated-days hole) " days")})

      (= :spam-candidate (:hole/status hole))
      (conj {:tension/type :issue-spam-candidate
             :tension/source :github-issue
             :tension/issue-id (:issue/id hole)
             :tension/detected-at (str now)
             :tension/summary (str "Issue #" (:issue/number hole)
                                   " looks unanchored/unowned; candidate for close-or-reframe")})

      (contains? flags :unanchored)
      (conj {:tension/type :issue-unanchored
             :tension/source :github-issue
             :tension/issue-id (:issue/id hole)
             :tension/detected-at (str now)
             :tension/summary (str "Issue #" (:issue/number hole)
                                   " has no mission/claim anchors")}))))

(def ^:private blocker-title-re
  #"(?i)\b(block(?:ed|er)?|broken|regression|fails?|failure|timeout|stuck|can't|cannot|error|bug)\b")

(defn- hole-priority-score
  [hole]
  (let [flags (set (:hole/flags hole))
        anchored? (or (seq (get-in hole [:hole/anchors :missions]))
                      (seq (get-in hole [:hole/anchors :claims])))
        status (:hole/status hole)]
    (+ (* 2 (if (= :stale status) 1 0))
       (* 3 (if (= :spam-candidate status) 1 0))
       (if (contains? flags :no-assignee) 2 0)
       (if (contains? flags :no-label) 1 0)
       (if (contains? flags :unanchored) 1 0)
       (if anchored? 1 0)
       (min 3 (long (/ (or (:issue/updated-days hole) 0) 14))))))

(defn- cluster-stats
  [holes anchor-path]
  (let [pairs (for [h holes
                    anchor (get-in h anchor-path)]
                [anchor h])]
    (->> pairs
         (group-by first)
         (map (fn [[anchor pair-rows]]
                (let [hs (mapv second pair-rows)
                      stale (count (filter #(contains? (set (:hole/flags %)) :stale) hs))
                      unowned (count (filter #(contains? (set (:hole/flags %)) :no-assignee) hs))
                      unlabeled (count (filter #(contains? (set (:hole/flags %)) :no-label) hs))
                      score (+ (* 3 stale) (* 2 unowned) unlabeled (count hs))]
                  {:anchor anchor
                   :issues hs
                   :count (count hs)
                   :stale stale
                   :unowned unowned
                   :unlabeled unlabeled
                   :score score})))
         (sort-by (juxt (comp - :score) (comp - :count) :anchor))
         vec)))

(defn derive-strategic-intelligence
  "Derive HUD-style strategic bullets from issue-hole projections."
  [holes]
  (let [mission-clusters (cluster-stats holes [:hole/anchors :missions])
        claim-clusters (cluster-stats holes [:hole/anchors :claims])
        stale-count (count (filter #(contains? (set (:hole/flags %)) :stale) holes))
        unowned-count (count (filter #(contains? (set (:hole/flags %)) :no-assignee) holes))
        unanchored-count (count (filter #(contains? (set (:hole/flags %)) :unanchored) holes))
        spam-count (count (filter #(= :spam-candidate (:hole/status %)) holes))
        top-priority (->> holes
                          (sort-by (juxt (comp - hole-priority-score)
                                         (comp - :issue/updated-days)
                                         :issue/number))
                          (take 5)
                          vec)
        blocker-candidates (->> holes
                                (filter (fn [h]
                                          (or (re-find blocker-title-re (or (:issue/title h) ""))
                                              (contains? (set (:hole/flags h)) :stale))))
                                (sort-by (juxt (comp - hole-priority-score)
                                               (comp - :issue/updated-days)
                                               :issue/number))
                                (take 5)
                                vec)
        why-these-now
        (->> (concat
              (map (fn [{:keys [anchor count stale unowned]}]
                     (str "M-" anchor " cluster has " count " open issue(s)"
                          (when (pos? stale) (str " (" stale " stale"))
                          (when (pos? unowned) (str ", " unowned " unassigned"))
                          (when (pos? (or stale unowned))
                            ")")
                          " -- concentrated coordination debt."))
                   (take 3 mission-clusters))
              (when (and (empty? mission-clusters) (seq claim-clusters))
                (map (fn [{:keys [anchor count stale]}]
                       (str "Claim :" anchor " has " count " open issue(s)"
                            (when (pos? stale) (str " (" stale " stale)"))
                            " -- evidence/implementation drift risk."))
                     (take 2 claim-clusters)))
              (when (pos? unanchored-count)
                [(str unanchored-count " issue(s) are unanchored to mission/claim ids -- argument traceability gap.")])
              (when (pos? unowned-count)
                [(str unowned-count " issue(s) are unassigned -- ownership bottleneck on execution flow.")])
              (when (pos? stale-count)
                [(str stale-count " issue(s) are stale -- backlog entropy is compounding unless triaged.")]))
             (remove nil?)
             distinct
             (take 6)
             vec)
        blockers-risks
        (->> (concat
              (map (fn [h]
                     (let [flags (set (:hole/flags h))
                           reasons (cond-> []
                                     (re-find blocker-title-re (or (:issue/title h) "")) (conj "blocker-wording")
                                     (contains? flags :stale) (conj "stale")
                                     (contains? flags :no-assignee) (conj "unowned")
                                     (contains? flags :unanchored) (conj "unanchored"))]
                       (str "#" (:issue/number h) " " (:issue/title h)
                            " (" (str/join ", " reasons) ").")))
                   blocker-candidates)
              (when (pos? spam-count)
                [(str spam-count " issue(s) match spam-candidate profile -- close-or-reframe to reduce noise.")])
              (when (pos? unanchored-count)
                [(str "Unanchored issues: " unanchored-count
                      " -- these can't be routed through mission/devmap governance yet.")]))
             (remove nil?)
             distinct
             (take 7)
             vec)]
    {:why-these-now why-these-now
     :blockers-risks blockers-risks
     :priority-issues (mapv :issue/id top-priority)
     :clusters {:missions (mapv #(select-keys % [:anchor :count :stale :unowned :unlabeled :score])
                                (take 10 mission-clusters))
                :claims (mapv #(select-keys % [:anchor :count :stale :unowned :unlabeled :score])
                              (take 10 claim-clusters))}
     :metrics {:stale-count stale-count
               :unowned-count unowned-count
               :unanchored-count unanchored-count
               :spam-candidate-count spam-count}}))

(defn build-issue-hole-export-from-issues
  "Build EDN export from already-fetched issue maps."
  [issues {:keys [repo now known-mission-ids known-claim-ids stale-days old-days]
           :or {now (Instant/now)
                stale-days 14
                old-days 30
                known-mission-ids #{}
                known-claim-ids #{}}}]
  (let [holes (mapv #(issue->hole % {:repo repo
                                     :now now
                                     :known-mission-ids known-mission-ids
                                     :known-claim-ids known-claim-ids
                                     :stale-days stale-days
                                     :old-days old-days})
                    issues)
        tensions (mapv identity (mapcat #(hole->tensions % now) holes))
        status-counts (frequencies (map :hole/status holes))
        flag-counts (frequencies (mapcat :hole/flags holes))
        anchored-count (count (filter (fn [h]
                                        (or (seq (get-in h [:hole/anchors :missions]))
                                            (seq (get-in h [:hole/anchors :claims]))))
                                      holes))
        strategic (derive-strategic-intelligence holes)]
    {:issue-hole-export/version 1
     :repo repo
     :generated-at (str now)
     :summary {:open-issues (count holes)
               :anchored anchored-count
               :unanchored (- (count holes) anchored-count)
               :by-status status-counts
               :flags flag-counts}
     :issues holes
     :tensions tensions
     :strategic/intelligence strategic}))

(defn fetch-open-issues
  "Fetch open issues for REPO via gh CLI.
   Returns vector of maps with keyword keys."
  [repo limit]
  (let [payload (run-command! "gh" "issue" "list"
                              "--repo" repo
                              "--state" "open"
                              "--limit" (str limit)
                              "--json" "number,title,body,url,state,createdAt,updatedAt,labels,assignees,comments,author")]
    (if (str/blank? payload)
      []
      (json/parse-string payload true))))

(defn build-issue-hole-export
  "Fetch open GH issues and project them into EDN issue-hole export.

   opts:
   - :repo (owner/name) defaults from git origin
   - :repo-root defaults to user.dir
   - :limit defaults to 200
   - :stale-days defaults to 14
   - :old-days defaults to 30"
  [{:keys [repo repo-root limit stale-days old-days now]
    :or {repo-root (System/getProperty "user.dir")
         limit 200
         stale-days 14
         old-days 30
         now (Instant/now)}}]
  (let [repo (or repo (repo-slug-from-origin))
        known-mission-ids (load-mission-ids repo-root)
        known-claim-ids (load-claim-ids repo-root)
        issues (fetch-open-issues repo limit)]
    (build-issue-hole-export-from-issues issues {:repo repo
                                                 :now now
                                                 :known-mission-ids known-mission-ids
                                                 :known-claim-ids known-claim-ids
                                                 :stale-days stale-days
                                                 :old-days old-days})))

(defn write-export!
  [path export]
  (spit path (with-out-str (pprint/pprint export)))
  {:ok true :path path})

(defn- parse-int
  [s default]
  (try
    (Integer/parseInt (str s))
    (catch Exception _ default)))

(defn- parse-argv
  [argv]
  (loop [args (seq argv)
         acc {}]
    (if-let [arg (first args)]
      (case arg
        "--repo" (recur (nnext args) (assoc acc :repo (second args)))
        "--repo-root" (recur (nnext args) (assoc acc :repo-root (second args)))
        "--limit" (recur (nnext args) (assoc acc :limit (parse-int (second args) 200)))
        "--stale-days" (recur (nnext args) (assoc acc :stale-days (parse-int (second args) 14)))
        "--old-days" (recur (nnext args) (assoc acc :old-days (parse-int (second args) 30)))
        "--out" (recur (nnext args) (assoc acc :out (second args)))
        "--hud" (recur (next args) (assoc acc :hud? true))
        "--help" (assoc acc :help? true)
        (throw (ex-info "Unknown argument" {:arg arg})))
      acc)))

(defn- print-hud!
  [export]
  (let [intel (:strategic/intelligence export)
        why (:why-these-now intel)
        blockers (:blockers-risks intel)]
    (println "WHY THESE NOW")
    (if (seq why)
      (doseq [line why] (println " " line))
      (println "  (none)"))
    (println)
    (println "BLOCKERS/RISKS")
    (if (seq blockers)
      (doseq [line blockers] (println " " line))
      (println "  (none)"))))

(defn -main
  [& argv]
  (try
    (let [{:keys [help? out hud?] :as opts} (parse-argv argv)]
      (if help?
        (do
          (println "Usage: clojure -M -m futon3c.peripheral.issue-holes [options]")
          (println "  --repo owner/name       GitHub repo slug (default: infer from origin)")
          (println "  --repo-root /path       Local repo root for mission/claim ledgers")
          (println "  --limit N               Open issue fetch limit (default: 200)")
          (println "  --stale-days N          Staleness threshold (default: 14)")
          (println "  --old-days N            Old-age threshold (default: 30)")
          (println "  --out path.edn          Write EDN to file (default: stdout)")
          (println "  --hud                   Print concise WHY NOW / BLOCKERS HUD")
          (println "  --help"))
        (let [export (build-issue-hole-export opts)]
          (when out
            (write-export! out export)
            (println (str "Wrote issue-hole export: " out)))
          (cond
            hud? (print-hud! export)
            (nil? out) (pprint/pprint export)))))
    (catch Exception e
      (binding [*out* *err*]
        (println "issue-holes export failed:" (.getMessage e))
        (when-let [data (ex-data e)]
          (println "context:" data)))
      (System/exit 1))
    (finally
      ;; CLI mode can load namespaces that create background agent threads.
      ;; Ensure process shutdown once export is done.
      (shutdown-agents))))
