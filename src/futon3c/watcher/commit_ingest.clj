(ns futon3c.watcher.commit-ingest
  "Substrate-2 commit-vertex ingestion. Used by the in-JVM multi-repo
   watcher (`futon3c.watcher.multi`).

   Posts five hyperedge types per ingested commit:
     code/v05/commit   (vertex)  endpoints [sha], props {repo author timestamp subject}
     code/v05/author   (vertex)  endpoints [email], props {repo name}
     code/v05/authored (edge)    author → commit
     code/v05/precedes (edge)    commit_t → commit_{t+1}
     code/v05/edits    (edge)    commit → var (per-repo prefixed; v0 HEAD-snapshot)

   Idempotent: futon1a computes stable hx/ids from {hx-type, endpoints};
   re-posting updates the same record. Re-ingestion never duplicates.

   Ported from /home/joe/code/futon3/scripts/commit_ingest_lib.clj
   (bb script, original) as part of E-live-means-live's Path B
   migration into futon3c. See futon3/holes/excursions/E-live-means-live.md
   §6 for design and reload-safety considerations."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.shell :refer [sh]]
            [babashka.http-client :as http]
            [cheshire.core :as json]))

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(def directed-types
  #{"code/v05/authored" "code/v05/precedes" "code/v05/edits"})

(defn directed-endpoints [hx-type endpoints]
  (if (and (directed-types hx-type) (= 2 (count endpoints)))
    (conj (vec endpoints) (str "dir:" (first endpoints) "→" (second endpoints)))
    endpoints))

(defn post-hyperedge!
  [hx-type endpoints labels & [props]]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type "hx/endpoints" endpoints}
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))
        resp (try
               (http/post (str FUTON1A "/api/alpha/hyperedge")
                          {:headers {"Content-Type" "application/json"
                                     "X-Penholder" PENHOLDER}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (and (= 200 (:status resp)) (or (:hyperedge body) (:hx/id body)))
     :status (:status resp) :body body}))

;; ---------- git layer ----------

(defn run-git [repo & args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" repo args)]
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "git error:" args (str/trim err))))
    out))

(defn list-commits
  "Returns vector of commit maps in chronological order (oldest first).
   With since-sha=nil → all commits (full --reverse log).
   With since-sha=<sha> → commits strictly after that sha.

   Each commit map: {:sha :email :name :ts :subject}.
   Separator between log fields is U+001F (ASCII Unit Separator),
   guaranteed not to appear inside subject lines."
  ([repo] (list-commits repo nil))
  ([repo since-sha]
   ;; U+001F (Unit Separator) — guaranteed not in commit subjects.
   (let [sep ""
         range-spec (when (and since-sha (not (str/blank? since-sha)))
                      (str since-sha "..HEAD"))
         git-args (cond-> ["log" "--reverse" "--no-merges"
                           (str "--format=%H" sep "%ae" sep "%an" sep "%at" sep "%s")]
                    range-spec (conj range-spec))
         text (apply run-git repo git-args)]
     (->> (str/split-lines text)
          (remove str/blank?)
          (mapv (fn [line]
                  (let [[sha email name ts subject] (str/split line (re-pattern sep))]
                    {:sha sha :email email :name name
                     :ts (when (seq ts) (parse-long ts))
                     :subject subject})))))))

(defn files-changed
  "Names of files added/modified in this commit (no deletions)."
  [repo sha]
  (let [text (run-git repo "show" "--name-status" "--format=" sha)]
    (->> (str/split-lines text)
         (remove str/blank?)
         (keep (fn [line]
                 (let [[status path] (str/split line #"\s+" 2)]
                   (when (and path (#{"A" "M"} status))
                     (str/trim path))))))))

;; ---------- substrate-2 query ----------

(defn last-indexed-commit-sha
  "Queries futon1a for the SHA of the most-recently-indexed commit-vertex
   for repo-label. Returns nil if no commits indexed.

   Uses the :timestamp prop to determine 'most recent' — bitemporal
   reasoning is XTDB's job, this is just `(max-by :timestamp)`."
  [repo-label]
  (try
    (let [resp (http/get (str FUTON1A "/api/alpha/hyperedges?type=code/v05/commit"
                              "&repo=" repo-label)
                         {:throw false})
          body (when (= 200 (:status resp)) (:body resp))
          parsed (when (string? body)
                   (edn/read-string {:default (fn [_t v] v)} body))
          edges (:hyperedges parsed)]
      (when (seq edges)
        (let [latest (apply max-key (fn [e] (or (-> e :hx/props :timestamp) 0))
                            edges)
              hx-id (:hx/id latest)]
          (when (string? hx-id)
            (last (str/split hx-id #":"))))))
    (catch Exception e
      (binding [*out* *err*]
        (println "last-indexed-commit-sha error:" repo-label (.getMessage e)))
      nil)))

;; ---------- ingestion primitives (each idempotent) ----------

(defn ingest-author! [labels base-props email name]
  (post-hyperedge! "code/v05/author" [email] labels (merge base-props {"name" name})))

(defn ingest-commit-and-authored! [labels base-props commit]
  (let [{:keys [sha email name ts subject]} commit
        r1 (post-hyperedge! "code/v05/commit" [sha] labels
                            (merge base-props {"author-email" email
                                               "author-name" name
                                               "timestamp" ts
                                               "subject" subject}))
        r2 (post-hyperedge! "code/v05/authored" [email sha] labels base-props)]
    [r1 r2]))

(defn ingest-precedes! [labels base-props prev-sha next-sha]
  (post-hyperedge! "code/v05/precedes" [prev-sha next-sha] labels base-props))

(defn ingest-edits-for-commit!
  "For one commit, posts code/v05/edits edges to all vars in files it
   changed. `file->vars` is invoked as `(file->vars path)` returning a
   seq of unprefixed var qnames at HEAD for that path; it can be either
   a map (backfill) or a function (live mode). Var qnames get per-repo
   prefixed via `(str repo-label \"/\" qname)` to match phase-1 vertex
   ID convention."
  [labels base-props repo-label commit file->vars repo-root]
  (let [pf (fn [q] (str repo-label "/" q))
        files (files-changed repo-root (:sha commit))]
    (vec
     (for [path files
           :let [vs (file->vars path)]
           :when (seq vs)
           v vs]
       (post-hyperedge! "code/v05/edits" [(:sha commit) (pf v)] labels base-props)))))

;; ---------- high-level ingestion ----------

(defn ingest-commits-batch!
  "The shared ingestion loop. Walks `commits` (already-listed,
   chronological) and posts everything: per-commit author (deduped within
   batch), commit+authored, precedes (linking each commit to the next),
   and edits.

   Args map: {:commits :repo-root :repo-label :file->vars :prev-sha :verbose?}
   Returns: {:n-ingested <int> :latest-sha <string-or-nil> :n-failed <int>}"
  [{:keys [commits repo-root repo-label file->vars prev-sha verbose?]}]
  (let [labels ["v05" "phase-3" repo-label]
        base-props {"repo" repo-label "phase" 3}
        seen-authors (atom #{})
        n-failed (atom 0)
        check! (fn [r] (when-not (:ok? r) (swap! n-failed inc)) r)]
    (when (seq commits)
      (let [unique-authors (into {} (for [c commits] [(:email c) (:name c)]))]
        (when verbose?
          (println "[L4→L0] writing" (count unique-authors) "author vertices"))
        (doseq [[email name] unique-authors]
          (swap! seen-authors conj email)
          (check! (ingest-author! labels base-props email name))))

      (when verbose?
        (println "[L4→L0] writing" (count commits) "commit vertices + authored edges"))
      (doseq [c commits
              r (ingest-commit-and-authored! labels base-props c)]
        (check! r))

      (when verbose?
        (println "[L4→L0] writing precedes edges (linear chain)"))
      (let [chain (cond->> (map :sha commits)
                    prev-sha (cons prev-sha))]
        (doseq [[a b] (partition 2 1 chain)]
          (check! (ingest-precedes! labels base-props a b))))

      (when verbose?
        (println "[L4→L0] writing edits edges (per-repo prefixed)"))
      (doseq [c commits
              r (ingest-edits-for-commit! labels base-props repo-label
                                          c file->vars repo-root)]
        (check! r)))

    {:n-ingested (count commits)
     :latest-sha (some-> commits last :sha)
     :n-failed @n-failed}))

(defn ingest-all-commits!
  "Backfill mode. Walks ALL commits in the repo. Idempotent.
   Args map: {:repo-root :repo-label :file->vars}.
   Returns: {:n-ingested :latest-sha :n-failed}."
  [{:keys [repo-root repo-label file->vars]}]
  (let [commits (list-commits repo-root nil)]
    (ingest-commits-batch!
     {:commits commits
      :repo-root repo-root
      :repo-label repo-label
      :file->vars file->vars
      :prev-sha nil
      :verbose? true})))

(defn ingest-new-commits!
  "Live mode. Queries substrate-2 for the most-recently-indexed commit,
   then ingests new commits since that point. Used by
   futon3c.watcher.multi per cycle.

   Args map: {:repo-root :repo-label :file->vars}.
   Returns: {:n-ingested :latest-sha :n-failed}."
  [{:keys [repo-root repo-label file->vars]}]
  (let [since-sha (last-indexed-commit-sha repo-label)
        commits (list-commits repo-root since-sha)]
    (ingest-commits-batch!
     {:commits commits
      :repo-root repo-root
      :repo-label repo-label
      :file->vars file->vars
      :prev-sha since-sha
      :verbose? false})))
