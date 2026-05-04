(ns futon3c.watcher.commit-ingest
  "Substrate-2 commit-vertex ingestion. Used by the in-JVM multi-repo
   watcher (`futon3c.watcher.multi`).

   Posts hyperedge types per ingested commit:
     code/v05/commit         (vertex)  endpoints [sha], props {repo author timestamp subject [block-tag block-kind block-slug]}
     code/v05/author         (vertex)  endpoints [email], props {repo name}
     code/v05/authored       (edge)    author → commit
     code/v05/precedes       (edge)    commit_t → commit_{t+1}
     code/v05/edits          (edge)    commit → var (per-repo prefixed; v0 HEAD-snapshot)
     code/v05/block-trailer  (edge)    commit ↔ block-tag (only when commit body carries Block: trailer)

   Block: trailer detection feeds the metabolic-balance/mana economy:
   per M-bounded-in-flight-state, every Block (one revolution of the
   futonic loop) earns +1 mana for the session that authored it. Per
   commit with a Block: trailer, the ingestion loop resolves a
   session-id from the futon3c evidence store (Bash tool calls record
   `[main <sha>]` git stdout into :step claims with :evidence/session-id)
   and calls `nonstarter.db/credit-block-mana!`. Idempotent on commit-sha.

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
(def FUTON3C (or (System/getenv "FUTON3C_URL") "http://localhost:7070"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))
(def NONSTARTER_DB
  (or (System/getenv "FUTON5_DB")
      (str (System/getProperty "user.home") "/code/futon5/data/nonstarter.db")))

(def directed-types
  #{"code/v05/authored" "code/v05/precedes" "code/v05/edits"
    "code/v05/block-trailer"})

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

(def ^:private block-footer-pattern
  "Matches a futonic-Block trailer: `Block: <kind>-<YYYY-MM-DD>-<slug>`.
   Per `library/structure/block-as-futonic-revolution.flexiarg` +
   M-bounded-in-flight-state."
  #"(?m)^\s*Block:\s+([a-z][a-z0-9-]*?)-(\d{4}-\d{2}-\d{2})-([A-Za-z0-9._-]+)\s*$")

(defn parse-block-trailer
  "Extract a Block: trailer map from a commit body, or nil."
  [body]
  (when (string? body)
    (when-let [m (re-find block-footer-pattern body)]
      {:tag  (str (nth m 1) "-" (nth m 2) "-" (nth m 3))
       :kind (nth m 1)
       :ymd  (nth m 2)
       :slug (nth m 3)})))

(defn list-commits
  "Returns vector of commit maps in chronological order (oldest first).
   With since-sha=nil → all commits (full --reverse log).
   With since-sha=<sha> → commits strictly after that sha.

   Each commit map: {:sha :email :name :ts :subject :body :block}.
   :body holds the full commit body (subject + blank + body); :block
   holds the parsed Block: trailer or nil.

   Field separator: U+001F (Unit Separator). Record separator: U+001E
   (Record Separator). Both are guaranteed not to appear in commit
   content."
  ([repo] (list-commits repo nil))
  ([repo since-sha]
   (let [field-sep ""
         record-sep ""
         range-spec (when (and since-sha (not (str/blank? since-sha)))
                      (str since-sha "..HEAD"))
         git-args (cond-> ["log" "--reverse" "--no-merges"
                           (str "--format=%H" field-sep "%ae" field-sep
                                "%an" field-sep "%at" field-sep "%B" record-sep)]
                    range-spec (conj range-spec))
         text (apply run-git repo git-args)]
     (->> (str/split text (re-pattern record-sep))
          (map str/trim)
          (remove str/blank?)
          (mapv (fn [chunk]
                  (let [[sha email name ts body]
                        (str/split chunk (re-pattern field-sep) 5)
                        body (when body (str/trim body))
                        subject (when body (first (str/split-lines body)))]
                    {:sha sha :email email :name name
                     :ts (when (seq ts) (parse-long ts))
                     :subject subject
                     :body body
                     :block (parse-block-trailer body)})))))))

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

;; ---------- session-id resolver (mana attribution) ----------
;;
;; The evidence store currently captures session lifecycle events
;; (chat-turn, invoke-heartbeat, invoke-complete, …) but does NOT
;; capture raw `git commit` stdout — so commit-sha → session can't be
;; recovered by string-matching the hash.
;;
;; Heuristic: a commit at time T attributes to the agent session whose
;; most-recent activity event happened just-before T (and within a
;; bounded window). This works for the common case where the operator
;; is actively driving one agent at a time, that agent makes the commit
;; through its tool surface, and the session emitted at minimum an
;; :invoke-heartbeat shortly before the commit.

(def ^:private attribution-window-ms
  "How far back (ms) we look for session activity before a commit.
   30 minutes covers a long agent turn but rejects stale parking."
  (* 30 60 1000))

(defn- iso->ms
  [s]
  (try
    (when (string? s)
      (.toEpochMilli (java.time.Instant/parse s)))
    (catch Exception _ nil)))

(defn- fetch-evidence-window
  [since-iso limit]
  (try
    (let [params (cond-> []
                   (string? since-iso) (conj (str "since=" since-iso))
                   (pos-int? limit) (conj (str "limit=" limit)))
          qs (when (seq params) (str "?" (str/join "&" params)))
          resp (http/get (str FUTON3C "/api/alpha/evidence" qs)
                         {:throw false :timeout 5000})
          body (when (= 200 (:status resp)) (:body resp))]
      (when (string? body)
        (some-> (json/parse-string body true) :entries)))
    (catch Exception _ nil)))

(defn resolve-session-for-commit
  "Resolve which session authored a commit by timestamp-window heuristic.

   commit-ts-ms is the commit's authorship time (epoch ms). Returns the
   session-id that emitted the most-recent evidence event in the window
   [commit-ts - 30min, commit-ts], or nil if no session was active in
   that window.

   Optional :since-iso narrows the evidence-store fetch window for
   efficiency (defaults to 1 day before the commit)."
  [commit-ts-ms & {:keys [since-iso limit]
                   :or {limit 1000}}]
  (when (number? commit-ts-ms)
    (let [since (or since-iso
                    (-> (java.time.Instant/ofEpochMilli
                          (- commit-ts-ms (* 24 60 60 1000)))
                        str))
          entries (or (fetch-evidence-window since limit) [])
          ;; Keep entries with a session-id and a parseable timestamp
          ;; that falls within the attribution window.
          window-lo (- commit-ts-ms attribution-window-ms)
          window-hi commit-ts-ms
          candidates (->> entries
                          (keep (fn [e]
                                  (when-let [sid (:evidence/session-id e)]
                                    (when-let [ts (iso->ms (:evidence/at e))]
                                      (when (<= window-lo ts window-hi)
                                        {:sid sid :ts ts}))))))]
      (when (seq candidates)
        (:sid (apply max-key :ts candidates))))))

;; ---------- mana credit ----------

(defn- nonstarter-db-fns
  "Resolve nonstarter.db functions on demand. Returns nil if futon5 isn't
   on the classpath (graceful degradation; commit ingestion still works)."
  []
  (try
    (require '[nonstarter.db])
    (require '[nonstarter.schema])
    {:credit-block-mana! (resolve 'nonstarter.db/credit-block-mana!)
     :connect! (resolve 'nonstarter.schema/connect!)}
    (catch Throwable _ nil)))

(defonce ^:private !mana-ds (atom nil))

(defn- mana-ds
  "Lazy single-shot connection to the nonstarter SQLite db. Cached in an
   atom (refreshed only on nil)."
  []
  (or @!mana-ds
      (when-let [{:keys [connect!]} (nonstarter-db-fns)]
        (try
          (let [ds (connect! NONSTARTER_DB)]
            (reset! !mana-ds ds)
            ds)
          (catch Throwable t
            (binding [*out* *err*]
              (println "mana-ds connect failed:" (.getMessage t)))
            nil)))))

(defn credit-block-mana-for-commit!
  "Idempotent +1 mana for the agent who authored a Block-footered commit.

   Resolves session-id via the timestamp-window heuristic
   (resolve-session-for-commit on the commit's authorship time), then
   calls nonstarter.db/credit-block-mana!. Returns the credit-fn's
   result map (with :credited true/false + :reason).

   Returns nil silently if nonstarter isn't on the classpath."
  [{:keys [sha ts block]} & {:keys [verbose?]}]
  (when (and sha block)
    (when-let [{:keys [credit-block-mana!]} (nonstarter-db-fns)]
      (when-let [ds (mana-ds)]
        (let [commit-ts-ms (when ts (* 1000 (long ts)))
              session-id (when commit-ts-ms
                           (resolve-session-for-commit commit-ts-ms))
              result (credit-block-mana! ds {:session-id session-id
                                              :commit-sha sha
                                              :turn nil})]
          (when (and verbose? (:credited result))
            (println (str "[mana] +1 → "
                          (subs (or session-id "?") 0
                                (min 8 (count (or session-id ""))))
                          " for " (subs sha 0 7)
                          " (block: " (:tag block) ")")))
          result)))))

;; ---------- ingestion primitives (each idempotent) ----------

(defn ingest-author! [labels base-props email name]
  (post-hyperedge! "code/v05/author" [email] labels (merge base-props {"name" name})))

(defn ingest-commit-and-authored! [labels base-props commit]
  (let [{:keys [sha email name ts subject block]} commit
        commit-props (cond-> (merge base-props {"author-email" email
                                                "author-name" name
                                                "timestamp" ts
                                                "subject" subject})
                       block (assoc "block-tag"  (:tag block)
                                    "block-kind" (:kind block)
                                    "block-slug" (:slug block)))
        r1 (post-hyperedge! "code/v05/commit" [sha] labels commit-props)
        r2 (post-hyperedge! "code/v05/authored" [email sha] labels base-props)
        r3 (when block
             (post-hyperedge! "code/v05/block-trailer"
                              [sha (:tag block)]
                              labels
                              (merge base-props
                                     {"block-tag"  (:tag block)
                                      "block-kind" (:kind block)
                                      "block-slug" (:slug block)})))]
    (cond-> [r1 r2] r3 (conj r3))))

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
   edits, and — for Block-footered commits — a +1 mana credit to the
   resolved session.

   Args map: {:commits :repo-root :repo-label :file->vars :prev-sha :verbose?}
   Returns: {:n-ingested <int> :latest-sha <string-or-nil> :n-failed <int>
             :n-blocks <int> :n-mana-credited <int>}"
  [{:keys [commits repo-root repo-label file->vars prev-sha verbose?]}]
  (let [labels ["v05" "phase-3" repo-label]
        base-props {"repo" repo-label "phase" 3}
        seen-authors (atom #{})
        n-failed (atom 0)
        n-blocks (atom 0)
        n-credited (atom 0)
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
        (check! r))

      ;; Mana credits: one +1 per Block-footered commit, idempotent on sha
      (doseq [c commits
              :when (:block c)]
        (swap! n-blocks inc)
        (let [result (credit-block-mana-for-commit! c :verbose? verbose?)]
          (when (:credited result)
            (swap! n-credited inc)))))

    {:n-ingested (count commits)
     :latest-sha (some-> commits last :sha)
     :n-failed @n-failed
     :n-blocks @n-blocks
     :n-mana-credited @n-credited}))

(defn ingest-all-commits!
  "Backfill mode. Walks ALL commits in the repo. Idempotent.
   Args map: {:repo-root :repo-label :file->vars}.
   Returns: {:n-ingested :latest-sha :n-failed :n-blocks :n-mana-credited}."
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
   Returns: {:n-ingested :latest-sha :n-failed :n-blocks :n-mana-credited}."
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
