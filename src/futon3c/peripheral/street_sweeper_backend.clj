(ns futon3c.peripheral.street-sweeper-backend
  "Backend for the Street Sweeper peripheral.

   Per E-street-sweeper.md (2026-05-25). Wraps an inner ToolBackend; adds
   working-tree-hygiene tool implementations. Substantive tools enforce
   Pilot-I1 (cg-id citation) + INV-1..14 from the shapes file.

   Workflow helpers (classify-packet, auto-approve?, write-defer-manifest)
   live here too so the data-driven invariant set in shapes is the single
   source of truth.

   Cross-refs:
   - clone-from: war_machine_pilot_backend.clj (tool-dispatch + cg-id shape)
   - shapes:     street_sweeper_shapes.clj (invariant data)"
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.peripheral.tools :as tools]
            [futon3c.peripheral.street-sweeper-shapes :as sss]))

(def ^:private futon3c-base "http://127.0.0.1:7070")
(def ^:private repos-root "/home/joe/code")
(def ^:private deferred-storage-root "/home/joe/code/storage/sweeper-deferred")

;; =============================================================================
;; INV-1 cg-id binding registry (in-process; defonce survives reloads)
;; =============================================================================

(defonce ^:private cg-bindings
  (atom {}))

(defn- register-cg-binding!
  "Bind a cg-id to {:repo, :files-allowed, :bound-at-ms}. Subsequent
   substantive tool calls citing the cg-id are checked against the binding."
  [cg-id repo files-allowed]
  (swap! cg-bindings assoc cg-id
         {:repo repo
          :files-allowed (vec (or files-allowed []))
          :bound-at-ms (System/currentTimeMillis)}))

(defn- lookup-cg-binding
  "Return the binding for `cg-id` iff it exists and is within TTL."
  [cg-id]
  (when-let [b (get @cg-bindings cg-id)]
    (let [age (- (System/currentTimeMillis) (:bound-at-ms b))]
      (when (< age sss/cg-binding-ttl-ms)
        (assoc b :age-ms age)))))

(defn list-cg-bindings
  "Public — list active cg-bindings (for debugging / operator inspection)."
  []
  (let [now (System/currentTimeMillis)]
    (->> @cg-bindings
         (map (fn [[id b]]
                (assoc b :cg-id id
                       :age-ms (- now (:bound-at-ms b))
                       :expired? (>= (- now (:bound-at-ms b)) sss/cg-binding-ttl-ms))))
         vec)))

;; =============================================================================
;; INV-26 per-repo policy (.sweeper-policy.edn) reader
;; =============================================================================

(defn read-repo-policy
  "Read the per-repo street-sweeper policy file if present. Returns the
   policy map or nil. Safe to call repeatedly; caller is responsible for
   caching across packets within a sweep run."
  [repo]
  (try
    (let [p (str repos-root "/" repo "/" sss/repo-policy-filename)
          f (io/file p)]
      (when (.exists f)
        (edn/read-string (slurp f))))
    (catch Throwable _ nil)))

;; =============================================================================
;; arg-map normalization (mirrors pilot backend)
;; =============================================================================

(defn- arg-map [args]
  (cond
    (and (= 1 (count args)) (map? (first args))) (first args)
    (even? (count args)) (apply hash-map args)
    :else {}))

;; =============================================================================
;; Pilot-I1 enforcement (cg-id required on substantive tools)
;; =============================================================================

(defn- substantive-arg-check
  [tool-id {:keys [consent-gate-event-id] :as args}]
  (cond
    (not (string? consent-gate-event-id))
    {:ok false
     :error (str "Pilot-I1 violation: substantive tool " tool-id
                 " requires :consent-gate-event-id in args (string).")
     :pilot-invariant :Pilot-I1}

    (not (str/starts-with? consent-gate-event-id "cg-"))
    {:ok false
     :error (str "Pilot-I1 violation: :consent-gate-event-id " consent-gate-event-id
                 " not well-formed (expected prefix 'cg-').")
     :pilot-invariant :Pilot-I1}

    :else nil))

;; =============================================================================
;; Repo discovery + status
;; =============================================================================

(defn- repo-path [repo-name]
  (str repos-root "/" repo-name))

(defn- valid-repo? [repo-name]
  (and (string? repo-name)
       (re-matches #"[a-z][a-z0-9-]*" repo-name)
       (.isDirectory (io/file (repo-path repo-name)))
       (.exists (io/file (repo-path repo-name) ".git"))))

(defn repo-status
  "Read `git status --porcelain` for one repo."
  [{:keys [repo]}]
  (cond
    (not (valid-repo? repo))
    {:ok false :error (str "invalid repo: " (pr-str repo))}

    :else
    (try
      (let [r (shell/sh "git" "status" "--porcelain" :dir (repo-path repo))]
        (if (zero? (:exit r))
          (let [lines (->> (str/split-lines (or (:out r) ""))
                           (remove str/blank?))
                entries (mapv (fn [l]
                                (let [code (subs l 0 2)
                                      path (str/trim (subs l 3))]
                                  {:code code :path path
                                   :status (cond
                                             (str/starts-with? code "??") :untracked
                                             (str/starts-with? code " M") :modified
                                             (str/starts-with? code "M ") :staged
                                             (str/starts-with? code " D") :deleted
                                             :else :other)}))
                              lines)]
            {:ok true :result {:repo repo :count (count entries) :entries entries}})
          {:ok false :error (str "git status failed: " (:err r))}))
      (catch Throwable t
        {:ok false :error (str "repo-status threw: " (.getMessage t))}))))

(defn repo-diff
  "Read `git diff` (or `--staged` if :staged true) for one repo."
  [{:keys [repo staged files]}]
  (cond
    (not (valid-repo? repo))
    {:ok false :error (str "invalid repo: " (pr-str repo))}

    :else
    (try
      (let [base ["git" "diff"]
            base (if staged (conj base "--staged") base)
            cmd (vec (concat base (when (seq files) files)))
            r (apply shell/sh (concat cmd [:dir (repo-path repo)]))]
        (if (zero? (:exit r))
          {:ok true :result {:repo repo :staged (boolean staged)
                             :diff-text (:out r)
                             :diff-bytes (count (:out r))}}
          {:ok false :error (str "git diff failed: " (:err r))}))
      (catch Throwable t
        {:ok false :error (str "repo-diff threw: " (.getMessage t))}))))

;; =============================================================================
;; WM pressure reads
;; =============================================================================

(defn list-repos-with-pressure
  "GET /api/alpha/war-machine — return commit-hygiene queues sorted by pressure."
  [_]
  (try
    (let [resp (http/get (str futon3c-base "/api/alpha/war-machine")
                         {:headers {"Accept" "application/json"}})
          body (json/parse-string (:body resp) true)
          queues (or (get-in body [:commit-hygiene :queues])
                     (get-in body [:metabolic-balance :per-repo])
                     [])]
      {:ok true :result {:queues (vec (sort-by (comp - :pressure) queues))
                         :max-tier (get-in body [:metabolic-balance :max-tier])
                         :max-pressure (get-in body [:metabolic-balance :max-pressure])
                         :stale? (get-in body [:metabolic-balance :stale?])
                         :operator-clear (get body :operator-clear)}})
    (catch Throwable t
      {:ok false :error (str "list-repos-with-pressure failed: " (.getMessage t))})))

(defn current-metabolic-pressure
  "Compact pressure summary (max + per-channel) for fast polling."
  [_]
  (try
    (let [resp (http/get (str futon3c-base "/api/alpha/war-machine"))
          body (json/parse-string (:body resp) true)
          mb (:metabolic-balance body)]
      {:ok true :result {:max-pressure (:max-pressure mb)
                         :max-tier (:max-tier mb)
                         :stale? (:stale? mb)
                         :snapshot-age-minutes (:snapshot-age-minutes mb)
                         :channels (:channels mb)
                         :operator-cleared? (get-in body [:judgement :operator-cleared?])}})
    (catch Throwable t
      {:ok false :error (str "current-metabolic-pressure failed: " (.getMessage t))})))

;; =============================================================================
;; Transport (clone bell-emit pattern from pilot)
;; =============================================================================

(defn bell-emit
  [{:keys [agent-id prompt sweeper-event]}]
  (try
    (let [body (cond-> {:agent-id (or agent-id "claude-10")
                        :prompt   (or prompt "[street-sweeper] empty bell")}
                 sweeper-event (assoc :sweeper-event sweeper-event))
          resp (http/post (str futon3c-base "/api/alpha/bell")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string body)})]
      {:ok true :result {:status (:status resp)
                         :body (json/parse-string (:body resp) true)}})
    (catch Throwable t
      {:ok false :error (str "bell-emit failed: " (.getMessage t))})))

(defn consent-gate-emit
  "Emits an intent-handshake bell + returns a :consent-gate-event-id, AND
   registers the cg-id → {repo, files-allowed, bound-at} binding so INV-1
   can structurally verify that subsequent substantive tool calls match
   the bound repo + are within the bound file allowlist."
  [{:keys [repo intent files commit-message-draft success-criteria] :as payload}]
  (cond
    (not (valid-repo? repo))
    {:ok false :error (str "consent-gate-emit: invalid repo " (pr-str repo))
     :pilot-invariant :INV-1}

    :else
    (let [event-id (str "cg-" (java.util.UUID/randomUUID))
          bell-prompt (str "[sweeper/consent-gate-emit] " event-id
                           " repo=" repo
                           " intent=" (pr-str intent)
                           " files=" (count (or files [])))]
      (try
        (register-cg-binding! event-id repo files)
        (let [body {:agent-id "claude-10"
                    :prompt bell-prompt
                    :sweeper-event :sweeper/consent-gate-emit
                    :consent-gate-event-id event-id
                    :consent-gate-payload payload}
              resp (try (http/post (str futon3c-base "/api/alpha/bell")
                                   {:headers {"Content-Type" "application/json"}
                                    :body (json/generate-string body)})
                        (catch Throwable t
                          ;; Bell-emit failure does not undo binding registration:
                          ;; the cg-id is locally bound for this cycle even if the
                          ;; remote bell delivery fails. Return :bell-status so the
                          ;; caller knows.
                          {:status 0 :bell-error (.getMessage t)}))]
          {:ok true :result {:consent-gate-event-id event-id
                             :repo repo
                             :files (vec (or files []))
                             :bound? true
                             :bell-status (:status resp)
                             :bell-error (:bell-error resp)
                             :ttl-ms sss/cg-binding-ttl-ms
                             :payload payload}})
        (catch Throwable t
          {:ok false :error (str "consent-gate-emit failed: " (.getMessage t))})))))

;; =============================================================================
;; Stage-time invariant application
;; =============================================================================

(defn- file-meta [repo path repo-policy]
  (let [f (io/file (repo-path repo) path)]
    {:size (when (.exists f) (.length f))
     :exists? (.exists f)
     :repo-policy repo-policy}))

(defn apply-stage-invariants
  "Run shapes/stage-invariants over a list of paths in a repo.
   Returns {:rejected [...], :proposed [...], :ok [...]}.
   :rejected — INV with :rejects? true matched (hard block)
   :proposed — INV with :rejects? false matched (advisory; file enters defer)
   :ok       — passed all checks (can stage)

   Arity-2 (legacy): no repo-policy.  Arity-3: pass repo-policy for INV-10/26."
  ([repo paths] (apply-stage-invariants repo paths (read-repo-policy repo)))
  ([repo paths repo-policy]
   (reduce
    (fn [acc path]
      (let [meta (file-meta repo path repo-policy)
            hits (keep (fn [{:keys [check rejects?] :as inv}]
                         (when-let [v (check path meta)]
                           (assoc v :inv-id (:id inv) :inv-name (:name inv) :rejects? rejects?)))
                       sss/stage-invariants)
            hard-rejects (filter :rejects? hits)
            soft-flags (remove :rejects? hits)]
        (cond
          (seq hard-rejects)
          (update acc :rejected conj {:path path :hits (vec hard-rejects)})

          (seq soft-flags)
          (update acc :proposed conj {:path path :hits (vec soft-flags)})

          :else
          (update acc :ok conj path))))
    {:rejected [] :proposed [] :ok []}
    paths)))

;; =============================================================================
;; Auto-approve classification (INV-15)
;; =============================================================================

(defn classify-packet
  "Apply shapes/auto-approve-checks to a packet.
   Returns {:auto-approve? bool, :failed-checks [...], :defer-reasons [...]}.
   packet = {:repo, :files, :diff-text, :loc, ...}."
  [packet]
  (let [results (mapv (fn [{:keys [id name check defer-reason-if-fails defer-reason]}]
                        (let [pass? (try (check packet) (catch Throwable _ false))]
                          {:id id :name name :pass? pass?
                           :defer-reason (when-not pass?
                                           (or defer-reason-if-fails defer-reason))}))
                      sss/auto-approve-checks)
        failed (filter (complement :pass?) results)]
    {:auto-approve? (empty? failed)
     :failed-checks (vec failed)
     :defer-reasons (vec (keep :defer-reason failed))}))

;; =============================================================================
;; INV-14 — cross-repo build-order detection
;; =============================================================================

(defn extract-cross-repo-refs
  "Scan content for cross-repo hard-coded path references. Returns a seq of
   {:sister-repo, :rel-path, :match-text} (deduplicated)."
  [content]
  (when (string? content)
    (->> (re-seq sss/cross-repo-path-regex content)
         (keep (fn [m]
                 (let [sister (nth m 1 nil)
                       rest-raw (or (nth m 2 nil) "")
                       rel (cond
                             (str/blank? rest-raw) nil
                             (str/starts-with? rest-raw "/") (subs rest-raw 1)
                             :else rest-raw)]
                   (when (and sister rel)
                     {:sister-repo sister
                      :rel-path rel
                      :match-text (first m)}))))
         (distinct))))

(defn- sister-file-tracked?
  "True iff `rel-path` is tracked-without-modification in sister `repo`."
  [repo rel-path]
  (let [r-ls (try (shell/sh "git" "ls-files" "--error-unmatch" "--" rel-path
                            :dir (repo-path repo))
                  (catch Throwable _ {:exit 1}))]
    (and (zero? (:exit r-ls))
         (let [r-st (try (shell/sh "git" "status" "--porcelain" "--" rel-path
                                   :dir (repo-path repo))
                         (catch Throwable _ {:out "?"}))]
           (str/blank? (or (:out r-st) ""))))))

(defn check-cross-repo-deps
  "INV-14: scan each file in `files` (relative paths within `current-repo`)
   for cross-repo references. For each, verify the sister-repo file is
   tracked + clean. Return nil if all clean; else error map listing all
   unresolved deps.

   Override: pass :accept-broken-cross-ref true (with a rationale arg) to
   skip the check entirely.

   INV-28 (2026-05-26): mission/handoff doc packets (all files under
   `holes/`) get a free pass — they reference WIP work as documentation,
   not as build deps; flagging them is paternalistic noise."
  [current-repo files {:keys [accept-broken-cross-ref]}]
  (cond
    accept-broken-cross-ref               nil
    (sss/all-mission-docs? files)         nil
    :else
    (let [issues (atom [])]
      (doseq [f files]
        (let [full-path (str (repo-path current-repo) "/" f)
              content (try (slurp full-path) (catch Throwable _ ""))]
          (doseq [ref (extract-cross-repo-refs content)]
            (let [sister (:sister-repo ref)
                  rel   (:rel-path ref)]
              (when (and (not= sister current-repo)
                         (valid-repo? sister)
                         (not (sister-file-tracked? sister rel)))
                (swap! issues conj
                       {:in-staged-file f
                        :sister-repo sister
                        :referenced-rel-path rel
                        :status (if (.exists (io/file (repo-path sister) rel))
                                  :exists-but-untracked-or-dirty
                                  :missing)
                        :match-text (:match-text ref)}))))))
      (when (seq @issues)
        {:ok false
         :error :inv-14-cross-repo-untracked-dep
         :pilot-invariant :INV-14
         :issues @issues
         :hint "the referenced sister-repo files must be committed first OR pass :accept-broken-cross-ref true with rationale"}))))

;; =============================================================================
;; Substantive tools — :repo-stage / :repo-commit / :repo-revert-staged
;; =============================================================================

(defn- one-repo-cg-check
  "INV-1: cg-id authorises exactly one repo. Verifies (a) :repo is valid,
   (b) the cg-id has an unexpired binding in the registry, (c) the bound
   repo matches the args :repo, (d) when the binding has a non-empty
   :files-allowed, the args :files are a subset of that allowlist."
  [{:keys [repo consent-gate-event-id files]}]
  (cond
    (not (valid-repo? repo))
    {:ok false :error (str "INV-1: invalid repo " (pr-str repo))
     :pilot-invariant :INV-1}

    :else
    (let [binding (lookup-cg-binding consent-gate-event-id)]
      (cond
        (nil? binding)
        {:ok false
         :error (str "INV-1: cg-id " consent-gate-event-id
                     " has no live binding (expired, unknown, or never registered via consent-gate-emit)")
         :pilot-invariant :INV-1
         :hint "emit a consent-gate-emit first; cg-id is only valid for the bound repo within TTL"}

        (not= repo (:repo binding))
        {:ok false
         :error (str "INV-1: cg-id " consent-gate-event-id
                     " bound to repo " (:repo binding)
                     " but args carry " repo)
         :pilot-invariant :INV-1
         :bound-repo (:repo binding)
         :args-repo repo}

        (and (seq (:files-allowed binding))
             (seq files)
             (let [allowed (set (map str (:files-allowed binding)))]
               (not (every? allowed (map str files)))))
        {:ok false
         :error (str "INV-1 (files sub-rule): args :files are not a subset of cg-bound :files-allowed")
         :pilot-invariant :INV-1
         :allowed (:files-allowed binding)
         :requested (vec files)}

        :else nil))))

(defn repo-stage
  "git add specific files. cg-id required (Pilot-I1). INV-1 + INV-14 +
   INV-2/10/13 enforced inline before staging."
  [{:keys [repo files] :as args}]
  (or (substantive-arg-check :repo-stage args)
      (one-repo-cg-check args)
      (cond
        (not (sequential? files))
        {:ok false :error ":files must be a sequence of relative paths"}

        :else
        (let [inv-result (apply-stage-invariants repo (mapv str files))
              ok-files (:ok inv-result)]
          (cond
            (empty? ok-files)
            {:ok false
             :error "no files passed stage-time invariants (INV-2/10/13)"
             :invariant-result inv-result}

            ;; INV-14 — check cross-repo deps in the would-stage files
            :else
            (if-let [xref-error (check-cross-repo-deps repo ok-files args)]
              xref-error
              (try
                (let [r (apply shell/sh (concat ["git" "add" "--"] ok-files
                                                [:dir (repo-path repo)]))]
                  (if (zero? (:exit r))
                    {:ok true :result {:repo repo
                                       :staged ok-files
                                       :invariant-result inv-result
                                       :cross-repo-deps :clean
                                       :consent-gate-event-id (:consent-gate-event-id args)}}
                    {:ok false :error (str "git add failed: " (:err r))}))
                (catch Throwable t
                  {:ok false :error (str "repo-stage threw: " (.getMessage t))}))))))))

(defn repo-commit
  "git commit -m <message>. cg-id required. INV-3 file-count cap enforced."
  [{:keys [repo message commit-cap-override] :as args}]
  (or (substantive-arg-check :repo-commit args)
      (one-repo-cg-check args)
      (cond
        (not (string? message))
        {:ok false :error ":message must be a string"}

        (str/blank? message)
        {:ok false :error "INV-3 (sub-rule): commit message cannot be empty"}

        (> (count message) 5000)
        {:ok false :error "INV-3 (sub-rule): commit message exceeds 5000 chars"}

        :else
        (try
          ;; INV-3: file count cap
          (let [staged-r (shell/sh "git" "diff" "--staged" "--name-only"
                                   :dir (repo-path repo))
                staged-files (->> (str/split-lines (or (:out staged-r) ""))
                                  (remove str/blank?)
                                  vec)
                cap (or commit-cap-override sss/packet-size-cap-files)]
            (cond
              (empty? staged-files)
              {:ok false :error "no staged files to commit"}

              (> (count staged-files) cap)
              {:ok false
               :error (str "INV-3: would commit " (count staged-files)
                           " files (cap " cap "); pass :commit-cap-override with rationale")
               :pilot-invariant :INV-3
               :file-count (count staged-files)}

              :else
              (let [trailer (str "\n\nCo-Authored-By: claude-2 via :street-sweeper <noreply@anthropic.com>"
                                 "\nSweeper-cg-id: " (:consent-gate-event-id args))
                    full-msg (str message trailer)
                    r (shell/sh "git" "commit" "-m" full-msg
                                :dir (repo-path repo))]
                (if (zero? (:exit r))
                  (let [sha-r (shell/sh "git" "rev-parse" "HEAD" :dir (repo-path repo))
                        sha (str/trim (or (:out sha-r) ""))]
                    {:ok true :result {:repo repo
                                       :sha sha
                                       :file-count (count staged-files)
                                       :files staged-files
                                       :consent-gate-event-id (:consent-gate-event-id args)
                                       :message-first-line (first (str/split-lines message))}})
                  {:ok false :error (str "git commit failed: " (:err r) (:out r))}))))
          (catch Throwable t
            {:ok false :error (str "repo-commit threw: " (.getMessage t))})))))

(defn repo-revert-staged
  "git reset HEAD <file> — unstage. cg-id required."
  [{:keys [repo files] :as args}]
  (or (substantive-arg-check :repo-revert-staged args)
      (one-repo-cg-check args)
      (try
        (let [paths (mapv str (or files []))
              r (apply shell/sh (concat ["git" "reset" "HEAD" "--"] paths
                                        [:dir (repo-path repo)]))]
          (if (zero? (:exit r))
            {:ok true :result {:repo repo :unstaged paths}}
            {:ok false :error (str "git reset failed: " (:err r))}))
        (catch Throwable t
          {:ok false :error (str "repo-revert-staged threw: " (.getMessage t))}))))

;; =============================================================================
;; Defer-queue manifest writer (INV-16)
;; =============================================================================

(defn write-defer-manifest
  "Write a defer-queue manifest + patches for the current sweep run.
   args: {:sweep-ts <ISO-string> :deferred-packets [...]}
   Each packet: {:repo, :files, :defer-reason, :diff-text, :commit-message-draft}.
   No cg-id required: writes only to ~/code/storage/sweeper-deferred/<ts>/,
   not to any repo (per shapes/substantive-tools docstring)."
  [{:keys [sweep-ts deferred-packets] :as args}]
  (let [ts (or sweep-ts
                   (str/replace (str (java.time.Instant/now)) #"[:.]" "-"))
            root (str deferred-storage-root "/" ts)
            manifest-path (str root "/manifest.edn")]
        (try
          (io/make-parents manifest-path)
          (let [manifest {:sweep-ts ts
                          :sweeper-version "v0"
                          :defer-count (count (or deferred-packets []))
                          :packets (vec (or deferred-packets []))
                          :defer-reason-frequencies (frequencies (keep :defer-reason
                                                                       (or deferred-packets [])))}]
            (spit manifest-path (with-out-str (clojure.pprint/pprint manifest)))
            ;; Write each packet's diff as a patch file (best-effort)
            (doseq [[i pkt] (map-indexed vector (or deferred-packets []))]
              (when-let [diff (:diff-text pkt)]
                (let [name (format "packet-%03d-%s.patch" i (or (:repo pkt) "unknown"))]
                  (spit (str root "/" name) diff))))
            ;; INV-17 v2 (2026-05-25): multi-dimensional clustering.
            ;; Cluster the deferred packets across (defer-reason, ...) pairs
            ;; so the candidate-invariants are SPECIFIC enough to be
            ;; promotable — e.g. instead of generic "34 packets deferred for
            ;; :security-sensitive-diff", surface "13 of those are *.mjs files
            ;; (Playwright verify scripts) — propose INV: skip security check
            ;; for test-context paths."
            (let [top-ext (fn [pkt]
                            (let [exts (keep (fn [f]
                                               (let [s (str f)
                                                     i (.lastIndexOf s ".")]
                                                 (when (and (pos? i) (< i (dec (count s))))
                                                   (clojure.string/lower-case (subs s (inc i))))))
                                             (:files pkt))]
                              (when (seq exts)
                                (first (last (sort-by val (frequencies exts)))))))
                  suffix-pat (fn [pkt]
                               (let [suffs (keep (fn [f]
                                                   (let [s (str f)]
                                                     (cond
                                                       (re-find #"-verify\." s) "*-verify.*"
                                                       (re-find #"-test\." s)   "*-test.*"
                                                       (re-find #"-spec\." s)   "*-spec.*"
                                                       (re-find #"_test\." s)   "*_test.*"
                                                       :else nil)))
                                                 (:files pkt))]
                                 (when (seq suffs) (first (frequencies suffs)))))
                  enriched (mapv (fn [pkt]
                                   (assoc pkt :top-ext (top-ext pkt)
                                          :suffix-pattern (when-let [s (suffix-pat pkt)] (first s))))
                                 (or deferred-packets []))
                  by-reason (frequencies (keep :defer-reason enriched))
                  by-reason-ext (frequencies (keep #(when (and (:defer-reason %) (:top-ext %))
                                                      [(:defer-reason %) (:top-ext %)])
                                                   enriched))
                  by-reason-suffix (frequencies (keep #(when (and (:defer-reason %) (:suffix-pattern %))
                                                         [(:defer-reason %) (:suffix-pattern %)])
                                                      enriched))
                  by-reason-repo (frequencies (keep #(when (and (:defer-reason %) (:repo %))
                                                       [(:defer-reason %) (:repo %)])
                                                    enriched))
                  threshold 3
                  candidates (concat
                              (for [[reason n] by-reason :when (>= n threshold)]
                                {:proposed-id (keyword (str "inv-candidate-" (name reason)))
                                 :dim :defer-reason
                                 :pattern reason
                                 :evidence-count n
                                 :draft-rule (str "Coarse: " n " packets deferred for " reason ".")
                                 :suggested-action :review})
                              (for [[[reason ext] n] by-reason-ext :when (>= n threshold)]
                                {:proposed-id (keyword (str "inv-candidate-" (name reason) "-x-ext-" ext))
                                 :dim :defer-reason-x-ext
                                 :pattern {:reason reason :ext ext}
                                 :evidence-count n
                                 :draft-rule (str n " packets with extension '" ext
                                                  "' deferred for " reason
                                                  " — consider per-extension narrowing.")
                                 :suggested-action :review})
                              (for [[[reason suffix] n] by-reason-suffix :when (>= n threshold)]
                                {:proposed-id (keyword (str "inv-candidate-" (name reason) "-x-suffix"))
                                 :dim :defer-reason-x-suffix
                                 :pattern {:reason reason :suffix suffix}
                                 :evidence-count n
                                 :draft-rule (str n " packets with suffix '" suffix
                                                  "' deferred for " reason
                                                  " — likely a context the check should exempt.")
                                 :suggested-action :review})
                              (for [[[reason repo] n] by-reason-repo :when (>= n threshold)]
                                {:proposed-id (keyword (str "inv-candidate-" (name reason) "-x-repo-" repo))
                                 :dim :defer-reason-x-repo
                                 :pattern {:reason reason :repo repo}
                                 :evidence-count n
                                 :draft-rule (str n " packets in repo '" repo
                                                  "' deferred for " reason
                                                  " — concentrated by repo, may warrant a repo-specific INV.")
                                 :suggested-action :review}))]
              (when (seq candidates)
                (spit (str root "/candidate-invariants.edn")
                      (with-out-str (clojure.pprint/pprint (vec candidates))))))
            {:ok true :result {:manifest-path manifest-path
                               :defer-count (:defer-count manifest)
                               :candidates? (seq (filter (fn [[_ n]] (>= n 3))
                                                         (:defer-reason-frequencies manifest)))}})
          (catch Throwable t
            {:ok false :error (str "write-defer-manifest threw: " (.getMessage t))}))))

;; =============================================================================
;; SweeperBackend defrecord
;; =============================================================================

(defrecord SweeperBackend [inner-backend]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (case tool-id
      :repo-status                (repo-status (arg-map args))
      :repo-diff                  (repo-diff (arg-map args))
      :list-repos-with-pressure   (list-repos-with-pressure (arg-map args))
      :current-metabolic-pressure (current-metabolic-pressure (arg-map args))
      :bell-emit                  (bell-emit (arg-map args))
      :consent-gate-emit          (consent-gate-emit (arg-map args))
      :repo-stage                 (repo-stage (arg-map args))
      :repo-commit                (repo-commit (arg-map args))
      :repo-revert-staged         (repo-revert-staged (arg-map args))
      :write-defer-manifest       (write-defer-manifest (arg-map args))
      ;; Delegate :read/:glob/:grep/:bash-readonly/:cycle-* to inner
      (tools/execute-tool inner-backend tool-id args))))

(defn make-sweeper-backend
  ([] (make-sweeper-backend (tools/make-mock-backend)))
  ([inner-backend] (->SweeperBackend inner-backend)))
