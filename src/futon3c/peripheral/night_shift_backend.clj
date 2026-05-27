(ns futon3c.peripheral.night-shift-backend
  "Structured git/PR backend for the Night Shift peripheral.

   The backend exposes only narrow, typed operations. There is no generic
   shell escape hatch: branch creation, staging, commit creation, push, and
   PR creation each pass through explicit invariant checks first."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.peripheral.night-shift-shapes :as shapes]
            [futon3c.peripheral.tools :as tools])
  (:import [java.time Instant]
           [java.time.temporal ChronoUnit]))

(def ^:private futon3c-base "http://127.0.0.1:7070")

(defn- now-inst []
  (Instant/now))

(defn- now-iso []
  (str (now-inst)))

(defn- shell-fn-default
  [& argv]
  (apply shell/sh argv))

(defn- repo-file
  [repo-path relative-path]
  (io/file repo-path relative-path))

(defn- normalize-args
  [args]
  (cond
    (and (= 1 (count args)) (map? (first args))) (first args)
    (even? (count args)) (apply hash-map args)
    :else {}))

(defn- ok
  [result]
  {:ok true :result result})

(defn- fail
  [reason message & [data]]
  (cond-> {:ok false
           :reason reason
           :error message}
    data (assoc :data data)))

(defn- run*
  [shell-fn argv]
  (apply shell-fn argv))

(defn- git*
  [shell-fn repo-path & args]
  (run* shell-fn (into ["git" "-C" repo-path] args)))

(defn- ensure-repo!
  [shell-fn repo-path]
  (let [f (io/file repo-path)]
    (cond
      (not (.exists f))
      (fail :repo-not-found (str "Repo path does not exist: " repo-path))

      (not (.isDirectory f))
      (fail :repo-not-directory (str "Repo path is not a directory: " repo-path))

      :else
      (let [{:keys [exit out err]} (git* shell-fn repo-path "rev-parse" "--show-toplevel")]
        (if (zero? exit)
          (ok {:repo-root (str/trim out)})
          (fail :not-a-git-repo
                (str "Repo path is not a git repo: " repo-path)
                {:stderr err}))))))

(defn- current-branch
  [shell-fn repo-path]
  (let [{:keys [exit out err]} (git* shell-fn repo-path "rev-parse" "--abbrev-ref" "HEAD")]
    (if (zero? exit)
      (ok (str/trim out))
      (fail :branch-read-failed "Could not determine current branch" {:stderr err}))))

(defn- dirty-files
  [shell-fn repo-path]
  (let [{:keys [exit out err]} (git* shell-fn repo-path "status" "--porcelain")]
    (if (zero? exit)
      (ok (->> (str/split-lines out)
               (remove str/blank?)
               (map #(subs % 3))
               vec))
      (fail :status-failed "Could not read git status" {:stderr err}))))

(defn- worktree-clean?
  [shell-fn repo-path]
  (let [r (dirty-files shell-fn repo-path)]
    (if-not (:ok r)
      r
      (ok (empty? (:result r))))))

(defn- tracked-file?
  [shell-fn repo-path path]
  (zero? (:exit (git* shell-fn repo-path "ls-files" "--error-unmatch" "--" path))))

(defn- ignored-path?
  [shell-fn repo-path path]
  (zero? (:exit (git* shell-fn repo-path "check-ignore" "-q" "--" path))))

(defn- branch-exists?
  [shell-fn repo-path branch-name]
  (zero? (:exit (git* shell-fn repo-path "show-ref" "--verify" "--quiet"
                      (str "refs/heads/" branch-name)))))

(defn- ref-exists?
  [shell-fn repo-path ref-name]
  (zero? (:exit (git* shell-fn repo-path "rev-parse" "--verify" "--quiet" ref-name))))

(defn- preferred-base-ref
  [shell-fn repo-path explicit-ref]
  (or explicit-ref
      (some #(when (ref-exists? shell-fn repo-path %) %)
            ["origin/master" "origin/main" "master" "main" "HEAD"])))

(defn- repo-name
  [repo-root]
  (.getName (io/file repo-root)))

(defn- frame-workspace-root
  [repo-root agenda-id branch-name]
  (let [agenda-segment (shapes/branch-segment agenda-id "agenda")
        branch-segment (shapes/branch-segment branch-name "night-shift")]
    (str (io/file repo-root ".state" "night-shift-frames" agenda-segment branch-segment))))

(defn- frame-checkout-root
  [workspace-root]
  (str (io/file workspace-root "checkout")))

(defn- frame-readme
  [metadata]
  (str "# Night Shift Frame\n\n"
       "- Frame: `" (get metadata "frame/id") "`\n"
       "- Source repo: `" (get metadata "frame/source-repo-root") "`\n"
       "- Base ref: `" (get metadata "frame/base-ref") "`\n"
       "- Branch: `" (get metadata "frame/branch") "`\n"
       "- Frame root: `" (get metadata "frame/workspace-root") "`\n"
       "- Checkout root: `" (get metadata "frame/checkout-root") "`\n\n"
       "Discipline:\n"
       "- work in this isolated frame, not the dirty source checkout\n"
       "- operator review remains the merge boundary\n"
       "- frame receipts describe the workspace; they do not replace isolation\n"))

(defn- frame-metadata
  [{:keys [repo-root agenda-id branch-name base-ref workspace-root consent-gate-event-id]}]
  {"workspace/schema" "night-shift-frame.v1"
   "workspace/generated-at" (now-iso)
   "workspace/owner-repo" (repo-name repo-root)
   "frame/id" (str "night-shift/" (shapes/branch-segment agenda-id "agenda") "/" (shapes/branch-segment branch-name "night-shift"))
   "frame/type" "night-shift"
   "frame/source-repo-root" repo-root
   "frame/base-ref" base-ref
   "frame/branch" branch-name
   "frame/workspace-root" workspace-root
   "frame/checkout-root" (frame-checkout-root workspace-root)
   "frame/consent-gate-event-id" consent-gate-event-id
   "artifacts" {"workspace-metadata" (str (io/file workspace-root "workspace.json"))
                "workspace-readme" (str (io/file workspace-root "README.md"))}})

(defn- feature-branch-ok!
  [shell-fn repo-path expected-branch]
  (let [branch-r (current-branch shell-fn repo-path)]
    (if-not (:ok branch-r)
      branch-r
      (let [branch (:result branch-r)]
        (cond
          (shapes/protected-branch? branch)
          (fail :protected-branch
                (str "Night Shift refuses substantive actions on protected branch " branch)
                {:branch branch})

          (not (shapes/valid-feature-branch? branch))
          (fail :invalid-feature-branch
                (str "Current branch does not satisfy night-shift pattern: " branch)
                {:branch branch})

          (and expected-branch (not= expected-branch branch))
          (fail :branch-mismatch
                (str "Current branch " branch " does not match active branch " expected-branch)
                {:current-branch branch
                 :expected-branch expected-branch})

          :else
          (ok branch))))))

(defn- validate-consent-gate
  [cg-id]
  (cond
    (not (string? cg-id))
    (fail :missing-consent-gate "Night Shift substantive action requires :consent-gate-event-id")

    (not (str/starts-with? cg-id "cg-"))
    (fail :invalid-consent-gate "Consent gate ids must start with cg-" {:consent-gate-event-id cg-id})

    :else
    (ok cg-id)))

(defn- validate-secret-paths
  [paths]
  (if-let [path (some #(when (shapes/secret-path? %) %) paths)]
    (fail :secret-pattern-match
          (str "Path is blocked by the night-shift secret blacklist: " path)
          {:path path})
    (ok true)))

(defn build-commit-message
  [{:keys [message source-agenda source-recommendation consent-gate-event-id author]}]
  (str (str/trim (or message ""))
       "\n\n"
       "Source-Agenda: " source-agenda "\n"
       "Source-Recommendation: " source-recommendation "\n"
       "Consent-Gate: " consent-gate-event-id "\n"
       "Co-Authored-By: " author " via :night-shift <noreply@anthropic.com>\n"))

(defn repo-scan
  [{:keys [repo-path shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        repo-r (ensure-repo! shell-fn repo-path)]
    (if-not (:ok repo-r)
      repo-r
      (let [branch-r (current-branch shell-fn repo-path)
            dirty-r (dirty-files shell-fn repo-path)]
        (if (and (:ok branch-r) (:ok dirty-r))
          (ok {:repo-root (get-in repo-r [:result :repo-root])
               :branch (:result branch-r)
               :dirty-files (:result dirty-r)
               :dirty? (boolean (seq (:result dirty-r)))})
          (or (when-not (:ok branch-r) branch-r)
              dirty-r))))))

(defn branch-create
  [{:keys [repo-path branch-name base-ref consent-gate-event-id shell-fn require-clean-worktree?]
    :or {base-ref "HEAD" require-clean-worktree? true}}]
  (let [shell-fn (or shell-fn shell-fn-default)
        cg-r (validate-consent-gate consent-gate-event-id)
        repo-r (ensure-repo! shell-fn repo-path)]
    (cond
      (not (:ok cg-r)) cg-r
      (not (:ok repo-r)) repo-r
      (not (shapes/valid-feature-branch? branch-name))
      (fail :invalid-branch-name
            (str "Branch name must match " shapes/branch-pattern)
            {:branch-name branch-name})

      (branch-exists? shell-fn repo-path branch-name)
      (fail :branch-collision
            (str "Branch already exists: " branch-name)
            {:branch-name branch-name})

      :else
      (let [clean-r (worktree-clean? shell-fn repo-path)]
        (cond
          (and require-clean-worktree? (not (:ok clean-r))) clean-r
          (and require-clean-worktree? (not (:result clean-r)))
          (fail :dirty-worktree
                "Night Shift refuses to branch from a dirty worktree; isolate operator changes first."
                {:dirty-files (get-in clean-r [:result])})

          :else
          (let [{:keys [exit err]} (git* shell-fn repo-path "checkout" "-b" branch-name base-ref)]
            (if (zero? exit)
              (ok {:branch branch-name
                   :base-ref base-ref
                   :created-at (now-iso)})
              (fail :branch-create-failed
                    (str "git checkout -b failed for " branch-name)
                    {:stderr err}))))))))

(defn frame-provision
  [{:keys [source-repo-path repo-path agenda-id short-slug branch-name base-ref consent-gate-event-id shell-fn frame-root]
    :or {short-slug "night-shift"}}]
  (let [shell-fn (or shell-fn shell-fn-default)
        repo-path (or source-repo-path repo-path)
        cg-r (validate-consent-gate consent-gate-event-id)
        repo-r (ensure-repo! shell-fn repo-path)]
    (cond
      (not (:ok cg-r)) cg-r
      (not (:ok repo-r)) repo-r
      (str/blank? agenda-id) (fail :missing-agenda "frame-provision requires :agenda-id")
      :else
      (let [repo-root (get-in repo-r [:result :repo-root])
            branch-name (or branch-name
                            (str "e-night-shift/"
                                 (shapes/branch-segment agenda-id "agenda")
                                 "/"
                                 (shapes/branch-segment short-slug "night-shift")))
            base-ref (preferred-base-ref shell-fn repo-path base-ref)
            workspace-root (or frame-root (frame-workspace-root repo-root agenda-id branch-name))
            checkout-root (frame-checkout-root workspace-root)]
        (cond
          (not (shapes/valid-feature-branch? branch-name))
          (fail :invalid-branch-name
                (str "Branch name must match " shapes/branch-pattern)
                {:branch-name branch-name})

          (str/blank? (str base-ref))
          (fail :missing-base-ref "Could not determine a base ref for frame provisioning")

          (branch-exists? shell-fn repo-path branch-name)
          (fail :branch-collision
                (str "Branch already exists: " branch-name)
                {:branch-name branch-name})

          (.exists (io/file workspace-root))
          (fail :frame-root-exists
                (str "Frame workspace already exists: " workspace-root)
                {:frame-root workspace-root})

          :else
          (let [{:keys [exit err]} (git* shell-fn repo-path
                                         "worktree" "add" "-b" branch-name checkout-root base-ref)]
            (if (zero? exit)
              (let [metadata (frame-metadata {:repo-root repo-root
                                              :agenda-id agenda-id
                                              :branch-name branch-name
                                              :base-ref base-ref
                                              :workspace-root workspace-root
                                              :consent-gate-event-id consent-gate-event-id})
                    metadata-path (get-in metadata ["artifacts" "workspace-metadata"])
                    readme-path (get-in metadata ["artifacts" "workspace-readme"])]
                (spit metadata-path (json/generate-string metadata {:pretty true}))
                (spit readme-path (frame-readme metadata))
                (ok {:frame-id (get metadata "frame/id")
                     :repo-path checkout-root
                     :frame-root workspace-root
                     :source-repo-root repo-root
                     :branch branch-name
                     :base-ref base-ref
                     :metadata-path metadata-path
                     :readme-path readme-path}))
              (fail :frame-provision-failed
                    "git worktree add failed"
                    {:stderr err
                     :branch branch-name
                     :frame-root workspace-root
                     :base-ref base-ref}))))))))

(defn read-file
  [{:keys [repo-path path shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        repo-r (ensure-repo! shell-fn repo-path)
        f (repo-file repo-path path)]
    (cond
      (not (:ok repo-r)) repo-r
      (not (.exists f)) (fail :file-not-found (str "File does not exist: " path))
      (not (.isFile f)) (fail :not-a-file (str "Not a file: " path))
      :else (ok {:path path :content (slurp f)}))))

(defn- apply-change
  [content {:keys [op old-string new-string]}]
  (case op
    :replace-first
    (if (str/includes? content old-string)
      (str/replace-first content old-string new-string)
      ::missing)

    :replace-all-occurrences
    (if (str/includes? content old-string)
      (str/replace content old-string new-string)
      ::missing)

    ::unsupported))

(defn edit-file
  [{:keys [repo-path path change-set consent-gate-event-id expected-branch shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        cg-r (validate-consent-gate consent-gate-event-id)
        repo-r (ensure-repo! shell-fn repo-path)
        branch-r (feature-branch-ok! shell-fn repo-path expected-branch)
        f (repo-file repo-path path)]
    (cond
      (not (:ok cg-r)) cg-r
      (not (:ok repo-r)) repo-r
      (not (:ok branch-r)) branch-r
      (not (.exists f)) (fail :file-not-found (str "File does not exist: " path))
      (not (tracked-file? shell-fn repo-path path))
      (fail :untracked-target "edit-file only applies to tracked files" {:path path})
      :else
      (let [secret-r (validate-secret-paths [path])]
        (if-not (:ok secret-r)
          secret-r
          (let [content (slurp f)
                updated (reduce (fn [acc change]
                                  (if (string? acc)
                                    (apply-change acc change)
                                    acc))
                                content
                                change-set)]
            (cond
              (= ::missing updated)
              (fail :edit-target-missing "One of the requested edit targets was not found in the file"
                    {:path path})

              (= ::unsupported updated)
              (fail :unsupported-edit-op "Night Shift only supports replace-first/replace-all-occurrences edits")

              :else
              (do
                (spit f updated)
                (ok {:path path
                     :branch (:result branch-r)
                     :changes (count change-set)})))))))))

(defn create-file
  [{:keys [repo-path path content consent-gate-event-id expected-branch shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        cg-r (validate-consent-gate consent-gate-event-id)
        repo-r (ensure-repo! shell-fn repo-path)
        branch-r (feature-branch-ok! shell-fn repo-path expected-branch)
        f (repo-file repo-path path)]
    (cond
      (not (:ok cg-r)) cg-r
      (not (:ok repo-r)) repo-r
      (not (:ok branch-r)) branch-r
      (.exists f) (fail :file-exists (str "File already exists: " path))
      :else
      (let [secret-r (validate-secret-paths [path])]
        (cond
          (not (:ok secret-r)) secret-r
          (ignored-path? shell-fn repo-path path)
          (fail :ignored-path "Night Shift refuses to create gitignored files" {:path path})
          :else
          (do
            (io/make-parents f)
            (spit f content)
            (ok {:path path
                 :branch (:result branch-r)
                 :bytes (count (str content))})))))))

(defn- commit-cap-ok
  [paths {:keys [commit-cap-override]}]
  (let [count-paths (count paths)]
    (cond
      (<= count-paths shapes/default-commit-cap) (ok count-paths)

      (and (map? commit-cap-override)
           (integer? (:max-files commit-cap-override))
           (<= count-paths (:max-files commit-cap-override))
           (seq (:rationale commit-cap-override)))
      (ok count-paths)

      :else
      (fail :commit-cap-exceeded
            (str "Night Shift commit cap exceeded: " count-paths
                 " files > " shapes/default-commit-cap)
            {:file-count count-paths
             :commit-cap shapes/default-commit-cap}))))

(defn repo-stage
  [{:keys [repo-path paths consent-gate-event-id expected-branch shell-fn commit-cap-override]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        cg-r (validate-consent-gate consent-gate-event-id)
        repo-r (ensure-repo! shell-fn repo-path)
        branch-r (feature-branch-ok! shell-fn repo-path expected-branch)
        secret-r (validate-secret-paths paths)
        cap-r (commit-cap-ok paths {:commit-cap-override commit-cap-override})]
    (cond
      (not (:ok cg-r)) cg-r
      (not (:ok repo-r)) repo-r
      (not (:ok branch-r)) branch-r
      (not (:ok secret-r)) secret-r
      (not (:ok cap-r)) cap-r
      :else
      (let [{:keys [exit err]} (apply git* shell-fn repo-path
                                      (concat ["add" "--"] paths))]
        (if (zero? exit)
          (ok {:paths (vec paths)
               :branch (:result branch-r)
               :file-count (:result cap-r)})
          (fail :git-add-failed "git add failed" {:stderr err}))))))

(defn repo-commit
  [{:keys [repo-path message source-agenda source-recommendation consent-gate-event-id author expected-branch shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        cg-r (validate-consent-gate consent-gate-event-id)
        repo-r (ensure-repo! shell-fn repo-path)
        branch-r (feature-branch-ok! shell-fn repo-path expected-branch)]
    (cond
      (not (:ok cg-r)) cg-r
      (not (:ok repo-r)) repo-r
      (not (:ok branch-r)) branch-r
      (str/blank? message) (fail :empty-commit-message "Commit message must be non-blank")
      (or (str/blank? source-agenda) (str/blank? source-recommendation))
      (fail :missing-provenance "Commit provenance requires source agenda and source recommendation")
      :else
      (let [{:keys [exit out err]} (git* shell-fn repo-path "diff" "--cached" "--name-only")
            staged-paths (->> (str/split-lines out) (remove str/blank?) vec)
            secret-r (validate-secret-paths staged-paths)
            cap-r (commit-cap-ok staged-paths {})]
        (cond
          (not (zero? exit)) (fail :staged-diff-failed "Could not inspect staged paths" {:stderr err})
          (empty? staged-paths) (fail :nothing-staged "No staged files to commit")
          (not (:ok secret-r)) secret-r
          (not (:ok cap-r)) cap-r
          :else
          (let [full-message (build-commit-message
                              {:message message
                               :source-agenda source-agenda
                               :source-recommendation source-recommendation
                               :consent-gate-event-id consent-gate-event-id
                               :author (or author "unknown-agent")})
                commit-r (git* shell-fn repo-path "commit" "-m" full-message)]
            (if (zero? (:exit commit-r))
              (let [sha-r (git* shell-fn repo-path "rev-parse" "HEAD")]
                (if (zero? (:exit sha-r))
                  (ok {:branch (:result branch-r)
                       :sha (str/trim (:out sha-r))
                       :message full-message
                       :staged-paths staged-paths})
                  (fail :commit-succeeded-sha-failed
                        "Commit succeeded but HEAD sha could not be read"
                        {:stderr (:err sha-r)})))
              (fail :commit-failed "git commit failed" {:stderr (:err commit-r)}))))))))

(defn run-tests
  [{:keys [repo-path test-command consent-gate-event-id expected-branch shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        cg-r (validate-consent-gate consent-gate-event-id)
        repo-r (ensure-repo! shell-fn repo-path)
        branch-r (feature-branch-ok! shell-fn repo-path expected-branch)]
    (cond
      (not (:ok cg-r)) cg-r
      (not (:ok repo-r)) repo-r
      (not (:ok branch-r)) branch-r
      (str/blank? test-command) (fail :missing-test-command "run-tests requires a non-blank :test-command")
      :else
      (let [{:keys [exit out err]} (run* shell-fn ["bash" "-lc" (str "cd " (pr-str repo-path) " && " test-command)])]
        (ok {:branch (:result branch-r)
             :command test-command
             :passed? (zero? exit)
             :exit exit
             :stdout out
             :stderr err})))))

(defn push-feature-branch
  [{:keys [repo-path branch-name remote consent-gate-event-id expected-branch shell-fn force]
    :or {remote "origin"}}]
  (let [shell-fn (or shell-fn shell-fn-default)
        cg-r (validate-consent-gate consent-gate-event-id)
        branch-r (feature-branch-ok! shell-fn repo-path expected-branch)
        branch (or branch-name (when (:ok branch-r) (:result branch-r)))]
    (cond
      force (fail :force-unreachable "Night Shift forbids force push")
      (not (:ok cg-r)) cg-r
      (not (:ok branch-r)) branch-r
      :else
      (let [{:keys [exit err]} (git* shell-fn repo-path "push" "-u" remote branch)]
        (if (zero? exit)
          (ok {:branch branch :remote remote})
          (fail :push-failed "git push failed" {:stderr err :remote remote :branch branch}))))))

(defn pr-create
  [{:keys [repo-path title body base-branch draft? ready-for-review? tests-passed? tests-failing-context expected-branch shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        branch-r (feature-branch-ok! shell-fn repo-path expected-branch)]
    (cond
      (not (:ok branch-r)) branch-r
      (and ready-for-review? (not tests-passed?))
      (fail :tests-block-ready
            "Ready-for-review PRs require passing tests"
            {:tests-failing-context tests-failing-context})

      (str/blank? title)
      (fail :missing-pr-title "PR title must be non-blank")

      :else
      (let [branch (:result branch-r)
            argv (cond-> ["gh" "pr" "create" "--base" (or base-branch "master")
                          "--head" branch
                          "--title" title
                          "--body" (or body "")]
                   (or draft? (not ready-for-review?)) (conj "--draft"))
            {:keys [exit out err]} (run* shell-fn argv)]
        (if (zero? exit)
          (ok {:branch branch
               :draft? (or draft? (not ready-for-review?))
               :pr-url (str/trim out)})
          (fail :pr-create-failed "gh pr create failed" {:stderr err :argv argv}))))))

(defn bell-emit
  [{:keys [agent-id prompt payload]}]
  (try
    (let [body (cond-> {:agent-id (or agent-id "claude-1")
                        :prompt (or prompt "[night-shift] empty bell")}
                 payload (assoc :payload payload))
          resp (http/post (str futon3c-base "/api/alpha/bell")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string body)})]
      (ok {:status (:status resp)
           :body (try (json/parse-string (:body resp) true)
                      (catch Throwable _ (:body resp)))}))
    (catch Throwable t
      (fail :bell-failed (str "bell-emit failed: " (.getMessage t))))))

(defn list-agendas
  [{:keys [ranked-actions missions substrate-threads]}]
  (ok {:ranked-actions (vec (or ranked-actions []))
       :missions (vec (or missions []))
       :substrate-threads (vec (or substrate-threads []))}))

(defn reap-check
  [{:keys [repo-path max-age-days bell-fn agent-id shell-fn]}]
  (let [shell-fn (or shell-fn shell-fn-default)
        bell-fn (or bell-fn bell-emit)
        repo-r (ensure-repo! shell-fn repo-path)
        max-age (long (or max-age-days shapes/default-stale-days))]
    (if-not (:ok repo-r)
      repo-r
      (let [{:keys [exit out err]} (git* shell-fn repo-path
                                         "for-each-ref"
                                         "refs/heads/e-night-shift/*"
                                         "--format=%(refname:short)|%(committerdate:iso8601)")]
        (if-not (zero? exit)
          (fail :reap-scan-failed "Could not enumerate night-shift branches" {:stderr err})
          (let [cutoff (.minus (now-inst) max-age ChronoUnit/DAYS)
                branches (->> (str/split-lines out)
                              (remove str/blank?)
                              (keep (fn [line]
                                      (let [[branch committed-at] (str/split line #"\|" 2)
                                            inst (try (Instant/parse (str/replace committed-at #" " "T"))
                                                      (catch Throwable _ nil))]
                                        (when (and branch inst (.isBefore inst cutoff))
                                          {:branch branch :committed-at committed-at}))))
                              vec)
                bell-r (bell-fn {:agent-id (or agent-id "claude-1")
                                 :prompt (str "[night-shift/reap-check] discard candidates for "
                                              repo-path ": " (pr-str (mapv :branch branches)))
                                 :payload {:event :night-shift/discard-candidates
                                           :repo-path repo-path
                                           :branches branches}})]
            (if (:ok bell-r)
              (ok {:repo-path repo-path
                   :discard-candidates branches
                   :bell bell-r})
              bell-r)))))))

(defrecord NightShiftBackend [shell-fn bell-fn]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (let [m (normalize-args args)
          m (cond-> m
              true (assoc :shell-fn (or shell-fn shell-fn-default))
              bell-fn (assoc :bell-fn bell-fn))]
      (case tool-id
        :list-agendas        (list-agendas m)
        :repo-scan           (repo-scan m)
        :read-file           (read-file m)
        :bell-emit           (bell-emit m)
        :reap-check          (reap-check m)
        :frame-provision     (frame-provision m)
        :branch-create       (branch-create m)
        :edit-file           (edit-file m)
        :create-file         (create-file m)
        :repo-stage          (repo-stage m)
        :repo-commit         (repo-commit m)
        :run-tests           (run-tests m)
        :push-feature-branch (push-feature-branch m)
        :pr-create           (pr-create m)
        :hop-in              (ok {:hop :in :payload m})
        :hop-out             (ok {:hop :out :payload m})
        (fail :unknown-tool (str "Unknown night-shift tool: " tool-id))))))

(defn make-night-shift-backend
  ([] (make-night-shift-backend {}))
  ([{:keys [shell-fn bell-fn]}]
   (->NightShiftBackend (or shell-fn shell-fn-default) bell-fn)))
