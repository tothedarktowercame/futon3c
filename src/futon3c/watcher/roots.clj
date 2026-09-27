(ns futon3c.watcher.roots
  "The authoritative watch-root table shared by the multi-watcher bootstrap
  and the inbox-zero witness producer.

  Repo identity discipline: a checkout is identified by its watcher label
  (e.g. \"futon3c-d\", \"futon5-d2\"), not its directory basename — two
  checkouts of one repo carry distinct labels. Before 2026-08-24 the witness
  producer derived repo-id from the basename, so every claim's :repo/id
  disagreed with its observations' :repo/id (\"futon3c\" vs \"futon3c-d\").
  Joins must key on :worktree/id + :path regardless; this table only makes
  the human-facing labels agree going forward.

  Installation override (2026-09-27): when FUTON3C_INSTALLATION_WATCH_ROOT
  names a content directory, the watch/sweep tables become a single
  installation root instead of Joe's futon checkouts. That content dir may be
  a SUBTREE of a larger git repo (mfuton lives under the `gh` repo, not its
  own repo), so `git-scope-for` resolves the enclosing git root and the
  subtree path: commit-ingest then scopes `git log -- <subtree>` and strips
  the subtree prefix, keeping file-path keys subtree-relative so they match
  the file cold-scan of the content dir. Unset (the default) reproduces Joe's
  layout byte-for-byte."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def ^:private default-watch-roots
  [{:path "/home/joe/code/futon0"  :label "futon0-d"}
   {:path "/home/joe/code/futon1"  :label "futon1-d"}
   {:path "/home/joe/code/futon1a" :label "futon1a-d"}
   {:path "/home/joe/code/futon2"  :label "futon2-d"}
   {:path "/home/joe/code/futon3"  :label "futon3-d"}
   {:path "/home/joe/code/futon3a" :label "futon3a-d"}
   {:path "/home/joe/code/futon3b" :label "futon3b-d"}
   {:path "/home/joe/code/futon3c" :label "futon3c-d"}
   {:path "/home/joe/code/futon4"  :label "futon4-elisp-d"}
   {:path "/home/joe/code/futon5"  :label "futon5-d2"}
   {:path "/home/joe/code/futon5a" :label "futon5a-d"}
   {:path "/home/joe/code/futon6"  :label "futon6-py-d"}
   {:path "/home/joe/code/futon7"  :label "futon7-d"}
   {:path "/home/joe/code/futon7a" :label "futon7a-d"}])

(def ^:private default-sweep-extra
  [{:path "/home/joe/code/apm-lean" :label "apm-lean-d"}
   {:path "/home/joe/code/futon1b"  :label "futon1b-d"}
   {:path "/home/joe/code/mathlib4" :label "mathlib4-d"}
   {:path "/home/joe/code/p4ng"     :label "p4ng-d"}
   {:path "/home/joe/code/voxterm"  :label "voxterm-d"}])

(defn- normalize [p] (some-> p (str/replace "\\" "/")))

(def ^:private installation-content-root
  (some-> (System/getenv "FUTON3C_INSTALLATION_WATCH_ROOT") str/trim not-empty normalize))

(def ^:private installation-label
  (or (some-> (System/getenv "FUTON3C_INSTALLATION_WATCH_LABEL") str/trim not-empty)
      "installation"))

(def watch-roots
  (if installation-content-root
    [{:path installation-content-root :label installation-label}]
    default-watch-roots))

(def sweep-roots
  "Roots the inbox-zero commit-notice sweeper measures pressure over.

  A superset of watch-roots: every repo the cleanliness gate covers, not only
  the ones the file watcher observes. The two lists differ on purpose. Witness
  production needs an open file handle per root and pays for every write, so
  mathlib4 — an upstream checkout of ~5k files nobody here edits wholesale —
  stays out of it. Pressure needs none of that: attribution keys on git status
  mtimes against the agency job ledger, so a root costs one `git status` per
  pass whether or not it is watched.

  Before 2026-09-18 the sweeper used watch-roots directly, so dirt in apm-lean,
  futon1b, mathlib4, p4ng and voxterm generated no pressure at all. The hourly
  gate saw those five and the pressure model did not, which is the gap that
  made inbox zero look inert from outside."
  (if installation-content-root
    [{:path installation-content-root :label installation-label}]
    (into default-watch-roots default-sweep-extra)))

(def ^:private label-by-path
  (into {} (map (juxt :path :label)) watch-roots))

(defn label-for
  "Watcher label for ROOT-PATH, or nil when the root is not watched."
  [root-path]
  (get label-by-path (str root-path)))

(defn- git-toplevel [dir]
  (try
    (let [{:keys [exit out]} (shell/sh "git" "-C" dir "rev-parse" "--show-toplevel")]
      (when (zero? exit) (str/trim out)))
    (catch Throwable _ nil)))

(defn git-scope-for
  "Git scope for a watched ROOT-PATH: {:git-root <repo-for-git-log>
   :subtree <path-under-repo-or-nil>}.

  For an ordinary checkout the content dir IS the git root, so :git-root is the
  path and :subtree is nil (commit-ingest behaves exactly as before). For the
  installation content root (FUTON3C_INSTALLATION_WATCH_ROOT), resolve the
  enclosing git toplevel; when the content dir sits below it, return that
  toplevel as :git-root and the relative path as :subtree so commit-ingest can
  run `git -C <git-root> log -- <subtree>` and strip the `<subtree>/` prefix."
  [root-path]
  (let [rp (normalize (str root-path))]
    (if (and installation-content-root (= rp installation-content-root))
      (let [top (normalize (git-toplevel rp))
            subtree (when (and top (not= top rp)
                               (str/starts-with? (str rp "/") (str top "/")))
                      (subs rp (inc (count top))))]
        {:git-root (or top rp) :subtree subtree})
      {:git-root rp :subtree nil})))
