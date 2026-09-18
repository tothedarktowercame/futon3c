(ns futon3c.watcher.roots
  "The authoritative watch-root table shared by the multi-watcher bootstrap
  and the inbox-zero witness producer.

  Repo identity discipline: a checkout is identified by its watcher label
  (e.g. \"futon3c-d\", \"futon5-d2\"), not its directory basename — two
  checkouts of one repo carry distinct labels. Before 2026-08-24 the witness
  producer derived repo-id from the basename, so every claim's :repo/id
  disagreed with its observations' :repo/id (\"futon3c\" vs \"futon3c-d\").
  Joins must key on :worktree/id + :path regardless; this table only makes
  the human-facing labels agree going forward.")

(def watch-roots
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
  (into watch-roots
        [{:path "/home/joe/code/apm-lean" :label "apm-lean-d"}
         {:path "/home/joe/code/futon1b"  :label "futon1b-d"}
         {:path "/home/joe/code/mathlib4" :label "mathlib4-d"}
         {:path "/home/joe/code/p4ng"     :label "p4ng-d"}
         {:path "/home/joe/code/voxterm"  :label "voxterm-d"}]))

(def ^:private label-by-path
  (into {} (map (juxt :path :label)) watch-roots))

(defn label-for
  "Watcher label for ROOT-PATH, or nil when the root is not watched."
  [root-path]
  (get label-by-path (str root-path)))
