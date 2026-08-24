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

(def ^:private label-by-path
  (into {} (map (juxt :path :label)) watch-roots))

(defn label-for
  "Watcher label for ROOT-PATH, or nil when the root is not watched."
  [root-path]
  (get label-by-path (str root-path)))
