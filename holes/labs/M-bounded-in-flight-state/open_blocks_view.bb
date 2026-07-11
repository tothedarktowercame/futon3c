#!/usr/bin/env bb
;; open_blocks_view.bb — the War Machine "open-blocks" view slice (B5-F4 flight).
;;
;; Parses real Block: footers from futon3c git history using the EXACT same
;; regex as the shipped parser (commit_ingest.clj block-footer-pattern), lists
;; blocks with their kind/date/slug, commit sha, author, and disposition
;; (open/closed status inferred from commit history).
;;
;; Usage:  bb open_blocks_view.bb [--repo <path>] [--limit <N>]
;; Default repo: the futon3c repo (auto-detected relative to this script).
;; Runs from any cwd (file-relative path resolution).

(ns open-blocks-view
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :as pp]))

;; ---------------------------------------------------------------------------
;; The block-footer-pattern — EXACT copy of commit_ingest.clj's regex
;; (futon3c/src/futon3c/watcher/commit_ingest.clj L90-93).
;; Matches: Block: <kind>-<YYYY-MM-DD>-<slug>
;; ---------------------------------------------------------------------------
(def block-footer-pattern
  #"(?m)^\s*Block:\s+([a-z][a-z0-9-]*?)-(\d{4}-\d{2}-\d{2})-([A-Za-z0-9._-]+)\s*$")

(defn parse-block-trailer
  "Extract a Block: trailer map from a commit body, or nil.
   Exact semantic match to commit_ingest.clj/parse-block-trailer (L101-108)."
  [body]
  (when (string? body)
    (when-let [m (re-find block-footer-pattern body)]
      {:tag  (str (nth m 1) "-" (nth m 2) "-" (nth m 3))
       :kind (nth m 1)
       :ymd  (nth m 2)
       :slug (nth m 3)})))

(defn find-repo
  "Auto-detect the futon3c repo root relative to this script's location."
  []
  (let [script-path *file*
        script-dir (.getCanonicalPath (java.io.File. script-path))
        ;; This script is in holes/labs/M-bounded-in-flight-state/ under futon3c root.
        ;; Walk up until we find .git
        start-dir (if (.exists (java.io.File. script-dir))
                    script-dir
                    (.getCanonicalPath (java.io.File. ".")))]
    (loop [dir (java.io.File. start-dir)]
      (cond
        (nil? dir) "."
        (.exists (java.io.File. (str (.getCanonicalPath dir) "/.git")))
        (.getCanonicalPath dir)
        :else (recur (.getParentFile dir))))))

(defn list-block-commits
  "Walk git log and extract commits with Block: footers.
   Returns a list of maps: {:sha :author :date :subject :block :body}
   where :block is the parsed trailer map or nil."
  [repo limit]
  (let [field-sep "\u001F"
        record-sep "\u001E"
        git-args (cond-> ["log" "--reverse" "--no-merges"
                          (str "--format=%H" field-sep "%an" field-sep
                               "%ad" field-sep "%s" field-sep "%b" record-sep)
                          "--date=short"]
                   limit (conj (str "-" limit)))
        {:keys [exit out err]} (apply sh "git" "-C" repo git-args)]
    (if (zero? exit)
      (->> (str/split out (re-pattern record-sep))
           (map str/trim)
           (remove str/blank?)
           (keep (fn [chunk]
                   (let [[sha author date subject body]
                         (str/split chunk (re-pattern field-sep) 5)
                         body (when body (str/trim body))
                         block (parse-block-trailer body)]
                     (when block
                       {:sha sha :author author :date date
                        :subject (str/trim subject)
                        :block block})))))
      (do
        (binding [*out* *err*]
          (println "git error:" err))
        []))))

(defn -main
  [& args]
  (let [args (or args [])
        repo (or (some #(when (and (string? %) (str/starts-with? % "--repo"))
                          (nth (str/split % #"=" 2) 1 nil))
                        args)
                 (find-repo))
        limit (or (some #(when (and (string? %) (str/starts-with? % "--limit"))
                           (parse-long (nth (str/split % #"=" 2) 1 nil)))
                         args)
                  nil)]
    (println "=== open_blocks_view — War Machine open-blocks view ===")
    (println (str "repo: " repo))
    (println (str "limit: " (or limit "all")))
    (println)

    (let [blocks (list-block-commits repo limit)]
      (if (empty? blocks)
        (println "No Block: footers found in git history.")
        (do
          (println (str "Found " (count blocks) " Block: footers:"))
          (println)
          (println (format "%-12s %-10s %-40s %-12s %-20s %s"
                               "DATE" "KIND" "SLUG" "SHA(8)" "AUTHOR" "SUBJECT"))
          (println (apply str (repeat 120 "-")))
          (doseq [{:keys [sha author date subject block]} blocks]
            (let [{:keys [kind ymd slug]} block]
              (println (format "%-12s %-10s %-40s %-12s %-20s %s"
                                   date kind
                                   (if (> (count slug) 40) (str (subs slug 0 37) "...") slug)
                                   (subs sha 0 (min 8 (count sha)))
                                   (if (> (count author) 20) (str (subs author 0 17) "...") author)
                                   (if (> (count subject) 40) (str (subs subject 0 37) "...") subject)))))

          ;; Summary by kind
          (println)
          (println "--- Summary by kind ---")
          (let [by-kind (group-by #(get-in % [:block :kind]) blocks)]
            (doseq [[kind items] (sort by-kind)]
              (println (str "  " kind ": " (count items) " block(s)"))))

          ;; All blocks are CLOSED (they have commit footers = closure phase).
          ;; This is the pipeline blind spot from Q-21: only closed Blocks are visible.
          (println)
          (println "--- Disposition ---")
          (println (str "  All " (count blocks) " blocks are CLOSED (visible via Block: footers on commits)."))
          (println "  In-flight blocks (identified but uncommitted) are NOT visible at this surface.")
          (println "  (Per Q-21: requires a separate identification step — disposition-to-Block bridge.)"))))))

(-main *command-line-args*)
