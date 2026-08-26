#!/usr/bin/env bb
;; apm-condition.bb — register an operational condition for a running APM
;; campaign, by appending to <campaign-root>/conditions.edn.
;;
;; The pinned git revision on a frame's manifest records the CODE. It does not
;; record what was done to the running instrument: which namespaces were
;; reloaded into the shared JVM and when (HEAD can be ahead of what runs, or
;; behind it), arm files, futon3 library edits, substrate reassignments,
;; prereg amendments. This file is the changelog for those. Every entry in
;; force (no :until) is copied into the next minted frame's manifest under
;; :conditions (futon3c.apm.countdown-control/campaign-conditions, read per
;; tick), so a frame says which conditions held when it was minted.
;;
;; Usage:
;;   scripts/apm-condition.bb --by <agent-id> --kind <kind> --note "<text>" \
;;       [--amendment N] [--expect "<prediction>"] [--field "[:receipt/x :y]"] \
;;       [--since-frame fNN] [--namespaces ns1,ns2] [--loaded true|false] \
;;       [--sha <sha>] [--campaign-root <dir>]
;;   scripts/apm-condition.bb --by <agent-id> --withdraw C-3 [--note "<why>"]
;;   scripts/apm-condition.bb --list
;;
;; Kinds (free keywords; these are the ones in use): reload, code, arm,
;; library, substrate, queue, prereg, contract.
;; :at and :head (futon3c HEAD at registration) are stamped automatically.
;; Registering is not loading: a :kind :code entry with :loaded? false says a
;; commit is on master but not in the JVM. scripts/apm-reload.sh reloads AND
;; registers a :kind :reload entry — use it rather than a bare proof-eval
;; reload while a campaign is running.
(require '[clojure.edn :as edn]
         '[clojure.pprint :as pp]
         '[clojure.string :as str]
         '[babashka.process :refer [shell]])

(def default-root
  "/home/joe/code/futon3c/data/apm-campaigns/jit-all-open-nontopology-v1")
(def repo "/home/joe/code/futon3c")

(defn parse-args [args]
  (loop [args args opts {}]
    (if (empty? args)
      opts
      (let [[k v & more] args]
        (cond
          (= k "--list") (recur (rest args) (assoc opts :list true))
          (str/starts-with? k "--") (recur more (assoc opts (keyword (subs k 2)) v))
          :else (throw (ex-info (str "unexpected argument " k) {})))))))

(defn now-utc []
  (.format (java.time.ZonedDateTime/now java.time.ZoneOffset/UTC)
           (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm'Z'")))

(defn head-sha []
  (str/trim (:out (shell {:out :string} "git" "-C" repo "rev-parse" "--short" "HEAD"))))

(defn read-registry [path]
  (if (.isFile (java.io.File. path))
    (let [value (edn/read-string (slurp path))]
      (when-not (and (vector? value) (every? map? value))
        (throw (ex-info "conditions.edn is not a vector of maps; refusing to append" {:path path})))
      value)
    []))

(defn write-registry! [path entries]
  (let [text (with-out-str
               (println "[")
               (doseq [e entries] (pp/pprint e))
               (println "]"))
        check (edn/read-string text)]
    (when-not (= check entries)
      (throw (ex-info "round-trip mismatch; not writing" {})))
    (spit path text)))

(defn next-id [entries]
  (str "C-" (inc (count entries))))

(let [{:keys [by kind note amendment expect field since-frame namespaces loaded
              sha campaign-root withdraw list] :as opts} (parse-args *command-line-args*)
      root (or campaign-root default-root)
      path (str root "/conditions.edn")
      entries (read-registry path)]
  (cond
    list
    (doseq [e entries]
      (println (format "%-5s %s %-14s %-6s %s%s" (:id e) (:at e) (:by e) (name (:kind e))
                       (:note e) (if (:until e) (str "  [withdrawn " (:until e) "]") ""))))

    withdraw
    (do (when-not by (throw (ex-info "--by is required" {})))
        (let [idx (some (fn [[i e]] (when (= withdraw (:id e)) i)) (map-indexed vector entries))]
          (when-not idx (throw (ex-info (str "no entry " withdraw) {})))
          (when (:until (nth entries idx))
            (throw (ex-info (str withdraw " already withdrawn") {})))
          (let [updated (update entries idx assoc :until (now-utc) :withdrawn-by by
                                :withdrawal-note (or note ""))]
            (write-registry! path updated)
            (pp/pprint (nth updated idx)))))

    :else
    (do (doseq [[k v] {:by by :kind kind :note note}]
          (when (str/blank? v) (throw (ex-info (str "--" (name k) " is required") {}))))
        (let [entry (cond-> {:id (next-id entries) :at (now-utc) :by by
                             :kind (keyword kind) :note note :head (head-sha)}
                      amendment (assoc :amendment (Long/parseLong amendment))
                      expect (assoc :expect expect)
                      field (assoc :field (edn/read-string field))
                      since-frame (assoc :since-frame since-frame)
                      namespaces (assoc :namespaces (vec (str/split namespaces #",")))
                      loaded (assoc :loaded? (= "true" loaded))
                      sha (assoc :sha sha))]
          (write-registry! path (conj entries entry))
          (pp/pprint entry)))))
