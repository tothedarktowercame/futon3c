#!/usr/bin/env bb
;; Read-only error-time memory recall for Lean proof runners.
;;
;; This script NEVER writes the evidence store. It logs each query locally so
;; ground control can later join row -> job and write receipts under the
;; established seat discipline.

(ns error-recall
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def default-base "http://127.0.0.1:7073")
(def query-limit 8)
(def result-limit 3)
(def term-limit 12)
(def timeout-ms 10000)
(def retry-backoff-ms 5000)
(def excerpt-limit 300)

(def error-stopwords
  #{"error" "failed" "expected" "type" "term" "unknown" "identifier"
    "invalid" "tactic" "goal" "unsolved" "line" "column"
    "requires" "require" "could" "would" "there" "where" "have"
    "with" "from" "into" "that" "this" "when" "then" "than"
    "the" "and" "for" "not" "was" "were"
    ;; file-path noise observed in first field data (sorry-0285.jsonl):
    ;; error texts carry paths; their components are zero-signal.
    "Main.lean" "Main" "lean" "problems" "prove" "need" "under"
    "current" "imports" "constant" "syntax" "projection"})

(def lean-token-pattern
  #"[A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*")

(defn path-noise?
  "File-path components tokenize as identifiers but carry zero recall
  signal AND dilute matching (first field data, sorry-0285: the
  acceptance case stopped surfacing once path terms joined the query)."
  [token]
  (or (str/ends-with? token ".lean")
      (re-matches #"(?i)a\d\d[a-z]\d\d" token)   ; problem ids
      (contains? #{"Main" "problems" "ConstructionTargets" "lib"} token)))

(defn identifier-like? [token]
  (and (not (path-noise? token))
       (or (str/includes? token "_")
           (str/includes? token ".")
           (boolean (re-find #"[a-z][A-Z]" token)))))

(defn extract-terms
  "Preserve Lean identifiers exactly; lowercase only ordinary vocabulary."
  [error-text]
  (let [tokens (re-seq lean-token-pattern (or error-text ""))
        identifiers (filter identifier-like? tokens)
        words (for [token tokens
                    :let [word (str/lower-case token)]
                    :when (and (>= (count word) 4)
                               (not (identifier-like? token))
                               (not (path-noise? token))
                               (not (error-stopwords word)))]
                word)]
    (->> (concat identifiers words)
         distinct
         (take term-limit)
         vec)))

(defn encode [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn parse-edn [body]
  (when (seq body)
    (edn/read-string {:default (fn [_tag value] value)} body)))

(defn retryable? [status body]
  (or (= 503 status)
      (= :expensive-read-busy (:error/code body))
      (= :expensive-read-busy (:error body))))

(defn query-store
  "Return text-search results, or nil on any unavailable/busy store seam."
  [base terms]
  (when (seq terms)
    (let [url (str (str/replace base #"/$" "")
                   "/api/alpha/evidence/text-search?q="
                   (encode (str/join " " terms))
                   "&limit=" query-limit)]
      (loop [attempt 1]
        (let [response
              (try
                (http/get url
                          {:headers {"accept" "application/edn"}
                           :timeout timeout-ms
                           :throw false})
                (catch Exception _ nil))
              status (:status response)
              body (try (parse-edn (:body response))
                        (catch Exception _ nil))]
          (cond
            (= 200 status) (vec (or (:results body) []))
            (and (= attempt 1) (retryable? status body))
            (do (Thread/sleep retry-backoff-ms)
                (recur 2))
            :else nil))))))

(defn memory-result? [result]
  (= :memory (get-in result [:entry :evidence/type])))

(defn lane-rank [entry]
  (let [tags (set (:evidence/tags entry))]
    (cond
      (tags :arc-lane) 0
      (tags :solve-lane) 1
      :else 2)))

(defn choose-hits [results]
  (->> results
       (filter memory-result?)
       (sort-by (fn [result]
                  [(lane-rank (:entry result))
                   (double (or (:score result) 0.0))
                   (get-in result [:entry :evidence/id])]))
       (take result-limit)
       vec))

(defn hit-line [result]
  (let [entry (:entry result)
        evidence-body (:evidence/body entry)
        inner (:body evidence-body)
        name (or (:name evidence-body)
                 (:name inner)
                 (:evidence/id entry))
        rule (or (:rule evidence-body) (:rule inner))
        before (or (:before evidence-body) (:before inner))
        after (or (:after evidence-body) (:after inner))
        guidance (or rule
                     (when (or before after)
                       (str (or before "?") " → " (or after "?")))
                     (:how-to-apply evidence-body)
                     (:hook evidence-body)
                     "[no compact rule]")]
    (str name " | " (str/replace (str guidance) #"\s+" " ")
         " | " (:evidence/id entry))))

(defn excerpt [text]
  (let [compact (str/replace (or text "") #"\s+" " ")]
    (subs compact 0 (min excerpt-limit (count compact)))))

(defn log-path [state-dir row-id]
  (io/file state-dir (str row-id ".jsonl")))

(defn append-log! [state-dir row-id error-text terms hits]
  (let [path (log-path state-dir row-id)
        record {:ts (str (Instant/now))
                :row-id row-id
                :error-excerpt (excerpt error-text)
                :terms terms
                :surfaced-memory-ids
                (mapv #(get-in % [:entry :evidence/id]) hits)}]
    (io/make-parents path)
    (spit path (str (json/generate-string record) "\n") :append true)
    record))

(defn parse-args [args]
  (loop [remaining args
         opts {}]
    (if-let [arg (first remaining)]
      (if (= "--row" arg)
        (if-let [row-id (second remaining)]
          (recur (nnext remaining) (assoc opts :row-id row-id))
          nil)
        (assoc opts :error-text (str/join " " remaining)))
      opts)))

(defn recall!
  ([args] (recall! args {}))
  ([args overrides]
   (let [{:keys [row-id error-text]} (parse-args args)]
     (if (or (str/blank? row-id) (str/blank? error-text))
       0
       (let [base (or (:base overrides)
                      (System/getenv "ERROR_RECALL_BASE")
                      default-base)
             state-dir (or (:state-dir overrides)
                           (System/getenv "ERROR_RECALL_STATE_DIR")
                           (str (or (System/getenv "FUTON3C_DIR")
                                    "/home/joe/code/futon3c")
                                "/.state/error-recall"))
             terms (extract-terms error-text)
             results (query-store base terms)
             hits (if results (choose-hits results) [])]
         ;; Logging is local and best-effort: even a local permission problem
         ;; must not turn recall into a proof-session failure.
         (try (append-log! state-dir row-id error-text terms hits)
              (catch Exception _ nil))
         (doseq [hit hits] (println (hit-line hit)))
         0)))))

(defn -main [& args]
  (try
    (recall! args)
    (catch Exception _ 0)))

(when (= *file* (System/getProperty "babashka.file"))
  (System/exit (apply -main *command-line-args*)))
