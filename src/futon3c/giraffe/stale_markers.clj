(ns futon3c.giraffe.stale-markers
  "Advisory markers read from an item's pinned source.  These markers never
  change status, readiness, or field membership."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.security MessageDigest)
           (java.time LocalDate)))

(defn- sha256 [f]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream f)]
      (let [buf (byte-array 8192)]
        (loop []
          (let [n (.read in buf)]
            (when (pos? n)
              (.update digest buf 0 n)
              (recur))))))
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn- pinned-lines [{:keys [path] expected-sha :sha256}]
  (when (and (string? path) (string? expected-sha))
    (let [f (io/file path)]
      (when (and (.isFile f) (= expected-sha (sha256 f)))
        (str/split-lines (slurp f))))))

(defn- indexed-line [lines pred]
  (first (keep-indexed (fn [i line]
                         (when (pred line) {:line (inc i) :quote line}))
                       lines)))

(defn- body-closeout [lines]
  ;; Prefer the explicit transition phrase when a file also has a close-out
  ;; heading.  Header status is excluded by accepting CLOSED/DONE only after
  ;; the first Status line.
  (let [status-lines (keep-indexed
                      (fn [i line]
                        (when (re-find #"(?i)^\s*\*\*Status:?\*\*" line) i))
                      lines)
        header-status (first status-lines)]
    (some-> (or (indexed-line lines #(re-find #"(?i)READY-TO-CLOSE" %))
                (indexed-line lines
                              (fn [line]
                                (let [i (.indexOf ^java.util.List lines line)]
                                  (and (or (nil? header-status) (> i header-status))
                                       (re-find #"(?i)^\s*\*\*Status:?\*\*.*\b(CLOSED|DONE)\b"
                                                line)))))
                (indexed-line lines #(re-find #"(?i)^##+\s+.*Close-out\b" %)))
            (assoc :kind :body-closeout))))

(defn- field-date [lines]
  (some #(some-> (re-find #"(?i)^\s*\*\*Date:?\*\*\s*(\d{4}-\d{2}-\d{2})" %)
                 second LocalDate/parse)
        lines))

(defn- timebox-weeks [lines]
  (some #(some-> (re-find #"(?i)^\s*\*\*Timebox:?\*\*\s*(\d+)\s+weeks?\b" %)
                 second Long/parseLong)
        lines))

(defn markers
  "Return advisory stale markers for ITEM at AS-OF.  An unreadable or
  digest-mismatched source is unmarked rather than read from mutable bytes."
  [as-of item]
  (if-let [lines (pinned-lines (:source-pin item))]
    (let [closeout (body-closeout lines)
          date (field-date lines)
          weeks (timebox-weeks lines)
          expired-on (when (and date weeks) (.plusWeeks date weeks))
          expired? (and expired-on (.isBefore expired-on (LocalDate/parse as-of)))]
      (cond-> []
        closeout (conj closeout)
        expired? (conj {:kind :timebox-expired
                        :date (str date)
                        :weeks weeks
                        :expired-on (str expired-on)})))
    []))

(defn annotate-input [input]
  (update input :items
          (fn [items]
            (mapv #(assoc % :stale-markers (markers (:as-of input) %)) items))))
