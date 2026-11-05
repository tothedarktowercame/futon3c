(ns futon3c.file-change.click-probe
  "Read-only adapter from existing WM loop click logs to Giraffe probes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private click-file #"click-\d+-(\d+)\.log")

(defn parse-log
  "Return one probe map for a loop click log, or nil for a repeat/no probe."
  [file]
  (when-let [[_ epoch] (re-matches click-file (.getName (io/file file)))]
    (let [text (slurp file)
          at-ms (* 1000 (Long/parseLong epoch))
          failed? (str/includes? text "freshness probe failed")
          fresh (re-find #"giraffe field\s+fresh \(([^)]+)\)" text)
          stale (or (re-find #"giraffe field\s+stale \(newest ([^)]+)\)" text)
                    (re-find #"stale field ([^ ]+) not refreshed" text))
          minted (re-find #"giraffe field\s+minted ([^ ]+)" text)]
      (cond
        failed? {:at-ms at-ms :fresh? nil :minted? false
                 :source-file (.getPath (io/file file))}
        fresh {:at-ms at-ms :fresh? true :minted? false :run-id (second fresh)
               :source-file (.getPath (io/file file))}
        stale {:at-ms at-ms :fresh? false :minted? (boolean minted)
               :run-id (if minted (second minted) (second stale))
               :source-file (.getPath (io/file file))}
        :else nil))))

(defn read-loop-probes [root]
  (->> (.listFiles (io/file root))
       (keep #(when (.isFile ^java.io.File %) (parse-log %)))
       (sort-by :at-ms)
       vec))
