(ns futon3c.wm.ordinary-click-budget
  "Issue-time accounting for Joe's five ordinary clicks. Specialized RUN4/R10
   requests retain their own authority; only the plain HTTP branch calls this."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file StandardOpenOption]))

(def authorization
  ;; Sixth grant, Joe 2026-09-23, heard directly by claude-5 in the operator
  ;; buffer: "I award five more clicks, please keep following the plan step by
  ;; step with dispatches to zai".
  ;; The five earlier grants (AUTH-ordinary-click-budget-2026-09-19.md @
  ;; d18e4f9c, -renewal-2026-09-19.md @ 52f75d1d, -renewal-2-2026-09-20.md @
  ;; fa49ed93, -renewal-3-2026-09-21.md @ 1a51bc2e, -renewal-4-2026-09-21.md @
  ;; 00e7f9d6) each allocated five and were fully consumed. Consumption counts
  ;; ledger entries whose :authorization equals THIS map, so the earlier
  ;; twenty-five are neither double-counted nor erased -- they remain in the
  ;; ledger citing the authority in force when they were spent.
  {:path "futon2/holes/labs/wm-contract/AUTH-ordinary-click-budget-renewal-5-2026-09-23.md"
   :sha "c24c903b"})
(def allocated 5)
(def ^:dynamic *ledger-path*
  "/home/joe/code/futon2/data/wm-ordinary-clicks/consumption.jsonl")
(defonce ^:private issue-lock (Object.))

(defn consume!
  "Append and force consumption before the worker starts. Serialize the count
   and append across threads and processes; failed runs never refund a grant."
  [click-id issued-at caller]
  (locking issue-lock
    (let [file (io/file *ledger-path*)]
      (io/make-parents file)
      (with-open [channel (FileChannel/open
                          (.toPath file)
                          (into-array StandardOpenOption
                                      [StandardOpenOption/CREATE StandardOpenOption/READ
                                       StandardOpenOption/WRITE]))
                  _file-lock (.lock channel)]
        (let [entries (mapv #(json/parse-string % true)
                            (remove str/blank? (str/split-lines (slurp file))))
              consumed (count (filter #(= authorization (:authorization %)) entries))]
          (when (>= consumed allocated)
            (throw (ex-info
                    (str "Ordinary click budget exhausted. Return to Joe for renewal. Authority: "
                         (:path authorization) " @ " (:sha authorization))
                    {:status 409 :error :ordinary-click-budget-exhausted
                     :authorization authorization :allocated allocated
                     :consumed consumed :renewal "Joe"})))
          (let [entry {:click-id click-id :issued-at issued-at
                       :authorization authorization
                       :caller (or caller :caller-unknown)}
                buffer (ByteBuffer/wrap
                        (.getBytes (str (json/generate-string entry) "\n") "UTF-8"))]
            (.position channel (.size channel))
            (while (.hasRemaining buffer) (.write channel buffer))
            (.force channel true)
            ;; Force the ledger directory and its entry in the existing data
            ;; directory, including the first issue that creates this store.
            (doseq [dir [(.getParentFile file) (.getParentFile (.getParentFile file))]]
              (with-open [directory (FileChannel/open
                                     (.toPath dir)
                                     (into-array StandardOpenOption [StandardOpenOption/READ]))]
                (.force directory true)))
            entry))))))
