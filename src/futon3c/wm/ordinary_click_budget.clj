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
  ;; Eighth grant, Joe 2026-09-24, heard directly by claude-5 in the operator
  ;; buffer: "I award 5 more clicks".
  ;; The seventh (AUTH-ordinary-click-budget-renewal-6-2026-09-23.md @
  ;; 2996eb6b) was fully consumed, 7 of 7, so nothing carries forward and this
  ;; allocates five. The seven earlier grants allocated five each bar that one;
  ;; consumption counts ledger entries whose :authorization equals THIS map, so
  ;; the thirty-five already spent are neither double-counted nor erased --
  ;; each remains in the ledger citing the authority in force when it was
  ;; spent.
  ;;
  ;; The grant does not restart the machine. The grounding misreport that ran
  ;; from 2026-09-23 17:44 (entity :props read back as a string, so :resolved?
  ;; was false about a commit the entity does name) is repaired and loaded.
  ;; What holds the clicks now is Joe's stop-line rule, settled 2026-09-24:
  ;; "if there is a stop-line, in my vocabulary that means the system should be
  ;; repaired from outside." An open stop-line stops the line -- it is not
  ;; per-defect -- so no click issues while the board is dirty. 35 open
  ;; :machine-failure findings stand. The authorization document carries the
  ;; board and the repair route.
  {:path "futon2/holes/labs/wm-contract/AUTH-ordinary-click-budget-renewal-7-2026-09-24.md"
   :sha "b31ecb66"})
(def allocated
  "Five newly awarded; renewal-6 was fully consumed so nothing carries forward."
  5)
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
