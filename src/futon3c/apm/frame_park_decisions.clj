(ns futon3c.apm.frame-park-decisions
  "Receipt-keyed, append-only reconciliation of Claude park decisions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.problem-queue-supervisor :as queue])
  (:import (java.nio.file Files StandardCopyOption)))

(defn read-edn [path]
  (edn/read-string (slurp path)))

(defn atomic-write! [path value]
  (let [target (.toPath (io/file path))
        parent (.getParent target)
        temporary (Files/createTempFile parent ".park-decisions-" ".edn"
                                        (make-array java.nio.file.attribute.FileAttribute 0))]
    (spit (.toFile temporary) (str (pr-str value) "\n"))
    (Files/move temporary target
                (into-array StandardCopyOption
                            [StandardCopyOption/ATOMIC_MOVE
                             StandardCopyOption/REPLACE_EXISTING]))))

(defn reconcile-files! [queue-path decisions-path]
  (let [state (read-edn queue-path)
        records (:decisions (read-edn decisions-path))
        result (queue/reconcile-park-decisions state records)]
    (when (and (:ok result) (:changed? result))
      (atomic-write! queue-path (:state result)))
    result))

(defn -main [& [queue-path decisions-path]]
  (let [result (reconcile-files! queue-path decisions-path)]
    (println (pr-str (dissoc result :state)))
    (when-not (:ok result)
      (System/exit 1))))
