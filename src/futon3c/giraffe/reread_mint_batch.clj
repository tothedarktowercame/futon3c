(ns futon3c.giraffe.reread-mint-batch
  "Durable batching policy for expensive Giraffe mints."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.nio.channels FileChannel]
           [java.nio.file CopyOption Files StandardCopyOption StandardOpenOption]))

(def default-state-path
  (str (System/getProperty "user.home")
       "/.local/state/futon3c/giraffe-reread/mint-batch.edn"))

(defn read-state [path]
  (let [f (io/file path)]
    (if (.isFile f)
      (edn/read-string (slurp f))
      {:pending-landings 0 :last-mint-at-ms nil :last-result nil})))

(defn- write-state! [path state]
  (let [target (.toPath (io/file path))
        parent (.getParent target)
        _ (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0))
        temp (Files/createTempFile parent ".mint-batch-" ".edn"
                                   (make-array java.nio.file.attribute.FileAttribute 0))]
    (spit (.toFile temp) (pr-str state))
    (Files/move temp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                    StandardCopyOption/REPLACE_EXISTING]))
    state))

(defonce ^:private step-lock (Object.))

(defn- with-state-lock! [path f]
  ;; JVM callers must serialize before taking independent FileChannel locks:
  ;; overlapping locks in one JVM throw instead of blocking. The sibling file
  ;; remains stable when write-state! atomically replaces the state file.
  (locking step-lock
    (let [file (.getAbsoluteFile (io/file path))
          parent (.toPath (.getParentFile file))
          lock-path (.toPath (io/file (str file ".lock")))]
      (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0))
      (with-open [channel (FileChannel/open lock-path
                           (into-array StandardOpenOption
                                       [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                  _file-lock (.lock channel)]
        (f)))))

(defn step!
  "Record LANDED-COUNT and mint only when the window has elapsed or QUEUE-IDLE?
  demands the final mint. :refresh? also requests a mint after a settled turn,
  without inventing a reading landing. An exclusive sibling file lock covers
  read/decide/mint/write; a JVM lock also serializes in-process callers."
  [{:keys [path window-ms now-ms landed-count queue-idle? mint-fn refresh?]
    :or {path default-state-path landed-count 0}}]
  (when-not (pos-int? window-ms)
    (throw (ex-info "A positive mint batching window is required"
                    {:kind :giraffe-reread/mint-window-required})))
  (with-state-lock! path
    (fn []
      (let [state (update (read-state path) :pending-landings + landed-count)
            pending (:pending-landings state)
            last-at (:last-mint-at-ms state)
            due? (or refresh?
                     (and (pos? pending)
                          (or queue-idle? (nil? last-at) (>= (- now-ms last-at) window-ms))))]
        (if-not due?
          (assoc (write-state! path state) :status :deferred)
          (let [result (mint-fn)
                success? (contains? #{:minted :unchanged} (:status result))
                next (cond-> (assoc state :last-result result)
                       success? (assoc :pending-landings 0 :last-mint-at-ms now-ms))]
            (assoc (write-state! path next)
                   :status (if success? (:status result) :failed))))))))
