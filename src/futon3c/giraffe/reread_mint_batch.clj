(ns futon3c.giraffe.reread-mint-batch
  "Durable batching policy for expensive Giraffe mints."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.nio.file CopyOption Files StandardCopyOption]))

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

(defn step!
  "Record LANDED-COUNT and mint only when the window has elapsed or QUEUE-IDLE?
  demands the final mint. State is durable across timer JVMs."
  [{:keys [path window-ms now-ms landed-count queue-idle? mint-fn]
    :or {path default-state-path landed-count 0}}]
  (when-not (pos-int? window-ms)
    (throw (ex-info "A positive mint batching window is required"
                    {:kind :giraffe-reread/mint-window-required})))
  (let [state (update (read-state path) :pending-landings + landed-count)
        pending (:pending-landings state)
        last-at (:last-mint-at-ms state)
        due? (and (pos? pending)
                  (or queue-idle? (nil? last-at) (>= (- now-ms last-at) window-ms)))]
    (if-not due?
      (assoc (write-state! path state) :status :deferred)
      (let [result (mint-fn)
            success? (= :minted (:status result))
            next (cond-> (assoc state :last-result result)
                   success? (assoc :pending-landings 0 :last-mint-at-ms now-ms))]
        (assoc (write-state! path next)
               :status (if success? :minted :failed))))))
