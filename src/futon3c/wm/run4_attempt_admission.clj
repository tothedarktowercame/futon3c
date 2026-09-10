(ns futon3c.wm.run4-attempt-admission
  "Durable, append-only admission for one RUN4 trial attempt.

  A reservation is persisted and directory-fsynced before click creation.
  Reservation without a result is intentionally projected as indeterminate:
  reconciliation is required and automatic redispatch is forbidden."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.apm.library-loop-runner :as durable])
  (:import [java.nio.channels FileChannel]
           [java.nio.file StandardOpenOption]))

(defonce ^:private !jvm-locks (atom {}))

(def ^:dynamic *atomic-write!*
  "Fault-injection seam; production uses temp/fsync/atomic-rename/directory-fsync."
  durable/atomic-write-edn!)

(defn- safe-id? [value]
  (and (string? value)
       (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" value))))

(defn- attempt-dir [root attempt-id]
  (io/file root attempt-id))

(defn- read-edn [file]
  (when (.isFile file)
    (with-open [reader (java.io.PushbackReader. (io/reader file))]
      (edn/read {:eof nil} reader))))

(defn- content-digest [identity]
  (digest/sha256
   (pr-str [(:series-id identity) (:trial-id identity)
            (:pin-sha256 identity) (:casting identity)])))

(defn- with-store-lock [root f]
  (let [lock-file (io/file root ".admission.lock")
        mutex (get (swap! !jvm-locks #(if (contains? % root) % (assoc % root (Object.)))) root)]
    (io/make-parents lock-file)
    (locking mutex
      (with-open [channel (FileChannel/open
                           (.toPath lock-file)
                           (into-array StandardOpenOption
                                       [StandardOpenOption/CREATE
                                        StandardOpenOption/WRITE]))
                  lock (.lock channel)]
        (when-not (.isValid lock)
          (throw (ex-info "RUN4 admission lock unavailable"
                          {:error :run4-admission-lock-unavailable :status 500})))
        (f)))))

(defn- projection [reservation result duplicate?]
  (cond-> {:schema :wm/run4-attempt-admission-status-v1
           :attempt-id (:attempt-id reservation)
           :identity (:identity reservation)
           :content-sha256 (:content-sha256 reservation)
           :duplicate? duplicate?}
    result (assoc :state :click-recorded :result result)
    (nil? result) (assoc :state :reconciliation-required
                         :reason :reservation-exists-without-durable-click-result)))

(defn reserve!
  "Durably reserve REQUEST. Returns :new? true exactly once.

  Same attempt and content returns the existing projection. Same attempt with
  different content refuses. No reservation is returned before durable write."
  [root {:keys [attempt-id identity]}]
  (when-not (and (string? root) (not (.isAbsolute (io/file attempt-id)))
                 (safe-id? attempt-id) (map? identity))
    (throw (ex-info "RUN4 attempt admission refused"
                    {:error :run4-attempt-identity-invalid :status 403})))
  (let [dir (attempt-dir root attempt-id)
        reservation-file (io/file dir "reservation.edn")
        result-file (io/file dir "click-result.edn")
        digest (content-digest identity)
        reservation {:schema :wm/run4-attempt-reservation-v1
                     :attempt-id attempt-id
                     :identity identity
                     :content-sha256 digest}]
    (with-store-lock
      root
      (fn []
        (if-let [existing (read-edn reservation-file)]
          (if (= reservation existing)
            {:ok true :new? false
             :admission (projection existing (read-edn result-file) true)}
            {:ok false :status 409 :error :run4-attempt-content-conflict
             :attempt-id attempt-id})
          (do
            (*atomic-write!* reservation-file reservation)
            {:ok true :new? true
             :admission (projection reservation nil false)}))))))

(defn record-click!
  "Append the bounded click observation for an already durable reservation."
  [root attempt-id result]
  (with-store-lock
    root
    (fn []
      (let [dir (attempt-dir root attempt-id)
            reservation (read-edn (io/file dir "reservation.edn"))
            result-file (io/file dir "click-result.edn")
            observation {:schema :wm/run4-attempt-click-result-v1
                         :attempt-id attempt-id
                         :click (select-keys result [:started :rejected :click-id])}]
        (when-not reservation
          (throw (ex-info "RUN4 reservation missing"
                          {:error :run4-attempt-reservation-missing :status 500})))
        (if-let [existing (read-edn result-file)]
          (if (= observation existing)
            (projection reservation existing true)
            (throw (ex-info "RUN4 click result conflicts"
                            {:error :run4-click-result-conflict :status 409})))
          (do (*atomic-write!* result-file observation)
              (projection reservation observation false)))))))
