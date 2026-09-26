(ns futon3c.wm.run4-attempt-admission
  "Durable, append-only admission for one RUN4 trial attempt.

  Reservation without a valid click result is indeterminate and requires
  reconciliation. Existing malformed/orphan state is never treated as fresh."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.apm.library-loop-runner :as durable])
  (:import [java.nio.channels FileChannel]
           [java.nio.file Files Path StandardOpenOption]))

(defonce ^:private !jvm-locks (atom {}))

(def ^:dynamic *atomic-write!* durable/atomic-write-edn!)
(def ^:dynamic *create-directory!* #(Files/createDirectory % (make-array java.nio.file.attribute.FileAttribute 0)))
(def ^:dynamic *fsync-directory!*
  (fn [^Path path]
    (with-open [channel (FileChannel/open path (into-array StandardOpenOption
                                                            [StandardOpenOption/READ]))]
      (.force channel true))))

(defn- safe-id? [value]
  (and (string? value)
       (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" value))))

(defn- valid-casting? [casting]
  (and (map? casting)
       (every? #(and (string? %) (not (str/blank? %)))
               ((juxt :author :reviewer :repair-reviewer) casting))
       (not= (:author casting) (:reviewer casting))))

(defn- valid-identity? [identity]
  (and (map? identity)
       (or (string? (:series-id identity)) (keyword? (:series-id identity)))
       (or (string? (:trial-id identity)) (keyword? (:trial-id identity)))
       (string? (:pin-sha256 identity))
       (boolean (re-matches #"[0-9a-f]{64}" (:pin-sha256 identity)))
       (valid-casting? (:casting identity))))

(defn- content-digest [identity]
  (digest/sha256 (pr-str [(:series-id identity) (:trial-id identity)
                          (:pin-sha256 identity) (:casting identity)])))

(defn- parse-one [file]
  (try
    (with-open [reader (java.io.PushbackReader. (io/reader file))]
      (let [value (edn/read {:eof ::empty} reader)]
        (when (= ::empty value)
          (throw (ex-info "empty artifact" {})))
        (when-not (= ::end (edn/read {:eof ::end} reader))
          (throw (ex-info "trailing artifact form" {})))
        value))
    (catch Throwable cause
      (throw (ex-info "RUN4 admission artifact corrupt"
                      {:error :run4-attempt-state-corrupt :status 409
                       :path (.getAbsolutePath file)} cause)))))

(defn- reservation-valid? [value]
  (and (map? value)
       (= :wm/run4-attempt-reservation-v1 (:schema value))
       (safe-id? (:attempt-id value))
       (valid-identity? (:identity value))
       (= (content-digest (:identity value)) (:content-sha256 value))))

(defn- click-shape? [click]
  (and (map? click)
       (or (and (string? (:click-id click))
                (not (str/blank? (:click-id click)))
                (string? (:started-at click))
                (try (java.time.Instant/parse (:started-at click)) true
                     (catch Throwable _ false))
                (not (contains? click :rejected)))
           (and (= :already-running (:rejected click))
                (string? (:click-id click))
                (not (str/blank? (:click-id click)))
                (not (contains? click :started-at))))))

(defn- result-valid? [value attempt-id]
  (and (map? value)
       (= :wm/run4-attempt-click-result-v1 (:schema value))
       (= attempt-id (:attempt-id value))
       (click-shape? (:click value))))

(defn- read-artifact [file valid?]
  (cond
    (not (.exists file)) nil
    (not (.isFile file))
    (throw (ex-info "RUN4 admission artifact is not a file"
                    {:error :run4-attempt-state-corrupt :status 409
                     :path (.getAbsolutePath file)}))
    :else
    (let [value (parse-one file)]
      (when-not (valid? value)
        (throw (ex-info "RUN4 admission artifact schema invalid"
                        {:error :run4-attempt-state-corrupt :status 409
                         :path (.getAbsolutePath file)})))
      value)))

(defn- canonical-root [root]
  (when-not (and (string? root) (not (str/blank? root)))
    (throw (ex-info "RUN4 admission root invalid"
                    {:error :run4-admission-root-invalid :status 500})))
  (let [file (.getCanonicalFile (io/file root))]
    (when-not (.isDirectory file)
      (throw (ex-info "RUN4 admission root unavailable"
                      {:error :run4-admission-root-invalid :status 500})))
    file))

(defn- with-store-lock [root f]
  (let [root-file (canonical-root root)
        root-key (.getPath root-file)
        lock-file (io/file root-file ".admission.lock")
        mutex (get (swap! !jvm-locks #(if (contains? % root-key) %
                                         (assoc % root-key (Object.)))) root-key)]
    (locking mutex
      (with-open [channel (FileChannel/open (.toPath lock-file)
                                            (into-array StandardOpenOption
                                                        [StandardOpenOption/CREATE
                                                         StandardOpenOption/WRITE]))
                  lock (.lock channel)]
        (when-not (.isValid lock)
          (throw (ex-info "RUN4 admission lock unavailable"
                          {:error :run4-admission-lock-unavailable :status 500})))
        (f root-file)))))

(defn- projection [reservation result duplicate?]
  (cond-> {:schema :wm/run4-attempt-admission-status-v1
           :attempt-id (:attempt-id reservation)
           :identity (:identity reservation)
           :content-sha256 (:content-sha256 reservation)
           :duplicate? duplicate?}
    result (assoc :state (if (= :already-running (get-in result [:click :rejected]))
                           :busy-rejected
                           :click-recorded)
                  :result result)
    (nil? result) (assoc :state :reconciliation-required
                         :reason :reservation-exists-without-durable-click-result)))

(defn reserve! [root {:keys [attempt-id identity]}]
  (when-not (and (safe-id? attempt-id) (valid-identity? identity))
    (throw (ex-info "RUN4 attempt admission refused"
                    {:error :run4-attempt-identity-invalid :status 403})))
  (with-store-lock
    root
    (fn [root-file]
      (let [dir (io/file root-file attempt-id)
            reservation-file (io/file dir "reservation.edn")
            result-file (io/file dir "click-result.edn")
            reservation {:schema :wm/run4-attempt-reservation-v1
                         :attempt-id attempt-id :identity identity
                         :content-sha256 (content-digest identity)}]
        (if (.exists dir)
          (let [existing (read-artifact reservation-file reservation-valid?)
                result (when existing
                         (read-artifact result-file #(result-valid? % attempt-id)))]
            (when-not existing
              (throw (ex-info "RUN4 orphan attempt directory"
                              {:error :run4-attempt-state-corrupt :status 409
                               :path (.getAbsolutePath dir)})))
            (if (= reservation existing)
              {:ok true :new? false
               :admission (projection existing result true)}
              {:ok false :status 409 :error :run4-attempt-content-conflict
               :attempt-id attempt-id}))
          (do
            (*create-directory!* (.toPath dir))
            (*fsync-directory!* (.toPath root-file))
            (*atomic-write!* reservation-file reservation)
            {:ok true :new? true
             :admission (projection reservation nil false)}))))))

(defn record-click! [root attempt-id result]
  (when-not (safe-id? attempt-id)
    (throw (ex-info "RUN4 attempt id invalid"
                    {:error :run4-attempt-identity-invalid :status 500})))
  (let [click (select-keys result [:click-id :started-at :rejected])]
    (when-not (click-shape? click)
      (throw (ex-info "RUN4 click result malformed"
                      {:error :run4-click-result-invalid :status 500})))
    (with-store-lock
      root
      (fn [root-file]
        (let [dir (io/file root-file attempt-id)
              reservation (read-artifact (io/file dir "reservation.edn")
                                         reservation-valid?)
              result-file (io/file dir "click-result.edn")
              observation {:schema :wm/run4-attempt-click-result-v1
                           :attempt-id attempt-id :click click}]
          (when-not reservation
            (throw (ex-info "RUN4 reservation missing"
                            {:error :run4-attempt-reservation-missing :status 500})))
          (if-let [existing (read-artifact result-file
                                          #(result-valid? % attempt-id))]
            (if (= observation existing)
              (projection reservation existing true)
              (throw (ex-info "RUN4 click result conflicts"
                              {:error :run4-click-result-conflict :status 409})))
            (do (*atomic-write!* result-file observation)
                (projection reservation observation false))))))))
