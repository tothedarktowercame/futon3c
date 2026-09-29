(ns xiang2000-p2c-driver
  "Hermetic subprocess driver for P2c SIGKILL tests."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.atomic-file :as atomic-file]
            [futon3c.agency.followup-queue :as followup]
            [futon3c.agency.parked-on :as park]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.promise-replay :as replay]
            [futon3c.evidence.backend :as backend])
  (:import [java.nio.file Files StandardCopyOption CopyOption]))

(defn read-store [path]
  (let [file (io/file path)]
    (if (.exists file) (edn/read-string (slurp file)) {:entries {} :order []})))

(defn atomic-spit! [path value]
  (let [target (.toAbsolutePath (.toPath (io/file path)))
        tmp (Files/createTempFile (.getParent target) "p2c-history-" ".edn"
                                  (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile tmp) (pr-str value))
      (Files/move tmp target (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                     StandardCopyOption/REPLACE_EXISTING]))
      (finally (Files/deleteIfExists tmp)))))

(deftype FileBackend [path lock]
  backend/EvidenceBackend
  (-append [_ entry]
    (locking lock
      (let [state (atom (read-store path))
            result (backend/-append (backend/->AtomBackend state) entry)]
        (when (:ok result) (atomic-spit! path @state))
        result)))
  (-get [_ id]
    (backend/-get (backend/->AtomBackend (atom (read-store path))) id))
  (-exists? [_ id]
    (backend/-exists? (backend/->AtomBackend (atom (read-store path))) id))
  (-query [_ params]
    (backend/-query (backend/->AtomBackend (atom (read-store path))) params))
  (-count [_ params]
    (backend/-count (backend/->AtomBackend (atom (read-store path))) params))
  (-forks-of [_ id]
    (backend/-forks-of (backend/->AtomBackend (atom (read-store path))) id))
  (-delete! [_ ids]
    (locking lock
      (let [state (atom (read-store path))
            result (backend/-delete! (backend/->AtomBackend state) ids)]
        (atomic-spit! path @state)
        result)))
  (-all [_]
    (backend/-all (backend/->AtomBackend (atom (read-store path))))))

(defn paths [dir]
  {:park (str dir "/parked.edn")
   :followup (str dir "/followups.edn")
   :history (str dir "/history.edn")
   :ready (str dir "/ready")
   :result (str dir "/result.edn")})

(def park-request
  {:agent "p2c" :session "p2c-session" :surface "test"
   :awaiting ["dep-p2c"] :payload {:case :crash}})

(def followup-request
  {:agent "p2c" :session "p2c-session" :type :inbox-zero
   :dedupe-key "p2c-followup" :prompt "P2c hermetic crash probe"})

(defn reset-atoms! []
  (reset! (var-get #'park/!parked) nil)
  (reset! (var-get #'followup/!state) nil))

(defn block-at-boundary! [ready]
  (spit ready "ready")
  @(promise))

(defn with-stores [dir f]
  (let [{:keys [park followup history]} (paths dir)
        evidence (FileBackend. history (Object.))]
    (with-redefs-fn {#'park/store-path (constantly park)}
      #(binding [futon3c.agency.followup-queue/*path-override* followup
                 futon3c.agency.promise-history/*backend* evidence
                 futon3c.agency.promise-history/*heads* (atom {})]
         (reset-atoms!)
         (f evidence)))))

(defn initialise! []
  (park/clear!)
  (followup/clear!)
  (when-not (history/await-writes! 10000)
    (throw (ex-info "Initial history did not drain" {}))))

(defn await-history! [message]
  (when-not (history/await-writes! 10000)
    (throw (ex-info message {}))))

(defn prepared-ready! [now-ms]
  (let [id (:id (park/park! park-request {:now-ms now-ms}))]
    (await-history! "Park-made history did not drain")
    (park/ready-push! "p2c" "p2c-session" id "P2c ready prompt" :within-turn)
    (await-history! "Ready-enqueued history did not drain")
    id))

(defn mutate! [scenario dir]
  (with-stores
    dir
    (fn [_]
      (initialise!)
      (case scenario
        "park-made"
        (binding [history/*after-outbox-persist*
                  (fn [] (block-at-boundary! (:ready (paths dir))))]
          (park/park! park-request {:now-ms 1000}))

        "park-made-after-append"
        (binding [history/*after-outbox-append*
                  (fn [_] (block-at-boundary! (:ready (paths dir))))]
          (park/park! park-request {:now-ms 1000})
          (history/await-writes! 30000))

        "park-released"
        (let [id (:id (park/park! park-request {:now-ms 1000}))]
          (when-not (history/await-writes! 10000)
            (throw (ex-info "Park-made history did not drain" {:id id})))
          (binding [history/*after-outbox-persist*
                    (fn [] (block-at-boundary! (:ready (paths dir))))]
            (park/note-completion! "dep-p2c" {:ok true}
                                   {:now-ms 2000 :resume! (fn [_])})))

        "followup-enqueued"
        (binding [history/*after-outbox-persist*
                  (fn [] (block-at-boundary! (:ready (paths dir))))]
          (followup/enqueue! followup-request))

        "ready-enqueued"
        (let [id (:id (park/park! park-request {:now-ms 1000}))]
          (await-history! "Park-made history did not drain")
          (binding [history/*after-outbox-persist*
                    (fn [] (block-at-boundary! (:ready (paths dir))))]
            (park/ready-push! "p2c" "p2c-session" id "P2c ready prompt" :within-turn)))

        "ready-leased"
        (do (prepared-ready! 1000)
            (binding [history/*after-outbox-persist*
                      (fn [] (block-at-boundary! (:ready (paths dir))))]
              (park/ready-lease-one! "p2c" "p2c-session" 2000 100)))

        "ready-acked"
        (let [id (prepared-ready! 1000)]
          (park/ready-lease-one! "p2c" "p2c-session" 2000 100)
          (await-history! "Ready-leased history did not drain")
          (binding [history/*after-outbox-persist*
                    (fn [] (block-at-boundary! (:ready (paths dir))))]
            (park/ready-ack! id)))

        "ready-requeued"
        (do (prepared-ready! 1000)
            (park/ready-lease-one! "p2c" "p2c-session" 2000 100)
            (await-history! "Ready-leased history did not drain")
            (binding [history/*after-outbox-persist*
                      (fn [] (block-at-boundary! (:ready (paths dir))))]
              (park/sweep-leased! {:now-ms 2200})))

        "control-park"
        (do (park/park! park-request {:now-ms 1000})
            (when-not (history/await-writes! 10000)
              (throw (ex-info "Control history did not drain" {})))
            (block-at-boundary! (:ready (paths dir))))

        "control-followup"
        (do (followup/enqueue! followup-request)
            (when-not (history/await-writes! 10000)
              (throw (ex-info "Control history did not drain" {})))
            (block-at-boundary! (:ready (paths dir))))

        "atomic-park-write"
        (binding [atomic-file/*before-move*
                  (fn [_] (block-at-boundary! (:ready (paths dir))))]
          (park/park! park-request {:now-ms 1000}))

        (throw (ex-info "Unknown P2c case" {:case scenario}))))))

(defn restart! [dir]
  (let [{:keys [history result]} (paths dir)]
    (try
      (with-stores
        dir
        (fn [evidence]
          (let [live {:parked (park/snapshot) :followup (followup/snapshot)}
                _ (when-not (history/await-writes! 10000)
                    (throw (ex-info "Restart outbox did not drain" {})))
                ;; Re-draining an already drained outbox must be a no-op.
                _ (history/drain-now! (var-get #'park/!parked)
                                      (fn [state]
                                        (atomic-file/write! (:park (paths dir))
                                                            (pr-str state))))
                entries (vec (backend/-all evidence))
                corrupt-files (->> (.listFiles (io/file dir))
                                   (map #(.getName ^java.io.File %))
                                   (filter #(str/includes? % ".corrupt-"))
                                   sort vec)
                report (assoc (replay/compare-state entries live)
                              :readable? true
                              :history-ids (mapv :evidence/id entries)
                              :history-types (mapv :evidence/type entries)
                              :corruption-stats (atomic-file/stats)
                              :corrupt-files corrupt-files)]
            (spit result (pr-str report)))))
      (catch Throwable error
        (spit result (pr-str {:equal? false :readable? false
                              :error-class (str (class error))
                              :message (.getMessage error)
                              :history-readable? (try (map? (read-store history))
                                                      (catch Throwable _ false))}))))))

(let [[command scenario dir] *command-line-args*]
  (case command
    "mutate" (mutate! scenario dir)
    "restart" (restart! scenario)
    (throw (ex-info "Usage: driver mutate CASE DIR | restart DIR" {:args *command-line-args*}))))
