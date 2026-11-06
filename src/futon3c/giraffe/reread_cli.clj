(ns futon3c.giraffe.reread-cli
  "Reconcile one Giraffe re-read, optionally dispatch it, and report health."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.reread-mint :as mint]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.reread-worker :as worker]
            [futon3c.giraffe.run :as giraffe-run]))

(def default-pointer
  (or (System/getenv "GIRAFFE_READINGS_ROOT")
      "/home/joe/notes/wm-next/giraffe-reads-2026-11-04/corpus"))
(def default-root
  (or (System/getenv "GIRAFFE_ROOT") "/home/joe/code/storage/giraffe"))
(def default-edges
  (or (System/getenv "GIRAFFE_EDGES")
      "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json"))

(defn- batches [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile %))
       (filter #(re-matches #"batch-.*\.jsonl" (.getName %)))
       (sort-by #(.getName %))
       (mapv #(.getAbsolutePath %))))

(defn authority-snapshot
  "Rebuild open items and checked coverage from their authorities."
  [{:keys [pointer edges] :or {pointer default-pointer edges default-edges}}]
  (let [{root :path} (generation/resolve-generation pointer)
        census (excursions/missions-and-excursions->input
                {:repos missions/default-repos :edges-file edges
                 :include-tickets true})
        open-items (remove #(#{:completed :cancelled} (:state-group %))
                           (get-in census [:input :items]))
        checked (readings/consume
                 {:checker (str (io/file root "check_edges.py"))
                  :members-file (str (io/file root "members.json"))
                  :batches (batches root) :repos missions/default-repos
                  :scope :population/missions-excursions-tickets})]
    {:open-items open-items :coverage (:coverage checked)}))

(defn health
  "Read-only queue, generation and latest-mint health."
  [{:keys [store pointer giraffe-root now-ms]
    :or {pointer default-pointer giraffe-root default-root}}]
  (let [now-ms (or now-ms (System/currentTimeMillis))
        rows (->> (queue/requests store)
                  (filter #(contains? #{:pending :claimed :failed} (:state %)))
                  (mapv #(assoc % :age_ms (max 0 (- now-ms (:requested_at_ms %))))))
        generation (generation/resolve-generation pointer)
        latest (giraffe-run/newest-field-run giraffe-root)
        latest-file (when latest (io/file giraffe-root (:run-id latest)))]
    {:status (if (or (seq rows) (nil? latest)) :attention :ok)
     :requests rows
     :generation (select-keys generation [:path :digest])
     :last-mint (when latest
                  {:run-id (:run-id latest)
                   :at-ms (.lastModified latest-file)})}))

(defn run-once!
  "One reconciliation/worker/mint cycle. READER-SEAT is mandatory."
  [{:keys [reader-seat store pointer generations-dir giraffe-root edges
           snapshot-fn contact-fn mint-fn mint-coordinator now-ms]
    :or {pointer default-pointer giraffe-root default-root edges default-edges}}]
  (when (str/blank? reader-seat)
    (throw (ex-info "A named reader seat is required"
                    {:kind :giraffe-reread/reader-seat-required})))
  (let [now-ms (or now-ms (System/currentTimeMillis))
        store (or store (queue/open-store))
        snapshot ((or snapshot-fn authority-snapshot) {:pointer pointer :edges edges})
        reconciliation (queue/reconcile! store (:open-items snapshot)
                                         (:coverage snapshot) now-ms)
        contact (or contact-fn
                    (partial worker/agency-contact {:reader-seat reader-seat}))
        result (worker/run-once!
                {:store store :pointer pointer :generations-dir generations-dir
                 :contact-fn contact :now-ms now-ms})
        mint-result (when (= :landed (:status result))
                      (let [coordinator (or mint-coordinator
                                            (when mint-fn
                                              (mint/coordinator mint-fn))
                                            (if (and (= pointer default-pointer)
                                                     (= giraffe-root default-root)
                                                     (= edges default-edges))
                                              (mint/shared-coordinator)
                                              (mint/coordinator
                                               #(mint/supported-mint!
                                                 {:readings-root pointer
                                                  :giraffe-root giraffe-root
                                                  :edges edges}))))]
                        (mint/request! coordinator)
                        (mint/await-idle! coordinator (* 30 60 1000))))]
    {:reconciliation reconciliation :worker result :mint mint-result}))

(defn -main [& args]
  (let [health? (contains? (set args) "--health")
        reader-seat (System/getenv "FUTON3C_GIRAFFE_REREAD_READER")
        store (queue/open-store)]
    (println (pr-str (if health?
                       (health {:store store})
                       (run-once! {:store store :reader-seat reader-seat}))))))
