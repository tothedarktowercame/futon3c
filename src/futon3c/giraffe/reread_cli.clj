(ns futon3c.giraffe.reread-cli
  "Reconcile one Giraffe re-read, optionally dispatch it, and report health."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.reread-mint :as mint]
            [futon3c.giraffe.reread-mint-batch :as mint-batch]
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
    {:all-items (get-in census [:input :items])
     :open-items open-items :coverage (:coverage checked)}))

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

(defn drain-once!
  "One reader round and durable mint step; no authority census.
   :parallel? false keeps reader calls on the invoking service executor."
  [{:keys [reader-seat reader-seats kimi-task-repo store pointer generations-dir
           giraffe-root edges contact-fn contact-fns mint-fn now-ms
           new-item-cap mint-window-ms mint-state-path parallel?]
    :or {pointer default-pointer giraffe-root default-root edges default-edges parallel? true}}]
  (let [reader-seats (vec (or (seq reader-seats)
                              (when-not (str/blank? reader-seat) [reader-seat])))]
    (when (empty? reader-seats)
      (throw (ex-info "At least one named reader seat is required"
                      {:kind :giraffe-reread/reader-seat-required})))
    (when-not (and (integer? new-item-cap) (not (neg? new-item-cap)))
      (throw (ex-info "FUTON3C_GIRAFFE_REREAD_NEW_ITEM_CAP is required"
                      {:kind :giraffe-reread/new-item-cap-required})))
    (when-not (pos-int? mint-window-ms)
      (throw (ex-info "FUTON3C_GIRAFFE_REREAD_MINT_WINDOW_MS is required"
                      {:kind :giraffe-reread/mint-window-required})))
    (let [store (or store (queue/open-store))
          contact-for (fn [seat]
                        (or (get contact-fns seat)
                            contact-fn
                            (if (str/starts-with? seat "kimi-")
                              (partial worker/kimi-contact
                                       {:reader-seat seat :kimi-task-repo kimi-task-repo})
                              (partial worker/agency-contact {:reader-seat seat}))))
          run-seat (fn [seat]
                     (worker/run-once!
                      {:store store :pointer pointer :generations-dir generations-dir
                       :contact-fn (contact-for seat) :now-ms (or now-ms (System/currentTimeMillis))
                       :new-item-cap new-item-cap :reader-id seat}))
          results (if parallel?
                    (mapv deref (mapv #(future (run-seat %)) reader-seats))
                    (mapv run-seat reader-seats))
          landed (count (filter #(= :landed (:status %)) results))
          actionable (filter #(contains? #{:pending :claimed} (:state %))
                             (queue/requests store))
          mint-result (mint-batch/step!
                       {:path (or mint-state-path mint-batch/default-state-path)
                        :window-ms mint-window-ms :now-ms (or now-ms (System/currentTimeMillis))
                        :landed-count landed :queue-idle? (empty? actionable)
                        :mint-fn (or mint-fn
                                     #(mint/supported-mint!
                                       {:readings-root pointer
                                        :giraffe-root giraffe-root :edges edges}))})]
      {:workers results :mint mint-result})))

(defn run-once!
  "One full authority reconciliation followed by a reader/mint round."
  [{:keys [store snapshot-fn now-ms] :as opts}]
  ;; Validate configuration before touching an authority or opening the queue.
  (when-not (seq (or (:reader-seats opts)
                     (when-not (str/blank? (:reader-seat opts)) [(:reader-seat opts)])))
    (throw (ex-info "At least one named reader seat is required"
                    {:kind :giraffe-reread/reader-seat-required})))
  (when-not (and (integer? (:new-item-cap opts)) (not (neg? (:new-item-cap opts))))
    (throw (ex-info "New-item cap required" {:kind :giraffe-reread/new-item-cap-required})))
  (when-not (pos-int? (:mint-window-ms opts))
    (throw (ex-info "Mint window required" {:kind :giraffe-reread/mint-window-required})))
  (let [store (or store (queue/open-store))
        snapshot ((or snapshot-fn authority-snapshot) opts)
        reconciliation (queue/reconcile! store (:open-items snapshot) (:coverage snapshot)
                                          (or now-ms (System/currentTimeMillis)))]
    (assoc (drain-once! (assoc opts :store store)) :reconciliation reconciliation)))

(defn -main [& args]
  (let [health? (contains? (set args) "--health")
        reader-seats (some-> (System/getenv "FUTON3C_GIRAFFE_REREAD_READERS")
                             (str/split #",") (->> (remove str/blank?) vec))
        kimi-task-repo (System/getenv "KIMI_TASK_REPO")
        cap-text (System/getenv "FUTON3C_GIRAFFE_REREAD_NEW_ITEM_CAP")
        new-item-cap (when (re-matches #"\d+" (or cap-text ""))
                       (Long/parseLong cap-text))
        window-text (System/getenv "FUTON3C_GIRAFFE_REREAD_MINT_WINDOW_MS")
        mint-window-ms (when (re-matches #"\d+" (or window-text ""))
                         (Long/parseLong window-text))
        store (queue/open-store)]
    (println (pr-str (if health?
                       (health {:store store})
                       (run-once! {:store store :reader-seats reader-seats
                                   :kimi-task-repo kimi-task-repo
                                   :new-item-cap new-item-cap
                                   :mint-window-ms mint-window-ms}))))))
