(ns futon3c.arxana.workplan-transfer
  "Import an offline Arxana export. Default mode only reads the destination."
  (:require [clojure.edn :as edn]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]))

(defn dependency-order [entries]
  (let [ids (set (map #(get-in % [:record :id]) entries))]
    (loop [pending (vec entries) done #{} out []]
      (if (empty? pending) out
          (let [ready? (fn [{:keys [record]}]
                         (every? #(or (not (ids %)) (done %))
                                 (concat (keep identity [(:subject-clusion-id record) (:subject/id record)])
                                         (keep #(get-in % [:target :clusion/id]) (:endpoints record)))))
                ready (filterv ready? pending)]
            (when (empty? ready) (throw (ex-info "Cyclic record dependencies" {:remaining (count pending)})))
            (recur (filterv (complement ready?) pending)
                   (into done (map #(get-in % [:record :id]) ready)) (into out ready)))))))

(defn plan [graph-store entries]
  (reduce (fn [p {:keys [record] :as entry}]
            (let [r (store/get-entity graph-store (:id record))]
              (cond
                (:ok r) (update p (if (= record (:ok r)) :existing :conflicts) conj entry)
                (= :arxana.store/record-not-found (get-in r [:error :error/code])) (update p :new conj entry)
                :else (throw (ex-info "Destination read failed; refusing import" {:id (:id record) :response r})))))
          {:new [] :existing [] :conflicts []} (dependency-order entries)))

(defn execute! [graph-store p batch-prefix pause-ms]
  (when (seq (:conflicts p)) (throw (ex-info "Record ID conflicts; refusing import" {:ids (mapv #(get-in % [:record :id]) (:conflicts p))})))
  (mapv (fn [i batch]
          (let [r (http/append-batch! graph-store (str batch-prefix "/" i) batch)]
            (when (:error r) (throw (ex-info "Batch refused; stopped" {:batch i :response r})))
            (doseq [{:keys [record]} batch]
              (when-not (= record (:ok (store/get-entity graph-store (:id record))))
                (throw (ex-info "Readback mismatch" {:id (:id record)}))))
            (Thread/sleep pause-ms)
            r))
        (range) (partition-all 500 (:new p))))

(defn -main [& args]
  (let [write? (some #{"--write"} args)
        paths (remove #{"--write" "--dry-run"} args)
        [path url] paths]
    (when (or (nil? path) (> (count paths) 2) (and write? (some #{"--dry-run"} args)))
      (throw (ex-info "Usage: -m futon3c.arxana.workplan-transfer EXPORT.edn [URL] [--dry-run|--write]" {})))
    (let [s (http/http-store (or url "http://localhost:7073") "arxana-service")
          p (plan s (edn/read-string (slurp path)))
          summary (into {} (map (fn [[k v]] [k (count v)]) p))]
      ;; The local plan names every existing/new/conflicting record; it never
      ;; writes to the destination in dry-run mode.
      (spit (str path ".plan.edn") (pr-str p))
      (println (pr-str (assoc summary :mode (if write? :write :dry-run)
                                      :manifest (str path ".plan.edn"))))
      (when (seq (:conflicts p))
        (throw (ex-info "Record ID conflicts; stopped" {:ids (mapv #(get-in % [:record :id]) (:conflicts p))})))
      (when write?
        (execute! s p (str "workplan-fold/" (random-uuid)) 100)
        (println (pr-str {:verified (count (:new p))}))))))
