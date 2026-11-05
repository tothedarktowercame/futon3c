(ns futon3c.file-change.shadow-consumer
  "Non-actuating runtime consumer for projected file-change demands."
  (:require [futon3c.file-change.demand-projector :as projector]
            [futon3c.file-change.journal :as journal]))

(defn empty-state [debounce-ms max-wait-ms max-retained]
  {:pre [(pos-int? max-retained)]}
  {:schema :file-change/giraffe-shadow-v1
   :projector (projector/empty-state debounce-ms max-wait-ms)
   :max-retained max-retained :demands [] :outcomes []})

(defn- retain [state key value]
  (update state key #(->> (conj (vec %) value)
                          (take-last (:max-retained state)) vec)))

(defn- retain-demands [state demands]
  (update state :demands #(->> (into (vec %) demands)
                               (take-last (:max-retained state)) vec)))

(defn- ensure-registered! [journal-state now-ms deadline-ms]
  (swap! journal-state
         (fn [state]
           (if (get-in state [:consumers :giraffe-shadow])
             state
             (journal/register-consumer state :giraffe-shadow now-ms deadline-ms
                                        {:authority :giraffe-input})))))

(defn- rebuild-if-required!
  [journal-state consumer-state now-ms authority-read-fn warrant-demands read-limit]
  (when (= :rebuild-required
           (:status (journal/read-next @journal-state :giraffe-shadow read-limit)))
    (let [basis (journal/begin-rebuild @journal-state :giraffe-shadow)
          _ (swap! consumer-state #(if (:rebuild-requested-at-ms %)
                                     % (assoc % :rebuild-requested-at-ms now-ms)))
          requested-at (:rebuild-requested-at-ms @consumer-state)]
      (try
        (let [authority (authority-read-fn requested-at)]
          (if-not (and (map? authority)
                       (not-empty (:computed-sha256 authority)))
            (swap! consumer-state retain :outcomes
                   {:at-ms now-ms :outcome :held
                    :reason :giraffe-authority-unavailable})
            (let [receipt (volatile! nil)]
              (swap! journal-state
                     (fn [state]
                       (let [[state' rebuilt]
                             (journal/rebuild-consumer
                              state :giraffe-shadow (:through-seq basis) now-ms
                              {:authority :giraffe-input
                               :hash (:computed-sha256 authority)
                               :run-id (:run-id authority)})]
                         (vreset! receipt rebuilt)
                         state')))
              (swap! consumer-state
                     (fn [state]
                       (let [[projected demands]
                             (projector/project-rebuild
                              (:projector state) @receipt warrant-demands now-ms)]
                         (-> state
                             (assoc :projector projected)
                             (dissoc :rebuild-requested-at-ms)
                             (retain :outcomes
                                     {:at-ms now-ms :outcome :rebuilt
                                      :through-seq (:through-seq basis)
                                      :fresh? (boolean (:fresh? authority))
                                      :computed-sha256 (:computed-sha256 authority)
                                      :run-sha256 (:run-sha256 authority)
                                      :run-id (:run-id authority)})
                             (retain-demands demands))))))))
        (catch Exception e
          (swap! consumer-state retain :outcomes
                 {:at-ms now-ms :outcome :held :reason :rebuild-failed
                  :message (.getMessage e)}))))))

(defn- consume-entries!
  [journal-state consumer-state now-ms warrant-demands read-limit]
  (let [read (journal/read-next @journal-state :giraffe-shadow read-limit)]
    (when (and (= :ok (:status read)) (seq (:entries read)))
      (let [entries (:entries read)
            through (:seq (last entries))]
        (swap! consumer-state
               (fn [state]
                 (let [[projected demands]
                       (projector/project (:projector state) entries
                                          warrant-demands now-ms)]
                   (-> state
                       (assoc :projector projected)
                       (retain :outcomes {:at-ms now-ms :outcome :consumed
                                          :count (count entries)
                                          :through-seq through})
                       (retain-demands demands)))))
        (swap! journal-state journal/acknowledge :giraffe-shadow through now-ms)))))

(defn health [state journal-state now-ms]
  {:status :shadow :dispatch-enabled? false
   :retained-demands (count (:demands state))
   :retained-outcomes (count (:outcomes state))
   :last-demand (last (:demands state))
   :last-outcome (last (:outcomes state))
   :consumer (get-in (journal/health journal-state now-ms)
                     [:consumers :giraffe-shadow])})

(defn record-refusal!
  "Retain an isolated runtime refusal for watcher health."
  [consumer-state now-ms e]
  (swap! consumer-state retain :outcomes
         {:at-ms now-ms :outcome :refused :reason :shadow-cycle-failed
          :message (.getMessage ^Exception e)}))

(defn record-held!
  "Retain a normal asynchronous wait state without calling it a refusal."
  [consumer-state now-ms reason]
  (swap! consumer-state
         (fn [state]
           (let [prior (last (:outcomes state))]
             (if (and (= :held (:outcome prior)) (= reason (:reason prior)))
               (update state :outcomes
                       #(conj (vec (butlast %))
                              (-> prior (assoc :last-at-ms now-ms)
                                  (update :count (fnil inc 1)))))
               (retain state :outcomes
                       {:at-ms now-ms :last-at-ms now-ms :count 1
                        :outcome :held :reason reason}))))))

(defn run-once!
  "Consume bounded hints and retain projected outcomes without external calls."
  [journal-state consumer-state
   {:keys [now-ms deadline-ms authority-read-fn warrant-demands read-limit
           demand-sink-fn]
    :or {deadline-ms 300000 warrant-demands [] read-limit 256
         demand-sink-fn (constantly nil)}}]
  (locking consumer-state
    (ensure-registered! journal-state now-ms deadline-ms)
    (rebuild-if-required! journal-state consumer-state now-ms authority-read-fn
                          warrant-demands read-limit)
    (consume-entries! journal-state consumer-state now-ms warrant-demands read-limit)
    (let [state @consumer-state
          [projected due] (projector/due-demands (:projector state) now-ms)
          due (mapv #(assoc % :emitted-at-ms now-ms) due)]
      ;; Persist before clearing pending projector state. If the sink refuses,
      ;; the demand remains pending and the next watcher cycle retries it.
      (doseq [d due] (demand-sink-fn d))
      (reset! consumer-state
              (-> state (assoc :projector projected) (retain-demands due))))
    (health @consumer-state @journal-state now-ms)))
