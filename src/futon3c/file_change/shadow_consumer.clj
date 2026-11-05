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
    (let [basis (journal/begin-rebuild @journal-state :giraffe-shadow)]
      (try
        (let [authority (authority-read-fn)]
          (if-not (and (map? authority) (not-empty (:input-sha256 authority)))
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
                               :hash (:input-sha256 authority)
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
                             (retain :outcomes
                                     {:at-ms now-ms :outcome :rebuilt
                                      :through-seq (:through-seq basis)
                                      :authority-sha256 (:input-sha256 authority)})
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

(defn run-once!
  "Consume bounded hints and retain projected outcomes without external calls."
  [journal-state consumer-state
   {:keys [now-ms deadline-ms authority-read-fn warrant-demands read-limit]
    :or {deadline-ms 300000 warrant-demands [] read-limit 256}}]
  (ensure-registered! journal-state now-ms deadline-ms)
  (rebuild-if-required! journal-state consumer-state now-ms authority-read-fn
                        warrant-demands read-limit)
  (consume-entries! journal-state consumer-state now-ms warrant-demands read-limit)
  (swap! consumer-state
         (fn [state]
           (let [[projected due] (projector/due-demands (:projector state) now-ms)]
             (-> state (assoc :projector projected) (retain-demands due)))))
  (health @consumer-state @journal-state now-ms))
