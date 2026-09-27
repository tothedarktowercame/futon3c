(ns futon3c.diagramprover.wm-wire-envelope-products-13a
  "Envelope (temporal_update.clj:45-54) copies posterior/cursor and gates on
  receipt status. Digests belong to write-once!/read-receipt, not envelope.
  consume-temporal replays the exact record, but does not validate its cursor."
  (:require [clojure.java.io :as io]
            [futon2.aif.temporal-update :as temporal]
            [futon2.aif.token-belief-predecessor :as predecessor]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as courier]))

(defn products [field]
  (courier/isolated
   (fn [root]
     (let [{:keys [record stage]} (courier/fixture root)
           final (@#'temporal/finalize-record
                  (dissoc record :temporal-receipt :temporal-posterior :temporal-cursor)
                  (get-in record [:temporal-input :previous]) [])
           ;; A real exact posterior for a failed deterministic transition/check;
           ;; intervene only on the posterior field at the envelope's door.
           alternative (temporal/compute
                        (-> (:temporal-input final)
                            (assoc-in [:enacted :transition :theta] 0)
                            (assoc-in [:observed :result :observed] false)))
           changed (case field
                     :temporal-posterior (assoc final field alternative)
                     :temporal-cursor (assoc final field {:initial-event-id "wrong-start"
                                                         :consumed-event-ids #{}})
                     :temporal-receipt (assoc final field {:status :absent
                                                          :reason :no-previous-posterior}))
           read-one (fn [name r]
                      (let [env (temporal/envelope r)
                            publication (@#'temporal/write-once! (io/file root name) r)
                            previous (temporal/read-receipt (:receipt publication))]
                        {:envelope env :publication (:receipt publication)
                         :read previous
                         :consumed (@#'predecessor/consume-temporal
                                    {} stage {:temporal-context? true :temporal-previous previous})}))]
       {:original final :changed changed
        :a (read-one "a.edn" final) :b (read-one "b.edn" changed)}))))
