(ns futon3c.wm.r10-click-adapter
  "Commissioned R10 click composition. This namespace owns no authority paths
  and supplies no evidence-store fallback."
  (:require [futon3c.social.coordination-ledger :as ledger]
            [futon3c.wm.r10-commission :as commission]
            [futon3c.wm.r10-commission-binding :as binding]
            [futon3c.wm.runner-service :as runner]))

(defn- refuse! [code data]
  (throw (ex-info "R10 commissioned click refused"
                  (merge {:error/type :r10/click-refusal
                          :error/code code
                          :status 409}
                         data))))

(defn- evidence-store-for-config [config]
  (or (:evidence-store config)
      (get-in config [:registry :peripheral-config :evidence-store])))

(defn commissioned-click!
  "Spend the server-owned single-use commission on one runner click.
  Authority comes only from the server commission. Issuer provenance is descriptive."
  [{:keys [config click-fn issuer-provenance]
    :or {click-fn (requiring-resolve 'futon3c.wm.runner-service/click!)}}]
  (let [authorized (binding/authorized-commission)
        evidence-store (evidence-store-for-config config)]
    (when-not evidence-store
      (refuse! :r10/evidence-store-missing {:phase :before-reservation}))
    (when (:running? (runner/status))
      (refuse! :r10/runner-busy {:phase :before-reservation}))
    (let [result
          (ledger/run-scheduled-dispatch!
           {:commission authorized
            :evidence-store evidence-store
            :dispatch-fn
            (fn [linked]
              (:receipt
               (commission/dispatch-reserved!
                {:reservation-root binding/reservation-root
                 :commission linked
                 :dispatch-fn
                 (fn [_]
                   (let [click-result (click-fn (cond-> {} issuer-provenance
                                                  (assoc :issuer-provenance issuer-provenance)))]
                     (when (= :already-running (:rejected click-result))
                       (refuse! :r10/click-rejected
                                {:reason :already-running
                                 :dispatch/occurred false
                                 :click-result click-result}))
                     (let [click-id (:click-id click-result)]
                       {:node :R10
                        :commission/id (:commission/id linked)
                        :dispatch/id click-id
                        :click/id click-id
                        :click/result click-result})))})))})]
      (commission/mark-recorded!
       {:reservation-root binding/reservation-root
        :commission-id (:commission/id authorized)
        :evidence-id (:evidence/id result)})
      result)))
