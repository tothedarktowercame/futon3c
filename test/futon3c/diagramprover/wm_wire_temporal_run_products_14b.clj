(ns futon3c.diagramprover.wm-wire-temporal-run-products-14b
  "Second-layer interventions; existing courier fixtures remain unchanged."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as runner]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as courier]))

(defn publications [root]
  (let [a (courier/fixture (io/file root "true"))
        enact runner/enact-fn
        b (with-redefs [runner/enact-fn
                        (fn [opts]
                          (let [interpretations (:interpretations opts) check (:check-fn opts)]
                            (enact (assoc opts
                                     :interpretations
                                     (fn [f] (into {} (map (fn [[k v]] [k (assoc v :theta 1/2)])
                                                          (interpretations f))))
                                     :check-fn #(assoc (check %) :observed false)))))]
            (courier/fixture (io/file root "false")))]
    [a b]))

(defn run-products [field]
  (courier/isolated
   (fn [root]
     (let [[a b] (publications root)
           values (mapv #(get-in % [:enacted field]) [a b])
           products (mapv (fn [i value]
                            (let [enacted (assoc (:enacted a) field value)
                                  result (courier/run-carrier (:start a) (:click a) enacted)
                                  file (io/file root (str "flight-" i ".edn"))]
                              (spit file (pr-str result))
                              (edn/read-string (slurp file)))) (range) values)]
       {:values values :products products
        :stored (mapv #(get-in % [:enactments 0 field]) products)
        ;; run!:530 stores the courier. :observation/:step/:clicks are calculated
        ;; from the unchanged enactment/observation inputs, not this courier.
        :other-products (mapv #(update-in % [:enactments 0] dissoc field) products)}))))

(defn judge-products []
  (courier/isolated
   (fn [root]
     (let [[a b] (publications root)
           f (courier/run-carrier (:start a) (:click a) (:enacted a))
           first-entry (first (:enactments f))
           receipts (mapv #(get-in % [:enacted :temporal-receipt]) [a b])
           absence {:status :absent :reason :no-previous-posterior :detail nil}
           flights (mapv #(assoc f :enactments [first-entry (assoc first-entry :temporal-receipt %)])
                         (conj receipts absence))
           products (mapv #(flight/judge-opts % {}) flights)]
       {:inputs flights :products products
        :expected (mapv :previous [a b])
        :envelopes (mapv #(get-in % [:flight :temporal-previous]) products)
        :absence absence}))))
