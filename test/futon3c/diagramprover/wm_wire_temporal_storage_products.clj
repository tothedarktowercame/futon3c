(ns futon3c.diagramprover.wm-wire-temporal-storage-products
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.temporal-update :as temporal]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as courier]))

(defn products []
  (courier/isolated
   (fn [root]
     (let [{:keys [record]} (courier/fixture root)
           final (@#'temporal/finalize-record
                  (dissoc record :temporal-receipt :temporal-posterior :temporal-cursor)
                  (get-in record [:temporal-input :previous]) [])
           absent (assoc final :temporal-receipt {:status :absent :reason :no-previous-posterior})
           path (io/file root "published.edn") other-path (io/file root "absent.edn")
           a (@#'temporal/write-once! path final)
           b (@#'temporal/write-once! other-path absent)
           repeated (@#'temporal/write-once! path absent)
           event-repeat (@#'temporal/write-once! path
                         (assoc absent :temporal-receipt {:status :absent :reason :event-already-consumed}))
           receipt (:receipt a)
           bad-digest (assoc receipt :digest "not-the-byte-digest")
           missing-path (assoc receipt :record-path (str (io/file root "missing.edn")))
           wrong-path (assoc receipt :record-path (str other-path))]
       {:final final :absent absent :a a :b b :repeat repeated :event-repeat event-repeat
        :disk-a (edn/read-string (slurp path)) :disk-b (edn/read-string (slurp other-path))
        :receipt receipt :bad-digest bad-digest :missing-path missing-path :wrong-path wrong-path
        :good (temporal/read-receipt receipt)
        :digest-result (temporal/read-receipt bad-digest)
        :missing-result (temporal/read-receipt missing-path)
        :wrong-result (temporal/read-receipt wrong-path)
        :other-result (temporal/read-receipt (:receipt b))}))))
