(ns futon3c.diagramprover.wm-wire-temporal-previous-products
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.exact-belief-adapter :as exact]
            [futon2.aif.flight :as flight]
            [futon2.aif.temporal-input :as input]
            [futon2.aif.token-belief-predecessor :as predecessor]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as courier]
            [futon3c.diagramprover.wm-wire-token-input-support :as prior]))

(defn products [reader]
  (courier/isolated
   (fn [root]
     (let [{:keys [stage start enacted]} (courier/fixture root)
           opts (flight/judge-opts (assoc start :enactments [(select-keys enacted [:record-path :temporal-receipt])]) {})
           previous (get-in opts [:flight :temporal-previous])
           r (:record previous)
           ;; A second exact record, with a no-change transition and a false
           ;; observation. Intervention is on the envelope, not on q alone:
           ;; consume-temporal must replay the complete alternative record.
           other-record (exact/exact-update (:states r) (:observation-rows r)
                                             #(hash-map % 1) false
                                             {#{} 1} (:model r))
           other (assoc previous :record other-record)
           bad (assoc previous :domain #{[:another-target :done]})
           inspection (predecessor/inspect-trace nil opts)
           initial (@#'predecessor/initialization-input-receipt stage inspection prior/admission nil)
           run (fn [v]
                 (case reader
                   :inspect (predecessor/inspect-trace nil (assoc-in opts [:flight :temporal-previous] v))
                   :consume (@#'predecessor/consume-temporal initial stage (assoc inspection :temporal-previous v))
                   :receipt (predecessor/input-receipt stage (assoc inspection :temporal-previous v) prior/admission nil)))
           result {:carriers [previous other bad] :initial (:continuation-belief initial)
                   :replayed [(input/previous-belief previous) (input/previous-belief other)]
                   :products (mapv run [previous other bad])}
           file (io/file root "products.edn")]
       (spit file (pr-str result))
       (edn/read-string (slurp file))))))
