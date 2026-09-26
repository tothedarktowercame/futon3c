(require '[clojure.java.io :as io]
         '[clojure.test :as t]
         '[futon3c.wm.run4-infrastructure-reconciliation :as r]
         '[futon3c.wm.run4-infrastructure-reconciliation-test :as rt])
(let [capture (atom nil) original r/publish!]
  (with-redefs [r/publish! (fn [root record]
                           (when-not @capture (reset! capture [root record]))
                           (original root record))]
    (t/test-vars [#'rt/captures-positive-six-cell-prefix-and-refuses-corruption]))
  (let [[root record] @capture
        forged (assoc-in record [:identity :controller-attempt-id] "../escaped")]
    (original root forged)
    (prn {:escaped-publication? (.isFile (io/file root "../escaped.reconciliation.edn"))
          :stale-evidence-published? true})))
