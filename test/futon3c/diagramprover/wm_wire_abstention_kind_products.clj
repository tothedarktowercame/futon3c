(ns futon3c.diagramprover.wm-wire-abstention-kind-products
  (:require [clojure.edn :as edn]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as runner]
            [futon3c.diagramprover.wm-wire-r9-support :as r9]))

(defn pair []
  (let [record (:record (r9/run-tick (ex-info "cascade decision refused"
                                             {:kind :live-c-stale :target "M-wire"})))
        summary (runner/record-summary "M-wire" "kind-click" record)
        f (flight/start {:target "M-wire" :chosen-because {:kind :requested}}
                        {:kind :operator-declared :wants [:done] :declared-by "wire-test"}
                        {:id "kind-products"})
        read (fn [carrier]
               (let [opts {:sources-fn (constantly {}) :max-clicks 3
                           :observe-fn (fn [& _] {:done false})
                           :click-fn (constantly carrier)}
                     direct (flight/record-click f (merge carrier {:wants [:done]
                                                                  :before {:done false} :after {:done false}}))
                     loop-result (flight/run! f opts)
                     file (java.io.File/createTempFile "kind-product-" ".edn")]
                 (try
                   (spit file (pr-str {:direct direct :loop loop-result}))
                   {:carrier carrier :readback (edn/read-string (slurp file))}
                   (finally (.delete file)))))]
    [(read summary) (read (assoc-in summary [:abstention :kind] :pending))]))
