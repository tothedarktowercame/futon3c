(ns futon3c.diagramprover.wm-wire-summary-products
  "Summary interventions at the flight readers; records reread from temp files."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as runner]))

(defn products [reader field]
  (let [summary (runner/record-summary
                 "M-summary" "summary-click"
                 {:decision {:chosen {:target "M-summary" :id :action/a
                                      :candidate :candidate/a :precedence [:pattern/a]}}
                  :failure {:kind :transport-unavailable :stage :selection
                            :error "first diagnostic" :cause {:message "first cause"}}})
        other (case field
                :chosen {:id :action/b :candidate :candidate/b :precedence [:pattern/b]}
                :failure (assoc (:failure summary) :error "changed diagnostic"))
        f (flight/start {:target "M-summary" :chosen-because {:kind :requested}}
                        {:kind :operator-declared :wants [:done] :declared-by "wire-test"}
                        {:id "summary-products"})
        run (fn [carrier]
              (let [observations (atom 0)
                    clicks (atom 0)
                    result (case reader
                             :record-click
                             (flight/record-click f (merge carrier {:wants [:done]
                                                                   :before {:done false}
                                                                   :after {:done false}}))
                             :run
                             (flight/run! f {:sources-fn (constantly {}) :max-clicks 3
                                             :observe-fn (fn [& _] (swap! observations inc) {:done false})
                                             :click-fn (fn [_] (swap! clicks inc) carrier)}))
                    file (java.io.File/createTempFile "wire-summary-" ".edn")]
                (try
                  (spit file (pr-str result))
                  {:carrier carrier :record (edn/read-string (slurp (io/file file)))
                   :observations @observations :click-calls @clicks}
                  (finally (.delete file)))))]
    [(run summary) (run (assoc summary field other))]))
