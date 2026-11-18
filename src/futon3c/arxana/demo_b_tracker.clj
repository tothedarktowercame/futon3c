(ns futon3c.arxana.demo-b-tracker
  "Read-only join of a committed file manifest and store-backed catalogue facts."
  (:require [futon3c.arxana.benefit-filings-catalogue :as filings]
            [futon3c.arxana.catalogue-view :as catalogue]))

(defn tracker-view [manifest view]
  (if (not= :ok (:status view)) view
      {:status :ok :valid-time (:valid-time view)
       :provenance {:manifest {:kind :file :path "resources/arxana/demo-b-citations.edn" :record? false}
                    :catalogue {:kind :store-query :valid-time (:valid-time view)}}
       :reads (mapv (fn [read]
                      (let [choices (filterv #(some #{(:feature read)} (:applies-to %)) (:choices view))
                            states (map :enforcement-status choices)]
                        (assoc read :choices choices
                               :enforcement-status
                               (cond (empty? choices) {:status :unknown :reason :undocumented}
                                     (some #(= :violated (:status %)) states) {:status :violated}
                                     (every? #(= :enforced (:status %)) states) {:status :enforced}
                                     :else {:status :unknown :reasons (vec states)})))) (:reads manifest))
       :coverage (mapv #(select-keys % [:choice/key :enforcement-status :checks])
                       (filter #(= :coverage (:choice/key %)) (:choices view)))
       :views [{:mode :list :operation :cascade/benefit-filings}
               {:mode :h1 :operation :cascade/benefit-filings
                :benefit-id "arxana:clusion:sha256:9b3ce7c0fe64941175c8f03a6872a8ee6589e9fbc3161b721f0533ef9bc64877"}]
       :mojira {:status :not-built :source :tracker-declaration
                :note "Mojira presentation is the next packet; backend availability is not a built page."}}))

(defn read-tracker [store at]
  (tracker-view (filings/manifest) (catalogue/read-catalogue store at)))
