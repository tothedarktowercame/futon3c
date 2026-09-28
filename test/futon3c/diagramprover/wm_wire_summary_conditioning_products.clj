(ns futon3c.diagramprover.wm-wire-summary-conditioning-products
  "Real summary and increment carriers through run!'s conditioning branch."
  (:require [clojure.edn :as edn]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as runner]
             [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon3c.diagramprover.wm-wire-publication-support :as publication]
            [futon3c.diagramprover.wm-wire-rates-support :as rates]))

(defn pair [field]
  (let [target "M-conditioning"
        increment (:writer (publication/increment-observe identity))
        ma (wm-cd/measured-a-version
            [{:target target :cascade-problem {:locators {:done {:class :C3}}}}]
            {target (select-keys @rates/admitted-view [:labels :subjects :prior])})
        record {:decision {:chosen {:target target :candidate :candidate/a
                                    :precedence [:pattern/a]}
                           :measured-a ma :initial-belief-receipt {:value {#{} 1}}
                           :selection-certificate {:token-belief-stage
                            {:domain-inputs [{:target target :declaration
                             {:interpretations
                              {:pattern/a {:guard {:needs #{} :forbids #{}} :produces #{:done}}
                               :pattern/b {:guard {:needs #{} :forbids #{}} :produces #{:other}}}}}]}}}}
        summary (runner/record-summary target "conditioning-click" record)
        f (flight/start {:target target :chosen-because {:kind :requested}}
                        {:kind :operator-declared :wants [:done] :declared-by "wire-test"}
                        {:id "conditioning-products"})
        run (fn [click inc]
              (let [result (flight/run! f
                             {:sources-fn (constantly {:locators {target {:done {:class :C3}}}})
                              :max-clicks 1 :click-fn (constantly click)
                              :observe-fn (fn [& _] {:done true})
                              :enact-fn (fn [& _] {:enactment {:attempts []}})
                              :wc-fn (fn [& _] {:increment inc})
                              :fetch-run-record (constantly record)})
                    file (java.io.File/createTempFile "conditioning-product-" ".edn")]
                (try
                  (spit file (pr-str result))
                  {:click click :increment inc :run-record record
                   :record (edn/read-string (slurp file))}
                  (finally (.delete file)))))
        changed-key (assoc (:policy-key increment) 1 "M-other-policy")]
    [(run summary increment)
     (case field
       :chosen (run (assoc-in summary [:chosen :precedence] [:pattern/b]) increment)
       :increment (run summary (assoc increment :policy-key changed-key)))]))
