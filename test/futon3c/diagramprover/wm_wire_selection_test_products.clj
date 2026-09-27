(ns futon3c.diagramprover.wm-wire-selection-test-products
  "Run the real r9 test box, intervening only on the writer's returned law."
  (:require [clojure.test :as t]
            [futon2.aif.policy :as policy]
            [futon2.aif.selection-law-candidate-test :as reader]))

(defn assertion-report [field mutate]
  (let [writer policy/select-action-cascades
        read-file slurp
        reports (atom [])
        calls (atom [])
        fixture "test/fixtures/selection-law/row9-before@futon2-54e3c396.edn"]
    (with-redefs [policy/select-action-cascades
                  (fn [ranked opts]
                    (let [decision (writer ranked opts)
                          changed (update-in decision [:selection-law field] mutate)]
                      (swap! calls conj
                             {:scores (mapv :controller-score ranked)
                              :before (get-in decision [:selection-law field])
                              :after (get-in changed [:selection-law field])})
                      changed))
                  ;; Preserve the real pinned fixture bytes when the test runs
                  ;; from futon3c; no fixture value is replaced.
                  clojure.core/slurp
                  (fn [path & opts]
                    (apply read-file
                           (if (= path fixture)
                             (str "/home/joe/code/futon2/" fixture) path) opts))
                  t/report #(when (#{:pass :fail :error} (:type %))
                              (swap! reports conj
                                     (select-keys % [:type :expected :actual :message])))]
      (if (= field :enacted-steps)
        (reader/every-other-selection-law-key-is-unchanged)
        (reader/the-candidate-is-the-chosen-action-not-the-posterior-mode)))
    {:calls @calls :reports @reports}))
