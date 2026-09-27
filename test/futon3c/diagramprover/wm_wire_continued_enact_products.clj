(ns futon3c.diagramprover.wm-wire-continued-enact-products
  (:require [clojure.java.io :as io]
            [clojure.test :as t]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as runner]
            [futon2.aif.flight-enact-test :as enact-test]
            [futon3c.diagramprover.wm-wire-summary-conditioning-products :as conditioning]))

(defn chain-pair []
  (let [run flight/run! captured (atom [])]
    ;; Capture the real first click and its input ports without changing it.
    (with-redefs [flight/run! (fn [f opts]
                              (let [r (run f opts)]
                                (when (:fetch-run-record opts) (swap! captured conj [r opts])) r))]
      (conditioning/pair :chosen))
    (let [[first-flight opts] (first @captured)
          wc (:wc-fn opts)
          first-step (get-in first-flight [:enactments 0 :step])
          next-opts (-> opts
                        (assoc :max-clicks 2)
                        (update :click-fn (fn [click]
                                           (fn [o] (-> (click o)
                                                       (assoc :click-id "second-click")
                                                       (assoc-in [:chosen :precedence] [:pattern/b]))))))
          continue (fn [change]
                     (run (assoc first-flight :status :open)
                          (assoc next-opts :wc-fn
                                 (fn [& args] (update (apply wc args) :increment change)))))
          a (continue identity)
          b (continue #(update % :policy-key assoc 1 "M-other-policy"))]
      {:first first-step :a (get-in a [:enactments 1 :step])
       :b (get-in b [:enactments 1 :step])})))

(defn test-report [field change?]
  (let [factory runner/enact-fn reports (atom []) calls (atom []) paths (atom [])]
    (try
      (with-redefs [runner/enact-fn
                    (fn [opts]
                      (let [enact (factory opts)]
                        (fn [& args]
                          (let [r (apply enact args)
                                changed (if-not change? r
                                          (case field
                                            :attempts (assoc-in r [:enactment :attempts 0 :commit] "c-other")
                                            :grain-gate (assoc-in r [:enactment :grain-gate :reason] :different-reason)))]
                            (swap! paths conj (:record-dir opts))
                            (swap! calls conj {:written (:enactment r) :carrier (:enactment changed)})
                            changed))))
                    t/report #(when (#{:pass :fail :error} (:type %))
                                (swap! reports conj (select-keys % [:type :expected :actual :message])))]
        (enact-test/two-steps-two-attempts-each-with-its-check))
      {:reports @reports :calls @calls}
      (finally
        (doseq [dir (distinct @paths) file (reverse (file-seq (io/file dir)))]
          (io/delete-file file true))))))

(defn assertion-products [field]
  (let [a (test-report field false) b (test-report field true)
        target (if (= field :attempts) 1 6)
        ;; The full-record reread assertion also depends on the modified field;
        ;; deliberately leave the writer's file intact so it detects tampering.
        dependent #{target 10}
        other #(mapv (fn [r] (select-keys r [:type :expected :message]))
                     (keep-indexed (fn [i r] (when-not (dependent i) r)) (:reports %)))]
    {:a a :b b :target target :other-a (other a) :other-b (other b)}))
