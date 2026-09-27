(ns futon3c.diagramprover.wm-wire-kernel-out-support
  (:require [clojure.test :as t]
            [futon2.aif.flight :as flight]
            [futon3c.diagramprover.wm-wire-measured-support :as measured]
            [futon2.aif.efe :as efe] [futon2.aif.policy :as policy]
            [futon2.aif.cascade-selection :as selection]
            [futon2.aif.policy-prefix-evidence :as ppe]
            [futon2.aif.policy-prefix-admission :as admission]
            [futon2.aif.order-kernel-test :as order]
            [futon2.aif.coapply-kernel-test :as coapply]
            [futon3c.diagramprover.wm-wire-target-support :as target]
            [futon3c.diagramprover.wm-wire-ask-out-support :as ask]))
(def live-records-read
  (mapv #(assoc % :why "No same-record ranked controller-score beside a selection consumer. Tick policy f-prefix receipts are not-supplied; flight records lack ranker/controller-score and order-use reader products.") target/live-records-read))
(defn census []
  (mapv (fn [p] (let [r (target/pinned p)]
                 {:path (:path p) :fields (frequencies
                   (mapcat #(when (map? %) (filter #{:controller-score :order-use :f-prefix} (keys %)))
                           (tree-seq coll? seq r)))})) live-records-read))
(defn score [reader mutation]
  (let [entry (first (filter #(= :chain (:cascade-id %)) (#'order/rank)))
        value (:controller-score entry)
        changed (assoc entry :controller-score (case mutation :none value :absent {:absent :not-carried} :different (+ value 2)))
        r (try (if (= reader :law) (policy/select-action-cascades [changed] {:beta 1})
                   (#'policy/selection-candidate changed))
               (catch clojure.lang.ExceptionInfo e {:refusal (ex-data e)}))]
    {:writer value :reader (get r (if (= reader :law) :controller-score :g)) :result r}))
(defn order-read [which mutation]
  (let [id (if (= which :order) :chain :independent)
        real efe/rank-cascade-actions written (atom nil) reports (atom [])
        test-var (if (= which :order) #'order/the-chain-scores-its-order-and-equals-the-list-kernel
                     #'coapply/the-ranking-scores-a-non-chain-by-co-application)]
    (with-redefs [efe/rank-cascade-actions
                  (fn [& args]
                    (let [r (apply real args) value (:order-use (first (filter #(= id (:cascade-id %)) r)))]
                      (reset! written value)
                      (mapv #(if (= id (:cascade-id %))
                               (assoc % :order-use (case mutation :none value :absent {:absent :not-carried} :different {:order :different})) %) r)))
                  t/report #(swap! reports conj %)]
      (test-var))
    ;; clojure.test's equality report contains the actual value consumed by
    ;; the existing assertion, both on pass (= ...) and fail (not (= ...)).
    (let [report (first (filter #(#{:pass :fail} (:type %)) @reports))
          form (:actual report)
          equality (if (= 'not (first form)) (second form) form)]
      {:writer @written :reader (last equality) :reports @reports})))
(def prefix-entry
  (delay
    (let [{:keys [decision record]} (ask/step :none)
          action (:action decision)
          step (flight/conditioning-step
                 (assoc (measured/inputs record) :policy-key (admission/candidate-key action)))
          admitted (admission/admit (admission/candidate-key action) [{:step step}])
          _ (assert (= :admitted (:conditioning-status admitted)))
          entry {:action action :controller-score (:controller-score decision)}]
      (first (ppe/production-ranked [entry] nil {(:id action) admitted})))))
(defn prefix-read [mutation]
  (let [entry @prefix-entry value (:f-prefix entry)
        changed (assoc entry :f-prefix (case mutation :none value :absent {:status :absent :reason :not-carried}
                                            :different (update value :f inc)))
        r (try (#'policy/selection-candidate changed)
               (catch clojure.lang.ExceptionInfo e {:refusal (ex-data e)}))]
    {:writer value :reader (:f-prefix r) :result r}))
(defn prefix-effect []
  (let [a (#'policy/selection-candidate @prefix-entry)
        ;; Equal G and E isolate the observed prefix's contribution.
        base (assoc a :id :control :habit 1.0 :f 0.0 :f-status :computed)
        a (assoc a :id :observed :habit 1.0)
        changed (update a :f inc)]
    {:g (:g a) :f (:f a) :changed-f (:f changed)
     :posterior (selection/selection-posterior {:beta 1 :candidates [a base]})
     :changed-posterior (selection/selection-posterior {:beta 1 :candidates [changed base]})}))
