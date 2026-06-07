(ns futon3c.logic.capability-star-map-efe-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.efe :as efe]
            [futon3c.logic.capability-star-map-invariants :as inv]))

(def ^:private base-state
  {:observation {:loop-health 0.9
                 :support-coverage 0.9
                 :attack-coverage 0.9
                 :mission-health 0.7
                 :stack-pct 0.20
                 :consulting-pct 0.25
                 :portfolio-pct 0.25
                 :mathematics-pct 0.20
                 :active-repo-ratio 0.8
                 :sorry-count-norm 0.1
                 :coupling-density 0.2
                 :ticks-firing-ratio 0.0
                 :depositing-signal 0.1}
   :belief {}})

(defn- ensemble-fixture []
  (edn/read-string
   (slurp (io/file ".." "futon0" "holes" "missions"
                   "M-capability-star-map.ensemble.edn"))))

(defn- wm-region-graph-fixture []
  (let [ensemble (ensemble-fixture)
        caps (:capabilities ensemble)]
    {:star-map/region :wm
     :capabilities (-> caps
                       (assoc-in [:wm-overnight-unsupervised :pre-registered?] true)
                       (assoc-in [:efe-trustworthy-over-starmap :pre-registered?] true))
     :missions {:M-capability-star-map
                {:scope [:wm-steps-forward-guardrailed]
                 :produces [:efe-trustworthy-over-starmap]
                 :open-hole-count 1
                 :phase :instantiate
                 :status :active
                 :next-exit-operator-verify? false}

                :M-unagreed-exit
                {:scope [:wm-steps-forward-guardrailed]
                 :produces [:efe-trustworthy-over-starmap]
                 :open-hole-count 1
                 :phase :identify
                 :status :active
                 :next-exit-operator-verify? true}}
     :edges []}))

(deftest selected-star-map-trace-has-no-buck-or-gate-violations-test
  (testing "q-buck/q-gate are clean on the real Unit B selection trace"
    (let [graph (wm-region-graph-fixture)
          opts {:capability-graph graph
                :pre-registered-goal :wm-overnight-unsupervised}
          candidates [{:type :pursue :target :cap/pentagon}
                      {:type :open-mission :target :M-unagreed-exit}
                      {:type :open-mission :target :M-capability-star-map}]
          selected (efe/select-star-map-action base-state candidates opts)
          trace [(efe/selection-trace-step graph :wm-overnight-unsupervised
                                           (:action selected))]
          db (inv/build-db trace)]
      (is (= :M-capability-star-map (get-in selected [:action :target])))
      (is (false? (efe/safe-action? graph :wm-overnight-unsupervised
                                    {:type :pursue :target :cap/pentagon}))
          "pentagon pursuit is refused at the selector boundary")
      (is (false? (efe/safe-action? graph :wm-overnight-unsupervised
                                    {:type :open-mission :target :M-unagreed-exit}))
          "past-unagreed-exit advance is refused at the selector boundary")
      (is (= '() (inv/q-buck db)))
      (is (= '() (inv/q-gate db))))))
