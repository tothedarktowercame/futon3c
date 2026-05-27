(ns futon3c.aif.mission-delta-t-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.aif.mission-delta-t :as sut]))

(def ^:private war-machine-pilot
  "futon3c-d/mission/war-machine-pilot")

(def ^:private mission-wiring
  "futon3c-d/mission/mission-wiring")

(def ^:private action-cost-modelling
  "futon3c-d/mission/action-cost-modelling")

(def ^:private r3d-sorry
  "futon2/sorry/r3d-per-entity-attribution")

(def ^:private stub-lifts-sorry
  "futon2/sorry/stub-lifts-pending-aif-edn")

(def ^:private mission-cross-ref-kw
  (keyword "code" "v05/mission-cross-ref"))

(def ^:private file-to-mission-kw
  (keyword "code" "v05/file→mission"))

(def ^:private stack-generator-file
  "futon3c-d/file/src/futon3c/aif/stack_generator.clj")

(def ^:private mission-control-file
  "futon3c-d/file/src/futon3c/peripheral/mission_control_backend.clj")

(def ^:private synthetic-substrate
  {war-machine-pilot
  [{:hx/type "code/v05/mission-doc"
     :hx/endpoints [war-machine-pilot]
     :hx/props {"mission/phase" "verify"}}
    {:hx/type "code/v05/mission-cross-ref"
     :hx/endpoints [action-cost-modelling war-machine-pilot]
     :hx/props {"mission/source" "action-cost-modelling"
                "mission/target" "war-machine-pilot"}}
    {:hx/type "code/v05/mission-cross-ref"
     :hx/endpoints [mission-wiring war-machine-pilot]
     :hx/props {"mission/source" "mission-wiring"
                "mission/target" "war-machine-pilot"}}
    {:hx/type "code/v05/related-mission"
     :hx/endpoints [r3d-sorry war-machine-pilot]
     :hx/props {"sorry/source-registry-id" ":sorry/r3d-per-entity-attribution"}}]

   mission-wiring
   [{:hx/type "code/v05/mission-doc"
     :hx/endpoints [mission-wiring]
     :hx/props {"mission/phase" "derive"}}
    {:hx/type "code/v05/mission-cross-ref"
     :hx/endpoints [action-cost-modelling mission-wiring]
     :hx/props {"mission/source" "action-cost-modelling"
                "mission/target" "mission-wiring"}}
    {:hx/type "code/v05/mission-cross-ref"
     :hx/endpoints [war-machine-pilot mission-wiring]
     :hx/props {"mission/source" "war-machine-pilot"
                "mission/target" "mission-wiring"}}
    {:hx/type "code/v05/file→mission"
     :hx/endpoints [stack-generator-file mission-wiring]
     :hx/props {"file/source-path" "/home/joe/code/futon3c/src/futon3c/aif/stack_generator.clj"
                "mission/target" "mission-wiring"}}
    {:hx/type "code/v05/file→mission"
     :hx/endpoints [mission-control-file mission-wiring]
     :hx/props {"file/source-path" "/home/joe/code/futon3c/src/futon3c/peripheral/mission_control_backend.clj"
                "mission/target" "mission-wiring"}}]

   action-cost-modelling
   [{:hx/type "code/v05/mission-doc"
     :hx/endpoints [action-cost-modelling]
     :hx/props {"mission/phase" "instantiate"}}
    {:hx/type "code/v05/mission-cross-ref"
     :hx/endpoints [war-machine-pilot action-cost-modelling]
     :hx/props {"mission/source" "war-machine-pilot"
                "mission/target" "action-cost-modelling"}}
    {:hx/type "code/v05/mission-cross-ref"
     :hx/endpoints [mission-wiring action-cost-modelling]
     :hx/props {"mission/source" "mission-wiring"
                "mission/target" "action-cost-modelling"}}
    {:hx/type "code/v05/related-mission"
     :hx/endpoints [stub-lifts-sorry action-cost-modelling]
     :hx/props {"sorry/source-registry-id" ":sorry/stub-lifts-pending-aif-edn"}}]

   r3d-sorry
   [{:hx/type "code/v05/sorry"
     :hx/endpoints [r3d-sorry]
     :hx/props {"sorry/t" 1
                "sorry/status" ":open"}}]

   stub-lifts-sorry
   [{:hx/type "code/v05/sorry"
     :hx/endpoints [stub-lifts-sorry]
     :hx/props {"sorry/t" 1
                "sorry/status" ":open"}}]})

(defn- finite-nonzero?
  [x]
  (and (number? x)
       (Double/isFinite (double x))
       (not (zero? (double x)))))

(deftest delta-t-mission-case-studies-return-nonzero-values
  (with-redefs [sut/fetch-hyperedges-by-endpoint
                (fn [endpoint _opts]
                  (or (get synthetic-substrate endpoint)
                      (throw (ex-info "missing synthetic endpoint"
                                      {:endpoint endpoint}))))]
    (doseq [mission-endpoint [war-machine-pilot mission-wiring action-cost-modelling]]
      (testing mission-endpoint
        (let [result (sut/delta-t-mission mission-endpoint)]
          (is (finite-nonzero? (:delta-T result)))
          (is (pos? (:n-edges result)))
          (is (pos? (get-in result [:by-edge-type mission-cross-ref-kw :n-edges] 0))))))))

(deftest mission-wiring-picks-up-file-to-mission-edges-in-v0
  (with-redefs [sut/fetch-hyperedges-by-endpoint
                (fn [endpoint _opts]
                  (or (get synthetic-substrate endpoint)
                      (throw (ex-info "missing synthetic endpoint"
                                      {:endpoint endpoint}))))]
    (let [result (sut/delta-t-mission mission-wiring)]
      (is (finite-nonzero? (:delta-T result)))
      (is (pos? (get-in result [:by-edge-type file-to-mission-kw :n-edges] 0)))
      (is (pos? (get-in result [:by-source-type :file :n-edges] 0)))
      (is (= 0.5 (double (->> (:per-edge-contributions result)
                              (filter #(= :file (:source-type %)))
                              first
                              :source-T)))))))

(deftest phase-projection-table-uses-operator-confirmed-defaults
  (is (= 1.0 (sut/phase->t "head")))
  (is (= 0.5 (sut/phase->t "unknown")))
  (is (= 0.5 (sut/phase->t nil)))
  (is (= 0.3 (sut/phase->t :instantiate))))
