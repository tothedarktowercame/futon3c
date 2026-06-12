(ns futon3c.watcher.projections.flight-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.projections.flight :as flight]))

(def conforming-record
  {:flight/id "fixture-flight"
   :flight/derivation :full
   :organs {:field-read {:judgment {:class :chosen :target "M-demo"}
                         :ground "chosen from fixture neighbourhood"}
            :measurement {:judgment {:class :null :agreement true}
                          :ground {:scan-1 {:g 1.0} :scan-2 {:g 1.0}}}
            :warrant {:sorry {:kind :typed-gap
                              :why "fixture intentionally omits warrant"}}}})

(deftest project-conforming-flight
  (let [projection (flight/project-record "fixture.flight.edn" conforming-record)]
    (is (= "arxana/essay" (get-in projection [:essay :type])))
    (is (= "arxana/essay/flight/fixture-flight" (get-in projection [:essay :id])))
    (is (= 3 (count (:sections projection))))
    (is (= 3 (count (:annotations projection))))
    (is (= "arxana/flight-organ-annotation" (:hx-type (first (:annotations projection)))))
    (is (= #{"annotated" "flight"}
           (set (map :role (:endpoints (first (:annotations projection)))))))
    (is (= "typed-sorry"
           (some (fn [section]
                   (when (= "warrant" (get-in section [:props :organ/key]))
                     (get-in section [:props :organ/state])))
                 (:sections projection))))))

(deftest refuses-derivation-thin-record
  (testing "thin records are fallback-only, not canonical substrate writes"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not canonical-ingestable"
                          (flight/project-record "thin.edn"
                                                 (assoc conforming-record
                                                        :flight/id "thin"
                                                        :flight/derivation :thin))))))

(deftest refuses-transient-measurement
  (testing "transient measurements are not mask-IN canonical records"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not canonical-ingestable"
                          (flight/project-record "transient.edn"
                                                 (assoc-in conforming-record
                                                           [:organs :measurement :judgment :class]
                                                           :transient))))))
