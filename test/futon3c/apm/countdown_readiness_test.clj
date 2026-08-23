(ns futon3c.apm.countdown-readiness-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]))

(def readiness
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/countdown-10-readiness-v1.edn")))

(deftest countdown-means-ten-operator-stepped-complete-frames
  (is (zero? (:estimated-remaining-structural-slices readiness)))
  (is (nil? (:next-slice readiness)))
  (is (:structurally-ready? readiness))
  (is (= [:complete :complete :complete :complete :complete :complete]
         (mapv :status (:blocking-slices readiness))))
  (is (contains? (get-in readiness [:launch-rule
                                     :must-not-defer])
                 :student-memory-use-evidence))
  (is (contains? (get-in readiness [:launch-rule
                                     :may-defer-until-ten-closes])
                 :analyst-architecture-synthesis))
  (is (contains? (get-in readiness [:launch-rule :must-not-defer])
                 :analyst-post-close-receipt)))

(deftest every-readiness-claim-is-grounded-in-a-present-artifact
  (doseq [{:keys [evidence]} (concat (:ready readiness)
                                     (:blocking-slices readiness))
          path evidence]
    (is (.isFile (java.io.File. path)) path)))

(deftest blocking-slices-have-an-executable-finish-condition
  (is (= (range 1 7) (map :order (:blocking-slices readiness))))
  (is (every? #(and (string? (:problem %))
                    (string? (:done-when %)))
              (:blocking-slices readiness))))
