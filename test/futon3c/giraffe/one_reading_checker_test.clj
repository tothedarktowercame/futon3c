(ns futon3c.giraffe.one-reading-checker-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.giraffe.reread-worker :as worker]))

(deftest checker-contract-carries-placement-shape
  (let [contract (worker/checker-contract
                  "resources/giraffe/check_edges-v2.py")]
    (doseq [field ["PLACEMENT" "what" "status" "ways" "wing" "action"
                   "need_goes_to" "finish" "beneficiary" "confidence"
                   "evidence"]]
      (is (str/includes? contract field) field))))
