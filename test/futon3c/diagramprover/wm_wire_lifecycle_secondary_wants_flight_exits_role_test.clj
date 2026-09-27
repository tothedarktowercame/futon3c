(ns futon3c.diagramprover.wm-wire-lifecycle-secondary-wants-flight-exits-role-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire-lifecycle-support :as support]))

(def wire {:wire [:lifecycle-secondary-wants :lifecycle-flight-exits
                  [:role {:record :secondary-want}]]
           :kind :verified :test `secondary-role-reaches-flight-exits})

(deftest secondary-role-reaches-flight-exits
  (let [written (mapv :role (:criteria (support/supplied)))
        read (mapv :role (vals (:criteria-by-token (support/flight-exits))))]
    (is (= (repeat 8 :how) written))
    (is (= (frequencies written) (frequencies read)))))
