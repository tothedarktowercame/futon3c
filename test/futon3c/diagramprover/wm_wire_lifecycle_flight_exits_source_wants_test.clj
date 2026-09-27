(ns futon3c.diagramprover.wm-wire-lifecycle-flight-exits-source-wants-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire-lifecycle-support :as support]))

(def wire {:wire [:lifecycle-flight-exits :flight-source-wants-a-exits
                  [:wants {:record :lifecycle-exits}]]
           :kind :verified :test `secondary-wants-reach-source-wants})

(deftest secondary-wants-reach-source-wants
  (let [written (:wants (support/flight-exits))
        result (support/source-wants)]
    (is (= 8 (count written)))
    (is (= written (:wants result)))
    (is (= written (get-in result [:source :lifecycle-exits :supplied])))))
