(ns futon3c.diagramprover.wm-wire-flight-entry-r9-close-cause-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/observe :r9-close-cause identity))
(def wire {:wire [:flight-entry :r9-close-cause [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test `the-target-reaches-the-reader :check check
           :live-records-read support/live-records-read})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :r9-close-cause identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :r9-close-cause (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :r9-close-cause (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))
