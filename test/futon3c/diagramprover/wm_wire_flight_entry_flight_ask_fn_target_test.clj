(ns futon3c.diagramprover.wm-wire-flight-entry-flight-ask-fn-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/live-check :flight-ask-fn))
(def wire {:wire [:flight-entry :flight-ask-fn [:target {:record :flight}]]
           :kind :verified
           :test `the-target-reaches-the-reader :check check
           :record support/ask-pin})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :flight-ask-fn identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :flight-ask-fn (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :flight-ask-fn (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))
