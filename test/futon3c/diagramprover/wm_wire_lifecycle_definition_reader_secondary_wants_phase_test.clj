(ns futon3c.diagramprover.wm-wire-lifecycle-definition-reader-secondary-wants-phase-test
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.lifecycle-exits :as exits]
            [futon3c.diagramprover.wm-wire-lifecycle-support :as support]))

(def wire {:wire [:lifecycle-definition-reader :lifecycle-secondary-wants
                  [:phase {:record :lifecycle-definition-exit}]]
           :kind :verified :test `definition-phases-reach-supplied})

(deftest definition-phases-reach-supplied
  (let [written (mapv :phase (exits/definition-exits support/definition))
        read (mapv :phase (:criteria (support/supplied)))]
    (is (= exits/phases written))
    (is (= written read))))
