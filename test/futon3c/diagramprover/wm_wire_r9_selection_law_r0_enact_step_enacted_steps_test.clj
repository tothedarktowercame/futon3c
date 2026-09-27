(ns futon3c.diagramprover.wm-wire-r9-selection-law-r0-enact-step-enacted-steps-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))
(def positive (delay (support/enacted-steps :none)))
(defn check [] @positive)
(def wire {:wire [:r9-selection-law :r0-enact-step :enacted-steps]
           :kind :unverified :test `the-real-reader-handoff :check check
           :live-records-read support/live-records-read
           :note "enact-fn reads chosen :precedence, not selection-law :enacted-steps. Tampering enacted-steps does not change dispatched patterns; missing positional handoff is a map finding."})
(deftest the-real-reader-handoff
  (is (seq (support/census)))
  (let [o (check)] (is (not (w/received? o)))
    (is (seq (:attempted o)))))
(deftest absence-before-reader
  (let [o (support/enacted-steps :absent)]
    (is (not (w/received? o)))
    (is (= (:attempted (check)) (:attempted o)))))
(deftest different-value-before-reader
  (let [o (support/enacted-steps :different)]
    (is (not (w/received? o)))
    (is (= (:attempted (check)) (:attempted o)))))
