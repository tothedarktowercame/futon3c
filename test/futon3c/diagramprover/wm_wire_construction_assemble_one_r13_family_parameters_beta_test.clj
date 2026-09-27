(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-beta-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-support :as support]))
(defn observe [mutation] (support/family :beta mutation))
(defn check [] (observe :none))
(def wire {:wire [:construction-assemble-one :r13-family-parameters [:beta {:record :cascade-problem}]]
           :kind :witnessed-hermetically
           :test `the-observed-handoff :check check
           :live-records-read support/live-records-read})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o))
    (is (= {:beta 1 :horizon-steps 3} (:family o)))))
(deftest absence-before-reader-fails
  (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
