(ns futon3c.diagramprover.wm-wire-r2-store-coverage-r3-store-coverage-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-support :as support]))
(defn check [] (support/observe :coverage :none))
(def wire {:wire [:r2-store-coverage :r3-store-coverage :coverage]
           :kind :witnessed-hermetically :test `published-value-reaches-reader
           :check check :live-records-read support/live-records-read})
(deftest published-value-reaches-reader
  (is (w/received? (check))))
(deftest removed-file-breaks-handoff
  (let [o (support/observe :coverage :remove)]
    (is (empty? (:reader o)))
    (is (not (w/received? o)))))
(deftest changed-file-breaks-handoff
  (let [o (support/observe :coverage :different)]
    (is (= (:alternative o) (:reader o)))
    (is (not (w/received? o)))))
