(ns futon3c.diagramprover.wm-wire-flight-click-r9-measured-a-version-status-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-measured-support :as support]))
(defn check [] (support/status-observe))
(def wire {:wire [:flight-click :r9-measured-a-version :status]
           :kind :unverified :test `the-observed-handoff :check check
           :live-records-read support/live-records-read
           :note "Wrong writer: measured-a-version reads sourced-rates status, not http-click-fn status. Replacement: [:r6-sourced-rates :r9-measured-a-version :status]."})
(deftest the-observed-handoff
  (let [o (check)]
    (is (= 409 (:writer o)))
    (is (= :sourced (:reader o)))
    (is (not (w/received? o)))))
