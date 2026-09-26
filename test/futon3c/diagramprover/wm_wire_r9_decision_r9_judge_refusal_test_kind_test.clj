(ns futon3c.diagramprover.wm-wire-r9-decision-r9-judge-refusal-test-kind-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))
(defn check [] {:writer nil :reader nil})
(def wire {:wire [:r9-decision :r9-judge-refusal-test :kind]
           :kind :unverified :test `the-named-test-does-not-drive-this-writer :check check
           :live-records-read support/live-records-read
           :note "judge_refusal_abstention_test/run installs judge-fn throwing a supplied exception. The tests build ex-info or replay fixture failure-data, not cascade-decision-admitted output."})
(deftest the-named-test-does-not-drive-this-writer
  (let [text (slurp "../futon2/test/futon2/aif/judge_refusal_abstention_test.clj")]
    (is (.contains text "the-family-refusal-is-the-ticks-typed-abstention"))
    (is (.contains text ":judge-fn (fn [_] (throw judge-throws))")))
  (is (not (w/received? (check)))))
