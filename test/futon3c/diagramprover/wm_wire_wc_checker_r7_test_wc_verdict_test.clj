(ns futon3c.diagramprover.wm-wire-wc-checker-r7-test-wc-verdict-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))
(defn check [] {:writer nil :reader nil})
(def wire {:wire [:wc-checker :r7-test :wc-verdict]
           :kind :unverified :test `the-named-test-does-not-drive-this-writer :check check
           :live-records-read support/live-records-read
           :note "Map finding: wc-checker is siteless; declare its site or withdraw the box. Executable checker evidence does not supply the missing map declaration. selection_reads_fold_test/enactment-receipts is passed [] or a literal join-unverifiable map. It calls increment and selector, never the W_c checker."})
(deftest the-named-test-does-not-drive-this-writer
  (let [text (slurp "../futon2/test/futon2/aif/selection_reads_fold_test.clj")]
    (is (.contains text "join-unverifiable-verdicts-count-zero"))
    (is (.contains text "(enactment-receipts b 2 {:status :join-unverifiable})")))
  (is (not (w/received? (check)))))
