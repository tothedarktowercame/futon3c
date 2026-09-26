(ns futon3c.agency.invoke-controls-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.invoke-controls :as ic]))

(deftest register-interrupt-deregister-cycle
  (testing "registering then interrupting calls the control; deregistration is token-guarded"
    (let [test-agent (str "ic-test-agent-" (gensym))
          hits (atom 0)]
      (ic/register! test-agent "tok-1" {:interrupt! (fn [] (swap! hits inc) {:ok true})})
      (is (map? (ic/control-for test-agent)))
      (is (:ok (ic/interrupt! test-agent)))
      (is (= 1 @hits))
      ;; stale token must not remove the live control
      (ic/deregister! test-agent "stale-token")
      (is (map? (ic/control-for test-agent)))
      ;; unknown agent reports no-active-control
      (is (= :no-active-control (:action (ic/interrupt! "ic-unknown-agent"))))
      ;; snapshot exposes metadata but not the control fn
      (is (not (contains? (get (ic/snapshot) test-agent) :control)))
      (ic/deregister! test-agent "tok-1")
      (is (nil? (ic/control-for test-agent))))))
