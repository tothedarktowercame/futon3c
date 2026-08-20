(ns futon3c.apm.frame18-control-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.frame18-control :as control]))

(def valid-preflight-receipt?
  (deref #'futon3c.apm.frame18-control/valid-preflight-receipt?))

(deftest preflight-receipt-is-content-addressed-and-frame-bound
  (let [receipt (edn/read-string (slurp control/preflight-receipt-path))
        action {:frame-id "f18" :problem-id "a97J07"}]
    (is (true? (valid-preflight-receipt? receipt action)))
    (testing "changing either the evidence or the action invalidates the receipt"
      (is (false? (valid-preflight-receipt?
                   (assoc receipt :receipt/blob "wrong") action)))
      (is (false? (valid-preflight-receipt?
                   receipt (assoc action :frame-id "f19")))))))
