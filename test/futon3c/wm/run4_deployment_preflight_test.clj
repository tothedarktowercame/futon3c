(ns futon3c.wm.run4-deployment-preflight-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-deployment-preflight :as sut]))

(def template "holes/labs/wm-contract/runs/RUN4-U88-deployment-2026-09-10/server-config.disabled.edn")

(deftest committed-u88-template-is-current-disabled-and-honest
  (let [r (sut/inspect (slurp template))]
    (is (:template-disabled r))
    (is (= :unprovisioned (:credential r)))
    (is (= :current (:sources r)))
    (is (= :supported (:declaration r)))
    (is (= :unknown-not-loaded (:consumer-state r)))
    (is (= :open-activated (:mission r)))
    (is (false? (:eligible-to-launch? r)))))
