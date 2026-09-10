(ns futon3c.wm.run4-deployment-config-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-deployment-config :as sut]))

(def text (slurp "holes/labs/wm-contract/runs/RUN4-U88-deployment-2026-09-10/server-config.disabled.edn"))
(def deps {:credential (constantly nil) :resolve-mission (constantly nil)
           :action-admissible? (constantly false) :enable? false})

(deftest disabled-materialization-is-production-shaped-with-server-ports
  (let [c (sut/materialize text deps)]
    (is (false? (get-in c [:run4 :enabled?])))
    (is (false? (get-in c [:run4 :series :enabled?])))
    (is (fn? (get-in c [:run4 :resolve-mission])))
    (is (not (contains? (:run4 c) :bearer-token)))
    (is (= (get-in c [:run4 :admission-root])
           (get-in c [:run4 :series :controller-root])))))

(deftest malformed-template-and-unprovisioned-enable-refuse
  (is (= :invalid-deployment-contract
         (:reason (try (sut/materialize (str text "\n{:foreign true}") deps) nil
                       (catch Throwable e (ex-data e))))))
  (is (= :credential-unprovisioned
         (:reason (try (sut/materialize text (assoc deps :enable? true)) nil
                       (catch Throwable e (ex-data e)))))))
