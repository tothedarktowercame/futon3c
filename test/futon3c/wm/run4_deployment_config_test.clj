(ns futon3c.wm.run4-deployment-config-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-deployment-config :as sut]
            [futon3c.wm.run4-series-service :as service]
            [futon3c.wm.runner-service :as runner]))

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

(deftest actual-materialized-u88-template-is-inert-before-runner
  (let [calls (atom 0)
        c (sut/materialize text deps)]
    (with-redefs [runner/click! (fn [_] (swap! calls inc))]
      (is (= :run4-series-disabled
             (:reason (try (service/step! c {} {:run4-series-ref
                                                (get-in c [:run4 :series :manifest-ref])}) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (is (zero? @calls)))))

(deftest malformed-template-and-unprovisioned-enable-refuse
  (is (= :invalid-deployment-contract
         (:reason (try (sut/materialize (str text "\n{:foreign true}") deps) nil
                       (catch Throwable e (ex-data e))))))
  (is (= :credential-unprovisioned
         (:reason (try (sut/materialize text (assoc deps :enable? true)) nil
                       (catch Throwable e (ex-data e)))))))
