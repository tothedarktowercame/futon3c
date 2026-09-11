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

(deftest materializes-server-owned-execution-cohort-ports
  (let [cohort {:preregistration "/tmp/reviewed-cohort.edn"
                :data-root "/tmp/reviewed-cohort-data"
                :cohort-id :run4-successor
                :sha256 (apply str (repeat 64 "a"))}
        preflight (fn [_] nil)
        c (sut/materialize text
                           (assoc deps :execution-cohort cohort
                                       :cohort-preflight! preflight))]
    (is (= cohort (get-in c [:run4 :execution-cohort])))
    (is (identical? preflight (get-in c [:run4 :cohort-preflight!])))))

(deftest materializes-server-owned-historical-action-authority
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-historical-authority"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        authority {:repair-root (.getPath root)
                   :verification-root (.getPath root)
                   :verification-path (.getPath (java.io.File. root "verification.edn"))
                   :verification-sha256 (apply str (repeat 64 "a"))}
        c (sut/materialize text (assoc deps :historical-action authority))]
    (is (= authority (get-in c [:run4 :historical-action])))))

(deftest materializes-linked-historical-successor-only-with-store-authority
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-historical-successor"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        authority {:repair-root (.getPath root)
                   :verification-root (.getPath root)
                   :verification-path (.getPath (java.io.File. root "verification.edn"))
                   :verification-sha256 (apply str (repeat 64 "a"))}
        link {:repair-id "repair-057"
              :verification-id "verification-057"
              :verification-attempt {:kind :runner-execution :id "verification-attempt-001"}
              :successor {:series-id "run4-successor"
                          :trial-id "trial-002" :attempt-id "attempt-002"}}
        c (sut/materialize text (assoc deps :historical-action authority
                                            :historical-successor link))]
    (is (= link (get-in c [:run4 :historical-successor])))
    (is (= :invalid-deployment-contract
           (:reason (try (sut/materialize text (assoc deps :historical-successor link)) nil
                         (catch clojure.lang.ExceptionInfo e (ex-data e))))))
    (is (= :invalid-deployment-contract
           (:reason (try (sut/materialize
                          text (assoc deps :historical-action authority
                                           :historical-successor (assoc link :foreign true)))
                         nil
                         (catch clojure.lang.ExceptionInfo e (ex-data e))))))
    (is (= :invalid-deployment-contract
           (:reason (try (sut/materialize
                          text (assoc deps :historical-action authority
                                           :historical-successor (assoc link :repair-id false)))
                         nil
                         (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(deftest malformed-template-and-unprovisioned-enable-refuse
  (is (= :invalid-deployment-contract
         (:reason (try (sut/materialize (str text "\n{:foreign true}") deps) nil
                       (catch Throwable e (ex-data e))))))
  (is (= :credential-unprovisioned
         (:reason (try (sut/materialize text (assoc deps :enable? true)) nil
                       (catch Throwable e (ex-data e)))))))
