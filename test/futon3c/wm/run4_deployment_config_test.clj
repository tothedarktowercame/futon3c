(ns futon3c.wm.run4-deployment-config-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [futon3c.wm.run4-deployment-config :as sut]
            [futon3c.wm.run4-deployment-preflight :as preflight]
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

(deftest materializes-authenticated-server-owned-codex-fold-port
  (let [authority {:schema :wm/codex-fold-authority-v1
                   :root "/server/fold-authority"
                   :plan-ref "plan.edn"
                   :seat "codex-12"
                   :plan-sha256 (apply str (repeat 64 "a"))
                   :agency-base "http://127.0.0.1:7070"
                   :caller "run4-fold"}
        result {:wiring {:boxes []} :coverage-score-delta -1
                :policy-holes []}
        materialize #(with-redefs [preflight/inspect
                                   (constantly {:sources :current
                                                :declaration :supported})]
                       (sut/materialize %1 %2))
        c (materialize
           text (assoc deps :construction-wiring-fn (constantly result)
                            :construction-wiring-authority authority))]
    (is (= (assoc result :fold/authority authority)
           ((get-in c [:run4 :construction-wiring-fn]) {:shown []})))
    (doseq [bad [(dissoc authority :seat)
                 (assoc authority :seat "zai-5")
                 (assoc authority :plan-sha256 "")]]
      (is (= :invalid-deployment-contract
             (:reason (try
                        (materialize
                         text (assoc deps
                                     :construction-wiring-fn (constantly result)
                                     :construction-wiring-authority bad))
                        nil
                        (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))

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
              :verification-cohort {:preregistration "/authority/historical.edn"
                                    :data-root "/authority/historical"
                                    :cohort-id :historical :sha256 (apply str (repeat 64 "a"))}
              :historical-evidence {:roots {} :admission-request {} :started {}}
              :successor {:series-id "run4-successor"
                          :trial-id "trial-002" :attempt-id "attempt-002"}}
        c (sut/materialize text (assoc deps :historical-action authority
                                            :historical-successor link))]
    (is (= link (get-in c [:run4 :historical-successor])))
    (let [successor-only (sut/materialize text (assoc deps :historical-successor link))]
      (is (= link (get-in successor-only [:run4 :historical-successor])))
      (is (not (contains? (:run4 successor-only) :historical-action))))
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

(deftest independently-reviewed-historical-casting-is-materializable
  (let [template (edn/read-string text)
        casting {:author "codex-10" :reviewer "codex-12"
                 :repair-reviewer "codex-12"}
        materialized (sut/materialize (pr-str (assoc template :casting casting)) deps)]
    (is (= casting (get-in materialized [:run4 :casting])))
    (is (= :invalid-deployment-contract
           (:reason (try
                      (sut/materialize
                       (pr-str (assoc template :casting
                                      (assoc casting :repair-reviewer "codex-10"))) deps)
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(deftest frozen-repair057-packet-materializes-disabled
  (let [path "holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/server-config.disabled.edn"
        c (sut/materialize (slurp path) deps)]
    (is (false? (get-in c [:run4 :enabled?])))
    (is (= {:author "codex-10" :reviewer "codex-12" :repair-reviewer "codex-12"}
           (get-in c [:run4 :casting])))
    (is (= "holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/series-pin.edn"
           (get-in c [:run4 :series :manifest-ref])))))

(deftest casting-structure-and-separation-refuse-before-materialization
  (let [template (edn/read-string text)
        good {:author "codex-10" :reviewer "codex-12" :repair-reviewer "codex-12"}]
    (doseq [bad [(dissoc good :repair-reviewer)
                 (assoc good :reviewer "codex-10")
                 (assoc good :repair-reviewer " ")
                 (assoc good :author 10)
                 (assoc good :extra "codex-17")]]
      (is (= :invalid-deployment-contract
             (:reason (try
                        (sut/materialize (pr-str (assoc template :casting bad)) deps)
                        nil
                        (catch clojure.lang.ExceptionInfo e (ex-data e)))))
          (pr-str bad)))))
