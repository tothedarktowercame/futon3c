(ns futon3c.wm.run4-historical-visibility-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-historical-projection :as historical]
            [futon3c.wm.run4-run-visibility :as visibility]))

(def packet-root
  "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair058-admission-2026-09-11")

(defn- inputs []
  (edn/read-string (slurp (str packet-root "/LIVE-HISTORICAL-INPUTS.edn"))))

(deftest retained-repair058-bundle-projects-completed-historical-execution
  (let [{:keys [historical-evidence]} (inputs)
        roots (:roots historical-evidence)
        admission (:admission-request historical-evidence)
        started (:started historical-evidence)
        manifest-text (slurp (str packet-root
                                  "/authority/holes/labs/wm-contract/runs/"
                                  "RUN4-repair058-admission-2026-09-11/series-pin.edn"))
        bundle (historical/read-bundle! roots admission started)
        result (visibility/observe
                (:admission roots) manifest-text (constantly nil)
                (str (java.time.Instant/now)) nil
                (fn [actual-started]
                  (is (= started actual-started))
                  (historical/read-bundle! roots admission actual-started)))
        json-roundtrip (json/parse-string (json/generate-string result) true)
        trial (first (:trials json-roundtrip))]
    (is (= :wm/run4-historical-admission-bundle-v1 (:schema bundle)))
    (is (= "review" (:stage json-roundtrip)))
    (is (= "pending" (:result json-roundtrip)))
    (is (nil? (:worker json-roundtrip)))
    (is (= {:author "codex-10" :reviewer "codex-12"
            :repair_reviewer "codex-12" :active_workers []}
           (:assigned_roles json-roundtrip)))
    (is (= "completed" (get-in trial [:historical_execution :status])))
    (is (= "awaiting-successor-validation"
           (get-in trial [:historical_execution :resolution_status])))
    (is (= "repair-attempt-058-untyped-failure"
           (get-in trial [:actual_action :repair_id])))
    (is (= "authenticated-not-enacted"
           (get-in trial [:requested_task :status])))
    (is (= "wm-click-53f7d985-489f-48eb-a8fc-607c9cbab779"
           (get-in trial [:historical_execution :click_id])))
    (is (= "92a46da7-c02c-4177-a535-ac9d91e933e0"
           (get-in trial [:historical_execution :run_id])))))

(deftest historical-metadata-cannot-launder-a-task-success
  (let [{:keys [historical-evidence]} (inputs)
        roots (:roots historical-evidence)
        admission (:admission-request historical-evidence)
        started (:started historical-evidence)
        manifest-text (slurp (str packet-root
                                  "/authority/holes/labs/wm-contract/runs/"
                                  "RUN4-repair058-admission-2026-09-11/series-pin.edn"))
        bundle (historical/read-bundle! roots admission started)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"visibility refused"
         (visibility/observe
          (:admission roots) manifest-text (constantly nil)
          (str (java.time.Instant/now)) nil
          (constantly (assoc-in bundle [:classification :task-result]
                                :succeeded)))))))
