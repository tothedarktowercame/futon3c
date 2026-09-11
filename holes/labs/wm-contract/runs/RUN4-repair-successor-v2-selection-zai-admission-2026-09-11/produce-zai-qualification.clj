;; Zai-staffed successor-v2-selection qualification producer.
;; Runs the real run4-historical-qualification/produce! ONCE with the exact
;; three-check population of the accepted successor-selection qualification,
;; against explicit CURRENT source pins (bytes refreshed for new evidence;
;; futon2 source HEAD 810be2a9 unchanged). Records before/after source audit.
;; Read-only toward runtime sources; writes only under this packet directory.
(ns run4-zai-qualification-producer
  (:require [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-qualification :as qualification]))

(def sources
  ["/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure.edn"
   "/home/joe/code/futon2/src/futon2/aif/full_loop_runner.clj"
   "/home/joe/code/futon2/src/futon2/aif/repair_obligation.clj"
   "/home/joe/code/futon3c/src/futon3c/wm/run4_deployment_config.clj"
   "/home/joe/code/futon3c/src/futon3c/wm/run4_historical_action.clj"
   "/home/joe/code/futon3c/src/futon3c/wm/run4_trusted_entry.clj"
   "/home/joe/code/futon3c/src/futon3c/wm/run4_series_service.clj"
   "/home/joe/code/futon3c/src/futon3c/wm/run4_series_controller.clj"
   "/home/joe/code/futon3c/test/futon3c/wm/run4_historical_verification_test.clj"
   "/home/joe/code/futon3c/test/futon3c/wm/run4_trusted_entry_test.clj"
   "/home/joe/code/futon3c/test/futon3c/wm/run4_series_service_test.clj"])

(def verification-id "repair-successor-v2-selection-revalidation-zai-20260911-v1")
(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-zai-admission-2026-09-11")

(defn audit []
  (into {} (map (juxt identity #(digest/sha256 (slurp %))) sources)))

(defn -main [& _]
  (let [before (audit)
        plan-file (io/file packet "qualification-plan-zai.disabled.edn")
        plan {:schema :wm/historical-qualification-plan-v1
              :verification-id verification-id
              :repair-id "repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure"
              :sources (mapv (fn [p] {:path p :sha256 (before p)}) sources)
              :checks [{:id :actual-stop-line-selection-controls
                        :argv ["bash" "-lc"
                               "cd /home/joe/code/futon3c && clojure -M:test:test-all -n futon3c.wm.run4-historical-verification-test"]
                        :timeout-ms 120000}
                       {:id :new-admission-and-replay-controls
                        :argv ["bash" "-lc"
                               "cd /home/joe/code/futon3c && clojure -M:test:test-all -n futon3c.wm.run4-trusted-entry-test"]
                        :timeout-ms 120000}
                       {:id :exact-target-inspection-and-lock-controls
                        :argv ["bash" "-lc"
                               "cd /home/joe/code/futon3c && clojure -M:test:test-all -n futon3c.wm.run4-series-service-test"]
                        :timeout-ms 180000}]}
        _ (spit plan-file (str (pr-str plan) "\n"))
        _ (qualification/produce! {:source-root "/home/joe/code"
                                   :output-root (str packet "/offline-evidence")
                                   :manifest-path (.getPath plan-file)
                                   :manifest-sha256 (digest/sha256 (slurp plan-file))})
        after (audit)
        qfile (io/file packet "offline-evidence" (str verification-id ".qualification.edn"))
        drift (filter (fn [[p h]] (not= h (after p))) before)]
    (println {:verification-id verification-id
              :source-drift (vec (map first drift))
              :before-eq-after (empty? drift)
              :qualification-path (.getPath qfile)
              :qualification-sha256 (digest/sha256 (slurp qfile))})))
