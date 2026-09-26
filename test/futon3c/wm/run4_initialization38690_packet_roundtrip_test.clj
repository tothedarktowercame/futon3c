(ns futon3c.wm.run4-initialization38690-packet-roundtrip-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon2.aif.hermetic-repair-fixture :as hermetic] [clojure.java.io :as io]
            [clojure.edn :as edn] [clojure.string :as str]
            [clojure.java.shell :as shell]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.repair-obligation :as repair]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.full-loop-runner :as full]
            [futon2.aif.full-loop-runner-test :as ft]
            [futon3c.wm.run4-historical-action :as historical-action]
            [futon3c.wm.run4-historical-qualification :as qualification]
            [futon3c.wm.run4-historical-verification :as v]
            [futon3c.wm.run4-u88-roundtrip-test :as u]
            [futon3c.wm.run4-deployment-config :as deployment]
            [futon3c.wm.run4-series-queue :as queue]
            [futon3c.wm.runner-service :as runner]
            [futon3c.agency.registry :as registry]))
(use-fixtures :once hermetic/with-hermetic-stores)
(defn- tmp [] (.toFile (java.nio.file.Files/createTempDirectory "hist-v" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))
(defn- write! [file value]
  (spit file (str (pr-str value) "\n"))
  file)
(def packet-root "holes/labs/wm-contract/runs/RUN4-repair-initialization38690-admission-2026-09-11/")
(def review-root "holes/labs/wm-contract/runs/RUN4-repair-initialization38690-revalidation-2026-09-11/")
(defn with-authority [f]
  (let [root (tmp)
        store (doto (io/file root "store") .mkdir)
        findings (doto (io/file store "findings") .mkdir)
        quals (doto (io/file root "qualification") .mkdir)
        out (doto (io/file root "verification") .mkdir)
        source-repo (io/file root "futon2-source")
        clone-result (shell/sh "git" "clone" "--shared" "--no-checkout"
                               "/home/joe/code/futon2" (.getPath source-repo))
        _ (assert (zero? (:exit clone-result)) (:err clone-result))
        checkout-result (shell/sh "git" "-C" (.getPath source-repo) "checkout"
                                  "--detach" "810be2a9a19d70b054d9ef7ceb43a2349b7a923d")
        _ (assert (zero? (:exit checkout-result)) (:err checkout-result))
        original (edn/read-string (slurp (str review-root "offline-verification/repair-initialization38690-revalidation-20260911-v1.verification.edn")))
        finding (io/file findings (str (:repair-id original) ".edn"))
        _ (io/copy (io/file (get-in original [:finding :path])) finding)
        source-dir (doto (io/file root "sources") .mkdir)
        source-paths (mapv (fn [name]
                             (let [target (io/file source-dir name)]
                               (io/copy (io/file "/home/joe/code/futon2/src/futon2/aif" name)
                                        target)
                               (.getPath target)))
                           ["full_loop_cohort.clj" "full_loop_runner.clj"
                            "repair_obligation.clj"])
        plan-file (io/file root "qualification-plan.edn")
        plan {:schema :wm/historical-qualification-plan-v1
              :verification-id (:verification-id original)
              :repair-id (:repair-id original)
              :sources (mapv (fn [path] {:path path :sha256 (digest/sha256 (slurp path))})
                             source-paths)
              :checks [{:id :current-execution-authority
                        :argv ["/bin/true"] :timeout-ms 2000}]}
        _ (write! plan-file plan)
        _ (qualification/produce! {:source-root (.getPath root)
                                   :output-root (.getPath quals)
                                   :manifest-path (.getPath plan-file)
                                   :manifest-sha256 (digest/sha256 (slurp plan-file))})
        qualification-file (io/file quals (str (:verification-id original) ".qualification.edn"))
        qualification-sha (digest/sha256 (slurp qualification-file))
        job {:job-id "disposable-current-authority-review"
             :state "done" :agent-id "zai-1"
             :result-summary "FULL_LOOP_REVIEW: APPROVE"
             :result (str "HISTORICAL_VERIFICATION_SHA256: " qualification-sha)
             :execution {:executed true :tool-events 1 :command-events 1}}
        opts {:finding-root (.getPath findings)
              :qualification-root (.getCanonicalPath quals)
              :qualification-source-root (.getPath root)
              :output-root (.getPath out) :source-repo (.getPath source-repo)
              :finding-path (.getPath finding) :finding-sha256 (get-in original [:finding :sha256])
              :qualification-path (.getPath qualification-file)
              :qualification-sha256 qualification-sha
              :expected-check-ids [:current-execution-authority]
              :first-commit "8788443d7cf0c806261933e2c68009d84f57819d"
              :last-commit "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"
              :source-head "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"
              :verification-id (:verification-id original)
              :author (get-in original [:actors :author]) :reviewer (get-in original [:actors :reviewer])
              :review-job-id (:job-id job) :review-job-reader (fn [_] job)}
        cp (io/file root "cohort.edn")
        cp-source (io/file (str packet-root "authority/holes/labs/wm-contract/runs/RUN4-repair-initialization38690-admission-2026-09-11/cohort.edn"))]
    (try
      (is (= "5ecd38dd99fa442e0e45e8995c1ca4d668e60b8a447bb052eab05ff3967ce455"
             (digest/sha256 (slurp (str review-root "offline-verification/repair-initialization38690-revalidation-20260911-v1.verification.edn")))))
      (v/admit! opts)
      (io/copy cp-source cp)
      (is (= (slurp cp-source) (slurp cp)))
      (is (empty? (cohort/preregistration-errors (edn/read-string (slurp cp)))))
      (cohort/activate! (.getPath cp) (.getPath root))
      (let [vf (io/file out (str (:verification-id original) ".verification.edn"))]
        (f {:historical-action {:repair-root (.getPath store)
                                :verification-root (.getPath out)
                                :verification-path (.getPath vf)
                                :verification-sha256 (digest/sha256 (slurp vf))}
            :execution-cohort {:preregistration (.getCanonicalPath cp)
                               :data-root (.getCanonicalPath root)
                               :cohort-id :run4-initialization38690-admission-20260911-v1
                               :sha256 (digest/sha256 (slurp cp))}
            :cohort-preflight! cohort/execution-preflight}))
      (finally (delete-tree! root)))))

(deftest ^:slow materialized-historical-async-roundtrip
  (with-authority
    (fn [deps]
      (let [materialize deployment/materialize actual full/run-opportunity!
            dispatches (atom 0)]
        (with-redefs [u/template-path (str packet-root "server-config.disabled.edn")
                      deployment/materialize (fn [text dependencies] (materialize text (merge dependencies deps)))
                      full/run-opportunity!
                      (fn [opts]
                        (actual (merge opts (ft/isolated-runner-opts)
                                       {:cohort? true
                                        :dispatch-fn (fn [& _]
                                                       (swap! dispatches inc)
                                                       (throw (ex-info "Historical admission must not dispatch an agent" {})))
                                        :roster-fn (fn [_] {:codex-17 {:status "idle" :invoke-ready? true}
                                                            :zai-1 {:status "idle" :invoke-ready? true}})
                                        :repair-open-fn #(repair/open-obligations (get-in deps [:historical-action :repair-root]))})))]
          (#'u/with-service
           (fn [root cfg]
             (reset! runner/!status runner/initial-status)
             (registry/reset-registry!)
             (registry/register-agent! {:agent-id {:id/value "war-machine" :id/type :apparatus}
                                        :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}})
             (binding [full/*wm-status-reporting?* false]
               (let [request {:run4-series-ref (get-in cfg [:run4 :series :manifest-ref])}
                     queue-root (doto (io/file root "queue") .mkdir)
                     queue-config {:queue-id "initialization38690-queue"
                                   :state-root (.getPath queue-root)
                                   :visibility-file (.getPath (io/file queue-root "visibility.json"))
                                   :interval-ms 600000 :await-timeout-ms 900000
                                   :entries [{:entry-id "initialization38690"
                                              :series-id "run4-initialization38690-admission-20260911-v1"
                                              :server-config cfg :headers u/auth :request request
                                              :manifest-sha256 (get-in cfg [:run4 :series :manifest-sha256])
                                              :cohort-id (get-in deps [:execution-cohort :cohort-id])
                                              :cohort-sha256 (get-in deps [:execution-cohort :sha256])}]}
                     _ (is (= {:repair-id "repair-initialization-38690d22-879c-47ab-a72a-6aaf2cce85fa-initialization-failed"}
                              (historical-action/validate-applicable!
                               (get-in cfg [:run4 :historical-action]))))]
                 (queue/start! queue-config)
                 (let [observed (queue/tick! queue-config)
                       click-id (get-in observed [:in-flight :click-id])]
                   (is (zero? @dispatches) "Recorded review actors do not incur a new agent invocation")
                   (is (= :held (:status observed)))
                   (is (= :terminal-evidence-incomplete (:reason observed)))
                   (is (string? click-id))
                   (is (seq (.listFiles (io/file root "store-recordings"))))
                 (let [binding-file (io/file root "store-bindings" (str "click-run-binding-" click-id ".edn"))
                       binding (edn/read-string (slurp binding-file))
                       projection (edn/read-string (slurp (get-in binding [:run4/historical-projection :path])))
                       record (edn/read-string (slurp (:run-record binding)))
                       observation (edn/read-string (slurp (first (filter #(str/starts-with? (.getName %) "run4-historical-observation-") (.listFiles (io/file root "store-recordings"))))))
                       visible (json/parse-string (slurp (io/file root "store-visibility/run-visibility.json")) true)
                       cohort-binding (:execution-cohort deps)]
                   (is (= {:author "codex-17" :reviewer "zai-1" :repair-reviewer "zai-1"}
                          (get-in cfg [:run4 :casting])))
                   (is (= "initialization38690-historical-admission-001" (:controller-attempt/id projection)))
                   (is (= "repair-initialization38690-revalidation-20260911-v1"
                          (get-in projection [:repair-transition :verification-id])))
                   (is (= :run4-initialization38690-admission-20260911-v1 (:cohort-id cohort-binding)))
                   (is (= :verified (:binding-status binding)))
                   (is (= :historical-verification-awaiting-validation (:outcome binding)))
                   (is (= (:historical-verification record) (:repair-transition projection)))
                   (is (= :awaiting-validation (get-in projection [:repair-transition :repair/status])))
                   (is (= :authenticated-not-enacted (get-in projection [:requested-pin :status])))
                   (is (not= (:controller-attempt/id projection) (:runner-attempt/id projection)))
                   (is (= :unknown (:task-verdict observation)))
                   (is (= "pending" (:result visible)))
                   (is (not (.exists (io/file root "store-controller-and-admission/001-terminal.edn"))))
                   (is (= :held (:status (queue/tick! queue-config))))
                   (is (= "repair-initialization-38690d22-879c-47ab-a72a-6aaf2cce85fa-initialization-failed"
                          (:repair/id (first (repair/open-obligations
                                              (get-in deps [:historical-action :repair-root]))))))
                   (is (nil? (:repair/resolution
                              (get (#'repair/verified-admissions
                                    (get-in deps [:historical-action :repair-root]))
                                   "repair-initialization-38690d22-879c-47ab-a72a-6aaf2cce85fa-initialization-failed"))))
                   (let [ledger (cohort/ledger (:preregistration cohort-binding) (:data-root cohort-binding))]
                     (is (= 1 (:attempt-count ledger)))
                     (is (= 1 (:closed-count ledger)))
                     (is (= 0 (:remaining (cohort/execution-preflight cohort-binding false)))))
                   (queue/stop! queue-config))
               ))))))))

)

)
