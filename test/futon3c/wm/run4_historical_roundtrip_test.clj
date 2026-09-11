(ns futon3c.wm.run4-historical-roundtrip-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon2.aif.hermetic-repair-fixture :as hermetic] [clojure.java.io :as io]
            [clojure.edn :as edn] [clojure.string :as str] [clojure.java.shell :as shell]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.repair-obligation :as repair]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.full-loop-runner :as full]
            [futon2.aif.full-loop-runner-test :as ft]
            [futon3c.wm.run4-historical-qualification :as qualification]
            [futon3c.wm.run4-historical-verification :as v]
            [futon3c.wm.run4-u88-roundtrip-test :as u]
            [futon3c.wm.run4-deployment-config :as deployment]
            [futon3c.wm.run4-series-service :as series]
            [futon3c.wm.runner-service :as runner]
            [futon3c.agency.registry :as registry]))
(use-fixtures :once hermetic/with-hermetic-stores)
(defn- tmp [] (.toFile (java.nio.file.Files/createTempDirectory "hist-v" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- write! [f x] (spit f (str (pr-str x) "\n")) f)
(defn with-authority [f]
  (let [root (tmp) store (doto (io/file root "store") .mkdir)
        findings (doto (io/file store "findings") .mkdir) quals (doto (io/file root "q") .mkdir)
        out (doto (io/file root "o") .mkdir)
        source (write! (io/file root "source.edn") {:source :pinned})
        finding (write! (io/file findings "repair-057.edn")
                        {:repair/id "repair-057" :repair/status :open
                         :repair/class :machine-failure :repair/schema-version 3
                         :attempt-id "repair-attempt-057-untyped-failure"})
        plan-file (io/file root "plan.edn")
        plan {:schema :wm/historical-qualification-plan-v1 :verification-id "verify-1"
              :repair-id "repair-057"
              :sources [{:path (.getPath source) :sha256 (digest/sha256 (slurp source))}]
              :checks [{:id :recovery :argv ["/bin/true"] :timeout-ms 2000}
                       {:id :exhaustion :argv ["/bin/true"] :timeout-ms 2000}]}
        _ (write! plan-file plan)
        _ (qualification/produce! {:source-root (.getPath root) :output-root (.getPath quals)
                                   :manifest-path (.getPath plan-file)
                                   :manifest-sha256 (digest/sha256 (slurp plan-file))})
        q (io/file quals "verify-1.qualification.edn")
        qsha (digest/sha256 (slurp q))
        job {:job-id "review-1" :state "done"
             :agent-id "codex-17"
             :result-summary "FULL_LOOP_REVIEW: APPROVE"
             :result (str "HISTORICAL_VERIFICATION_SHA256: " qsha)
             :execution {:executed true :tool-events 1 :command-events 1}}
        opts {:finding-root (.getPath findings) :qualification-root (.getPath quals)
              :qualification-source-root (.getPath root)
              :output-root (.getPath out) :source-repo "/home/joe/code/futon2"
              :finding-path (.getPath finding) :finding-sha256 (digest/sha256 (slurp finding))
              :qualification-path (.getPath q) :qualification-sha256 qsha
              :expected-check-ids [:recovery :exhaustion]
              :first-commit "9ab503bd61be1d63e7a24731e8e8aa285a9e44da"
              :last-commit "3bdc381e76518e69f90077397fa46495da98e61c"
              :source-head (str/trim (:out (shell/sh "git" "-C" "/home/joe/code/futon2"
                                                     "rev-parse" "HEAD")))
              :verification-id "verify-1" :author "zai-2" :reviewer "codex-17"
              :review-job-id "review-1" :review-job-reader (fn [_] job)}]
    (v/admit! opts)
    (let [vf (io/file out "verify-1.verification.edn")
          cp (io/file root "cohort.edn")
          raw (pr-str (-> (edn/read-string (slurp cohort/default-preregistration))
                         (assoc :cohort/id :historical-roundtrip)
                         (assoc-in [:stopping-rule :target] 1)))]
      (spit cp raw)
      (cohort/activate! (.getPath cp) (.getPath root))
      (f {:historical-action {:repair-root (.getPath store)
                              :verification-root (.getPath out)
                              :verification-path (.getPath vf)
                              :verification-sha256 (digest/sha256 (slurp vf))}
          :execution-cohort {:preregistration (.getCanonicalPath cp)
                             :data-root (.getCanonicalPath root)
                             :cohort-id :historical-roundtrip :sha256 (digest/sha256 raw)}
          :cohort-preflight! cohort/execution-preflight}))))

(deftest ^:slow materialized-historical-async-roundtrip
  (with-authority
    (fn [deps]
      (let [materialize deployment/materialize actual full/run-opportunity!]
        (with-redefs [deployment/materialize (fn [text dependencies] (materialize text (merge dependencies deps)))
                      full/run-opportunity!
                      (fn [opts]
                        (actual (merge opts (ft/isolated-runner-opts)
                                       {:cohort? true :roster-fn (fn [_] {:zai-2 {:status "idle" :invoke-ready? true}
                                                            :codex-12 {:status "idle" :invoke-ready? true}
                                                            :codex-17 {:status "idle" :invoke-ready? true}})
                                        :repair-open-fn #(repair/open-obligations (get-in deps [:historical-action :repair-root]))})))]
          (#'u/with-service
           (fn [root cfg]
             (reset! runner/!status runner/initial-status)
             (registry/reset-registry!)
             (registry/register-agent! {:agent-id {:id/value "war-machine" :id/type :apparatus}
                                        :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}})
             (binding [full/*wm-status-reporting?* false]
               (let [request {:run4-series-ref (get-in cfg [:run4 :series :manifest-ref])}
                     start (series/step! cfg u/auth request)]
                 (prn {:review-start start})
                 (is (= :trial-started (:status start)))
                 (prn {:review-completion (runner/await-click! (:click-id start))})
                 (let [observed (series/step! cfg u/auth request)]
                   (prn {:review-observation observed})
                   (is (= :awaiting-terminal-evidence (:status observed)))
                   (is (seq (.listFiles (io/file root "store-recordings"))))
                 (let [binding-file (io/file root "store-bindings" (str "click-run-binding-" (:click-id start) ".edn"))
                       binding (edn/read-string (slurp binding-file))
                       projection (edn/read-string (slurp (get-in binding [:run4/historical-projection :path])))
                       record (edn/read-string (slurp (:run-record binding)))
                       observation (edn/read-string (slurp (first (filter #(str/starts-with? (.getName %) "run4-historical-observation-") (.listFiles (io/file root "store-recordings"))))))
                       visible (json/parse-string (slurp (io/file root "store-visibility/run-visibility.json")) true)
                       cohort-binding (:execution-cohort deps)]
                   (is (= :verified (:binding-status binding)))
                   (is (= :historical-verification-awaiting-validation (:outcome binding)))
                   (is (= (:historical-verification record) (:repair-transition projection)))
                   (is (= :awaiting-validation (get-in projection [:repair-transition :repair/status])))
                   (is (= :authenticated-not-enacted (get-in projection [:requested-pin :status])))
                   (is (not= (:controller-attempt/id projection) (:runner-attempt/id projection)))
                   (is (= :unknown (:task-verdict observation)))
                   (is (= "pending" (:result visible)))
                   (is (not (.exists (io/file root "store-controller-and-admission/001-terminal.edn"))))
                   (is (= :awaiting-terminal-evidence (:status (series/step! cfg u/auth request))))
                   (let [ledger (cohort/ledger (:preregistration cohort-binding) (:data-root cohort-binding))]
                     (is (= 1 (:attempt-count ledger)))
                     (is (= 1 (:closed-count ledger)))))
               ))))))))

)

)
