(ns futon3c.wm.run4-historical-packet-roundtrip-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon2.aif.hermetic-repair-fixture :as hermetic] [clojure.java.io :as io]
            [clojure.edn :as edn] [clojure.string :as str]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.repair-obligation :as repair]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.full-loop-runner :as full]
            [futon2.aif.full-loop-runner-test :as ft]
            [futon3c.wm.run4-historical-verification :as v]
            [futon3c.wm.run4-u88-roundtrip-test :as u]
            [futon3c.wm.run4-deployment-config :as deployment]
            [futon3c.wm.run4-series-service :as series]
            [futon3c.wm.runner-service :as runner]
            [futon3c.agency.registry :as registry]))
(use-fixtures :once hermetic/with-hermetic-stores)
(defn- tmp [] (.toFile (java.nio.file.Files/createTempDirectory "hist-v" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))
(def packet-root "holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/")
(def review-root "holes/labs/wm-contract/runs/RUN4-U88-successor-deployment-2026-09-11/")
(defn with-authority [f]
  (let [root (tmp)
        store (doto (io/file root "store") .mkdir)
        findings (doto (io/file store "findings") .mkdir)
        out (doto (io/file root "verification") .mkdir)
        original (edn/read-string (slurp (str review-root "repair-057-revalidation-20260911-v1.verification.edn")))
        finding (io/file findings (str (:repair-id original) ".edn"))
        _ (io/copy (io/file (get-in original [:finding :path])) finding)
        q (:qualification original)
        job (:job (json/parse-string (slurp (str review-root "repair-057-independent-review-job.json")) true))
        impl (:implementation original)
        opts {:finding-root (.getPath findings)
              :qualification-root (.getCanonicalPath (io/file review-root))
              :qualification-source-root "/home/joe/code/futon2"
              :output-root (.getPath out) :source-repo "/home/joe/code/futon2"
              :finding-path (.getPath finding) :finding-sha256 (get-in original [:finding :sha256])
              :qualification-path (:path q) :qualification-sha256 (:sha256 q)
              :expected-check-ids (:check-ids q)
              :first-commit (:first impl) :last-commit (:last impl) :source-head (:source-head impl)
              :verification-id (:verification-id original)
              :author (get-in original [:actors :author]) :reviewer (get-in original [:actors :reviewer])
              :review-job-id (:job-id job) :review-job-reader (fn [_] job)}
        cp (io/file root "cohort.edn")
        cp-source (io/file (str packet-root "authority/holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/cohort.edn"))]
    (try
      (is (= "6ca6f397b1f7531c91a0f4b9ab6cbc3d2712eb4b347c3c897b91a62c72958829"
             (digest/sha256 (slurp (str review-root "repair-057-revalidation-20260911-v1.verification.edn")))))
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
                               :cohort-id :run4-repair057-admission-20260911-v1
                               :sha256 (digest/sha256 (slurp cp))}
            :cohort-preflight! cohort/execution-preflight}))
      (finally (delete-tree! root)))))

(deftest ^:slow materialized-historical-async-roundtrip
  (with-authority
    (fn [deps]
      (let [materialize deployment/materialize actual full/run-opportunity!]
        (with-redefs [u/template-path (str packet-root "server-config.disabled.edn")
                      deployment/materialize (fn [text dependencies] (materialize text (merge dependencies deps)))
                      full/run-opportunity!
                      (fn [opts]
                        (actual (merge opts (ft/isolated-runner-opts)
                                       {:cohort? true :roster-fn (fn [_] {:codex-10 {:status "idle" :invoke-ready? true}
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
                   (is (= {:author "codex-10" :reviewer "codex-12" :repair-reviewer "codex-12"}
                          (get-in cfg [:run4 :casting])))
                   (is (= "historical-verification-admission-001" (:controller-attempt/id projection)))
                   (is (= "repair-057-revalidation-20260911-v1"
                          (get-in projection [:repair-transition :verification-id])))
                   (is (= :run4-repair057-admission-20260911-v1 (:cohort-id cohort-binding)))
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
