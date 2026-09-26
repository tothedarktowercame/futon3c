;; Disposable lifecycle gate for the Zai-staffed successor-v2-selection
;; admission packet. Structural reuse of the accepted packet roundtrip test
;; (futon3c.wm.run4-successor-v2-selection-packet-roundtrip-test): recreates
;; the zai verification through the real verifier in an isolated checkout at
;; the pinned source HEAD with the REAL zai-1 Agency review job fetched live,
;; then exercises the new packet's cohort activation, materialized config,
;; historical-action applicability, and a disposable queue tick with a zai
;; roster. No reader is weakened; no live capacity, queue, or store touched.
(ns run-zai-packet-gate
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.repair-obligation :as repair]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.full-loop-runner :as full]
            [futon2.aif.full-loop-runner-test :as ft]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon3c.wm.run4-historical-action :as historical-action]
            [futon3c.wm.run4-historical-qualification :as qualification]
            [futon3c.wm.run4-historical-verification :as v]
            [futon3c.wm.run4-deployment-config :as deployment]
            [futon3c.wm.run4-series-queue :as queue]
            [futon3c.wm.run4-u88-roundtrip-test :as u]
            [futon3c.wm.runner-service :as runner]
            [futon3c.agency.registry :as registry]))

(def packet-root "holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-zai-admission-2026-09-11/")
(def cohort-source (str packet-root "authority/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-zai-admission-2026-09-11/cohort.edn"))
(def finding-source "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure.edn")
(def finding-sha "b888bcdd80d014c072f60faaa28d912908e4dc7674edce2f50d1ca6b45fc6068")
(def review-job-id "invoke-1789149846493-20228-9dfa3dda")

(defn tmp [] (.toFile (java.nio.file.Files/createTempDirectory "zai-gate" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn delete-tree! [root] (doseq [file (reverse (file-seq root))] (io/delete-file file true)))
(defn write! [file value] (spit file (str (pr-str value) "\n")) file)

(defn fetch-job [id]
  (let [raw (slurp (str "http://localhost:7070/api/alpha/invoke/jobs/" id))
        j (get (json/parse-string raw true) :job)]
    {:job-id (:job-id j) :state (:state j) :agent-id (:agent-id j)
     :result (:result j) :result-summary (:result-summary j)
     :terminal-message (:terminal-message j)
     :events (mapv #(update-keys % keyword) (:events j))
     :execution (update-keys (or (:execution j) {}) keyword)}))

(defn- run-gate []
  (let [root (tmp)
        store (doto (io/file root "store") .mkdir)
        findings (doto (io/file store "findings") .mkdir)
        quals (doto (io/file root "qualification") .mkdir)
        out (doto (io/file root "verification") .mkdir)
        source-repo (io/file root "futon2-source")
        _ (assert (zero? (:exit (shell/sh "git" "clone" "--shared" "--no-checkout" "/home/joe/code/futon2" (.getPath source-repo)))))
        _ (assert (zero? (:exit (shell/sh "git" "-C" (.getPath source-repo) "checkout" "--detach" "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"))))
        finding (io/file findings "repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure.edn")
        _ (io/copy (io/file finding-source) finding)
        _ (assert (= finding-sha (digest/sha256 (slurp finding))))
        obligation (edn/read-string (slurp finding))
        verification-id "repair-successor-v2-selection-revalidation-zai-20260911-v1"
        qualification-path "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-zai-admission-2026-09-11/offline-evidence/repair-successor-v2-selection-revalidation-zai-20260911-v1.qualification.edn"
        qualification-sha "655a867699f6fdd6ca789c903a4c7281e06066c3b20bcd387d45abc20703ea17"
        record (v/admit! {:finding-root (.getPath findings)
                          :qualification-root "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-zai-admission-2026-09-11/offline-evidence"
                          :qualification-source-root "/home/joe/code"
                          :output-root (.getPath out) :source-repo "/home/joe/code/futon2"
                          :finding-path (.getPath finding) :finding-sha256 finding-sha
                          :qualification-path qualification-path :qualification-sha256 qualification-sha
                          :expected-check-ids [:actual-stop-line-selection-controls
                                               :new-admission-and-replay-controls
                                               :exact-target-inspection-and-lock-controls]
                          :first-commit "8788443d7cf0c806261933e2c68009d84f57819d"
                          :last-commit "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"
                          :source-head "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"
                          :verification-id verification-id
                          :author "zai-2" :reviewer "zai-1"
                          :review-job-id review-job-id :review-job-reader fetch-job})
        vf (io/file out (str verification-id ".verification.edn"))
        candidate (repair/historical-verification-candidate (.getPath store)
                                                            {:verification-root (.getPath out)
                                                             :path (.getPath vf)
                                                             :sha256 (digest/sha256 (slurp vf))})
        entry (full/historical-revalidation-entry obligation candidate
                                                  {:author "zai-2" :repair-reviewer "zai-1"})
        cp (io/file root "cohort.edn")
        _ (io/copy (io/file cohort-source) cp)
        _ (assert (empty? (cohort/preregistration-errors (edn/read-string (slurp cp)))) "cohort preregistration errors")
        _ (cohort/activate! (.getPath cp) (.getPath root))
        deps {:historical-action {:repair-root (.getPath store)
                                  :verification-root (.getPath out)
                                  :verification-path (.getPath vf)
                                  :verification-sha256 (digest/sha256 (slurp vf))}
              :execution-cohort {:preregistration (.getCanonicalPath cp)
                                 :data-root (.getCanonicalPath root)
                                 :cohort-id :run4-successor-v2-selection-zai-admission-20260911-v1
                                 :sha256 (digest/sha256 (slurp cp))}
              :cohort-preflight! cohort/execution-preflight}
        materialize deployment/materialize
        actual full/run-opportunity!]
    (try
      (assert (some? entry) "historical entry not selected under zai casting")
      (assert (= {:author "zai-2" :reviewer "zai-1"} (:actors candidate)))
      (with-redefs [u/template-path (str packet-root "server-config.disabled.edn")
                    deployment/materialize (fn [text dependencies] (materialize text (merge dependencies deps)))
                    full/run-opportunity!
                    (fn [opts]
                      (actual (merge opts (ft/isolated-runner-opts)
                                     {:cohort? true
                                      :roster-fn (fn [_] {:zai-2 {:status "idle" :invoke-ready? true}
                                                          :zai-1 {:status "idle" :invoke-ready? true}})
                                      :repair-open-fn #(repair/open-obligations (.getPath store))})))]
        (#'u/with-service
         (fn [srv-root cfg]
           (reset! runner/!status runner/initial-status)
           (registry/reset-registry!)
           (registry/register-agent! {:agent-id {:id/value "war-machine" :id/type "apparatus"}
                                      :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}})
           (binding [full/*wm-status-reporting?* false]
             (let [queue-root (doto (io/file srv-root "queue") .mkdir)
                   queue-config {:queue-id "zai-gate-queue"
                                 :state-root (.getPath queue-root)
                                 :visibility-file (.getPath (io/file queue-root "visibility.json"))
                                 :interval-ms 600000 :await-timeout-ms 900000
                                 :entries [{:entry-id "successor-v2-selection-zai-historical-admission"
                                            :series-id "run4-successor-v2-selection-zai-admission-20260911-v1"
                                            :server-config cfg :headers u/auth
                                            :request {:run4-series-ref (get-in cfg [:run4 :series :manifest-ref])}
                                            :manifest-sha256 (get-in cfg [:run4 :series :manifest-sha256])
                                            :cohort-id (:cohort-id (:execution-cohort deps))
                                            :cohort-sha256 (get-in deps [:execution-cohort :sha256])}]}
                   applicable (historical-action/validate-applicable! (get-in cfg [:run4 :historical-action]))]
               (queue/start! queue-config)
               (let [observed (queue/tick! queue-config)
                     click-id (get-in observed [:in-flight :click-id])
                     binding-file (io/file srv-root "store-bindings" (str "click-run-binding-" click-id ".edn"))
                     binding (edn/read-string (slurp binding-file))
                     projection (edn/read-string (slurp (get-in binding [:run4/historical-projection :path])))
                     result {:gate "run4-zai-packet-gate"
                             :zai-actors (:actors record)
                             :review-job-id (get-in record [:review :job-id])
                             :historical-entry-selected true
                             :validate-applicable applicable
                             :tick-status (:status observed)
                             :tick-reason (:reason observed)
                             :casting (get-in cfg [:run4 :casting])
                             :binding-status (:binding-status binding)
                             :binding-outcome (:outcome binding)
                             :projection-attempt (:controller-attempt/id projection)
                             :projection-verification-id (get-in projection [:repair-transition :verification-id])
                             :projection-status (get-in projection [:repair-transition :repair/status])
                             :recordings (count (.listFiles (io/file srv-root "store-recordings")))}]
                 (queue/stop! queue-config)
                 (println (pr-str result))
                 (assert (= {:repair-id "repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure"} applicable))
                 (assert (= :held (:status observed)))
                 (assert (= :terminal-evidence-incomplete (:reason observed)))
                 (assert (= {:author "zai-2" :reviewer "zai-1" :repair-reviewer "zai-1"} (get-in cfg [:run4 :casting])))
                 (assert (= :verified (:binding-status binding)))
                 (assert (= :historical-verification-awaiting-validation (:outcome binding)))
                 (assert (= verification-id (get-in projection [:repair-transition :verification-id])))
                 (assert (= :awaiting-validation (get-in projection [:repair-transition :repair/status])))))))))

      (finally (delete-tree! root)))))

(hermetic/with-hermetic-stores run-gate)
