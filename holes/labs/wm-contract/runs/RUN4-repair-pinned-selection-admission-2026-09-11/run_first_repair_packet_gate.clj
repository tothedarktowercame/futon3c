;; Disposable lifecycle gate for the first pinned-selection-refused repair
;; admission packet (repair-ea1-78cd8a42, codex-17/zai-1 casting). Structural reuse of the accepted packet roundtrip test
;; (futon3c.wm.run4-successor-v2-selection-packet-roundtrip-test): recreates
;; the first-repair verification through the real verifier in an isolated checkout at
;; the pinned source HEAD with the REAL zai-1 Agency review job fetched live,
;; then exercises the new packet's cohort activation, materialized config,
;; historical-action applicability, and a disposable queue tick with a zai
;; roster. No reader is weakened; no live capacity, queue, or store touched.
(ns run-first-repair-packet-gate
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
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

(def packet-root "holes/labs/wm-contract/runs/RUN4-repair-pinned-selection-admission-2026-09-11/")
(def cohort-source (str packet-root "authority/holes/labs/wm-contract/runs/RUN4-repair-pinned-selection-admission-2026-09-11/cohort.edn"))
(def finding-source "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-ea1-78cd8a42e23392a5ce1412e49d181c3e7c3ac95aaa40d4f54c5c9d8ef3ae08eb--attempt-001-pinned-selection-refused.edn")
(def finding-sha "07129f197c22d25731d15e07befa0593d1c25665c7e09f7a2ca9371de9681267")
(defn head [] (str/trim (:out (clojure.java.shell/sh "git" "-C" "/home/joe/code/futon2" "rev-parse" "HEAD"))))
(def review-job-id "invoke-1789156072050-20265-61d62304")

(defn tmp [] (.toFile (java.nio.file.Files/createTempDirectory "first-repair-gate" (make-array java.nio.file.attribute.FileAttribute 0))))
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
        quals (io/file root "qualification-unused")
        out (doto (io/file root "verification") .mkdir)
        source-repo (io/file root "futon2-source")
        _ (assert (zero? (:exit (shell/sh "git" "clone" "--shared" "--no-checkout" "/home/joe/code/futon2" (.getPath source-repo)))))
        _ (assert (zero? (:exit (shell/sh "git" "-C" (.getPath source-repo) "checkout" "--detach" (head)))))
        finding (io/file findings "repair-ea1-78cd8a42e23392a5ce1412e49d181c3e7c3ac95aaa40d4f54c5c9d8ef3ae08eb--attempt-001-pinned-selection-refused.edn")
        _ (io/copy (io/file finding-source) finding)
        _ (assert (= finding-sha (digest/sha256 (slurp finding))))
        obligation (edn/read-string (slurp finding))
        verification-id "repair-ea1-78cd8a42-revalidation-20260911-v3"
        qualification-path "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-pinned-selection-admission-2026-09-11/offline-evidence/repair-ea1-78cd8a42-revalidation-20260911-v3.qualification.edn"
        qualification-sha "629ae20a3530ce4cacd340b960e9fdd91e0d434f6cc1315e556da40dc67c289f"
        record (v/admit! {:finding-root (.getPath findings)
                          :qualification-root "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-pinned-selection-admission-2026-09-11/offline-evidence"
                          :qualification-source-root "/home/joe/code"
                          :output-root (.getPath out) :source-repo "/home/joe/code/futon2"
                          :finding-path (.getPath finding) :finding-sha256 finding-sha
                          :qualification-path qualification-path :qualification-sha256 qualification-sha
                          :expected-check-ids [:authenticated-run4-pin-enters-normal-gated-runner-path
                                               :pinned-mission-identity-retains-real-proposer-action]
                          :first-commit "0d1e203cdcf8528614b50c0a176df006a84fb329"
                          :last-commit (head)
                          :source-head (head)
                          :verification-id verification-id
                          :author "codex-17" :reviewer "zai-1"
                          :review-job-id review-job-id :review-job-reader fetch-job})
        vf (io/file out (str verification-id ".verification.edn"))
        candidate (repair/historical-verification-candidate (.getPath store)
                                                            {:verification-root (.getPath out)
                                                             :path (.getPath vf)
                                                             :sha256 (digest/sha256 (slurp vf))})
        entry (full/historical-revalidation-entry obligation candidate
                                                  {:author "codex-17" :repair-reviewer "zai-1"})
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
                                 :cohort-id :run4-repair-pinned-selection-admission-20260911-v1
                                 :sha256 (digest/sha256 (slurp cp))}
              :cohort-preflight! cohort/execution-preflight}
        materialize deployment/materialize
        actual full/run-opportunity!]
    (try
      (assert (some? entry) "historical entry not selected under zai casting")
      (assert (= {:author "codex-17" :reviewer "zai-1"} (:actors candidate)))
      (with-redefs [u/template-path (str packet-root "server-config.disabled.edn")
                    deployment/materialize (fn [text dependencies] (materialize text (merge dependencies deps)))
                    full/run-opportunity!
                    (fn [opts]
                      (actual (merge opts (ft/isolated-runner-opts)
                                     {:cohort? true
                                      :roster-fn (fn [_] {:codex-17 {:status "idle" :invoke-ready? true}
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
                   queue-config {:queue-id "first-repair-gate-queue"
                                 :state-root (.getPath queue-root)
                                 :visibility-file (.getPath (io/file queue-root "visibility.json"))
                                 :interval-ms 600000 :await-timeout-ms 900000
                                 :entries [{:entry-id "repair-pinned-selection-historical-admission"
                                            :series-id "run4-repair-pinned-selection-admission-20260911-v1"
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
                     result {:gate "run4-first-repair-packet-gate"
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
                 (assert (= {:repair-id "repair-ea1-78cd8a42e23392a5ce1412e49d181c3e7c3ac95aaa40d4f54c5c9d8ef3ae08eb--attempt-001-pinned-selection-refused"} applicable))
                 (assert (= :held (:status observed)))
                 (assert (= :terminal-evidence-incomplete (:reason observed)))
                 (assert (= {:author "codex-17" :reviewer "zai-1" :repair-reviewer "zai-1"} (get-in cfg [:run4 :casting])))
                 (assert (= :verified (:binding-status binding)))
                 (assert (= :historical-verification-awaiting-validation (:outcome binding)))
                 (assert (= verification-id (get-in projection [:repair-transition :verification-id])))
                 (assert (= :awaiting-validation (get-in projection [:repair-transition :repair/status])))))))))

      (finally (delete-tree! root)))))

(hermetic/with-hermetic-stores run-gate)
