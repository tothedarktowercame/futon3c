(ns futon3c.wm.run4-u88-roundtrip-test
  "Actual U88 template/config and async evidence chain in disposable storage.
  Task core and environment reads are fixture ports; no worker execution,
  production mission activation, current-environment attestation or acceptance
  is claimed. Source files and C artifacts are copied from the committed packet."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [futon2.aif.mission-registry :as missions]
            [futon3c.wm.run4-deployment-config :as deployment]
            [futon3c.wm.run4-report-service :as report]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-runner :as full-runner]
            [futon3c.agency.registry :as registry]
            [futon3c.transport.http :as http]
            [futon3c.wm.run4-effective-environment :as effective]
            [futon3c.wm.run4-trusted-entry :as trusted]
            [futon3c.wm.runner-service :as runner]))


(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn- request [payload headers]
  {:request-method :post :uri "/api/alpha/wm/run4/series/step"
   :headers headers
   :body (java.io.ByteArrayInputStream.
          (.getBytes (json/generate-string payload) "UTF-8"))})





(def token (apply str (repeat 64 "c")))
(def auth {"authorization" (str "Bearer " token)})
(def casting {:author "zai-2" :reviewer "codex-12" :repair-reviewer "codex-17"})
(def template-path "holes/labs/wm-contract/runs/RUN4-U88-deployment-2026-09-10/server-config.disabled.edn")
(defn- with-service [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "u88-roundtrip" (make-array java.nio.file.attribute.FileAttribute 0)))
        t (edn/read-string (slurp template-path))
        manifest-ref (get-in t [:manifest :ref])
        read-local #(edn/read-string (slurp (io/file root %)))
        write-local (fn [p v] (io/make-parents (io/file root p)) (spit (io/file root p) (pr-str v)))
        hash-local #(digest/sha256 (slurp (io/file root %)))]
    (try
      (doseq [p (conj (into (:source-allowlist t) (:pin-allowlist t)) manifest-ref)]
        (io/make-parents (io/file root p))
        (io/copy (io/file (:authority-root t) p) (io/file root p)))
      (let [mission-ref (first (filter #(str/ends-with? % "M-u88-contextual-preferences.md") (:source-allowlist t)))
            mission-text (slurp (io/file root mission-ref))
            parser-path (io/file root "parser/futon2/holes/missions/M-u88-contextual-preferences.md")
            _ (io/make-parents parser-path)
            _ (spit parser-path mission-text)
            mission (first (:missions (missions/load-missions (.getPath (io/file root "parser")))))
            pin-ref (first (:pin-allowlist t))
            pin (update (read-local pin-ref) :sources #(mapv (fn [p] (assoc p :sha256 (hash-local (:path p)))) %))
            _ (write-local pin-ref pin)
            manifest (-> (read-local manifest-ref)
                         (update :source-pins #(mapv (fn [p] (assoc p :sha256 (hash-local (:path p)))) %))
                         (assoc-in [:trials 0 :pin-sha256] (hash-local pin-ref))
                         (assoc-in [:trials 0 :packet :sha256] (hash-local pin-ref)))
            _ (write-local manifest-ref manifest)
            stores (into {} (for [[k _] (:stores t)] [k (.getPath (doto (io/file root (str "store-" (name k))) .mkdirs))]))
            cm (get-in t [:reserved-unwired :control-map-ref])
            _ (io/make-parents (io/file root cm))
            _ (io/copy (io/file (get-in t [:reserved-unwired :control-map-root]) cm) (io/file root cm))
            t (-> t (assoc :authority-root (.getPath root) :stores stores)
                  (assoc-in [:manifest :sha256] (hash-local manifest-ref))
                  (assoc-in [:reserved-unwired :control-map-root] (.getPath root)))
            cfg (deployment/materialize (pr-str t)
                 {:credential (constantly token) :enable? true
                  :resolve-mission #(when (= (:id mission) %) mission)
                  :action-admissible? (fn [m a] (and (= (:id mission) (:id m))
                     (= {:type :advance-mission :target (:id mission)} a)))})]
        (is (= :open (:status-class mission)))
        (binding [trusted/*attest-effective-environment*
                  #(effective/attest % {:env-read (constantly "1") :var-read (constantly true)})]
          (f root cfg)))
      (finally (delete-tree! root)))))

(deftest async-wrapper-persists-to-reader-roots-and-terminal-roundtrips
  (with-service
    (fn [root cfg]
      (reset! runner/!status runner/initial-status)
      (registry/reset-registry!)
      (registry/register-agent!
       {:agent-id {:id/value "war-machine" :id/type :apparatus}
        :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}})
      (let [handler (http/make-handler cfg)
            payload {:run4-series-ref (get-in cfg [:run4 :series :manifest-ref])}
            seen-opts (atom nil)
            core
            (fn [opts]
              (reset! seen-opts opts)
              (let [action {:type :advance-mission :target "M-u88-contextual-preferences"}
                    judgment {:decision {:action {:type :no-op}}
                              :ranked-actions [{:rank 1 :action action}]
                              :admissible-actions [{:rank 1 :action action}]}
                    selected (full-runner/resolve-pinned-selection
                              opts judgment (select-keys opts (keys casting)))
                    identity (:identity selected)]
                {:attempt-id "worker-internal-attempt"
                 :outcome :grounded-change
                 :checkpoints
                 {:selection {:judgment {:outcome :ok}
                              :ground {:kind :wm-judgement :run4/task-pin identity
                                       :run4/operator-selection
                                       (:provenance selected)}}
                  :construction {:judgment {:run4/task-pin identity}
                                 :ground {:kind :decision-pinned-construction
                                          :run4/task-pin identity}}
                  :dispatch {:judgment {:agent "zai-2" :availability :invoke-ready
                                        :job-id "author-job-1"}
                             :ground {:kind :agency-dispatch}}
                  :build {:judgment {:commits ["commit-1"]
                                     :validation
                                     {:approved? true :review-job "review-job-1"
                                      :review-gate {:required? true :executed? true
                                                    :tool-events 2 :passed? true}}}
                          :ground {:kind :git-commit-and-independent-review}}
                  :adjudication
                  {:judgment {:build-match {:commit "commit-1"
                                            :review-approved? true}
                              :dial {:moved? true :implementation-id "impl-1"}}
                   :ground {:kind :authoritative-substrate-discharge}}}
                 :data {:commit "commit-1"
                        :author-job {:job-id "author-job-1"}
                        :review-job {:job-id "review-job-1"}
                        :witness {:resolved? true :dial-moved? true
                                  :implementation-id "impl-1"}}
                 :wm/route [{:node :R20 :via "scan" :at "2026-09-10T12:00:00Z"}
                            {:node :R12 :via "select" :at "2026-09-10T12:00:01Z"}] }))]
        (binding [full-runner/*wm-status-reporting?* false]
          (with-redefs-fn
            {#'full-runner/run-opportunity-core! core}
            (fn []
              (let [started-response (handler (request payload auth))
                    started-body (json/parse-string (:body started-response) true)
                    click-id (:click-id started-body)]
                (is (= "trial-started" (:status started-body)))
                (is (= :completed (:status (runner/await-click! click-id))))
                ;; Absent sheet option retains the full-loop default.
                (is (true? (:cohort? @seen-opts)))
                (is (true? (:ruled-outcome-c-enabled? @seen-opts)))
                (is (= {:anticipation 3 :cascade-rollout 5} (:policy-depth @seen-opts)))
                (is (= (.getCanonicalPath (io/file root "store-run-records"))
                       (.getCanonicalPath (io/file (:run-record-dir @seen-opts)))))
                (is (.isFile (io/file root "store-bindings"
                                      (str "click-run-binding-" click-id ".edn"))))
                (is (.isFile (io/file root "store-projections"
                                      (str "run4-terminal-projection-" click-id ".edn"))))
                (is (seq (.listFiles (io/file root "store-run-records"))))
                (let [run-record (->> (.listFiles (io/file root "store-run-records"))
                                      first slurp edn/read-string)]
                  (is (= :wm/run4-effective-environment-attestation-v1
                         (get-in run-record
                                 [:run4/effective-environment-attestation
                                  :schema]))))
                (let [terminal-response (handler (request payload auth))
                      terminal-body (json/parse-string (:body terminal-response) true)]
                  (is (= 200 (:status terminal-response)))
                  (is (= "trial-terminal" (:status terminal-body)))
                  (is (= "succeeded" (:task-result terminal-body)))
                  (is (.isFile (io/file root "store-recordings"
                                        "u88-trial-1-attempt-1.edn")))
                  (let [visible (json/parse-string
                                 (slurp (io/file root "store-visibility/run-visibility.json")) true)]
                    (is (= "wm/run-visibility-v1" (:schema visible)))
                    (is (= ["complete" "passed"] ((juxt :stage :result) visible))))
                  (is (.isFile (io/file root "store-controller-and-admission" "001-terminal.edn")))
                  (let [before (into {} (for [p (file-seq root) :when (.isFile p)] [(.getPath p) (slurp p)]))
                        result (report/report! cfg auth)]
                    (is (false? (:accepted? result)))
                    (is (thrown? clojure.lang.ExceptionInfo (report/report! cfg {})))
                    (is (= :operator-decision-required (:decision result)) (pr-str result))
                    (is (= before (into {} (for [p (file-seq root) :when (.isFile p)] [(.getPath p) (slurp p)]))))
                    (io/delete-file (io/file root "store-recordings/u88-trial-1-attempt-1.edn"))
                    (is (= :missing-realized-recording (:decision (report/report! cfg auth))))))))))))))
