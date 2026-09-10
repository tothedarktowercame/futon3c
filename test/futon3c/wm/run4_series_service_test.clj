(ns futon3c.wm.run4-series-service-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-runner :as full-runner]
            [futon3c.agency.registry :as registry]
            [futon3c.transport.http :as http]
            [futon3c.wm.run4-effective-environment :as effective]
            [futon3c.wm.run4-trusted-entry :as trusted]
            [futon3c.wm.runner-service :as runner]))

(def token (apply str (repeat 64 "c")))
(def casting {:author "codex-10" :reviewer "codex-17"
              :repair-reviewer "codex-1"})
(def mission {:id "M-outer-loop-successor" :status-class :active})

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn- write! [root path text]
  (spit (io/file root path) text))

(defn- request [payload headers]
  {:request-method :post :uri "/api/alpha/wm/run4/series/step"
   :headers headers
   :body (java.io.ByteArrayInputStream.
          (.getBytes (json/generate-string payload) "UTF-8"))})

(def auth {"authorization" (str "Bearer " token)})

(defn- with-service [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-series-service"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        source-text "eligible outer-loop issue\n"
        config-text (str (pr-str {:schema :wm/run4-pinned-run-config-v1
                                  :runner-options {:cohort? false
                                                   :accumulate-strategic-habit? false}
                                  :c-fold {:enabled? false}
                                  :serving-declaration
                                  {:required-environment
                                   {"FUTON_WM_FPI_DARK" "1"
                                    "FUTON_WM_BETA_DARK" "1"
                                    "FUTON_WM_TRACE_POLICY_DETAILS" "1"}
                                   :hierarchy {:model :single-level :scope :RUN4}
                                   :recording-requirement
                                   {:contract :wm/realized-recording-v1
                                    :environment
                                    {"FUTON_WM_RECORDING_CONTRACT" "1"}}}}) "\n")
        pin {:schema :wm/run4-task-pin-v1
             :series-id "RUN4-eligible-issues" :trial-id :outer-loop-successor
             :series-order :as-declared
             :candidate-task-ids [:outer-loop-successor]
             :selected-task-id :outer-loop-successor
             :sources [{:path "source.md" :sha256 (digest/sha256 source-text)}]
             :casting casting
             :operator-selection {:mode :operator-selected :operator "Joe"
                                  :authority-ref "reviewed eligible-task series"}
             :config {:path "config.edn" :sha256 (digest/sha256 config-text)}
             :mapping {:mission-id "M-outer-loop-successor"
                       :action {:type :advance-mission
                                :target "M-outer-loop-successor"}}}
        pin-text (pr-str pin)
        manifest {:schema :wm/run4-series-pin-v1
                  :series-id "RUN4-eligible-issues" :status :frozen
                  :order :ordinal
                  :stop-rule :attempt-each-once-even-after-fail-or-block
                  :casting casting
                  :source-pins [{:path "source.md"
                                 :sha256 (digest/sha256 source-text)}]
                  :trials [{:ordinal 1 :trial-id :outer-loop-successor
                            :attempt-id "eligible-attempt-1"
                            :pin-sha256 (digest/sha256 pin-text)
                            :packet {:path "pin.edn"
                                     :sha256 (digest/sha256 pin-text)}}]}
        controller-root (io/file root "controller")
        admission-root controller-root
        binding-root (io/file root "bindings")
        projection-root (io/file root "projections")
        run-record-root (io/file root "run-records")
        visibility-root (io/file root "visibility")
        recording-root (io/file root "recordings")
        cfg {:run4 {:enabled? true :bearer-token token :operator "Joe"
                    :casting casting
                    :admission-root (.getPath admission-root)
                    :pin-root (.getPath root) :pin-allowlist #{"pin.edn"}
                    :source-root (.getPath root)
                    :source-allowlist #{"source.md" "config.edn"}
                    :resolve-mission #(when (= "M-outer-loop-successor" %) mission)
                    :action-admissible? (fn [m action]
                                          (and (= mission m)
                                               (= {:type :advance-mission
                                                   :target "M-outer-loop-successor"}
                                                  action)))
                    :series {:enabled? true
                             :manifest-root (.getPath root)
                             :manifest-ref "series.edn"
                             :manifest-allowlist #{"series.edn"}
                             :controller-root (.getPath controller-root)
                             :binding-root (.getPath binding-root)
                             :projection-root (.getPath projection-root)
                             :run-record-root (.getPath run-record-root)
                             :recording-enabled? true
                             :recording-root (.getPath recording-root)
                             :visibility-enabled? true
                             :visibility-root (.getPath visibility-root)}}}]
    (try
      (doseq [dir [controller-root binding-root projection-root
                   run-record-root visibility-root recording-root]]
        (.mkdir dir))
      (write! root "source.md" source-text)
      (write! root "config.edn" config-text)
      (write! root "pin.edn" pin-text)
      (write! root "series.edn" (pr-str manifest))
      (binding [trusted/*attest-effective-environment*
                (fn [declaration]
                  {:schema :wm/run4-effective-environment-attestation-v1
                   :hierarchy (:hierarchy declaration)
                   :flags (mapv (fn [[flag consumer]]
                                  {:flag flag :required "1" :observed "1"
                                   :effective true :consumer consumer})
                                (sort-by key effective/flag-spec))
                   :recording
                   {:status :not-attested-by-this-component
                    :consumer "holes/labs/wm-contract/wm_step_observe.bb"}})]
        (f root cfg))
      (finally (delete-tree! root)))))

(deftest serving-route-starts-one-explicit-transition-and-resumes-idempotently
  (with-service
    (fn [root cfg]
      (let [handler (http/make-handler cfg)
            clicks (atom [])
            payload {:run4-series-ref "series.edn"}]
        (with-redefs [runner/click! (fn [opts]
                                      (swap! clicks conj opts)
                                      {:click-id "click-series-1"
                                       :started-at "2026-09-10T12:00:00Z"})]
          (let [started (handler (request payload auth))
                waiting (handler (request payload auth))]
            (is (= 200 (:status started)))
            (is (= "trial-started"
                   (:status (json/parse-string (:body started) true))))
            (is (= 200 (:status waiting)))
            (is (= "awaiting-terminal-evidence"
                   (:status (json/parse-string (:body waiting) true))))
            (is (= 1 (count @clicks)))
            (is (= "working" (:stage (json/parse-string
                                       (slurp (io/file root "visibility/run-visibility.json")) true))))
            (is (= casting (select-keys (first @clicks) (keys casting))))
            (is (.isFile (io/file root "controller" "001-started.edn")))
            (is (not (.exists (io/file root "controller" "001-terminal.edn"))))))))))

(deftest visibility-is-disabled-by-absence-without-changing-series-step
  (with-service
    (fn [root cfg]
      (let [cfg (update-in cfg [:run4 :series]
                           dissoc :visibility-enabled? :visibility-root)]
        (with-redefs [runner/click! (fn [_] {:click-id "click-no-visibility"
                                             :started-at "2026-09-10T12:00:00Z"})]
          (is (= 200 (:status ((http/make-handler cfg)
                               (request {:run4-series-ref "series.edn"} auth)))))
          (is (not (.exists (io/file root "visibility/run-visibility.json")))))))))

(deftest disabled-auth-shape-and-source-drift-refuse-before-click
  (with-service
    (fn [root cfg]
      (let [clicks (atom 0)
            payload {:run4-series-ref "series.edn"}]
        (with-redefs [runner/click! (fn [_] (swap! clicks inc))]
          (is (= 403 (:status ((http/make-handler (assoc-in cfg [:run4 :series :enabled?]
                                                            false))
                              (request payload auth)))))
          (is (= 403 (:status ((http/make-handler cfg) (request payload {})))))
          (is (= 403 (:status ((http/make-handler cfg)
                              (request (assoc payload :ports {}) auth)))))
          (is (= 403 (:status ((http/make-handler
                                (assoc-in cfg [:run4 :admission-root]
                                          (.getPath (io/file root "bindings"))))
                               (request payload auth)))))
          (write! root "source.md" "drifted\n")
          (is (= 500 (:status ((http/make-handler cfg) (request payload auth)))))
          (write! root "source.md" "eligible outer-loop issue\n")
          (write! root "config.edn" "{:schema :mutated}\n")
          (is (= 403 (:status ((http/make-handler cfg) (request payload auth)))))
          (is (zero? @clicks))
          (is (not (.exists (io/file root "controller" "eligible-attempt-1")))))))))

(deftest effective-environment-mismatch-refuses-before-click
  (with-service
    (fn [_ cfg]
      (let [clicks (atom 0)]
        (binding [trusted/*attest-effective-environment*
                  (fn [_]
                    (throw (ex-info "effective mismatch"
                                    {:reason :required-observed-effective-mismatch})))]
          (with-redefs [runner/click! (fn [_] (swap! clicks inc))]
            (is (= 500 (:status
                        ((http/make-handler cfg)
                         (request {:run4-series-ref "series.edn"} auth)))))
            (is (zero? @clicks))))))))

(deftest declared-corrupt-terminal-chain-stops-resume-without-redispatch
  (with-service
    (fn [root cfg]
      (let [handler (http/make-handler cfg)
            payload {:run4-series-ref "series.edn"}
            clicks (atom 0)]
        (with-redefs [runner/click! (fn [_]
                                      (swap! clicks inc)
                                      {:click-id "click-corrupt-chain"
                                       :started-at "2026-09-10T12:00:00Z"})]
          (is (= 200 (:status (handler (request payload auth)))))
          (write! (io/file root "bindings")
                  "click-run-binding-click-corrupt-chain.edn" "nil\n")
          (let [response (handler (request payload auth))
                body (json/parse-string (:body response) true)]
            (is (= 500 (:status response)))
            (is (= "run4-terminal-evidence-refused" (:error body)))
            (is (= 1 @clicks))
            (is (not (.exists (io/file root "controller" "001-terminal.edn"))))))))))

(deftest async-wrapper-persists-to-reader-roots-and-terminal-roundtrips
  (with-service
    (fn [root cfg]
      (reset! runner/!status runner/initial-status)
      (registry/reset-registry!)
      (registry/register-agent!
       {:agent-id {:id/value "war-machine" :id/type :apparatus}
        :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}})
      (let [handler (http/make-handler cfg)
            payload {:run4-series-ref "series.edn"}
            seen-opts (atom nil)
            core
            (fn [opts]
              (reset! seen-opts opts)
              (let [action {:type :advance-mission :target "M-outer-loop-successor"}
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
                  :dispatch {:judgment {:agent "codex-10" :availability :invoke-ready
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
                (is (false? (:cohort? @seen-opts)))
                (is (false? (:ruled-outcome-c-enabled? @seen-opts)))
                (is (= (.getCanonicalPath (io/file root "run-records"))
                       (.getCanonicalPath (io/file (:run-record-dir @seen-opts)))))
                (is (.isFile (io/file root "bindings"
                                      (str "click-run-binding-" click-id ".edn"))))
                (is (.isFile (io/file root "projections"
                                      (str "run4-terminal-projection-" click-id ".edn"))))
                (is (seq (.listFiles (io/file root "run-records"))))
                (let [run-record (->> (.listFiles (io/file root "run-records"))
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
                  (is (.isFile (io/file root "recordings"
                                        "eligible-attempt-1.edn")))
                  (let [visible (json/parse-string
                                 (slurp (io/file root "visibility/run-visibility.json")) true)]
                    (is (= "wm/run-visibility-v1" (:schema visible)))
                    (is (= ["complete" "passed"] ((juxt :stage :result) visible))))
                  (is (.isFile (io/file root "controller" "001-terminal.edn"))))))))))))
