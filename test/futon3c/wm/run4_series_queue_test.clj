(ns futon3c.wm.run4-series-queue-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.full-loop-runner :as full-runner]
            [futon3c.agency.registry :as registry]
            [futon3c.wm.run4-series-queue :as queue]
            [futon3c.wm.run4-series-service :as series]
            [futon3c.wm.run4-u88-roundtrip-test :as u]
            [futon3c.wm.runner-service :as runner]))

(defn- temp-root []
  (.toFile (java.nio.file.Files/createTempDirectory
            "run4-queue" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn- fixture [root id]
  (let [authority (doto (io/file root "authority") .mkdir)
        state (doto (io/file root "state") .mkdir)
        visible (doto (io/file root "visibility") .mkdir)
        manifest-text (pr-str {:schema :wm/run4-series-pin-v1 :series-id "series-1"
                               :status :frozen :order :ordinal
                               :stop-rule :attempt-each-once-even-after-fail-or-block
                               :casting {:author "codex-10" :reviewer "codex-12"
                                         :repair-reviewer "codex-12"}
                               :source-pins [{:path "source" :sha256 (apply str (repeat 64 "a"))}]
                               :trials [{:ordinal 1 :trial-id "task-1" :attempt-id "attempt-1"
                                         :pin-sha256 (apply str (repeat 64 "b"))
                                         :packet {:path "task" :sha256 (apply str (repeat 64 "b"))}}]})
        manifest (io/file authority "series.edn")
        _ (spit manifest manifest-text)
        manifest-sha (digest/sha256 manifest-text)
        cohort-sha (apply str (repeat 64 "c"))
        server {:run4 {:enabled? true
                       :casting {:author "codex-10" :reviewer "codex-12"
                                 :repair-reviewer "codex-12"}
                       :execution-cohort {:cohort-id :cohort-1 :sha256 cohort-sha}
                       :series {:enabled? true :manifest-root (.getPath authority)
                                :manifest-ref "series.edn"
                                :manifest-sha256 manifest-sha
                                :manifest-allowlist #{"series.edn"}}}}
        config {:queue-id id :state-root (.getPath state)
                :visibility-file (.getPath (io/file visible "queue.json"))
                :interval-ms 600000 :await-timeout-ms 1000
                :entries [{:entry-id "entry-1" :series-id "series-1"
                           :server-config server :headers {"authorization" "Bearer fixture"}
                           :request {:run4-series-ref "series.edn"}
                           :manifest-sha256 manifest-sha :cohort-id :cohort-1
                           :cohort-sha256 cohort-sha}]}]
    config))

(deftest incomplete-evidence-holds-and-restart-does-not-redispatch
  (let [root (temp-root) config (fixture root "hold-queue") calls (atom 0)
        awaits (atom 0)]
    (try
      (with-redefs [series/step! (fn [& _]
                                   (case (swap! calls inc)
                                     1 {:status :trial-started :click-id "click-1"}
                                     {:status :awaiting-terminal-evidence :click-id "click-1"}))
                    runner/await-click! (fn [_ _] (swap! awaits inc) {:status :completed})]
        (is (= "running" (:controller_state (queue/start! config))))
        (is (= :held (:status (queue/tick! config))))
        (is (= :terminal-evidence-incomplete (:reason (queue/read-state! config))))
        (is (= 2 @calls))
        (is (= :held (:status (queue/tick! config))))
        (is (= 2 @calls))
        ;; Simulate loss of process-local scheduler state without touching the
        ;; durable held record. A restarted process must not redispatch it.
        (let [runtimes (var-get #'queue/!runtimes)
              runtime (get @runtimes "hold-queue")]
          (.shutdownNow ^java.util.concurrent.ScheduledExecutorService
                        (:executor runtime))
          (swap! runtimes dissoc "hold-queue"))
        (is (= "held" (:controller_state (queue/recover! config))))
        (is (= 2 @calls))
        (is (= :held-requires-explicit-resume
               (try (queue/start! config) nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (= "running" (:controller_state (queue/resume! config))))
        (is (= :held (:status (queue/tick! config))))
        (is (= 3 @calls))
        (is (= 2 @awaits))
        (let [visible (json/parse-string (slurp (:visibility-file config)) true)]
          (is (= "held" (:controller_state visible)))
          (is (= "terminal-evidence-incomplete" (:hold_reason visible)))
          (is (= [] (:active_actors visible)))
          (is (= "codex-10" (get-in visible [:assigned_roles :author])))))
      (finally (queue/stop! config) (delete-tree! root)))))

(deftest corrupt-state-authority-and-source-drift-controls
  (let [root (temp-root) config (fixture root "corrupt-queue")
        state-file (io/file (:state-root config) "queue-state.edn")
        manifest (io/file (get-in config [:entries 0 :server-config :run4 :series :manifest-root])
                          "series.edn")
        original (slurp manifest)]
    (try
      (queue/start! config)
      (testing "operator stop remains available after frozen source drift"
        (spit manifest "drift")
        (is (= :stopped (:status (queue/stop! config))))
        (spit manifest original))
      (testing "changed server authority cannot inherit the old cursor"
        (is (= :state-invalid
               (try (queue/read-state!
                     (assoc-in config [:entries 0 :server-config :run4 :binding-root]
                               "/different-authority"))
                    nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
      (testing "cursor movement requires exact completed-entry provenance"
        (let [state (read-string (slurp state-file))]
          (spit state-file (pr-str (assoc state :in-flight
                                          {:entry-id "entry-1" :click-id 7})))
          (is (= :state-invalid
                 (try (queue/read-state! config) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
          (spit state-file (pr-str (assoc state :cursor 1)))
          (is (= :state-invalid
                 (try (queue/read-state! config) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))
      (testing "a state path that is not a regular file refuses"
        (io/delete-file state-file)
        (.mkdir state-file)
        (is (= :state-not-regular-file
               (try (queue/read-state! config) nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
      (finally (delete-tree! root)))))

(deftest explicit-stop-never-enters-series-service
  (let [root (temp-root) config (fixture root "stop-queue") calls (atom 0)]
    (try
      (with-redefs [series/step! (fn [& _] (swap! calls inc))]
        (queue/start! config)
        (is (= :stopped (:status (queue/stop! config))))
        (is (= 0 @calls))
        (is (= :stopped (:status (queue/tick! config))))
        (is (= 0 @calls)))
      (finally (delete-tree! root)))))

(deftest actual-series-service-refusal-is-a-durable-hold
  (let [root (temp-root) config (fixture root "actual-boundary-queue")]
    (try
      ;; This deliberately incomplete disposable server configuration reaches
      ;; the real service boundary and must fail closed before any click port.
      (queue/start! config)
      (is (= :held (:status (queue/tick! config))))
      (is (= :series-step-refused (:reason (queue/read-state! config))))
      (is (= "held" (:controller_state (queue/status config))))
      (finally (queue/stop! config) (delete-tree! root)))))

(deftest restart-recovery-and-unknown-evidence-are-fail-closed
  (let [root (temp-root) config (fixture root "recover-queue") calls (atom 0)]
    (try
      (with-redefs [series/step! (fn [& _]
                                   (swap! calls inc)
                                   {:status :unsupported-state})]
        (queue/start! config)
        (let [runtimes (var-get #'queue/!runtimes)
              runtime (get @runtimes "recover-queue")]
          (.shutdownNow ^java.util.concurrent.ScheduledExecutorService
                        (:executor runtime))
          (swap! runtimes dissoc "recover-queue"))
        (is (= "running" (:controller_state (queue/recover! config))))
        (is (= 0 @calls))
        (is (= :held (:status (queue/tick! config))))
        (is (= :unknown-series-state (:reason (queue/read-state! config))))
        (is (= 1 @calls))
        (is (= :held (:status (queue/tick! config))))
        (is (= 1 @calls)))
      (finally (queue/stop! config) (delete-tree! root)))))

(deftest lost-process-promise-uses-exact-existing-inspection
  (let [root (temp-root) config (fixture root "lost-promise-queue")
        inspected (atom [])]
    (try
      (queue/start! config)
      (#'queue/persist! config
                         (assoc (queue/read-state! config) :in-flight
                                {:entry-id "entry-1" :click-id "lost-click"}))
      (reset! (var-get #'runner/!completion) nil)
      (with-redefs [series/step! (fn [& _]
                                   (throw (ex-info "must not admit" {})))
                    series/inspect-started! (fn [& args]
                                              (swap! inspected conj args)
                                              {:status :trial-terminal})]
        (is (= :running (:status (queue/tick! config))))
        (is (= 1 (count @inspected)))
        (is (= "lost-click" (last (first @inspected))))
        (is (nil? (:in-flight (queue/read-state! config)))))
      (finally (queue/stop! config) (delete-tree! root)))))

(deftest strict-authority-and-finite-series-advancement
  (let [root (temp-root) config (fixture root "advance-queue") calls (atom 0)]
    (try
      (testing "manifest authority cannot be replaced by a caller path or digest"
        (is (thrown? clojure.lang.ExceptionInfo
                     (queue/validate-config!
                      (assoc-in config [:entries 0 :manifest-sha256]
                                (apply str (repeat 64 "d")))))))
      (with-redefs [series/step! (fn [& _]
                                   (case (swap! calls inc)
                                     1 {:status :trial-started :click-id "click-2"}
                                     2 {:status :trial-terminal :task-result :succeeded}
                                     {:status :series-terminal :series-id "series-1"}))
                    runner/await-click! (fn [_ _] {:status :completed})]
        (queue/start! config)
        (is (= :running (:status (queue/tick! config))))
        (is (= 0 (:cursor (queue/read-state! config))))
        (is (= :running (:status (queue/tick! config))))
        (is (= 1 (:cursor (queue/read-state! config))))
        (is (= :stopped (:status (queue/tick! config))))
        (is (= :queue-complete (:reason (queue/read-state! config))))
        (is (= 3 @calls)))
      (finally (queue/stop! config) (delete-tree! root)))))

(deftest ^:slow materialized-async-service-queue-roundtrip
  (#'u/with-service
   (fn [root base-cfg]
     (reset! runner/!status runner/initial-status)
     (registry/reset-registry!)
     (registry/register-agent!
      {:agent-id {:id/value "war-machine" :id/type :apparatus}
       :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}})
     (let [cohort-source (io/file "../futon2/holes/labs/wm-contract/runs"
                                  "RUN4-U88-cohort-2026-09-11/cohort.edn")
           cohort-raw (slurp cohort-source)
           cohort-spec (read-string cohort-raw)
           cohort-file (io/file root "queue-cohort.edn")
           cohort-root (doto (io/file root "queue-cohort-data") .mkdir)
           _ (spit cohort-file cohort-raw)
           cohort-binding {:preregistration (.getCanonicalPath cohort-file)
                           :data-root (.getCanonicalPath cohort-root)
                           :cohort-id (:cohort/id cohort-spec)
                           :sha256 (digest/sha256 cohort-raw)}
           _ (cohort/activate! (.getCanonicalPath cohort-file)
                               (.getCanonicalPath cohort-root))
           cfg (-> base-cfg
                   (assoc-in [:run4 :execution-cohort] cohort-binding)
                   (assoc-in [:run4 :cohort-preflight!] cohort/execution-preflight))
           series-config (get-in cfg [:run4 :series])
           manifest-ref (:manifest-ref series-config)
           manifest-text (slurp (io/file (:manifest-root series-config) manifest-ref))
           manifest (read-string manifest-text)
           cohort (get-in cfg [:run4 :execution-cohort])
           queue-root (doto (io/file root "queue-state") .mkdir)
           queue-visible (doto (io/file root "queue-visible") .mkdir)
           config {:queue-id "materialized-queue"
                   :state-root (.getPath queue-root)
                   :visibility-file (.getPath (io/file queue-visible "queue.json"))
                   :interval-ms 600000 :await-timeout-ms 10000
                   :entries [{:entry-id "materialized-entry"
                              :series-id (:series-id manifest)
                              :server-config cfg :headers u/auth
                              :request {:run4-series-ref manifest-ref}
                              :manifest-sha256 (digest/sha256 manifest-text)
                              :cohort-id (:cohort-id cohort)
                              :cohort-sha256 (:sha256 cohort)}]}
           casting (get-in cfg [:run4 :casting])
           core (fn [opts]
                  (let [action {:type :advance-mission
                                :target "M-u88-contextual-preferences"}
                        judgment {:decision {:action {:type :no-op}}
                                  :ranked-actions [{:rank 1 :action action}]
                                  :admissible-actions [{:rank 1 :action action}]}
                        selected (full-runner/resolve-pinned-selection
                                  opts judgment (select-keys opts (keys casting)))
                        identity (:identity selected)]
                    {:attempt-id "queue-worker-attempt" :outcome :grounded-change
                     :checkpoints
                     {:selection {:judgment {:outcome :ok}
                                  :ground {:kind :wm-judgement :run4/task-pin identity
                                           :run4/operator-selection (:provenance selected)}}
                      :construction {:judgment {:run4/task-pin identity}
                                     :ground {:kind :decision-pinned-construction
                                              :run4/task-pin identity}}
                      :dispatch {:judgment {:agent (:author casting)
                                            :availability :invoke-ready
                                            :job-id "queue-author-job"}
                                 :ground {:kind :agency-dispatch}}
                      :build {:judgment {:commits ["queue-commit"]
                                         :validation {:approved? true
                                                      :review-job "queue-review-job"
                                                      :review-gate {:required? true :executed? true
                                                                    :tool-events 2 :passed? true}}}
                              :ground {:kind :git-commit-and-independent-review}}
                      :adjudication {:judgment {:build-match {:commit "queue-commit"
                                                              :review-approved? true}
                                                :dial {:moved? true
                                                       :implementation-id "queue-impl"}}
                                     :ground {:kind :authoritative-substrate-discharge}}}
                     :data {:commit "queue-commit"
                            :author-job {:job-id "queue-author-job"}
                            :review-job {:job-id "queue-review-job"}
                            :witness {:resolved? true :dial-moved? true
                                      :implementation-id "queue-impl"}}
                     :wm/route [{:node :R20 :via "scan" :at "2026-09-11T12:00:00Z"}
                                {:node :R12 :via "select" :at "2026-09-11T12:00:01Z"}]}))]
       (binding [full-runner/*wm-status-reporting?* false]
         (with-redefs-fn {#'full-runner/run-opportunity-core! core}
           (fn []
             (queue/start! config)
             ;; Start through the actual service, let the async producer finish,
             ;; then discard only its process-local promise. Queue recovery must
             ;; finish through the exact durable started/evidence path.
             (let [started (series/step! cfg u/auth {:run4-series-ref manifest-ref})
                   click-id (:click-id started)]
               (is (= :trial-started (:status started)))
               (#'queue/persist! config
                                  (assoc (queue/read-state! config) :in-flight
                                         {:entry-id "materialized-entry"
                                          :click-id click-id}))
               (is (= :completed (:status (runner/await-click! click-id))))
               (reset! (var-get #'runner/!completion) nil))
             (is (= :running (:status (queue/tick! config))))
             (is (= 0 (:cursor (queue/read-state! config))))
             (is (= :running (:status (queue/tick! config))))
             (is (= 1 (:cursor (queue/read-state! config))))
             (is (= :stopped (:status (queue/tick! config))))
             (is (seq (.listFiles (io/file root "store-run-records"))))
             (is (seq (.listFiles (io/file root "store-bindings"))))
             (is (seq (.listFiles (io/file root "store-projections"))))
             (queue/stop! config))))))))
