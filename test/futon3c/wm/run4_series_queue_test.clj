(ns futon3c.wm.run4-series-queue-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-series-queue :as queue]
            [futon3c.wm.run4-series-service :as series]
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
  (let [root (temp-root) config (fixture root "hold-queue") calls (atom 0)]
    (try
      (with-redefs [series/step! (fn [& _]
                                   (case (swap! calls inc)
                                     1 {:status :trial-started :click-id "click-1"}
                                     {:status :awaiting-terminal-evidence :click-id "click-1"}))
                    runner/await-click! (fn [_ _] {:status :completed})]
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
        (let [visible (json/parse-string (slurp (:visibility-file config)) true)]
          (is (= "held" (:controller_state visible)))
          (is (= "terminal-evidence-incomplete" (:hold_reason visible)))
          (is (= [] (:active_actors visible)))
          (is (= "codex-10" (get-in visible [:assigned_roles :author])))))
      (finally (queue/stop! config) (delete-tree! root)))))

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
