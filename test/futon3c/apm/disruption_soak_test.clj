(ns futon3c.apm.disruption-soak-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.durable-coordinator :as coordinator])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def disruptions
  [:namespace-reload :job-killed-at-cap :process-restart :revision-moved
   :state-file-partial :duplicate-activation :memory-store-down])

(defn- fixture [disruption part]
  (-> (io/file "test/resources/disruptions" (name disruption)
               (str (name part) ".edn"))
      slurp edn/read-string))

(defn- temp-paths []
  (let [root (Files/createTempDirectory "apm-disruption-"
                                        (make-array FileAttribute 0))]
    {:registry (str (.resolve root "registry.edn"))
     :state (str (.resolve root "state.edn"))}))

(defn- await-first-tick [started]
  (deref (:first-tick started) 2000 {:ok false :error/code :tick-timeout}))

(defn- read-state [path]
  (edn/read-string (slurp path)))

(defn- registry-count [path]
  (count (:entries (edn/read-string (slurp path)))))

(defn- persisted-fixture-state [coordinator-id disruption]
  (let [base (merge {:state/type :live-regulator
                     :regulator/id coordinator-id
                     :regulator/status :running}
                    (fixture disruption :state))
        intent (coordinator/make-intent coordinator-id base
                                        (fixture disruption :intent))]
    (assoc base
           :regulator/ticks 1
           :coordinator/pending-intent intent
           :coordinator/pending-pre-state-digest (:pre-state/digest intent))))

(defn- run-case! [disruption]
  (let [{:keys [registry state]} (temp-paths)
        coordinator-id (str "soak:" (name disruption))
        activation-calls (atom 1)
        reconcile-calls (atom 0)
        original (persisted-fixture-state coordinator-id disruption)
        intent-digest (get-in original [:coordinator/pending-intent :intent/digest])
        adapter-key (keyword "soak" (name disruption))]
    (coordinator/register-adapter!
     adapter-key
     (fn [_]
       {:decide-fn (fn [_]
                     (swap! activation-calls inc)
                     {:ok false :error/code :unexpected-second-activation})
        :reconcile-fn
        (fn [_ _]
          (swap! reconcile-calls inc)
          (case disruption
            :job-killed-at-cap
            {:ok true :status :job-killed :job/state :killed}

            :memory-store-down
            {:ok true :status :awaiting-memory-store}

            {:ok true :status :awaiting-job}))}))
    (is (:ok (coordinator/register!
              {:registry-path registry :coordinator-id coordinator-id
               :adapter adapter-key :config {} :state-path state
               :period-ms 100000})))
    (spit state (str (pr-str original) "\n"))
    (try
      (case disruption
        :namespace-reload
        (require 'futon3c.apm.durable-coordinator :reload)

        :process-restart
        (let [started (coordinator/start-registered! registry coordinator-id)]
          (is (:ok started))
          (is (:ok (await-first-tick started)))
          (coordinator/stop! coordinator-id))

        :revision-moved
        (spit state
              (str (pr-str (assoc original
                                  :fixture/revision "moved"
                                  :coordinator/pending-pre-state-digest
                                  "revision-moved-after-intent"))
                   "\n"))

        :state-file-partial
        ;; Atomic persistence means a torn replacement must never become the
        ;; accepted state. Restore the captured state before recovery and
        ;; retain the torn bytes as evidence beside it.
        (do (spit (str state ".partial") "{:state/type :live-regulator")
            (spit state (str (pr-str original) "\n")))

        :duplicate-activation
        (let [started (coordinator/start-registered! registry coordinator-id)]
          (is (:ok started))
          (is (:ok (await-first-tick started)))
          (coordinator/stop! coordinator-id))

        nil)
      (let [started (coordinator/start-registered! registry coordinator-id)]
        (is (:ok started))
        (is (not= :tick-timeout
                  (:error/code (await-first-tick started)))))
      (let [after (read-state state)]
        {:phase (:fixture/phase after)
         :intent-digest (get-in after [:coordinator/pending-intent :intent/digest])
         :activation-calls @activation-calls
         :registry-count (registry-count registry)
         :status (:regulator/status after)
         :reconcile-calls @reconcile-calls
         :expected-digest intent-digest})
      (finally (coordinator/stop! coordinator-id)))))

(deftest every-known-disruption-resumes-or-refuses-at-the-same-intent
  (doseq [disruption disruptions]
    (testing (name disruption)
      (let [{:keys [phase intent-digest activation-calls registry-count
                    expected-digest status reconcile-calls]}
            (run-case! disruption)]
        (is (= :solve phase))
        (is (= expected-digest intent-digest))
        (is (= 1 activation-calls))
        (is (= 1 registry-count))
        (if (= :revision-moved disruption)
          (is (zero? reconcile-calls))
          (is (pos? reconcile-calls)))
        (if (= :revision-moved disruption)
          (is (= :failed status))
          (is (contains? #{:running :stopped} status)))))))
