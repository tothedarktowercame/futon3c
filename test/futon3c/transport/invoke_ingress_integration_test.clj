(ns futon3c.transport.invoke-ingress-integration-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [futon3c.agency.invoke-ingress-controller :as ingress]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard :as bb]
            [futon3c.social.coordination-ledger :as coordination-ledger]
            [futon3c.transport.http :as http])
  (:import (java.nio.file Files)))

(defn- refusal [f]
  (:refusal (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e)))))

(defn- durable-controller []
  (let [dir (Files/createTempDirectory
             "invoke-http-ingress-"
             (make-array java.nio.file.attribute.FileAttribute 0))
        store (ingress/file-deferred-store (.resolve dir "deferred.edn"))]
    (ingress/initialize-file-store! store)
    (ingress/controller {:auth-token "integration-secret"
                         :deferred-store store})))

(defn- controller-config [controller]
  {:status :active :controller controller :restart-authorized? false
   :lifecycle-wiring :creation-only})

(defn- snapshot [controller]
  (ingress/verification-snapshot
   controller {:remote-addr "127.0.0.1" :auth-token "integration-secret"}))

(defn- invoke-request [id]
  {:requested-job-id id :agent-id "worker-1" :prompt "bounded work"
   :caller "operator-1" :surface "bell" :mode "work"})

(defn- with-http-fixture [controller f]
  (let [ledger (atom (#'http/default-invoke-jobs-ledger))
        active-index (atom nil)
        persisted (atom [])]
    (with-redefs-fn
      {#'http/!invoke-ingress-controller-config
       (atom (controller-config controller))
       #'http/!invoke-jobs-ledger ledger
       #'http/!active-invoke-job-index active-index
       #'http/persist-invoke-jobs-ledger!
       (fn [record] (swap! persisted conj record) true)
       #'http/read-commission-archive (constantly nil)
       #'coordination-ledger/record-invoke-edge! (constantly true)
       #'bb/project-agents! (constantly true)}
      #(f {:ledger ledger :persisted persisted}))))

(deftest closed-intake-refuses-before-ledger-mutation
  (let [controller (durable-controller)]
    (try
      (with-http-fixture
        controller
        (fn [{:keys [ledger persisted]}]
          (ingress/close-intake! controller)
          (is (= :ingress/intake-closed
                 (refusal #(#'http/create-invoke-job! (invoke-request "closed-1")))))
          (is (empty? (:jobs @ledger)))
          (is (empty? @persisted))))
      (finally (ingress/release-controller! controller)))))

(deftest entrant-is-counted-before-writer-lock-and-finished-after-acceptance
  (let [controller (durable-controller)
        writer-lock (var-get #'http/invoke-jobs-writer-lock)]
    (try
      (with-http-fixture
        controller
        (fn [{:keys [ledger]}]
          (locking writer-lock
            (let [result (future (#'http/create-invoke-job! (invoke-request "blocked-1")))]
              (loop [remaining 100]
                (when (and (zero? (:waiting-writer (snapshot controller)))
                           (pos? remaining))
                  (Thread/sleep 5)
                  (recur (dec remaining))))
              (is (= 1 (:waiting-writer (snapshot controller))))
              (is (= 1 (:waiting-writer (ingress/close-intake! controller))))
              (is (empty? (:jobs @ledger)))
              ;; Leaving locking releases the real invoke writer lock.
              (is (not (realized? result)))))
          (loop [remaining 100]
            (when (and (zero? (count (:jobs @ledger))) (pos? remaining))
              (Thread/sleep 5)
              (recur (dec remaining))))
          (is (= #{"blocked-1"} (set (keys (:jobs @ledger)))))
          (is (= 0 (:waiting-writer (snapshot controller))))
          (is (= 1 (:accepted-queued (snapshot controller))))))
      (finally (ingress/release-controller! controller)))))

(deftest ledger-failure-releases-entrant-without-acceptance
  (let [controller (durable-controller)]
    (try
      (with-http-fixture
        controller
        (fn [_]
          (with-redefs-fn
            {#'http/persist-invoke-jobs-ledger!
             (fn [_] (throw (ex-info "planted ledger failure" {})))}
            (fn []
              (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                    #"planted ledger failure"
                                    (#'http/create-invoke-job!
                                     (invoke-request "failed-1"))))))
          (is (= 0 (:waiting-writer (snapshot controller))))
          (is (= 0 (:accepted-queued (snapshot controller))))))
      (finally (ingress/release-controller! controller)))))

(deftest persistence-boundaries-preserve-conservative-accounting
  (doseq [[stage expected-count expected-drained?]
          [[:temp-forced 0 true]
           [:renamed 1 false]]]
    (let [controller (durable-controller)
          dir (Files/createTempDirectory
               "invoke-http-persist-"
               (make-array java.nio.file.attribute.FileAttribute 0))
          path (str (.resolve dir "ledger.edn"))
          real-persist (var-get #'http/persist-invoke-jobs-ledger!)]
      (try
        (with-http-fixture
          controller
          (fn [{:keys [ledger]}]
            (with-redefs-fn
              {#'http/persist-invoke-jobs-ledger! real-persist
               #'http/invoke-jobs-store-path (constantly path)}
              (fn []
                (binding [http/*invoke-jobs-persist-stage-hook*
                          (fn [observed _]
                            (when (= stage observed)
                              (throw (ex-info "planted persistence boundary" {}))))]
                  (let [error (try
                                (#'http/create-invoke-job!
                                 (invoke-request (str "boundary-" (name stage))))
                                nil
                                (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                    (is (= (= stage :renamed) (:committed? error)))
                    (is (= expected-count (count (:jobs @ledger))))
                    (when (= stage :renamed)
                      (is (= expected-count
                             (count (:jobs (edn/read-string (slurp path)))))))))))
            (ingress/close-intake! controller)
            (is (= expected-count (:accepted-queued (snapshot controller))))
            (is (= expected-drained? (:drained? (snapshot controller))))))
        (finally (ingress/release-controller! controller))))))

(deftest duplicate-stable-id-is-accounted-once
  (let [controller (durable-controller)]
    (try
      (with-http-fixture
        controller
        (fn [{:keys [ledger]}]
          (is (= "stable-1" (#'http/create-invoke-job! (invoke-request "stable-1"))))
          (is (= "stable-1" (#'http/create-invoke-job! (invoke-request "stable-1"))))
          (is (= ["stable-1"] (:job-order @ledger)))
          (is (= 1 (:accepted-queued (snapshot controller))))))
      (finally (ingress/release-controller! controller)))))

(deftest execution-terminal-and-delivery-transitions-are-idempotent
  (doseq [[id terminal-state]
          [["success-1" "done"] ["failure-1" "failed"]
           ["cancel-queued-1" "cancelled"]]]
    (let [controller (durable-controller)]
      (try
        (with-http-fixture
          controller
          (fn [_]
            (#'http/create-invoke-job! (invoke-request id))
            (when-not (= "cancelled" terminal-state)
              (is (true? (#'http/mark-invoke-job-running! id)))
              (is (false? (#'http/mark-invoke-job-running! id)))
              (is (= 1 (:executing (snapshot controller)))))
            (with-redefs-fn
              {#'http/parked-on-notify! (constantly {})
               #'http/auto-bellback-enabled? (constantly false)
               #'http/inbox-agent? (constantly false)
               #'reg/get-agent (constantly {})}
              (fn []
                (is (true? (#'http/finalize-invoke-job!
                            id terminal-state (when-not (= "done" terminal-state) terminal-state)
                            nil {:ok (= "done" terminal-state)} nil)))
                (is (false? (#'http/finalize-invoke-job!
                             id terminal-state nil nil {:ok true} nil)))))
            (is (= 1 (:final-delivery (snapshot controller))))
            (is (true? (#'http/record-invoke-job-delivery-by-job-id!
                        id {:surface "test" :destination "fixture"
                            :delivered? true :note "terminal delivery"})))
            (is (false? (#'http/record-invoke-job-delivery-by-job-id!
                         id {:surface "test" :destination "fixture"
                             :delivered? true :note "duplicate"})))
            (is (true? (:drained? (snapshot controller))))))
        (finally (ingress/release-controller! controller))))))

(deftest service-configuration-is-explicit-durable-and-never-ready
  (let [controller (durable-controller)
        config-atom (atom {:status :inactive})]
    (try
      (with-redefs-fn
        {#'http/!invoke-ingress-controller-config config-atom}
        (fn []
          (is (= {:status :active :controller controller
                  :restart-authorized? false :lifecycle-wiring :creation-only}
                 (http/configure-invoke-ingress-controller!
                  {:schema :agency/invoke-ingress-http-v1
                   :controller controller})))
          (is (= :ingress/http-controller-already-configured
                 (refusal #(http/configure-invoke-ingress-controller!
                            {:schema :agency/invoke-ingress-http-v1
                             :controller controller}))))))
      (is (= :ingress/http-controller-invalid
             (refusal #(http/configure-invoke-ingress-controller!
                        {:schema :agency/invoke-ingress-http-v1
                         :controller (ingress/controller
                                      {:auth-token "test" :test-only? true})}))))
      (finally (ingress/release-controller! controller)))))

(deftest partial-service-configuration-refuses
  (with-redefs-fn
    {#'http/!invoke-ingress-controller-config
     (atom {:status :active :restart-authorized? false})}
    (fn []
      (is (= :ingress/http-config-partial
             (refusal #(#'http/create-invoke-job! (invoke-request "partial-1"))))))))
