(ns futon3c.transport.invoke-ingress-integration-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.invoke-ingress-controller :as ingress]
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
