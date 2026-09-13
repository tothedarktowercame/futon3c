(ns futon3c.agency.invoke-lifecycle-snapshot-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.invoke-ingress-controller :as ingress]
            [futon3c.agency.invoke-lifecycle-reconciliation :as reconciliation]
            [futon3c.agency.invoke-lifecycle-snapshot :as snapshot])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(defn- refusal [f] (:refusal (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
(defn- sha [bs]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))))))
(defn- records [controller]
  {:controller (fn [_] (assoc (ingress/verification-snapshot
                                controller {:remote-addr "127.0.0.1" :auth-token "s"})
                               :schema :agency/ingress-controller-snapshot-v1
                               :scope :isolated-fixture
                               :provenance {:producer "fixture" :owner-id "owner"}))
   :hot-ledger (fn [g] {:schema :agency/invoke-hot-ledger-snapshot-v1 :generation g
                        :scope :isolated-fixture :provenance {:producer "fixture" :owner-id "owner"}
                        :jobs {"a" {:job-id "a" :state :terminal :trace-id "ta"}}})
   :accepted-queue (fn [g] {:schema :agency/accepted-queue-snapshot-v1 :generation g
                            :scope :isolated-fixture :provenance {:producer "fixture" :owner-id "owner"}
                            :job-ids []})
   :execution (fn [g] {:schema :agency/execution-snapshot-v1 :generation g
                       :scope :isolated-fixture :provenance {:producer "fixture" :owner-id "owner"}
                       :job-ids []})
   :final-delivery (fn [g] {:schema :agency/final-delivery-snapshot-v1 :generation g
                            :scope :isolated-fixture :provenance {:producer "fixture" :owner-id "owner"}
                            :records {"a" {:job-id "a" :status :complete :trace-id "ta"}}})
   :deferred (fn [g] {:schema :agency/deferred-resume-snapshot-v1 :generation g
                      :scope :isolated-fixture :provenance {:producer "fixture" :owner-id "owner"}
                      :order [] :records {}})})
(defn- fixture-boundary []
  (let [controller (ingress/controller {:auth-token "s" :test-only? true})
        _ (ingress/close-intake! controller)
        b (snapshot/boundary {:owner-id "owner" :generation 1 :scope :isolated-fixture})
        revision (atom 0)]
    (doseq [[kind capture] (records controller)]
      (snapshot/register-provider! b kind {:owner-id "owner" :capture capture
                                           :revision #(deref revision)}))
    {:boundary b :revision revision}))

(deftest atomic-capture-feeds-external-completeness-boundary
  (let [{:keys [boundary]} (fixture-boundary)
        capture (snapshot/capture! boundary)
        resolvers (snapshot/capture-resolvers capture)
        subject (:coverage-subject capture)
        authority (assoc subject :schema :agency/accepted-job-completeness-authority-v1
                         :scope :isolated-fixture
                         :provenance {:producer "external-fixture-review"
                                      :authority :independent-fixture})
        bytes (.getBytes (pr-str authority) StandardCharsets/UTF_8)
        result (reconciliation/reconcile
                (assoc resolvers :expected-scope :isolated-fixture
                       :completeness-authority
                       (fn [] {:bytes bytes :expected-sha256 (sha bytes)
                               :path "external-authority"})))]
    (is (= :absent (:completeness-authority capture)))
    (is (= :complete-census (:status result)))
    (is (= :isolated-fixture (:scope result)))
    (is (false? (:restart-authorized? result)))))

(deftest absent-and-uncoordinated-providers-refuse
  (let [b (snapshot/boundary {:owner-id "owner" :generation 1 :scope :isolated-fixture})]
    (is (= :snapshot/providers-incomplete (refusal #(snapshot/capture! b))))
    (is (= :snapshot/provider-uncoordinated
           (refusal #(snapshot/register-provider!
                      b :controller {:owner-id "candidate" :capture identity :revision (constantly 0)}))))))

(deftest common-lock-excludes-mutation-and-revision-drift-refuses
  (let [{:keys [boundary]} (fixture-boundary)
        entered (promise) release (promise)
        mutation (future (snapshot/mutate! boundary #(do (deliver entered true) @release)))]
    ;; Mutation owns the boundary; capture cannot complete until it publishes a new generation.
    @entered
    (let [capture-future (future (snapshot/capture! boundary))]
      (is (= ::blocked (deref capture-future 50 ::blocked)))
      (deliver release true) @mutation
      (is (= :snapshot/provider-record-invalid
             (refusal #(deref capture-future)))))
  (let [{:keys [boundary revision]} (fixture-boundary)]
    (snapshot/register-provider!
     (snapshot/boundary {:owner-id "other" :generation 1 :scope :isolated-fixture})
     :controller {:owner-id "other" :capture identity :revision (constantly 0)})
    ;; One provider changes its own revision during capture, outside the common mutation API.
    (swap! (:providers boundary) update :execution
           assoc :capture (fn [g] (swap! revision inc)
                            {:schema :agency/execution-snapshot-v1 :generation g
                             :scope :isolated-fixture
                             :provenance {:producer "fixture" :owner-id "owner"}
                             :job-ids []}))
    (is (= :snapshot/concurrent-provider-change
           (refusal #(snapshot/capture! boundary))))))

(deftest production-boundary-is-unavailable
  (is (= :snapshot/boundary-config-invalid
         (refusal #(snapshot/boundary {:owner-id "production" :generation 1 :scope :production})))))
