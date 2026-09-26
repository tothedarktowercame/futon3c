(ns futon3c.agency.invoke-lifecycle-reconciliation-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.invoke-lifecycle-reconciliation :as reconcile])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files StandardOpenOption)
           (java.security MessageDigest)))

(defn- sha [bs]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))))))
(defn- refusal [f] (:refusal (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
(defn- source! [dir name record]
  (let [p (.resolve dir (str name ".edn")) bs (.getBytes (pr-str record) StandardCharsets/UTF_8)]
    (Files/write p bs (into-array StandardOpenOption [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE]))
    (reconcile/file-snapshot-resolver p (sha bs))))
(def provenance {:producer "isolated-lifecycle-fixture"})
(defn- scoped [m] (assoc m :scope :isolated-fixture :provenance provenance))
(defn- records []
  {:controller (scoped {:schema :agency/ingress-controller-snapshot-v1 :mode :closed :generation 7
                        :waiting-writer 0 :accepted-queued 0 :executing 0 :final-delivery 0})
   :hot-ledger (scoped {:schema :agency/invoke-hot-ledger-snapshot-v1 :generation 7
                :jobs {"a" {:job-id "a" :state :terminal :trace-id "trace-a"}
                       "b" {:job-id "b" :state :terminal :trace-id "trace-b"}}})
   :accepted-queue (scoped {:schema :agency/accepted-queue-snapshot-v1 :generation 7 :job-ids []})
   :execution (scoped {:schema :agency/execution-snapshot-v1 :generation 7 :job-ids []})
   :final-delivery (scoped {:schema :agency/final-delivery-snapshot-v1 :generation 7
                    :records {"a" {:job-id "a" :status :complete :trace-id "trace-a"}
                              "b" {:job-id "b" :status :complete :trace-id "trace-b"}}})
   :deferred (scoped {:schema :agency/deferred-resume-snapshot-v1 :generation 7 :order ["park-1"]
                      :records {"park-1" {:status :pending
                                          :payload {:requested-job-id "park-1"}}}})})
(defn- fixture [mutate]
  (let [dir (Files/createTempDirectory "lifecycle-reconcile-" (make-array java.nio.file.attribute.FileAttribute 0))
        rs (mutate (records))
        sources (into {} (map (fn [[k v]] [k (source! dir (name k) v)]) rs))
        digests (into {} (map (fn [[k resolver]] [k (:expected-sha256 (resolver))]) sources))
        authority (scoped {:schema :agency/accepted-job-completeness-authority-v1
                           :generation 7 :source-digests digests
                           :job-universe [{:job-id "a" :trace-id "trace-a"}
                                          {:job-id "b" :trace-id "trace-b"}]})
        authority (assoc-in authority [:provenance :authority] :independent-fixture)]
    (assoc sources
           :completeness-authority (source! dir "authority" authority)
           :expected-scope :isolated-fixture)))

(deftest complete-census-is-pinned-and-never-authorizes-restart
  (let [r (reconcile/reconcile (fixture identity))]
    (is (= :complete-census (:status r)))
    (is (= :isolated-fixture (:scope r)))
    (is (= 2 (:job-count r)))
    (is (:zero-in-flight? r))
    (is (false? (:restart-authorized? r)))
    (is (= {:count 1 :ids ["park-1"] :accounting :separate-from-accepted-jobs}
           (:deferred-resumes r)))
    (is (= 7 (count (:pins r))))))

(defn- reconcile-mutated [f]
  (reconcile/reconcile (fixture f)))

(deftest incomplete-and-conflicting-censuses-refuse
  (testing "omitted and nonterminal"
    (is (= :reconcile/omitted-queued-job
           (refusal (fn []
                      (reconcile-mutated
                       (fn [r] (-> r
                                   (assoc-in [:hot-ledger :jobs "a" :state] :queued)
                                   (update-in [:final-delivery :records] dissoc "a"))))))))
    (is (= :reconcile/nonterminal-accepted-jobs
           (refusal (fn []
                      (reconcile-mutated
                       (fn [r] (-> r
                                          (assoc-in [:hot-ledger :jobs "a" :state] :queued)
                                          (assoc-in [:accepted-queue :job-ids] ["a"])
                                          (assoc-in [:controller :accepted-queued] 1)
                                          (update-in [:final-delivery :records] dissoc "a")))))))))
  (testing "duplicate and conflicting trace"
    (is (= :reconcile/duplicate-lifecycle
           (refusal (fn []
                      (reconcile-mutated
                       (fn [r] (-> r
                                   (assoc-in [:accepted-queue :job-ids] ["a"])
                                   (assoc-in [:execution :job-ids] ["a"])
                                   (assoc-in [:controller :accepted-queued] 1)
                                   (assoc-in [:controller :executing] 1))))))))
    (is (= :reconcile/terminal-join-invalid
           (refusal (fn []
                      (reconcile-mutated
                       (fn [r] (assoc-in r [:final-delivery :records "a" :trace-id]
                                         "other"))))))))
  (testing "missing source, stale generation, unknown delivery"
    (is (= :reconcile/source-missing
           (refusal (fn [] (reconcile/reconcile
                            (dissoc (fixture identity) :execution))))))
    (is (= :reconcile/stale-generation
           (refusal (fn []
                      (reconcile-mutated
                       (fn [r] (assoc-in r [:execution :generation] 6)))))))
    (is (= :reconcile/delivery-records-invalid
           (refusal (fn []
                      (reconcile-mutated
                       (fn [r] (-> r
                                   (assoc-in [:hot-ledger :jobs "a" :state] :final-delivery)
                                   (assoc-in [:final-delivery :records "a" :status] :mystery))))))))))

(deftest source-pin-and-closed-generation-are-mandatory
  (let [sources (fixture identity)
        bad-source (fn [] {:bytes (.getBytes "{}" StandardCharsets/UTF_8)
                           :expected-sha256 "wrong" :path "mutated"})]
    (is (= :reconcile/source-pin-mismatch
           (refusal (fn [] (reconcile/reconcile
                            (assoc sources :controller bad-source)))))))
  (is (= :reconcile/controller-not-closed-and-idle
         (refusal (fn []
                    (reconcile-mutated
                     (fn [r] (assoc-in r [:controller :waiting-writer] 1))))))))

(deftest strict-source-and-completeness-controls
  (doseq [[kind field] [[:accepted-queue :job-ids] [:execution :job-ids]
                         [:final-delivery :records] [:deferred :order]
                         [:deferred :records]]]
    (is (= :reconcile/source-shape-invalid
           (refusal (fn []
                      (reconcile-mutated (fn [r] (update r kind dissoc field))))))))
  (is (= :reconcile/completeness-coverage-mismatch
         (refusal (fn []
                    (reconcile-mutated
                     (fn [r] (-> r
                                 (update-in [:hot-ledger :jobs] dissoc "a")
                                 (update-in [:final-delivery :records] dissoc "a"))))))))
  (is (= :reconcile/source-identities-invalid
         (refusal (fn []
                    (reconcile-mutated
                     (fn [r] (assoc-in r [:accepted-queue :job-ids] ["a" "a"])))))))
  (is (= :reconcile/stale-generation
         (refusal (fn []
                    (reconcile-mutated
                     (fn [r] (assoc-in r [:deferred :generation] 6)))))))
  (let [sources (fixture identity)
        bytes (byte-array [(byte -61) (byte 40)])]
    (is (= :reconcile/source-invalid-utf8
           (refusal (fn []
                      (reconcile/reconcile
                       (assoc sources :execution
                              (fn [] {:bytes bytes :expected-sha256 (sha bytes)
                                      :path "malformed-utf8"}))))))))
  (is (= :reconcile/production-completeness-authority-unavailable
         (refusal (fn []
                    (reconcile/reconcile (assoc (fixture identity)
                                                :expected-scope :production))))))
  (let [authority-a (:completeness-authority (fixture identity))
        sources-b (fixture (fn [r] (-> r
                                          (assoc-in [:deferred :order] [])
                                          (assoc-in [:deferred :records] {}))))]
    (is (= :reconcile/completeness-authority-invalid
           (refusal
            (fn []
              (reconcile/reconcile
               (assoc sources-b :completeness-authority authority-a)))))))

)
