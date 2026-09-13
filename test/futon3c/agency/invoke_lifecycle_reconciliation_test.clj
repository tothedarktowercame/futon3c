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
(defn- records []
  {:controller {:schema :agency/ingress-controller-snapshot-v1 :mode :closed :generation 7 :waiting-writer 0}
   :hot-ledger {:schema :agency/invoke-hot-ledger-snapshot-v1 :generation 7
                :jobs {"a" {:job-id "a" :state :terminal :trace-id "trace-a"}
                       "b" {:job-id "b" :state :terminal :trace-id "trace-b"}}}
   :accepted-queue {:schema :agency/accepted-queue-snapshot-v1 :generation 7 :job-ids []}
   :execution {:schema :agency/execution-snapshot-v1 :generation 7 :job-ids []}
   :final-delivery {:schema :agency/final-delivery-snapshot-v1 :generation 7
                    :records {"a" {:job-id "a" :status :complete :trace-id "trace-a"}
                              "b" {:job-id "b" :status :complete :trace-id "trace-b"}}}
   :deferred {:schema :agency/deferred-resume-snapshot-v1 :order ["park-1"]
              :records {"park-1" {:status :pending :payload {:requested-job-id "park-1"}}}}})
(defn- fixture [mutate]
  (let [dir (Files/createTempDirectory "lifecycle-reconcile-" (make-array java.nio.file.attribute.FileAttribute 0))
        rs (mutate (records))]
    (into {} (map (fn [[k v]] [k (source! dir (name k) v)]) rs))))

(deftest complete-census-is-pinned-and-never-authorizes-restart
  (let [r (reconcile/reconcile (fixture identity))]
    (is (= :complete-census (:status r)))
    (is (= 2 (:job-count r)))
    (is (:zero-in-flight? r))
    (is (false? (:restart-authorized? r)))
    (is (= {:count 1 :ids ["park-1"] :accounting :separate-from-accepted-jobs}
           (:deferred-resumes r)))
    (is (= 6 (count (:pins r))))))

(deftest incomplete-and-conflicting-censuses-refuse
  (testing "omitted and nonterminal"
    (is (= :reconcile/omitted-queued-job
           (refusal (fn [] (reconcile/reconcile
                            (fixture (fn [r] (-> r
                                                (assoc-in [:hot-ledger :jobs "a" :state] :queued)
                                                (update-in [:final-delivery :records] dissoc "a")))))))))
    (is (= :reconcile/nonterminal-accepted-jobs
           (refusal (fn [] (reconcile/reconcile
                            (fixture (fn [r] (-> r
                                                (assoc-in [:hot-ledger :jobs "a" :state] :queued)
                                                (assoc-in [:accepted-queue :job-ids] ["a"])
                                                (update-in [:final-delivery :records] dissoc "a")))))))))
  (testing "duplicate and conflicting trace"
    (is (= :reconcile/duplicate-lifecycle
           (refusal (fn [] (reconcile/reconcile
                            (fixture (fn [r] (-> r
                                                (assoc-in [:accepted-queue :job-ids] ["a"])
                                                (assoc-in [:execution :job-ids] ["a"])))))))))
    (is (= :reconcile/terminal-join-invalid
           (refusal (fn [] (reconcile/reconcile
                            (fixture (fn [r] (assoc-in r [:final-delivery :records "a" :trace-id]
                                                      "other"))))))))
  (testing "missing source, stale generation, unknown delivery"
    (is (= :reconcile/source-missing
           (refusal (fn [] (reconcile/reconcile (dissoc (fixture identity) :execution))))))
    (is (= :reconcile/stale-generation
           (refusal (fn [] (reconcile/reconcile
                            (fixture (fn [r] (assoc-in r [:execution :generation] 6))))))))
    (is (= :reconcile/delivery-state-unknown
           (refusal (fn [] (reconcile/reconcile
                            (fixture (fn [r] (-> r
                                                (assoc-in [:hot-ledger :jobs "a" :state] :final-delivery)
                                                (assoc-in [:final-delivery :records "a" :status] :mystery))))))))))

(deftest source-pin-and-closed-generation-are-mandatory
  (let [sources (fixture identity)]
    (is (= :reconcile/source-pin-mismatch
           (refusal (fn [] (reconcile/reconcile
                            (assoc sources :controller
                                   (fn [] {:bytes (.getBytes "{}" StandardCharsets/UTF_8)
                                           :expected-sha256 "wrong" :path "mutated"}))))))))
  (is (= :reconcile/controller-not-closed-and-idle
         (refusal (fn [] (reconcile/reconcile
                          (fixture (fn [r] (assoc-in r [:controller :waiting-writer] 1)))))))))
