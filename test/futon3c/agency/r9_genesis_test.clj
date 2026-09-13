(ns futon3c.agency.r9-genesis-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.r9-checker :as r9]
            [futon3c.agency.r9-genesis :as genesis]))

(defn- commission [job agent prompt]
  (let [c {:agent-id agent :prompt prompt :caller "codex-26" :surface "bell"}]
    {:schema :agency/invoke-request-commission-v1 :job-id job
     :request-digest (r9/request-digest c) :commission c}))

(def base
  {:schema :wm/r9-genesis-candidate-v1 :kind :genesis
   :author "codex-23" :reviewer "codex-26"
   :author-job-id "author-job" :reviewer-job-id "review-job"
   :author-trace-id "author-trace" :reviewer-trace-id "review-trace"
   :external-root {:id "operator-delegation-event"}
   :author-commission (commission "author-job" "codex-23" "implement")
   :reviewer-commission (commission "review-job" "codex-26" "review")
   :acceptance {:id "delegated-acceptance"}
   :artifacts {:source {:id :source :sha256 "source-sha"}
               :tests {:id :tests :sha256 "tests-sha"}
               :review {:id :review :sha256 "review-sha"}}})

(defn- opts [candidate]
  (let [authority (-> "r9-genesis/source-backed-authority.edn"
                      io/resource slurp edn/read-string)]
    {:candidate candidate
     :root-resolver (fn [_] (:external-root authority))
     :trace-resolver (fn [id] (get-in authority [:traces id]))
     :artifact-resolver (fn [pin] (get-in authority [:artifacts
                                                      (keyword (:sha256 pin))]))
     :acceptance-resolver (fn [_] (:acceptance authority))
     :predecessor-resolver (fn [p] {:status :verified
                                     :checker-source-sha256
                                     (:checker-source-sha256 p)})}))

(defn- refusal [options]
  (:refusal (try (genesis/verify-candidate options)
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))))

(deftest genesis-boundary-positive-and-refusals
  (is (= :verified-for-independent-review
         (:decision (genesis/verify-candidate (opts base)))))
  (is (= :r9/author-equals-reviewer
         (refusal (opts (assoc base :reviewer "codex-23")))))
  (is (= :r9/external-root-unverified
         (refusal (assoc (opts base) :root-resolver (constantly {:status :missing})))))
  (is (= :r9/candidate-authored-authority
         (refusal (assoc (opts base) :root-resolver
                         (constantly {:status :verified :authority-origin :candidate})))))
  (is (= :r9/commission-join-mismatch
         (refusal (opts (assoc-in base [:reviewer-commission :commission :prompt]
                                  "tampered")))))
  (is (= :r9/trace-job-join-mismatch
         (refusal (assoc (opts base) :trace-resolver
                         (constantly {:status :verified :job-id "wrong"})))))
  (is (= :r9/artifact-pin-mismatch
         (refusal (assoc (opts base) :artifact-resolver
                         (constantly {:status :verified :sha256 "wrong"})))))
  (is (= :r9/candidate-authored-authority
         (refusal (assoc (opts base) :acceptance-resolver
                         (constantly {:status :verified
                                      :authority-origin :candidate})))))
  (is (= :r9/predecessor-unverified
         (refusal (assoc (opts (assoc base :kind :successor
                                      :predecessor {:checker-source-sha256 "prior"}))
                         :predecessor-resolver (constantly {:status :missing})))))
  (is (= :r9/predecessor-unverified
         (refusal (opts (assoc base :kind :successor :predecessor nil)))))
  (testing "successor requires and binds a previously anchored checker"
    (is (= :verified-for-independent-review
           (:decision (genesis/verify-candidate
                       (opts (assoc base :kind :successor
                                    :predecessor {:checker-source-sha256 "prior"})))))))
  (testing "the real located host event remains an explicit refusal"
    (is (= :r9/external-root-unverified
           (refusal (assoc (opts base) :root-resolver
                           genesis/located-host-event-resolver))))))
