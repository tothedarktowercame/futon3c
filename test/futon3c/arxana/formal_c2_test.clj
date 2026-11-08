(ns futon3c.arxana.formal-c2-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn]))

(defn request [mid op record]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "formal-c2-test" :body {:record record}})

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (with-open [node (xtn/start-node)]
    (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"})]
      (try
        (f node {:graph-store
                 (store-http/http-store
                  (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                  f1/service-principal)})
        (finally (f1-server/stop-server! server))))))

(deftest c2-validity-and-store-boundary
  (with-stack
    (fn [node cfg]
      (let [raw {:type-revision [:interpretation 1]
                 :endpoints [{:role :source :kind :manifestation-scope
                              :target {:artifact/revision-id "r1" :locator {:sentence 1}}}
                             {:role :intent :kind :entity :target {:intent/id :delegate}}
                             {:role :target :kind :external-ref :target {:ref/id "a"}}]
                 :asserted-content {:reading/state :candidate}}
            claim (records/make-clusion raw)
            a-base {:subject-clusion-id (:id claim) :basis :model
                    :standing :unverified :coverage :complete
                    :method {:id "xiang" :version 1} :content {} :source-refs []}
            e1 (records/make-attestation (assoc a-base :producer "one"))
            e2 (records/make-attestation (assoc a-base :producer "two"))
            accepted (records/make-assessment
                      {:subject-clusion-id (:id claim) :verdict :accepted
                       :actor "joe" :policy {:id "review" :revision 1}
                       :reason "fits" :time "2026-11-08T00:00:00Z"})
            rejected (records/make-assessment
                      {:subject-clusion-id (:id claim) :verdict :rejected
                       :actor "reviewer" :policy {:id "review" :revision 1}
                       :reason "does not fit" :time "2026-11-08T00:00:01Z"})]
        (is (:ok (service/dispatch cfg (request "claim" :clusion/assert claim))))
        (is (:ok (service/dispatch cfg (request "e1" :evidence/append e1))))
        (is (:ok (service/dispatch cfg (request "e2" :evidence/append e2))))
        (is (:ok (service/dispatch cfg (request "a1" :assessment/append accepted))))
        (is (:ok (service/dispatch cfg (request "a2" :assessment/append rejected))))
        (testing "append_keeps / attest_keeps_claims / assess_keeps_claims"
          (is (= claim (f1/clusion-by-id node (:id claim)))))
        (testing "two_interpreters_one_claim"
          (is (= #{(:id e1) (:id e2)} (set (map :id (f1/evidence-for node (:id claim)))))))
        (testing "assess_keeps_evidence and conflicting_assessments_coexist"
          (is (= 2 (count (f1/evidence-for node (:id claim)))))
          (is (= #{:accepted :rejected}
                 (set (map :verdict (f1/assessments-for node (:id claim)))))))
        (testing "JVM rejects digest and declaration errors before persistence"
          (is (= :invalid (get-in (service/dispatch cfg
                                                    (request "wrong-id" :clusion/assert
                                                             (assoc claim :id "arxana:clusion:sha256:wrong")))
                                  [:error :error/category])))
          (is (nil? (f1/outcome-by-message node "wrong-id")))
          (is (= :invalid (get-in (service/dispatch cfg
                                                    (request "unknown-type" :clusion/assert
                                                             (assoc claim :id nil :type-revision [:unknown 1])))
                                  [:error :error/category])))
          (is (nil? (f1/outcome-by-message node "unknown-type"))))))))

(deftest golden-records-round-trip
  (with-stack
    (fn [node cfg]
      (let [golden (edn/read-string
                    (slurp "test/resources/arxana/formal_arxana/c1/golden-records.edn"))]
        (doseq [[family entries] golden
                {:keys [record expected-id attestations]} entries
                :when record]
          (let [claim (records/make-clusion record)]
            (is (= expected-id (:id claim)) (str family))
            (is (:ok (service/dispatch cfg
                                       (request (str "claim-" expected-id)
                                                :clusion/assert claim))))
            (is (= claim (f1/clusion-by-id node expected-id)))
            (doseq [{attestation :record expected-attestation-id :expected-id} attestations]
              (let [attestation (records/make-attestation attestation)]
                (is (= expected-attestation-id (:id attestation)))
                (is (:ok (service/dispatch cfg
                                           (request (str "evidence-" expected-attestation-id)
                                                    :evidence/append attestation))))))))))))
