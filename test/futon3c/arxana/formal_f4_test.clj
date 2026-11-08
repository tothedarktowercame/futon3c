(ns futon3c.arxana.formal-f4-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.compile.compiler :as compiler]
            [futon3c.arxana.formal-f1 :as formal-f1]
            [futon3c.arxana.formal-f4 :as f4]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [futon3c.arxana.write :as write]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-f4a-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})]
          (try
            (let [cfg {:graph-store (store-http/http-store
                                     (str "http://127.0.0.1:"
                                          (.getPort (.getAddress server)))
                                     f1/service-principal)
                       :artifact-provider (artifact/in-memory-provider)}]
              (f node cfg))
            (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))

(defn dispatch [cfg mid op body]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id mid
                         :operation op :principal/id "formal-f4-test" :body body}))

(defn read-record [cfg id]
  (get-in (dispatch cfg (str "read:" id) :entity/get {:entity/id id}) [:ok :record]))

(defn fixture-plan []
  (let [turn (last (formal-f1/add-operator-judgement (formal-f1/plan-fixtures)))
        claim (first (:claims turn))
        attestation (first (filter #(= (:subject-clusion-id %) (:id claim))
                                   (:attestations turn)))]
    {:turn turn :claim claim :attestation attestation
     :f4 (f4/build-plan claim attestation)}))

(deftest typed-recursive-composite-and-stage-local-criticism
  (with-stack
    (fn [_ cfg]
      (let [{:keys [turn claim f4]} (fixture-plan)
            _ (formal-f1/materialize! cfg [turn])
            writes (f4/materialize! #(service/dispatch cfg %) f4)]
        (is (every? :ok writes))
        (is (compiler/non-executable? (:candidate f4)))
        (is (= :deferred (get-in f4 [:decision :decision/status])))
        (is (nil? (:receipt f4)) "F4a has no execution receipt")
        (doseq [r (:records f4)]
          (is (= r (read-record cfg (:id r))) "every stage/link/composite is public"))
        (is (= [:source-act :intent :candidate :decision]
               (mapv :role (:endpoints (:composite f4)))))
        (is (= f4/topology-evidence
               (get-in f4 [:composite :asserted-content :topology/evidence])))
        (is (= (:id (:composite f4))
               (get-in f4 [:group :endpoints 0 :target :clusion/id])))
        (let [before (into {} (map (juxt :id identity)
                                   (cons claim (:records f4))))
              view-before (get-in (dispatch cfg "f4:view-before" :formal-f4/composite
                                            {:composite/id (get-in f4 [:composite :id])})
                                  [:ok])
              candidate-id (get-in f4 [:stages :candidate :id])
              intent-id (get-in f4 [:stages :intent :id])
              rejected (records/make-assessment
                        {:subject-clusion-id candidate-id :verdict :rejected
                         :actor "reviewer" :policy {:id "formal-f4/stage-review" :revision 1}
                         :reason "candidate is too broad" :time "2026-11-08T12:00:00.000Z"})
              contradicted (records/make-attestation
                            {:subject-clusion-id intent-id :producer "reviewer"
                             :basis :human :standing :contradicted :coverage :complete
                             :method {:id "formal-f4/critique" :version 1}
                             :content {:reason "intent misreads the request"} :source-refs []})]
          (is (:ok (dispatch cfg "f4:reject" :assessment/append {:record rejected})))
          (is (:ok (dispatch cfg "f4:contradict" :evidence/append {:record contradicted})))
          (doseq [[id value] before]
            (is (= value (read-record cfg id)) "criticism never rewrites a stage"))
          (let [view (get-in (dispatch cfg "f4:view" :formal-f4/composite
                                       {:composite/id (get-in f4 [:composite :id])})
                             [:ok])
                rows (into {} (map (juxt :port identity) (:stages view)))]
            (is (= [:rejected] (get-in rows [:candidate :assessment/verdicts])))
            (is (= [(:id contradicted)]
                   (get-in rows [:intent :evidence/attestation-ids])))
            (is (= (first (filter #(= :source-act (:port %)) (:stages view-before)))
                   (get rows :source-act))
                "candidate/intent criticism does not change source standing")
            (is (= (first (filter #(= :decision (:port %)) (:stages view-before)))
                   (get rows :decision))
                "candidate/intent criticism does not change decision standing")))))))

(deftest composite-port-refusals-are-typed
  (with-stack
    (fn [_ cfg]
      (let [{:keys [turn f4]} (fixture-plan)
            _ (formal-f1/materialize! cfg [turn])
            stages (take 6 (:records f4))]
        (doseq [r stages]
          (is (:ok (dispatch cfg (str "seed:" (:id r)) :clusion/assert {:record r}))))
        (testing "a missing port target is refused before persistence"
          (let [bad (records/make-clusion
                     (assoc (:composite f4) :id nil
                            :endpoints (assoc (:endpoints (:composite f4)) 3
                                              {:role :decision :kind :clusion
                                               :target {:clusion/id "missing"}})))
                result (dispatch cfg "bad:missing" :clusion/assert {:record bad})]
            (is (= :arxana.composite/missing-target
                   (get-in result [:error :error/code])))
            (is (nil? (read-record cfg (:id bad))))))
        (testing "a real record of the wrong declared port type is refused"
          (let [bad (records/make-clusion
                     (assoc (:composite f4) :id nil
                            :endpoints (assoc (:endpoints (:composite f4)) 3
                                              {:role :decision :kind :clusion
                                               :target {:clusion/id
                                                        (get-in f4 [:stages :intent :id])}})))
                result (dispatch cfg "bad:type" :clusion/assert {:record bad})]
            (is (= :arxana.composite/port-type-mismatch
                   (get-in result [:error :error/code])))
            (is (nil? (read-record cfg (:id bad))))))
        (testing "direct cycles are refused by the validity boundary"
          (let [self (assoc (:group f4)
                            :endpoints [{:role :child :kind :clusion
                                         :target {:clusion/id (:id (:group f4))}}])]
            (is (= :arxana.composite/cycle
                   (:error (try (write/validate-composite! (:graph-store cfg) self)
                                nil
                                (catch clojure.lang.ExceptionInfo e
                                  (ex-data e))))))))))))
