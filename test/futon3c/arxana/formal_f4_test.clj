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
        (testing "through the public port a self-containing composite cannot even be named"
          ;; Review (claude-13): ids are content hashes and a port target must
          ;; already be stored, so no write can close a cycle; a forged
          ;; self-reference fails the identity check before the cycle check.
          (let [forged (assoc (:group f4)
                              :endpoints [{:role :child :kind :clusion
                                           :target {:clusion/id (:id (:group f4))}}])
                result (dispatch cfg "bad:forged-cycle" :clusion/assert {:record forged})]
            (is (:error result))
            (is (nil? (read-record cfg (:id forged))))))
        (testing "direct cycles are refused by the validity boundary"
          (let [self (assoc (:group f4)
                            :endpoints [{:role :child :kind :clusion
                                         :target {:clusion/id (:id (:group f4))}}])]
            (is (= :arxana.composite/cycle
                   (:error (try (write/validate-composite! (:graph-store cfg) self)
                                nil
                                (catch clojure.lang.ExceptionInfo e
                                  (ex-data e))))))))))))

(defn assert-record! [cfg mid record]
  (dispatch cfg mid :clusion/assert {:record record}))

(deftest bounded-execution-requires-stored-capability-and-admission
  (with-stack
    (fn [_ cfg]
      (let [{:keys [turn f4]} (fixture-plan)
            _ (formal-f1/materialize! cfg [turn])
            _ (f4/materialize! #(service/dispatch cfg %) f4)
            original-composite (read-record cfg (get-in f4 [:composite :id]))
            deferred (read-record cfg (get-in f4 [:stages :decision :id]))
            candidate-stage (get-in f4 [:stages :candidate])
            capability (records/make-clusion
                        {:type-revision [:d6/capability 1]
                         :endpoints [(f4/endpoint :candidate candidate-stage)]
                         :asserted-content {:principal/id "executor-a"
                                            :dynamics f4/bounded-read-dynamics
                                            :granted-by "fixture-author"
                                            :granted-at "2026-11-08T13:00:00.000Z"}})
            admitted (f4/admitted-decision candidate-stage (:candidate f4))
            expected (get-in f4 [:candidate :preconditions 0 :revision/id])
            execute (fn [mid principal cap decision revision time]
                      (service/dispatch
                       cfg {:protocol/version "arxana.v1" :message/id mid
                            :operation :formal-f4/execute :principal/id principal
                            :body {:composite/id (get-in f4 [:composite :id])
                                   :capability/id cap :decision/id decision
                                   :expected/revision-id revision
                                   :attempted-at time :query/limit 3}}))]
        (is (:ok (assert-record! cfg "capability" capability)))
        (is (:ok (assert-record! cfg "admitted" admitted)))
        (testing "success reads both grants from the store and records both receipts"
          (let [result (get-in (execute "execute-ok" "executor-a" (:id capability)
                                        (:id admitted) expected
                                        "2026-11-08T13:01:00.000Z") [:ok])
                attempt (read-record cfg (:attempt/id result))
                successor (read-record cfg (:composite/id result))
                receipt-stage (read-record cfg
                                           (get-in (last (:endpoints successor))
                                                   [:target :clusion/id]))
                view (get-in (dispatch cfg "view-success" :formal-f4/composite
                                       {:composite/id (:id successor)}) [:ok])]
            (is (= :ok (:result/status result)))
            (is (= :ran-with-output (:outcome attempt)))
            (is (seq (:result/edge-ids result)))
            (is (= :applied (get-in receipt-stage
                                    [:asserted-content :action/receipt :receipt/outcome])))
            (is (nil? (get-in receipt-stage
                              [:asserted-content :action/receipt :predicted-effects])))
            (is (= :ran-with-output
                   (:attempt/outcome (last (:stages view)))))
            (is (= original-composite
                   (read-record cfg (get-in f4 [:composite :id]))))
            (is (= deferred (read-record cfg (:id deferred))))))
        (testing "each failed boundary check is typed and leaves a refused receipt"
          (let [wrong-principal (records/make-clusion
                                 (assoc capability :id nil
                                        :asserted-content
                                        (assoc (:asserted-content capability)
                                               :principal/id "someone-else")))
                wrong-dynamics (records/make-clusion
                                (assoc capability :id nil
                                       :asserted-content
                                       (assoc (:asserted-content capability)
                                              :dynamics f4/dynamics)))
                other-decision (records/make-clusion
                                (assoc admitted :id nil
                                       :asserted-content
                                       (update (:asserted-content admitted)
                                               :decision/record assoc
                                               :candidate-action/id "other-candidate")))
                _ (doseq [[mid r] [["wrong-principal" wrong-principal]
                                   ["wrong-dynamics" wrong-dynamics]
                                   ["other-decision" other-decision]]]
                    (is (:ok (assert-record! cfg mid r))))
                cases [["no-cap" "executor-a" "missing-cap" (:id admitted) expected
                        :arxana.execution/capability-missing]
                       ["principal" "executor-a" (:id wrong-principal) (:id admitted) expected
                        :arxana.execution/principal-not-capable]
                       ["dynamics" "executor-a" (:id wrong-dynamics) (:id admitted) expected
                        :arxana.execution/dynamics-not-capable]
                       ["deferred" "executor-a" (:id capability) (:id deferred) expected
                        :arxana.execution/not-admitted]
                       ["other-admission" "executor-a" (:id capability) (:id other-decision) expected
                        :arxana.execution/admission-for-different-candidate]
                       ["stale" "executor-a" (:id capability) (:id admitted) "old-revision"
                        :arxana.execution/stale-revision]]]
            (doseq [[n principal cap decision revision code] cases]
              (let [result (get-in (execute (str "refuse-" n) principal cap decision revision
                                            (str "2026-11-08T13:02:0"
                                                 (.indexOf cases [n principal cap decision revision code])
                                                 ".000Z"))
                                   [:ok])
                    attempt (read-record cfg (:attempt/id result))
                    successor (read-record cfg (:composite/id result))
                    receipt-stage (read-record cfg
                                               (get-in (last (:endpoints successor))
                                                       [:target :clusion/id]))]
                (is (= :refused (:result/status result)) n)
                (is (= code (:refusal/code result)) n)
                (is (= :refused (:outcome attempt)) n)
                (is (nil? (get-in receipt-stage
                                  [:asserted-content :action/receipt])) n)
                (is (= [] (:result/edge-ids result)) n)))))))))
