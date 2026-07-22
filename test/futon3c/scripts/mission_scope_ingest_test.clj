(ns futon3c.scripts.mission-scope-ingest-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.scripts.mission-scope-ingest :as ingest]))

(deftest removal-prefers-live-batch-contract
  (let [calls (atom [])
        documents [{:table :hyperedges :id "hx-1"}
                   {:table :entities :id "scope-1"}]]
    (with-redefs [ingest/http-edn
                  (fn [_ method url & [body]]
                    (swap! calls conj [method url body])
                    {:status 200 :body {:ok true :count 2}})]
      (is (= {:deleted-count 2
              :retained-entity-count 0
              :retraction-mode :batch}
             (#'ingest/delete-docs! :client "http://substrate" "api" documents)))
      (is (= 1 (count @calls)))
      (is (= [:post "http://substrate/api/alpha/documents/retract"]
             (subvec (first @calls) 0 2)))
      (is (= documents (get-in @calls [0 2 :documents]))))))

(deftest removal-falls-back-only-for-an-unported-route
  (let [calls (atom [])
        documents [{:table :hyperedges :id "hx-1"}
                   {:table :entities :id "scope-1"}]]
    (with-redefs [ingest/http-edn
                  (fn [_ method url & [body]]
                    (swap! calls conj [method url body])
                    (cond
                      (= url "http://substrate/api/alpha/documents/retract")
                      {:status 404 :body "No context found for request"}

                      (= method :get)
                      {:status 200 :body {:hx/id "hx-1" :hx/type :mission-scope/phase
                                          :hx/endpoints ["mission" "scope-1"]}}

                      :else {:status 200 :body {:ok true :retracted? true}}))]
      (is (= {:deleted-count 1
              :retained-entity-count 1
              :retraction-mode :per-hyperedge-archival}
             (#'ingest/delete-docs! :client "http://substrate" "api" documents)))
      (is (= 3 (count @calls)))
      (is (= [:post "http://substrate/api/alpha/hyperedge"]
             (subvec (last @calls) 0 2)))
      (is (= {:hx/id "hx-1" :hx/type :mission-scope/phase
              :hx/endpoints ["mission" "scope-1"] :hx/op "retract"
              :penholder "api"}
             (last (last @calls)))))))

(deftest removal-does-not-mask-a-genuine-rejection
  (let [calls (atom 0)]
    (with-redefs [ingest/http-edn
                  (fn [& _]
                    (swap! calls inc)
                    {:status 403 :body {:error {:layer 3 :reason :forbidden}}})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"request failed"
                            (#'ingest/delete-docs!
                             :client "http://substrate" "api"
                             [{:table :hyperedges :id "hx-1"}])))
      (is (= 1 @calls)))))

(deftest operator-gates-become-stable-typed-substrate-records
  (let [raw-scope {:scope-id "M-learning-loop:scope-999"
                   :binder-type "operator-gate"
                   :parent nil
                   :ends [{:role "entity" :ident "M-learning-loop"}
                          {:role "environment"
                           :name "operator gate: operator-acceptance"
                           :phase "head"}
                          {:role "operator-gate"
                           :kind "operator-acceptance"
                           :text "inspect the capability graph"
                           :source-line 4}]
                   :hx/content {:position 80 :end 150}
                   :gate-kind "operator-acceptance"
                   :gate-text "inspect the capability graph"
                   :source-line 4}
        [scope] (#'ingest/stable-scopes-for-binder
                 "M-learning-loop" "/missing/M-learning-loop.md"
                 [raw-scope] "operator-gate")
        entity (#'ingest/scope-entity-spec
                "M-learning-loop" "/missing/M-learning-loop.md" scope)
        hyperedge (#'ingest/scope-hyperedge
                   {:id "mission-doc/learning-loop"
                    :external-id "M-learning-loop"}
                   {:id (:id entity) :name (:name entity)} [] scope)]
    (is (some #{"operator-gate"}
              (var-get #'ingest/structural-binders)))
    (is (= "learning-loop/operator-gate/operator-acceptance/inspect-the-capability-graph"
           (:scope-id scope)))
    (is (= "scope/operator-gate" (:type entity)))
    (is (= {:operator-gate/kind "operator-acceptance"
            :operator-gate/text "inspect the capability graph"
            :operator-gate/source-line 4}
           (select-keys (:props entity)
                        [:operator-gate/kind :operator-gate/text
                         :operator-gate/source-line])))
    (is (= "mission-scope/operator-gate" (:hx/type hyperedge)))
    (is (= {:operator-gate/kind "operator-acceptance"
            :operator-gate/text "inspect the capability graph"
            :operator-gate/source-line 4}
           (select-keys (:props hyperedge)
                        [:operator-gate/kind :operator-gate/text
                         :operator-gate/source-line])))))
