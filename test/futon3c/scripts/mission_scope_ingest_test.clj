(ns futon3c.scripts.mission-scope-ingest-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.scripts.mission-scope-ingest :as ingest]))

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
