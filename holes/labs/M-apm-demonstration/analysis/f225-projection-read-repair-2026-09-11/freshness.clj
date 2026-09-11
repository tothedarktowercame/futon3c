(require '[test-temporal :as t] '[clojure.test :as test]
         '[futon1b-graph :as graph] '[futon1b-server :as server]
         '[xtdb.node :as xtn])
(with-open [node (xtn/start-node)]
  (binding [t/*node* node test/*report-counters* (ref test/*initial-report-counters*)]
    (test/test-vars [#'t/memory-edge-write-advances-projection-generation])
    (let [id "hx-projection-generation-memory"
          endpoint "pattern/projection-generation-memory"
          before (graph/memory-projection-components node {:endpoints [endpoint] :limit 3})]
      (test/is (= :reviewed (get-in before [:groups 0 :components 0 :edge :hx/props :attachment-status])))
      (test/is (:ok (server/upsert-hyperedge!
                    node (assoc-in (get-in before [:groups 0 :components 0 :edge])
                                   [:hx/props :attachment-status] :proposed))))
      (let [changed (graph/memory-projection-components node {:endpoints [endpoint] :limit 3})]
        (test/is (= :proposed (get-in changed [:groups 0 :components 0 :edge :hx/props :attachment-status])))
        (test/is (> (get-in changed [:temporal-basis :projection-generation])
                    (get-in before [:temporal-basis :projection-generation]))))
      (test/is (:ok (graph/retract-documents! node {:documents [{:table :hyperedges :id id}]})))
      (test/is (= [] (get-in (graph/memory-projection-components node {:endpoints [endpoint] :limit 3})
                             [:groups 0 :components]))))
    (prn @test/*report-counters*)
    (when (pos? (+ (:fail @test/*report-counters*) (:error @test/*report-counters*)))
      (throw (ex-info "freshness validation failed" @test/*report-counters*)))))
(shutdown-agents)
