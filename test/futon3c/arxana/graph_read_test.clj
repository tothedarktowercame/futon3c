(ns futon3c.arxana.graph-read-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.graph-read :as read]
            [futon3c.arxana.store :as store]))

(defn graph-store [adj]
  (reify store/ArxanaGraphStore
    (get-entity [_ _]) (find-entities [_ _]) (put-entity! [_ _])
    (get-hyperedge [_ _]) (find-hyperedges [_ _]) (put-hyperedges! [_ _])
    (retract-hyperedge! [_ _ _]) (capabilities [_] {})
    (conditional-write! [_ _])
    (bounded-query! [_ q] {:ok {:status :ok :watermark "w"
                                :edge-ids (vec (get adj (:endpoint-id q) []))}})))

(deftest read-bounded--mirrors-lean-read_bounded
  (let [result (read/traverse (graph-store {"0" ["1" "2"] "1" ["3"]})
                              {:start/id "0" :edge/type [:x 1]
                               :endpoint/role :source :max-depth 10 :max-nodes 2})]
    (is (= :truncated (:status result)))
    (is (= :max-nodes (:truncation/reason result)))
    (is (<= (count (:nodes result)) 2))))

(deftest read-nodup--mirrors-lean-read_nodup
  (let [result (read/traverse (graph-store {"0" ["1"] "1" ["2"] "2" ["0"]})
                              {:start/id "0" :edge/type [:x 1]
                               :endpoint/role :source :max-depth 10 :max-nodes 10})]
    (is (= :complete (:status result)))
    (is (= ["0" "1" "2"] (:nodes result)))
    (is (= (count (:nodes result)) (count (distinct (:nodes result)))))))

(deftest lean-worked-example-three-cycle-completes
  (is (= {:status :complete :nodes ["0" "1" "2"]}
         (read/traverse (graph-store {"0" ["1"] "1" ["2"] "2" ["0"]})
                        {:start/id "0" :edge/type [:x 1] :endpoint/role :source
                         :max-depth 10 :max-nodes 10}))))

(deftest lean-worked-example-maxnodes-two-truncates
  (let [r (read/traverse (graph-store {"0" ["1"] "1" ["2"] "2" ["0"]})
                         {:start/id "0" :edge/type [:x 1] :endpoint/role :source
                          :max-depth 10 :max-nodes 2})]
    (is (= :truncated (:status r)))
    (is (= :max-nodes (:truncation/reason r)))
    (is (= ["0" "1"] (:nodes r)))))
