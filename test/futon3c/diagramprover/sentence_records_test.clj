(ns futon3c.diagramprover.sentence-records-test
  (:require [clojure.test :refer [deftest is]] [clojure.edn :as edn]
            [futon3c.diagramprover.sentence-records :as s]
            [futon3c.diagramprover.store-finding-view-test :as fake]
            [futon3c.diagramprover.finding-view :as v]
            [futon3c.arxana.store-http :as http]
            [futon3c.diagramprover.publish-findings :as p]))
(def base "test/futon3c/diagramprover/fixtures/")
(def built (delay (s/records-from-switch-off (str base "a97J08-sentence-switch-off.json")
                                           (edn/read-string (slurp (str base "sentence-q83.edn"))))))
(def at "2026-11-18T01:00:00.000Z")
(defn rule [suffix] (first (for [[name r] (:rules @built) :when (.endsWith ^String name suffix)] r)))
(defn store [rs] (fake/fake-store rs nil (atom []) false))
(deftest planted-and-endpoint-mutation
  (let [b @built f (rule "._merge_right_or_splits") g (rule "._sentence_core")
        ids (set (map :id (:roles b)))
        sup (v/supersedes-record "fixture-successor" (:id f) "2026-11-18T00:00:00.000Z")
        sg (v/supersedes-record "fixture-successor" (:id g) "2026-11-18T00:00:00.000Z")
        future (assoc sup :event-time "2026-11-19T00:00:00.000Z")]
    (is (= s/q83-id (:id f)))
    (is (= 17 (count (:rules b))))
    (is (= 5 (count ids)))
    (is (= ids (set (:roles (s/rule-fibre (store (:records b)) (:id f) at)))))
    (is (empty? (:roles (s/rule-fibre (store (:records b)) (:id g) at))))
    (doseq [id ids]
      (is (= [(:id sup)] (:stale-because (s/sentence-view (store (conj (:records b) sup)) id at))))
      (is (false? (:stale? (s/sentence-view (store (conj (:records b) sg)) id at))))
      (is (false? (:stale? (s/sentence-view (store (conj (:records b) future)) id at)))))
    (let [broken (mapv #(if (= [:rule/application 1] (:type-revision %))
                         (-> % (assoc-in [:asserted-content :rule] (get-in % [:endpoints 1 :target :clusion/id]))
                             (update :endpoints subvec 0 1)) %) (:records b))]
      (is (empty? (:roles (s/rule-fibre (store broken) (:id f) at)))))))
(deftest interventions-and-validation
  (let [b @built r (rule ".render.proof_body") outer (:id (first (:roles b)))
        app (first (filter #(and (= outer (get-in % [:endpoints 0 :target :clusion/id]))
                                (= (:id r) (get-in % [:endpoints 1 :target :clusion/id]))) (:applications b)))
        iv (get-in app [:asserted-content :interventions])]
    (is (= ["Q55" "Q66"] (mapv :unit iv)))
    (is (= [["In either case, the claim holds."] ["In either case, we obtain a contradiction."]]
           (mapv :switched-wordings iv)))
    (is (not= (set (map :switched-wordings iv)) #{(:switched-wordings (last iv))})))
  (is (= {:validated (count (:records @built))}
         (p/validate-batch! {:message-id "sentence-offline" :rows (mapv #(hash-map :operation :clusion/assert :record %) (:records @built))}))))
(deftest history-and-index-refusals
  (let [history (s/history-records (str base "a97J08-sentence-history.json"))
        st (store history)
        back (mapv #(get-in (http/get-entity-as-of st (:id %) at) [:ok :asserted-content :wordings]) history)]
    (println :counts (frequencies (map :type-revision (:records @built))) :history back)
    (is (= 10 (count history)))
    (is (= (mapv #(get-in % [:asserted-content :wordings]) history) back))
    (is (= [] (nth back 2) (nth back 3)))
    (is (= ["In all four cases, we obtain a contradiction."] (last back))))
  (doseq [override [{:status :stale-index} {:status :ok :truncated? true}]]
    (let [st (fake/fake-store (:records @built) override (atom []) false)]
      (is (= :inconclusive (:status (s/sentence-view st (:id (first (:roles @built))) at))))
      (is (= :inconclusive (:status (s/rule-fibre st s/q83-id at)))))))
