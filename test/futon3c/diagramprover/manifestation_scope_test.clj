(ns futon3c.diagramprover.manifestation-scope-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.sentence-records :as s]
            [futon3c.diagramprover.sentence-records-test :as fixture]
            [futon3c.diagramprover.store-finding-view-test :as fake]
            [futon3c.arxana.store :as store]))
(def history (delay (s/history-records (str fixture/base "a97J08-sentence-history.json"))))
(deftest scoped-history-and-preserved-ids
  (let [h @history times (mapv #(get-in % [:asserted-content :renderer-commit-time]) (take 8 h))]
    (is (every? #(= [:sentence/manifestation 2] (:type-revision %)) (take 8 h)))
    (is (every? #(= :declaration (get-in % [:asserted-content :scope])) (take 8 h)))
    (is (= times (sort times)))
    (is (every? #(re-matches #".*T.*\.\d{3}Z" %) times))
    (is (= ["arxana:clusion:sha256:d09342931ee7a319f1f473f83d26fb70b68e767137e4a07a73a56185ad458506"
            "arxana:clusion:sha256:dd87f2eb160d43d426f0ec35c850ac3e3f6d6277508bccdcc2fc30db4b8df6d9"]
           (mapv :id (take-last 2 h))))
    (println :revision-2-ids (mapv :id (take 8 h)))))
(deftest invalid-scope-refused
  (is (= :sentence/invalid-scope
         (try (s/scoped-manifestation {:id "fixture-role"} "commit" "2026-11-17T00:00:00Z" "sha" [] :sentence)
              nil (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))))
(deftest two-index-reader
  (let [b @fixture/built rs (vals (into {} (map (juxt :id identity) (concat @history (:manifestations b)))))
        st (fake/fake-store rs nil (atom []) false)
        outer (:id (first (:roles b))) inner (:id (second (:roles b)))
        result (s/role-manifestations st outer fixture/at)]
    (is (= 14 (count rs)))
    (is (= 10 (count (:manifestations result))))
    (is (= {:declaration 8 :role 2} (frequencies (map :scope (:manifestations result)))))
    (is (= [:role] (mapv :scope (:manifestations (s/role-manifestations st inner fixture/at)))))
    (doseq [rev [1 2] refusal [{:status :stale-index} {:status :ok :truncated? true}]]
      (let [bad (reify store/ArxanaGraphStore
                  (bounded-query! [_ q]
                    (if (= [:sentence/manifestation rev] (:edge-type q)) {:ok refusal}
                        (store/bounded-query! st q))))]
        (is (= :inconclusive (:status (s/role-manifestations bad outer fixture/at))))))
    (is (empty? (:manifestations (s/role-manifestations st outer "2026-01-01T00:00:00.000Z"))))))
(deftest revision-3-is-read
  (let [b @fixture/built inner (second (:roles b))
        v3 (assoc (s/scoped-manifestation-v3 inner "9756e848" "2026-11-18T06:15:39Z" "sha" ["w"] :role)
                  :event-time "2026-11-18T06:15:39.000Z")
        rs (vals (into {} (map (juxt :id identity) (concat @history (:manifestations b) [v3]))))
        st (fake/fake-store rs nil (atom []) false)
        got (:manifestations (s/role-manifestations st (:id inner) "2026-11-19T00:00:00.000Z"))]
    (is (= 2 (count got)))
    (is (some #(and (= (:id v3) (get-in % [:record :id])) (= :role (:scope %))) got))
    (let [bad (reify store/ArxanaGraphStore
                (bounded-query! [_ q]
                  (if (= [:sentence/manifestation 3] (:edge-type q)) {:ok {:status :stale-index}}
                      (store/bounded-query! st q))))]
      (is (= :inconclusive (:status (s/role-manifestations bad (:id inner) "2026-11-19T00:00:00.000Z")))))))
