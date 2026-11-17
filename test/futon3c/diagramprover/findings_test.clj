(ns futon3c.diagramprover.findings-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [futon3c.diagramprover.findings :as f]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.host-pairs-test :as q]))

(def pins {:checker-sha "fixture-checker" :schema-digest "fixture-schema"
           :mode :unordered :max-states 100 :event-time "2026-11-17T00:00:00.000Z"})
(def left-rule {:name :left :rule-id "rule:left@1" :pattern '[:f ?x] :guards []
                :rewrite #(vector :a (get % '?x))})
(def right-rule {:name :right :rule-id "rule:right@1" :pattern '[:f ?x] :guards []
                 :rewrite #(vector :b (get % '?x))})
(defn simple [term host-id]
  (f/findings (hp/host-pairs [left-rule right-rule] term {:settled-by? true})
              (assoc pins :host-id host-id)))
(defn nonjoining [fs] (filter #(= :non-joining (:finding/status %)) fs))

(deftest q87-groups-and-explicit-priority-correction
  (let [plain (nonjoining (f/findings (q/run-host [q/r1 q/r2]) pins))
        equal (nonjoining (f/findings (hp/host-pairs [q/r1 q/r2] q/host {:observe q/observe :settled-by? true}) pins))
        ordered (nonjoining (f/findings (hp/host-pairs [q/r1 (assoc q/r2 :priority 10)] q/host
                                                     {:observe q/observe :settled-by? true}) pins))]
    (is (= 4 (count plain)))
    (is (every? #(= 3 (:finding/occurrences %)) plain))
    (is (every? #(= :failing (:finding/evaluation %)) plain))
    (is (= #{[:app [:lam [:choose [:app [:h] [:bvar 0]]]] [:bvar 0]]
             [:app [:lam [:choose [:app [:h2] [:bvar 0]]]] [:bvar 0]]
             [:app [:lam [:choose [:app [:h] [:bvar 0]]]] [:i]]
             [:app [:lam [:choose [:app [:h2] [:bvar 0]]]] [:i]]}
           (set (map :finding/overlap plain))))
    (doseq [record plain]
      (is (= (if (= [:i] (last (:finding/overlap record))) #{:created} #{:root})
             (:finding/kinds record)))
      (is (= (if (= [:i] (last (:finding/overlap record))) 1 0)
             (count (:finding/witness-path record)))))
    ;; E2 as originally phrased uses equal priorities; it does NOT settle.
    (is (every? #(= #{:nothing} (:finding/settled-by %)) equal))
    (is (every? #(= :failing (:finding/evaluation %)) equal))
    (is (= 4 (count ordered)))
    (is (every? #(contains? (:finding/settled-by %) :priority) ordered))
    (is (every? #(= :settled (:finding/evaluation %)) ordered))))

(deftest nothing-residual-and-priority-mutation
  (let [report (hp/host-pairs [left-rule right-rule] [:f [:c]] {:settled-by? true})
        records (f/findings report pins)
        record (first records)
        accepted (first (f/findings report pins {:residuals {(f/finding-key record) "residual:test"}}))
        ordered (first (f/findings (hp/host-pairs [(assoc left-rule :priority 1) right-rule]
                                                [:f [:c]] {:settled-by? true}) pins))]
    (is (= 1 (count records)))
    (is (= ["rule:left@1" "rule:right@1"] (:finding/rules record)))
    (is (= #{:nothing} (:finding/settled-by record)))
    (is (= :failing (:finding/evaluation record)))
    (is (= :failing-accepted (:finding/evaluation accepted)))
    (is (= "residual:test" (:finding/residual accepted)))
    (is (f/valid-finding? accepted))
    (is (not (f/valid-finding? (assoc accepted :finding/evaluation :met))))
    (is (= #{:priority} (:finding/settled-by ordered)))
    (is (= :settled (:finding/evaluation ordered)))))

(deftest multiple-hosts-and-status-key-mutation
  (let [c1 (first (simple [:f [:c]] "h1"))
        c2 (first (simple [:f [:c]] "h2"))
        d (first (simple [:f [:d]] "h3"))
        merged (f/merge-hosts [c1 c2 d])
        c (first (filter #(= [:f [:c]] (:finding/overlap %)) merged))
        joining (assoc c1 :finding/status :joining :finding/evaluation :met)
        correct (f/merge-hosts [c1 joining])
        broken (group-by #(take 2 (f/finding-key %)) [c1 joining])]
    (is (= 2 (count merged)))
    (is (= ["h1" "h2"] (:finding/hosts c)))
    (is (= 2 (:finding/host-count c)))
    (is (= 2 (:finding/occurrences c)))
    (is (= #{1 2} (set (map :finding/host-count merged))))
    (is (= [c1 c2] (:finding/host-evidence c)))
    (is (every? f/valid-finding? merged))
    (is (= 2 (count correct)))
    (is (= 1 (count broken)))))

(deftest joining-and-inconclusive-evaluations
  (let [joining (f/findings (q/run-host [q/guarded q/r2]) pins)
        bounded (f/findings (hp/host-pairs [left-rule right-rule] [:f [:c]] {:max-states 0})
                           (assoc pins :max-states 0))]
    (is (seq joining))
    (is (every? #(= :met (:finding/evaluation %)) joining))
    (is (seq bounded))
    (is (every? #(= :unknown (:finding/evaluation %)) bounded))))

(deftest shape-and-edn-round-trip
  (let [records (simple [:f [:c]] "h1") record (first records)]
    (is (= records (edn/read-string (f/findings->edn records))))
    (is (not (f/valid-finding? (dissoc record :finding/pins))))
    (is (not (f/valid-finding? (assoc record :finding/evaluation :approved))))
    (is (not (f/valid-finding? (assoc-in record [:finding/pins :event-time] "bad-time"))))
    (is (not (f/valid-finding? (assoc record :function identity))))
    (is (thrown? clojure.lang.ExceptionInfo (f/findings->edn [(dissoc record :finding/pins)])))))

(deftest schema-data-digest
  (let [schemas [left-rule right-rule] digest (f/schema-digest schemas)
        guarded (assoc left-rule :guards [[:guard-a (constantly true)]])]
    (is (re-matches #"[0-9a-f]{64}" digest))
    (is (= digest (f/schema-digest schemas)))
    (is (not= digest (f/schema-digest (reverse schemas))))
    (is (not= digest (f/schema-digest [(assoc left-rule :priority 1) right-rule])))
    (is (not= (f/schema-digest [guarded])
              (f/schema-digest [(assoc guarded :guards [[:guard-b (constantly true)]])])))))
