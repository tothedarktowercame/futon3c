(ns futon3c.diagramprover.opaque-rules-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [futon3c.diagramprover.opaque-rules :as o]
            [futon3c.diagramprover.findings :as f]
            [futon3c.diagramprover.finding-view :as view]
            [futon3c.diagramprover.arxana-records :as ar]))
(def raw (o/load-table "test/futon3c/diagramprover/fixtures/a97J08-step-table-q56-raw,q83.json"))
(def wrapped (o/load-table "test/futon3c/diagramprover/fixtures/a97J08-step-table-q78,q83.json"))
(def a (:start raw))
(def b (get-in raw [:table (keyword a) :q56-raw]))
(def c (get-in raw [:table (keyword a) :q83]))
(def d (get-in raw [:table (keyword c) :q56-raw]))
(def pins {:checker-sha "fixture" :schema-digest "captured-rules" :mode :unordered
           :event-time "2026-11-17T00:00:00.000Z" :host/locator :transcribed})
(defn outer [diff side] (get-in diff [side :observations 0 :outer-arity]))

(deftest version-diff-r1-and-pins
  (let [diff (o/rule-set-diff wrapped [:q78] wrapped [:q83] "supersession:fixture")
        wrong (o/rule-set-diff (assoc wrapped :start c) [:q78] wrapped [:q83] "supersession:fixture")]
    (is (= :different (:comparison diff)))
    (is (= [2 4] [(outer diff :a) (outer diff :b)]))
    (is (= #{a} (get-in diff [:a :normal-forms])))
    (is (= #{c} (get-in diff [:b :normal-forms])))
    (is (not= (outer diff :a) (outer wrong :a)))
    (is (= (:pins wrapped) (get-in diff [:a :pins]) (get-in diff [:b :pins])))
    (is (= "supersession:fixture" (:supersession-id diff)))
    (is (not= diff (dissoc diff :supersession-id)))))

(deftest no-pairs-r2-missing-is-not-same
  (let [r (o/table-pairs wrapped)
        mutant (update wrapped :table #(into {} (map (fn [[s row]] [s (into {} (remove (fn [[_ v]] (= "same" v)) row))]) %)))
        m (o/table-pairs mutant)]
    (is (= :complete (:status r)))
    (is (empty? (:pairs r)))
    (is (empty? (:pairs m)))
    (is (= [:q78 :q83] (:rules r) (:rules m)))
    (is (= :inconclusive (:status m)))))

(deftest raw-pair-r3-and-bound
  (let [r (o/table-pairs raw) p (first (:pairs r))
        fs (o/table-findings raw pins)]
    (is (= 1 (count (:pairs r))))
    (is (= :complete (:status r)))
    (is (= :non-joining (:status p)))
    (is (= [#{b} #{d}] (mapv :normal-forms (:normalisations p))))
    (is (= [false true] (mapv #(get-in % [:observations 0 :well-nested?]) (:normalisations p))))
    (is (= :different (:observation-comparison p)))
    (is (= :unknown (:arity-consistent? (o/observation (get-in raw [:states (keyword b)])))))
    (is (= :inconclusive (:status (first (:pairs (o/table-pairs (assoc raw :bound_hit true)))))))
    (is (= #{:whole-sequence} (:finding/kinds (first fs))))
    (is (= :opaque (get-in fs [0 :finding/pins :rule-form])))
    (is (every? f/valid-finding? fs))
    (is (= fs (edn/read-string (f/findings->edn fs))))))

(deftest supersession-r4-keeps-finding
  (let [r1 (ar/rule-record {:name :q56-raw :pattern [:opaque] :rewrite-doc [:external]} :step)
        r2 (ar/rule-record {:name :q83 :pattern [:opaque] :rewrite-doc [:external]} :step)
        fs (o/table-findings raw pins)
        record (:finding (ar/finding-records (first fs) {:q56-raw (:id r1) :q83 (:id r2)} {}))
        sup (view/supersedes-record (:id r2) (:id r1) "2026-11-17T01:00:00.000Z")
        records [r1 r2 record sup]
        time "2026-11-17T02:00:00.000Z"]
    (is (= #{} (view/stale-findings records "2026-11-17T00:00:00.000Z")))
    (is (= #{(:id record)} (view/stale-findings records time)))
    (is (= {:stale :failing} (:display (view/finding-view records (:id record) time))))
    (is (not= (view/stale-findings records time)
              (view/stale-findings (remove #(= record %) records) time)))))

(deftest arity-diff-r7
  (let [diff (o/rule-set-diff wrapped [:q83] wrapped [:q78 :q83] "supersession:r7")
        oa (get-in diff [:a :observations 0]) ob (get-in diff [:b :observations 0])
        wrong-first (assoc-in diff [:b :normal-forms] #{c})]
    (is (= [#{c} #{d}] (mapv #(get-in diff [% :normal-forms]) [:a :b])))
    (is (= :different (:comparison diff) (:observation-comparison diff)))
    (is (= [4 4] [(outer diff :a) (outer diff :b)]))
    (is (= [true false] [(:arity-consistent? oa) (:arity-consistent? ob)]))
    ;; Mutation 1 loses the arity defect: observation comparison becomes equal.
    (is (= (select-keys oa [:well-nested?]) (select-keys ob [:well-nested?])))
    (is (not= oa ob))
    ;; Mutation 2 stops at the first successor, before q78's move.
    (is (= (get-in wrong-first [:a :normal-forms]) (get-in wrong-first [:b :normal-forms])))
    (is (not= (get-in diff [:a :normal-forms]) (get-in diff [:b :normal-forms])))))

(deftest multiple-normal-forms-and-bound-are-inconclusive
  (let [diff (o/rule-set-diff raw [:q56-raw :q83] wrapped [:q83] "s")]
    (is (= #{b d} (get-in diff [:a :normal-forms])))
    (is (= :inconclusive (:comparison diff)))
    (is (= :inconclusive (:comparison (o/rule-set-diff (assoc wrapped :bound_hit true) [:q83] wrapped [:q83] "s"))))))
