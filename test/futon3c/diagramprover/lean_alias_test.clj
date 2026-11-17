(ns futon3c.diagramprover.lean-alias-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.lean-alias :as a]
            [futon3c.diagramprover.lean-import :as li]
            [futon3c.diagramprover.term-schema :as ts]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.strategy :as s]
            [futon3c.diagramprover.findings :as f]))

;; m93J06 is the complete capture. m00A03 exceeded 2 MB: retain the header
;; (hits adjusted to 1) and hit 10, selected by alias.source == "registry" and
;; alias.shape == "reindexed" (first such hit). All hit fields/pins are unchanged.
(def fixtures "test/futon3c/diagramprover/fixtures/")
(def hits (delay (:hits (li/load-hits (str fixtures "m93J06-exists_global_solution.jsonl")))))
(defn host [hit] (:host (a/annotate (:host (li/host hit {:loader-sha "fixture-loader"})) hit)))
(def witness (delay (first (filter #(= 2 (get-in % [:alias :n_args])) @hits))))
(defn schemas [b] (vec (concat (a/spine-schemas a/alias-apply (range 1 5))
                             (a/spine-schemas b (range 1 5)))))
(def full (schemas a/family-reindex))
(def opts {:observe a/observe})
(defn nonjoining [r] (filterv #(= :non-joining (:status %)) (:pairs r)))
(defn report [h] (hp/host-pairs (a/without-exemptions full) h opts))
(defn normal [ss h] (s/normalise-strategy ss h opts))
(defn defect [ss h] (get-in (normal ss h) [:observation :reindexed-source-symbols]))
(defn drop-guard [ss g]
  (mapv #(-> % (update :domain-guards disj g)
             (update :guards (fn [gs] (filterv (fn [[n _]] (not= n g)) gs)))) ss))
(def pins {:checker-sha "fixture-checker" :schema-digest "fixture-schema" :mode :unordered
           :max-states 100 :event-time "2026-11-17T00:00:00.000Z"})

(deftest captured-pair-and-findings-c1
  (let [h (host @witness) r (report h) [p] (nonjoining r)
        raw (:host (li/host @witness {:loader-sha "fixture-loader"}))]
    (is (= 1 (count (nonjoining r))))
    (is (= [:alias-apply-2 :family-reindex-2] (:schemas p)))
    (is (= :root (:kind p)))
    (is (= [] (:position p)))
    (is (= 3 (count (:states r))))
    (is (= #{0 1} (set (map #(get (a/observe %) :reindexed-source-symbols) (:results p)))))
    (is (every? f/valid-finding? (f/findings r pins)))
    (is (= 1 (count (f/findings r pins))))
    ;; Mutation: missing annotation must remove this conflict, not pass vacuously.
    (is (empty? (nonjoining (report raw))))))

(deftest exemption-sensitivity-c2
  (let [h (host @witness) p (first (nonjoining (report h)))
        settled (s/settled-by full h p opts)
        gs (filter #(= :guard (:kind %)) (:variants settled))
        domain (mapv #(update % :domain-guards conj :symbol-has-binder-suffix) full)
        high-a (mapv #(if (= :alias-apply (:base-name %)) (assoc % :priority 2) %) full)]
    (is (= #{:guard} (:settled-by settled)))
    (is (some #(and (= :family-reindex-2 (:schema %))
                    (= :symbol-has-binder-suffix (:guard %)) (:changed? %)) gs))
    (is (= 0 (get-in settled [:baseline :observation :reindexed-source-symbols])))
    (is (not (contains? (:settled-by (s/settled-by domain h p opts)) :guard)))
    ;; Raising A removes result sensitivity, although blocked receipts still label guard.
    (is (every? #(not (:changed? %))
                (filter #(= :guard (:kind %)) (:variants (s/settled-by high-a h p opts)))))))

(deftest defect-provenance-and-registry-c3-c4
  (let [h (host @witness) planted (host (assoc-in @witness [:alias :suffix_match] true))
        provenance (schemas a/family-reindex-provenance)
        registry-hit (first (:hits (li/load-hits (str fixtures "m00A03-bridge_1.jsonl"))))
        registry (host registry-hit)
        rn (normal full registry)
        ;; Broken provenance variant reads the suffix bit: exactly the printer variant.
        broken-provenance (schemas a/family-reindex)]
    (is (= 1 (defect (a/without-exemptions full) h)))
    (is (= :app (first (:normal-form (normal (a/without-exemptions full) h)))))
    (is (= :indexed (first (second (:normal-form (normal (a/without-exemptions full) h))))))
    (is (= 0 (defect full h) (defect provenance h)))
    (is (= 1 (defect full planted) (defect broken-provenance planted)))
    (is (= 0 (defect provenance planted)))
    (is (= "registry" (get-in registry-hit [:alias :source])))
    (is (= 0 (get-in rn [:observation :reindexed-source-symbols])))
    (is (some #(= :family-reindex (:base-name (:schema %))) (mapcat #(ts/instances % registry) full)))
    ;; Mutation: counting all indexed nodes would wrongly flag the registry symbol.
    (is (pos? (count (filter #(and (vector? %) (= :indexed (first %)))
                            (tree-seq vector? rest (:normal-form rn))))))))

(deftest arity-bound-c5
  (let [h (reduce (fn [x _] [:app x [:i]]) [:lam [:bvar 0]] (range 5))]
    (is (= {:spine-arity-out-of-range 1} (a/spine-out-of-range h 4)))
    (is (= {:spine-arity-out-of-range 0} (a/spine-out-of-range [:i] 4)))
    (is (not= {} (a/spine-out-of-range [:i] 4)))))

(deftest maximal-spine-c6
  (let [h (host @witness) mutant (drop-guard full :maximal-spine)
        inner #(filter (fn [i] (and (= [0] (:position i))
                                   (#{:alias-apply-1 :family-reindex-1} (:name i))))
                       (mapcat (fn [schema] (ts/instances schema h)) %))]
    (is (empty? (inner full)))
    (is (= 0 (defect full h)))
    (is (seq (inner mutant)))
    (is (some #(= [0] (:position %))
              (:pairs (hp/host-pairs (a/without-exemptions mutant) h opts))))))

(deftest all-captured-hits-c7
  (let [summarise (fn [hit]
                    (let [r (report (host hit)) ps (nonjoining r)]
                      {:hit (:hit hit) :states (count (:states r)) :status (:status r)
                       :nonjoining (count ps) :root (count (filter #(= [] (:position %)) ps))}))
        two (mapv summarise (filter #(= 2 (get-in % [:alias :n_args])) @hits))
        one (mapv summarise (filter #(= 1 (get-in % [:alias :n_args])) @hits))]
    (println "C7" (pr-str {:two two :one one}))
    (is (= 31 (count two)))
    (is (= 31 (count (filter #(= 1 (:nonjoining %) (:root %)) two))))))
