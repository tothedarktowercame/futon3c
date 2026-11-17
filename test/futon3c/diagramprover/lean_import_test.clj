(ns futon3c.diagramprover.lean-import-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [futon3c.diagramprover.lean-import :as li]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.host-pairs-test :as q]
            [futon3c.diagramprover.term-schema :as ts]))

(def capture "test/futon3c/diagramprover/fixtures/a02J03-borel_pi.jsonl")
(def zero-capture "test/futon3c/diagramprover/fixtures/m02J01-abs_integral.jsonl")
(def opts {:loader-sha "fixture-loader"})
(def search-opts {:max-states 12 :settled-by? true})
(defn conflicts [report]
  (filter #(= :non-joining (:status %)) (:pairs report)))
(def imported
  (delay (mapv (fn [hit]
                 (let [loaded (li/host hit opts)]
                   {:hit (:hit hit) :loaded loaded
                    :report (when (:host loaded)
                              (hp/host-pairs [ts/beta li/lean-family] (:host loaded) search-opts))}))
               (:hits (li/load-hits capture)))))

(deftest real-capture-comparison-and-rename-mutation
  (let [results @imported
        found (filter #(seq (conflicts (:report %))) results)
        shapes (set (mapcat #(map li/finding-shape (conflicts (:report %))) found))
        transcribed (set (map li/finding-shape (conflicts (hp/host-pairs [q/r1 q/r2] q/host search-opts))))
        hit (nth (:hits (li/load-hits capture)) 17)
        renamed (walk/postwalk #(if (= % "Classical.choose") "Fixture.other" %) hit)
        broken (hp/host-pairs [ts/beta li/lean-family] (:host (li/host renamed opts)) search-opts)]
    (is (= 44 (count results)))
    (is (every? #(contains? (:loaded %) :host) results))
    (is (= [17 18 20 22 25 27 29 31 33 35 36] (mapv :hit found)))
    ;; Correction: broad imported schema + choose guard yields guard evidence;
    ;; narrow transcribed schema has no guard, so its label stays nothing.
    (is (= #{{:rules #{:beta :family} :status :non-joining :settled-by #{:guard} :family-applied? true}} shapes))
    (is (= #{{:rules #{:beta :family} :status :non-joining :settled-by #{:nothing} :family-applied? true}} transcribed))
    (is (= (set (map #(dissoc % :settled-by) shapes)) (set (map #(dissoc % :settled-by) transcribed))))
    (is (empty? (conflicts broken)))
    (is (= {:complete 36 :inconclusive 8} (frequencies (map #(get-in % [:report :status]) results))))
    (println "Importer B1: hits" (mapv :hit found) "states" (reduce + (map #(count (get-in % [:report :states])) results)))))

(def planted
  {:root "r" :nodes
   {:r {:k "app" :c ["l" "i"] :arg_infos [0]}
    :l {:k "lam" :c ["d" "body"]}
    :d {:k "sort" :c []}
    :body {:k "app" :c ["ch" "v"] :arg_infos [0]}
    :ch {:k "const" :name "Classical.choose" :c []}
    :v {:k "bvar" :index 0 :c []}
    :i {:k "fvar" :name "fixture-i" :c []}}})

(deftest zero-hit-capture-and-planted-choose
  ;; No m02J01 host exists: this synthetic planted host is the substantive test,
  ;; not a claim that a no-conflict loop over zero hosts verified the printer.
  (let [loaded (li/load-hits zero-capture)
        report (hp/host-pairs [ts/beta li/lean-family] (:host (li/host planted opts)) search-opts)]
    (is (= {:hosts 0} {:hosts (count (:hits loaded))}))
    (is (= 0 (get-in loaded [:header :hits])))
    (is (seq (conflicts report)))
    (is (every? :family-applied? (map li/finding-shape (conflicts report))))))

(deftest app-spine-left-fold-and-right-fold-mutation
  (let [hit {:root "a" :nodes {:a {:k "app" :c ["l" "i" "j"] :arg_infos [1 0]}
                               :l {:k "lam" :c ["s" "v"]}
                               :s {:k "sort" :c []} :v {:k "bvar" :index 0 :c []}
                               :i {:k "fvar" :name "i" :c []} :j {:k "fvar" :name "j" :c []}}}
        term (:host (li/host hit opts))
        children [[:lam [:bvar 0]] [[:fvar "i"]] [[:fvar "j"]]]
        wrong (reduce (fn [tail head] [:app head tail]) (last children) (reverse (butlast children)))]
    (is (= [:app [:app [:lam [:bvar 0]] [[:fvar "i"]]] [[:fvar "j"]]] term))
    (is (= [1 0] [(get (meta (nth (second term) 2)) :arg-info) (get (meta (nth term 2)) :arg-info)]))
    (is (not= term wrong))
    (is (= 1 (count (ts/instances ts/beta term))))
    (is (not= (map :binding (ts/instances ts/beta term)) (map :binding (ts/instances ts/beta wrong))))))

(deftest pi-projection-and-binder-mutation
  (let [hit {:root "p" :nodes {:p {:k "pi" :c ["one" "body"]}
                               :one {:k "bvar" :index 1 :c []}
                               :body {:k "app" :c ["pair" "zero" "one"] :arg_infos [0 0]}
                               :pair {:k "const" :name "Fixture.pair" :c []}
                               :zero {:k "bvar" :index 0 :c []}}}
        term (:host (li/host hit opts))
        result (ts/subst term [:bvar 0])
        wrong (ts/subst [:pi (second term) (second (nth term 2))] [:bvar 0])]
    (is (= [:pi [:bvar 0] [:lam [:app [:app [[:const "Fixture.pair"]] [:bvar 0]] [:bvar 1]]]] result))
    (is (not= result wrong))))

(deftest refuses-without-truncating-and-flags
  (let [hit (first (:hits (li/load-hits capture)))
        full (li/host hit opts) refused (li/host hit (assoc opts :max-tree-nodes 2))
        null-host (:host (li/host {:root "n" :nodes {:n {:k "const" :c [] :name "F" :is_proof nil}}} opts))
        false-host (:host (li/host {:root "n" :nodes {:n {:k "const" :c [] :name "F" :is_proof false}}} opts))]
    (is (= :tree-too-large (:refused refused)))
    (is (not (contains? refused :host)))
    (is (= (:tree-nodes full) (:tree-nodes refused)))
    (is (not= refused (assoc refused :host [:cut])))
    (is (= :unknown (li/proof? null-host)))
    (is (= :unknown (li/proof? [:fresh])))
    (is (= false (li/proof? false-host)))
    (is (not= (li/proof? null-host) (boolean nil)))
    (is (= :refused-upstream (:refused (li/host {:refused "dag-too-large"} opts))))
    (is (= :probe-error (:refused (li/host {:probe_error "fixture"} opts))))
    (is (= "fixture-loader" (get-in full [:pins :loader-sha])))
    (is (= (:pins hit) (select-keys (:pins full) (keys (:pins hit)))))
    (is (contains? (meta (:host full)) :domain))))

(deftest other-constructors-and-mdata
  (let [hit {:root "m" :nodes {:m {:k "mdata" :c ["l"] :note "fixture"}
                               :l {:k "let" :c ["s" "p" "v"]}
                               :s {:k "sort" :c []}
                               :p {:k "proj" :type_name "Fixture.T" :index 1 :c ["lit"]}
                               :lit {:k "lit" :kind "nat" :value 3 :c []}
                               :v {:k "mvar" :name "fixture-m" :c []}}}
        loaded (li/host hit opts)]
    (is (= [:let [[:sort]] [[:proj "Fixture.T" 1] [[:lit "nat" 3]]] [:lam [[:mvar "fixture-m"]]]] (:host loaded)))
    (is (= #{:mdata-stripped :let-body-under-lam} (set (:projection loaded))))
    (is (= "fixture" (:note (first (:stripped-mdata (meta (:host loaded)))))))))
