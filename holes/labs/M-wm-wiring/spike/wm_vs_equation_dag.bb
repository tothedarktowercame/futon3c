#!/usr/bin/env bb
;; wm_vs_equation_dag.bb — the wiring map against the equation DAG (futon-2026
;; Figure 6A, p4ng/empirics-futon/gen_aif_dag.bb over aif-equations.edn).
;; A theory edge Ra -> Rb (symbol s) is what an AIF implementation must wire.
;; This joins the R-nodes to the wiring map's boxes BY FILE: the registry's
;; :code strings name the futon2 files that realise each node; a box whose
;; site file is one of them belongs to that node. Exogenous symbols with no
;; node (C) and the flight's producers of A and E are joined by the mission's
;; row table (row 2 = C, row 6 = A, row 7 = E). Then each theory edge is one
;; of: :declared (a map field from a box of Ra to a box of Rb), :boxed-no-field
;; (both ends have boxes, no field between them), :unboxed (an end has no box).
;;   bb wm_vs_equation_dag.bb [map-rev] > out.edn
(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.set :as set] '[clojure.java.shell :as sh])
(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def map-rev (first *command-line-args*))
(def m (edn/read-string {:default tagged-literal} (if map-rev (:out (sh/sh "git" "show" (str map-rev ":" map-path))) (slurp map-path))))
(def reg (edn/read-string {:default (fn [_ v] v)} (slurp "/home/joe/code/futon2/holes/labs/wm-contract/aif-equations.edn")))
(def eqs (remove #(= :retired (:status %)) (:equations reg)))
(def defs (into {} (map (fn [e] [(:defines e) e]) eqs)))
(def exo (into {} (map (fn [x] [(:symbol x) x]) (:exogenous reg))))
(defn source-node [s] (or (:node (defs s)) (:node (exo s))))
;; theory edges at R grain, as gen_aif_dag.bb derives them
(def theory (->> (for [e eqs s (:imports e) :let [a (source-node s) b (:node e)] :when (and a b (not= a b))] [[a b] s])
                 (reduce (fn [acc [ab s]] (update acc ab (fnil conj #{}) s)) {})))
;; node -> files, from the registry's :code strings
(def node-files (reduce (fn [acc e] (update acc (:node e) (fnil into #{}) (map second (re-seq #"([a-z0-9_]+\.clj)" (str (:code e))))))
                        {} (:equations reg)))
(def boxes (remove #(= :test (:box/kind %)) (:boxes m)))
(defn box-file [b] (some-> (or (:site b) (:intended-site b)) :file (str/replace #".*/" "")))
(def node-boxes (into {} (for [[n fs] node-files] [n (set (map :box/id (filter #(fs (box-file %)) boxes)))])))
;; node -> vars, the function names the :code strings name in parentheses (a precise join)
(def node-vars (reduce (fn [acc e] (update acc (:node e) (fnil into #{}) (map second (re-seq #"\(([a-z][a-z0-9-]*[a-z0-9][!?]?)[,) ]" (str (:code e))))))
                       {} (:equations reg)))
(def node-boxes-by-var (into {} (for [[n vs] node-vars] [n (set (map :box/id (filter #(vs (get-in % [:site :var])) boxes)))])))
(def no-site-nodes (vec (sort-by str (for [n (set (concat (map :node eqs) (keep :node (vals exo)))) :when (empty? (node-files n))] n))))
;; the flight's producers of exogenous symbols, by the mission's row table
;; C, A, E by the mission's row table; u (R16's action) is row 0's enactment step; o (R2's observation) is row 10's observation of the publication
(def exo-rows {:C 2 :A 6 :E 7 :u 0 :o 10})
(def row-boxes (fn [r] (set (map :box/id (filter #(= r (:row %)) boxes)))))
(def writers (reduce (fn [acc b] (reduce (fn [a f] (assoc a f (:box/id b))) acc (:writes b))) {} boxes))
(def readers (reduce (fn [acc b] (reduce (fn [a f] (update a f (fnil conj #{}) (:box/id b))) acc (:reads b))) {} boxes))
(defn fields-between [from-boxes to-boxes]
  (for [[f w] writers :when (from-boxes w) r (readers f) :when (to-boxes r)] [f w r]))
(defn edge-status [ba bb] (let [fs (fields-between ba bb)] [(cond (seq fs) :declared (and (seq ba) (seq bb)) :boxed-no-field :else :unboxed) (vec fs)]))
(def edge-report
  (for [[[a b] syms] (sort-by (comp str key) theory)
        :let [[st-file fs-file] (edge-status (node-boxes a) (node-boxes b))
              [st-var fs-var] (edge-status (node-boxes-by-var a) (node-boxes-by-var b))]]
    {:edge [a b] :symbols syms
     :by-file st-file :by-var st-var
     :fields-by-file fs-file :fields-by-var fs-var
     :registry-no-site (vec (filter (set no-site-nodes) [a b]))}))
;; the flight's producers of the registry's exogenous symbols, by the mission's row table
(def producer-report
  (for [[s r] (sort exo-rows)
        :let [from (row-boxes r)
              host (or (:node (exo s)) (:node (defs s)))
              importers (set (map :node (filter #(some #{s} (:imports %)) eqs)))
              fs (fields-between from (set (map :box/id boxes)))
              to-nodes (fn [box] (vec (sort-by str (for [[n bs] node-boxes :when (bs box)] n))))]]
    {:symbol s :produced-by-row r :registry-host-node host :imported-at (vec (sort-by str importers))
     :fields-leaving-the-row (vec (for [[f w rd] fs] [f w rd (to-nodes rd)]))}))
(def node-report (for [n (sort-by str (set (concat (map :node eqs) (keep :node (vals exo)))))]
                   {:node n :files (vec (sort (node-files n))) :boxes (vec (sort (node-boxes n)))}))
(prn {:map (or map-rev "working tree") :registry (:as-of reg)
      :theory-edges (count theory)
      :registry-names-no-site no-site-nodes
      :summary-by-file (frequencies (map :by-file edge-report))
      :summary-by-var (frequencies (map :by-var edge-report))
      :nodes (for [n node-report] (assoc n :vars (vec (sort (node-vars (:node n)))) :boxes-by-var (vec (sort (node-boxes-by-var (:node n))))))
      :edges edge-report
      :producers producer-report})
