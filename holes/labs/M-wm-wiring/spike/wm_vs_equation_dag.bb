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
;;   bb wm_vs_equation_dag.bb [map-rev] [registry-rev] > out.edn
;; registry-rev (or env REGISTRY_REV) reads the registry from that futon2
;; revision, like the map; publication runs pass it so another lane's
;; uncommitted registry edit is not read. Each edge also carries :inventory,
;; its <2>2b class: raw by-var credit is :declared only when wm-term-fields.edn
;; covers every symbol. Partial/wrong-term coverage is :does-not-carry; no
;; correspondence on any credited field is :cannot-tell. This branch still
;; wins over :hole (named in the registry's top-level :holes under :edge or
;; :edges; the entry's :status is
;; carried as :hole-status) wins over :code-path-note (an equation row's :code
;; records a "CODE-PATH NOTE for [<a> <b>]..." marker for the edge, the shape
;; RC7+RC8 used on :belief-state, futon2 9c4cc59a) else :none. Fixture env
;; overrides WM_VS_DAG_MAP_FILE / WM_VS_DAG_REGISTRY_FILE and
;; WM_VS_DAG_TERM_FIELDS_FILE are for the test. The term table's bytes are
;; independently pinned in the output by :term-fields-sha256.
(require '[clojure.edn :as edn] '[clojure.string :as str] '[clojure.set :as set] '[clojure.java.shell :as sh])
(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def registry-path "/home/joe/code/futon2/holes/labs/wm-contract/aif-equations.edn")
(def map-rev (first *command-line-args*))
(def registry-rev (or (second *command-line-args*) (System/getenv "REGISTRY_REV")))
(def map-file (System/getenv "WM_VS_DAG_MAP_FILE"))
(def registry-file (System/getenv "WM_VS_DAG_REGISTRY_FILE"))
(def m (edn/read-string {:default tagged-literal}
         (if map-file (slurp map-file)
             (if map-rev (:out (sh/sh "git" "show" (str map-rev ":" map-path))) (slurp map-path)))))
(def reg (edn/read-string {:default (fn [_ v] v)}
           (if registry-file (slurp registry-file)
               (if registry-rev
                 (:out (sh/sh "git" "-C" "/home/joe/code/futon2" "show" (str registry-rev ":holes/labs/wm-contract/aif-equations.edn")))
                 (slurp registry-path)))))
;; The table is a separate reviewed input, never inferred from field spelling.
(def term-fields-path (or (System/getenv "WM_VS_DAG_TERM_FIELDS_FILE")
                         "holes/labs/M-wm-wiring/wm-term-fields.edn"))
(def term-fields-text (slurp term-fields-path))
(def term-fields (edn/read-string term-fields-text))
(def term-fields-sha256
  (format "%064x" (java.math.BigInteger.
                   1 (.digest (java.security.MessageDigest/getInstance "SHA-256")
                              (.getBytes term-fields-text "UTF-8")))))
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
(def node-vars (reduce (fn [acc e] (update acc (:node e) (fnil into #{}) (map second (re-seq #"\(([a-z][a-z0-9-]*[a-z0-9][!?*]?)[,) :]" (str (:code e))))))
                       {} (:equations reg)))
(def node-boxes-by-var (into {} (for [[n vs] node-vars] [n (set (map :box/id (filter #(vs (get-in % [:site :var])) boxes)))])))
(def no-site-nodes (vec (sort-by str (for [n (set (concat (map :node eqs) (keep :node (vals exo)))) :when (empty? (node-files n))] n))))
;; the flight's producers of exogenous symbols, by the mission's row table
;; C, A, E by the mission's row table; u (R16's action) is row 0's enactment step; o (R2's observation) is row 10's observation of the publication
(def exo-rows {:C 2 :A 6 :E 7 :u 0 :o 10})
(def row-boxes (fn [r] (set (map :box/id (filter #(= r (:row %)) boxes)))))
(def writers (reduce (fn [acc b] (reduce (fn [a f] (assoc a f (:box/id b))) acc (:writes b))) {} boxes))
(def readers (reduce (fn [acc b] (reduce (fn [a f] (update a f (fnil conj #{}) (:box/id b))) acc (:reads b))) {} boxes))
;; a :passes entry (WM-PROVER-PASSES-I, 553d7edf) hands a value positionally from the
;; box whose var its :from :returns-of names (else the declaring box) to its :callee-box;
;; the join credits it as a field between those two boxes (as claude-10 asked at 3a8d3f26).
(def box-by-var (into {} (for [b boxes :let [v (get-in b [:site :var])] :when v] [v (:box/id b)])))
(def passes-edges (vec (for [b boxes p (:passes b)
                             :let [fn-name (some-> (get-in p [:from :returns-of]) (str/replace #".*/" ""))
                                   src (or (and fn-name (box-by-var fn-name)) (:box/id b))
                                   dst (get-in p [:to :callee-box])]
                             :when (and src dst)]
                         [(:value p) src dst])))
(defn fields-between [from-boxes to-boxes]
  (concat (for [[f w] writers :when (from-boxes w) r (readers f) :when (to-boxes r)] [f w r])
          (for [[f w r] passes-edges :when (and (from-boxes w) (to-boxes r))] [f w r])))
(defn edge-status [ba bb] (let [fs (fields-between ba bb)] [(cond (seq fs) :declared (and (seq ba) (seq bb)) :boxed-no-field :else :unboxed) (vec fs)]))
;; <2>2b inventory classes (JOIN-2B-I). Hole edges: the registry's top-level
;; :holes entries name edges under :edge (one) or :edges (several); entries
;; about a bare :symbol are not edges and are ignored.
(def hole-edges
  (into {} (for [h (:holes reg)
                 e (if (:edge h) [(:edge h)] (:edges h))]
             [(vec e) (:status h)])))
;; Code-path-note edges: an equation row's :code records that some of its
;; imported terms reach it through another row's update with the marker
;; "CODE-PATH NOTE for [<a> <b>], ..." (RC7+RC8, futon2 9c4cc59a, on
;; :belief-state for [:R2 :R1] [:R16 :R1] [:R4 :R1]). Match exactly that
;; marker in the :code field -- no other prose -- and take the edge literals
;; of the sentence it introduces (up to the first "(").
(def code-path-note-edges
  (into #{} (for [e (:equations reg)
                  :let [c (str (:code e))]
                  :when (str/includes? c "CODE-PATH NOTE for")
                  :let [after (subs c (+ (.indexOf c "CODE-PATH NOTE for") (count "CODE-PATH NOTE for")))
                        mention (first (str/split after #"\(" 2))
                        edges (map (fn [s] (edn/read-string s)) (re-seq #"\[:R[0-9A-Za-z]+ :R[0-9A-Za-z]+\]" mention))]
                  :when (seq edges)
                  edge edges]
              edge)))
(defn term-coverage [edge symbols fields]
  (let [credited (set (map first fields))
        matched (filterv #(and (= edge (:edge %)) (credited (:field %))) term-fields)
        covered (set/intersection symbols (set (map :term matched)))]
    {:entries matched :covered covered :uncovered (set/difference symbols covered)
     :status (cond (= covered symbols) :declared
                   (seq matched) :does-not-carry
                   :else :cannot-tell)}))
(defn inventory [edge by-var coverage]
  (cond (= :declared by-var) (:status coverage)
        (contains? hole-edges edge) :hole
        (contains? code-path-note-edges edge) :code-path-note
        :else :none))
(def edge-report
  (for [[[a b] syms] (sort-by (comp str key) theory)
        :let [[st-file fs-file] (edge-status (node-boxes a) (node-boxes b))
              [st-var fs-var] (edge-status (node-boxes-by-var a) (node-boxes-by-var b))
              coverage (when (= :declared st-var) (term-coverage [a b] syms fs-var))
              inv (inventory [a b] st-var coverage)]]
    (cond-> {:edge [a b] :symbols syms
             :by-file st-file :by-var st-var
             :fields-by-file fs-file :fields-by-var fs-var
             :inventory inv
             :registry-no-site (vec (filter (set no-site-nodes) [a b]))}
      coverage (assoc :term-coverage (dissoc coverage :status))
      (= :hole inv) (assoc :hole-status (hole-edges [a b])))))
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
(prn {:map (or map-rev map-file "working tree") :registry (:as-of reg)
      :registry-rev (or registry-rev registry-file "working tree")
      :term-fields term-fields-path :term-fields-sha256 term-fields-sha256
      :theory-edges (count theory)
      :registry-names-no-site no-site-nodes
      :summary-by-file (frequencies (map :by-file edge-report))
      :summary-by-var (frequencies (map :by-var edge-report))
      :summary-inventory (frequencies (map :inventory edge-report))
      :inventory-none (vec (sort-by str (map :edge (filter #(= :none (:inventory %)) edge-report))))
      :inventory-does-not-carry (vec (sort-by str (map :edge (filter #(= :does-not-carry (:inventory %)) edge-report))))
      :inventory-cannot-tell (vec (sort-by str (map :edge (filter #(= :cannot-tell (:inventory %)) edge-report))))
      :holes-not-in-dag (vec (sort-by str (remove (set (map :edge edge-report)) (keys hole-edges))))
      :nodes (for [n node-report] (assoc n :vars (vec (sort (node-vars (:node n)))) :boxes-by-var (vec (sort (node-boxes-by-var (:node n))))))
      :edges edge-report
      :producers producer-report})
