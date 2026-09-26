#!/usr/bin/env bb
;; E-retrieval-flows WS2 spectral classification.
;;
;; Graph construction and heat dynamics are lifted without semantic changes
;; from the frozen v0/v1 scripts beside this file. This instrument adds the
;; positive graph-Laplacian spectrum (D-A), component-local lambda_2, and a
;; preregistered consistency check: heat time-to-uniform should rank inversely
;; with lambda_2 of the seed component.

(ns retrieval-flow-sweep-v2
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def root "/home/joe/code/futon3c/holes/labs/M-typed-memories")
(def tolerance 1.0e-10)
(def jacobi-max-sweeps 200)
(def heat-epsilon 0.3)
(def heat-steps 60)

(defn read-edn [name]
  (edn/read-string (slurp (str root "/" name))))

(defn matrix [n]
  (make-array Double/TYPE n n))

(defn copy-matrix [a]
  (let [n (alength ^objects a)
        b (matrix n)]
    (dotimes [i n]
      (dotimes [j n]
        (aset ^doubles (aget ^objects b i) j
              (aget ^doubles (aget ^objects a i) j))))
    b))

(defn off-diagonal-norm [a]
  (let [n (alength ^objects a)]
    (Math/sqrt
     (* 2.0
        (reduce
         +
         0.0
         (for [i (range n)
               j (range (inc i) n)]
           (let [v (aget ^doubles (aget ^objects a i) j)]
             (* v v))))))))

(defn jacobi-eigenvalues
  "Deterministic cyclic Jacobi eigenvalues for a real symmetric matrix."
  [input]
  (let [a (copy-matrix input)
        n (alength ^objects a)]
    (loop [sweep 0]
      (let [norm-before (off-diagonal-norm a)]
        (if (or (< norm-before tolerance)
                (= sweep jacobi-max-sweeps))
          {:eigenvalues
           (->> (range n)
                (mapv #(aget ^doubles (aget ^objects a %) %))
                sort vec)
           :converged? (< norm-before tolerance)
           :sweeps sweep
           :off-diagonal-norm norm-before
           :tolerance tolerance
           :max-sweeps jacobi-max-sweeps}
          (do
            (doseq [p (range n)
                    q (range (inc p) n)]
              (let [apq (aget ^doubles (aget ^objects a p) q)]
                (when (> (Math/abs apq) 1.0e-15)
                  (let [app (aget ^doubles (aget ^objects a p) p)
                        aqq (aget ^doubles (aget ^objects a q) q)
                        tau (/ (- aqq app) (* 2.0 apq))
                        sign (if (neg? tau) -1.0 1.0)
                        t (/ sign
                             (+ (Math/abs tau)
                                (Math/sqrt (+ 1.0 (* tau tau)))))
                        c (/ 1.0 (Math/sqrt (+ 1.0 (* t t))))
                        s (* t c)]
                    (doseq [i (range n)
                            :when (and (not= i p) (not= i q))]
                      (let [aip (aget ^doubles (aget ^objects a i) p)
                            aiq (aget ^doubles (aget ^objects a i) q)
                            nip (- (* c aip) (* s aiq))
                            niq (+ (* s aip) (* c aiq))]
                        (aset ^doubles (aget ^objects a i) p nip)
                        (aset ^doubles (aget ^objects a p) i nip)
                        (aset ^doubles (aget ^objects a i) q niq)
                        (aset ^doubles (aget ^objects a q) i niq)))
                    (aset ^doubles (aget ^objects a p) p (- app (* t apq)))
                    (aset ^doubles (aget ^objects a q) q (+ aqq (* t apq)))
                    (aset ^doubles (aget ^objects a p) q 0.0)
                    (aset ^doubles (aget ^objects a q) p 0.0)))))
            (recur (inc sweep))))))))

(defn assert-p3! []
  (let [p3 (to-array-2d [[1.0 -1.0 0.0]
                         [-1.0 2.0 -1.0]
                         [0.0 -1.0 1.0]])
        result (jacobi-eigenvalues p3)
        expected [0.0 1.0 3.0]]
    (assert (:converged? result) "P3 Jacobi solve did not converge")
    (assert (every? true?
                    (map #(< (Math/abs (- %1 %2)) 1.0e-8)
                         (:eigenvalues result) expected))
            (str "P3 spectrum mismatch: " (:eigenvalues result)))
    (assoc result :expected expected :passed? true)))

(defn v0-graph []
  (let [control-edges (:control-edges
                       (read-edn "phase4-wm-corpus.edn"))
        edges
        (vec
         (distinct
          (mapcat
           (fn [edge]
             (let [mission (str (:mission-id edge))
                   pattern (str (:control-pattern-id edge))
                   relation (:relation edge)]
               (concat
                (for [memory (map str (:memory-ids edge))]
                  [memory pattern relation])
                [[pattern mission relation]])))
           control-edges)))]
    {:nodes (vec (distinct (mapcat #(take 2 %) edges)))
     :edges edges
     :relations (vec (distinct (map #(nth % 2) edges)))
     :source-control-edge-count (count control-edges)
     :seed "e-wm-compliance-support"}))

(def tactic-re
  #"\b(linarith|nlinarith|rw|simp|split_ifs|show|convert|exact|ring|gcongr|grep)\b")

(defn tactics-in [value]
  (distinct (map second (re-seq tactic-re (str value)))))

(defn v1-graph []
  (let [rules (:rules (read-edn "zai1-rewrite-rules-draft.edn"))
        edges
        (vec
         (mapcat
          (fn [rule]
            (let [rid (name (:rule/id rule))
                  trigger
                  (str "pat:"
                       (str/replace
                        (or (get-in rule [:rule/scope :context]) "ctx")
                        #"[^a-zA-Z]+" "-"))
                  problem
                  (str "prob:"
                       (get-in rule [:rule/evidence :problem] "a95A04"))]
              (concat
               [[rid problem :mined-from]
                [rid trigger :instance-of]]
               (for [tactic
                     (tactics-in (get-in rule [:rule/before :form]))]
                 [rid (str "tac:" tactic) :uses-tactic])
               (for [tactic
                     (tactics-in (get-in rule [:rule/after :form]))]
                 [rid (str "tac:" tactic) :prescribes-tactic]))))
          rules))]
    {:nodes (vec (distinct (mapcat #(take 2 %) edges)))
     :edges edges
     :relations
     [:mined-from :instance-of :uses-tactic :prescribes-tactic]
     :seed "rw-occurrence-fight-use-linarith"}))

(def theta-grid
  {:uniform
   {:mined-from 1.0 :instance-of 1.0
    :uses-tactic 1.0 :prescribes-tactic 1.0}
   :prescribe-heavy
   {:mined-from 0.3 :instance-of 0.3
    :uses-tactic 0.3 :prescribes-tactic 2.0}
   :uses-heavy
   {:mined-from 0.3 :instance-of 0.3
    :uses-tactic 2.0 :prescribes-tactic 0.3}
   :hub-off
   {:mined-from 0.0 :instance-of 1.0
    :uses-tactic 1.0 :prescribes-tactic 1.0}})

(defn active-edges [{:keys [edges]} theta]
  (filterv #(pos? (double (get theta (nth % 2) 0.0))) edges))

(defn components
  [nodes edges]
  (let [neighbors
        (reduce
         (fn [m [a b _]]
           (-> m
               (update a (fnil conj #{}) b)
               (update b (fnil conj #{}) a)))
         (zipmap nodes (repeat #{}))
         edges)]
    (loop [remaining (set nodes)
           found []]
      (if-let [start (first (sort remaining))]
        (let [component
              (loop [frontier [start]
                     seen #{}]
                (if-let [node (peek frontier)]
                  (if (seen node)
                    (recur (pop frontier) seen)
                    (recur (into (pop frontier) (get neighbors node))
                           (conj seen node)))
                  seen))]
          (recur (apply disj remaining component)
                 (conj found (vec (sort component)))))
        (->> found
             (sort-by (juxt (comp - count) first))
             vec)))))

(defn laplacian
  [nodes edges theta]
  (let [index (zipmap nodes (range))
        result (matrix (count nodes))]
    (doseq [[a b relation] edges
            :let [weight (double (get theta relation 0.0))]
            :when (and (pos? weight) (index a) (index b))]
      (let [i (index a)
            j (index b)]
        (aset ^doubles (aget ^objects result i) i
              (+ weight (aget ^doubles (aget ^objects result i) i)))
        (aset ^doubles (aget ^objects result j) j
              (+ weight (aget ^doubles (aget ^objects result j) j)))
        (aset ^doubles (aget ^objects result i) j
              (- (aget ^doubles (aget ^objects result i) j) weight))
        (aset ^doubles (aget ^objects result j) i
              (- (aget ^doubles (aget ^objects result j) i) weight))))
    result))

(defn component-spectrum [graph theta component]
  (let [allowed (set component)
        edges (filterv #(and (allowed (first %))
                             (allowed (second %)))
                       (:edges graph))
        solved (jacobi-eigenvalues (laplacian component edges theta))
        values (:eigenvalues solved)]
    {:nodes component
     :size (count component)
     :lambda-2 (when (> (count values) 1) (second values))
     :spectrum values
     :jacobi (dissoc solved :eigenvalues)}))

(defn spectral-gap [values]
  (first (filter #(> % 1.0e-8) values)))

(defn apply-negative-laplacian
  [graph theta x]
  (let [nodes (:nodes graph)
        index (zipmap nodes (range))]
    (reduce
     (fn [dx [a b relation]]
       (let [weight (double (get theta relation 0.0))
             i (index a)
             j (index b)
             xi (nth x i)
             xj (nth x j)]
         (if (zero? weight)
           dx
           (-> dx
               (update i + (* weight (- xj xi)))
               (update j + (* weight (- xi xj)))))))
     (vec (repeat (count nodes) 0.0))
     (:edges graph))))

(defn participation [x]
  (let [absolute (map #(Math/abs (double %)) x)
        total (reduce + absolute)]
    (if (< total 1.0e-12)
      0.0
      (/ 1.0
         (reduce + (map (fn [value]
                          (let [p (/ value total)] (* p p)))
                        absolute))))))

(defn heat-trace [graph theta]
  (let [seed-index (.indexOf ^java.util.List (:nodes graph) (:seed graph))
        x0 (assoc (vec (repeat (count (:nodes graph)) 0.0))
                  seed-index 1.0)]
    (loop [step 0
           x x0
           trace []]
      (if (= step heat-steps)
        trace
        (let [dx (apply-negative-laplacian graph theta x)
              advanced (mapv #(+ %1 (* heat-epsilon %2)) x dx)
              mass (reduce + (map #(Math/abs (double %)) advanced))
              normalized-x
              (if (> mass 1.0e-9)
                (mapv #(/ % mass) advanced)
                advanced)]
          (recur (inc step)
                 normalized-x
                 (conj trace {:step (inc step)
                              :participation
                              (participation normalized-x)})))))))

(defn time-to-uniform [trace]
  (let [tail-value
        (/ (reduce + (map :participation (take-last 5 trace))) 5.0)
        tolerance-band (* 0.05 (Math/abs tail-value))
        hit
        (first
         (filter #(<= (Math/abs (- (:participation %) tail-value))
                      tolerance-band)
                 trace))]
    {:steps (or (:step hit) (inc heat-steps))
     :reached? (some? hit)
     :tail-participation tail-value
     :relative-tolerance 0.05}))

(defn average-ranks [values]
  (let [groups (group-by second (map-indexed vector values))
        ordered (sort-by key groups)]
    (loop [position 1
           remaining ordered
           ranks (vec (repeat (count values) 0.0))]
      (if-let [[_ entries] (first remaining)]
        (let [width (count entries)
              average (/ (+ (* 2 position) width -1) 2.0)]
          (recur (+ position width)
                 (rest remaining)
                 (reduce #(assoc %1 (first %2) average) ranks entries)))
        ranks))))

(defn pearson [xs ys]
  (let [n (count xs)
        mx (/ (reduce + xs) n)
        my (/ (reduce + ys) n)
        dx (map #(- % mx) xs)
        dy (map #(- % my) ys)
        numerator (reduce + (map * dx dy))
        denominator
        (Math/sqrt (* (reduce + (map #(* % %) dx))
                      (reduce + (map #(* % %) dy))))]
    (if (< denominator 1.0e-15) 0.0 (/ numerator denominator))))

(defn spearman [xs ys]
  (pearson (average-ranks xs) (average-ranks ys)))

(defn v1-grid-result [graph frozen theta-name theta]
  (let [active (active-edges graph theta)
        census (components (:nodes graph) active)
        component-results
        (mapv #(component-spectrum graph theta %) census)
        seed-component
        (first (filter #(some #{(:seed graph)} (:nodes %))
                       component-results))
        full-solve
        (jacobi-eigenvalues
         (laplacian (:nodes graph) (:edges graph) theta))
        heat (time-to-uniform (heat-trace graph theta))
        joined
        (filterv #(= theta-name (:theta %)) (:results frozen))]
    {:theta theta-name
     :weights theta
     :component-census
     {:count (count census)
      :sizes (mapv count census)}
     :components component-results
     :seed-component-lambda-2 (:lambda-2 seed-component)
     :spectral-gap (spectral-gap (:eigenvalues full-solve))
     :full-spectrum (:eigenvalues full-solve)
     :jacobi (dissoc full-solve :eigenvalues)
     :heat-time-to-uniform heat
     :trajectory-classes
     (mapv #(select-keys % [:form :eps :class :final]) joined)
     :trajectory-class-counts (frequencies (map :class joined))}))

(defn v0-degenerate-result [graph]
  (let [theta (zipmap (:relations graph) (repeat 1.0))
        census (components (:nodes graph) (active-edges graph theta))
        component-results (mapv #(component-spectrum graph theta %) census)]
    {:node-count (count (:nodes graph))
     :source-control-edge-count (:source-control-edge-count graph)
     :expanded-two-hop-edge-count (count (:edges graph))
     :component-count (count census)
     :component-sizes (mapv count census)
     :components component-results
     :food-problem
     {:spectral-form
      :small-disconnected-components-bound-operator-expressiveness
      :largest-component-size (apply max (map count census))
      :whole-graph-zero-eigenvalue-multiplicity (count census)}}))

(def p3-validation (assert-p3!))
(def v0 (v0-graph))
(def v1 (v1-graph))
(def frozen-v1 (read-edn "retrieval-flow-sweep-v1-results.edn"))
(def grid-results
  (mapv (fn [[theta-name theta]]
          (v1-grid-result v1 frozen-v1 theta-name theta))
        theta-grid))
(def lambda-values (mapv :seed-component-lambda-2 grid-results))
(def time-values
  (mapv #(get-in % [:heat-time-to-uniform :steps]) grid-results))
(def rank-correlation (spearman lambda-values time-values))

(def output
  {:sweep/version 3
   :lineage
   {:v0-script "retrieval_flow_sweep.bb"
    :v1-script "retrieval_flow_sweep_v1.bb"
    :v1-results "retrieval-flow-sweep-v1-results.edn"}
   :numerics
   {:algorithm :cyclic-jacobi
    :tolerance tolerance
    :max-sweeps jacobi-max-sweeps
    :p3-validation p3-validation}
   :v0-degenerate (v0-degenerate-result v0)
   :v1
   {:node-count (count (:nodes v1))
    :typed-edge-count (count (:edges v1))
    :seed (:seed v1)
    :grid grid-results}
   :consistency-check
   {:hypothesis
    :heat-time-to-uniform-anti-correlates-with-seed-component-lambda-2
    :heat-epsilon heat-epsilon
    :heat-steps heat-steps
    :theta-order (mapv :theta grid-results)
    :lambda-2 lambda-values
    :time-to-uniform-steps time-values
    :spearman-rank-correlation rank-correlation
    :sign-expected :negative
    :passed? (neg? rank-correlation)}})

(spit (str root "/retrieval-flow-sweep-v2-results.edn")
      (pr-str output))
(println "v2 spectral sweep complete")
(println "P3:" (get-in output [:numerics :p3-validation :eigenvalues]))
(println "theta/lambda2/time:"
         (mapv #(select-keys % [:theta :seed-component-lambda-2
                               :heat-time-to-uniform])
               grid-results))
(println "Spearman rho:" rank-correlation
         "passed expected negative sign?"
         (get-in output [:consistency-check :passed?]))
