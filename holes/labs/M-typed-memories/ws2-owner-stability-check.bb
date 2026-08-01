#!/usr/bin/env bb
;; WS2 owner-review stability check (claude-6, 2026-07-28).
;; Preregistered in ws2-results-note.md §Owner-review addendum BEFORE this
;; run: at eps=0.1 all four theta-grid points are in the stable explicit-
;; Euler regime and lambda_2 is the slowest mode, so time-to-uniform should
;; rank inversely with seed-component lambda_2; Spearman rho <= -0.8.
;; Graph construction, heat dynamics, participation, and time-to-uniform
;; are copied verbatim in behavior from retrieval_flow_sweep_v2.bb
;; (lineage: v1 sweep) with eps changed 0.3 -> 0.1; lambda_2 values are
;; read from the frozen retrieval-flow-sweep-v2-results.edn, not recomputed.

(require '[clojure.edn :as edn]
         '[clojure.string :as str])

(def root "/home/joe/code/futon3c/holes/labs/M-typed-memories")
(def heat-epsilon 0.1)
(def heat-steps 60)

(defn read-edn [file] (edn/read-string (slurp (str root "/" file))))

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

(defn apply-negative-laplacian [graph theta x]
  (let [index (zipmap (:nodes graph) (range))]
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
     (vec (repeat (count (:nodes graph)) 0.0))
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
     :tail-participation tail-value}))

(defn average-ranks [values]
  (let [sorted (sort (map-indexed (fn [i v] [v i]) values))
        groups (partition-by first sorted)]
    (persistent!
     (reduce
      (fn [ranks group]
        (let [start (inc (count (filter #(< (first %) (ffirst group)) sorted)))
              mean-rank (/ (reduce + (range start (+ start (count group))))
                           (double (count group)))]
          (reduce (fn [r [_ i]] (assoc! r i mean-rank)) ranks group)))
      (transient (vec (repeat (count values) 0.0)))
      groups))))

(defn spearman [xs ys]
  (let [rx (average-ranks xs)
        ry (average-ranks ys)
        n (count xs)
        mean (fn [v] (/ (reduce + v) (double n)))
        mx (mean rx) my (mean ry)
        cov (reduce + (map #(* (- %1 mx) (- %2 my)) rx ry))
        sx (Math/sqrt (reduce + (map #(let [d (- % mx)] (* d d)) rx)))
        sy (Math/sqrt (reduce + (map #(let [d (- % my)] (* d d)) ry)))]
    (if (or (zero? sx) (zero? sy)) 0.0 (/ cov (* sx sy)))))

(let [graph (v1-graph)
      frozen (edn/read-string
              (slurp (str root "/retrieval-flow-sweep-v2-results.edn")))
      lambda2 (into {}
                    (map (juxt :theta :seed-component-lambda-2))
                    (get-in frozen [:v1 :grid]))
      grid-order [:uniform :prescribe-heavy :uses-heavy :hub-off]
      rows (mapv (fn [theta-key]
                   (let [t (time-to-uniform
                            (heat-trace graph (theta-grid theta-key)))]
                     {:theta theta-key
                      :lambda-2 (lambda2 theta-key)
                      :time-to-uniform t}))
                 grid-order)
      rho (spearman (mapv :lambda-2 rows)
                    (mapv (comp :steps :time-to-uniform) rows))
      ;; n=4, no ties: rho is a rational on a coarse lattice (multiples of
      ;; 1/10). Compare at 1e-9 so exact-equality cases (rho = -0.8 hits
      ;; the preregistered bound exactly) are not lost to float rounding.
      result {:check :ws2-owner-stability
              :date "2026-07-28"
              :heat-epsilon heat-epsilon
              :preregistered {:prediction "rho <= -0.8" :written-before-run true}
              :rows rows
              :spearman-rho rho
              :passed? (<= rho (+ -0.8 1.0e-9))}]
  (spit (str root "/ws2-owner-stability-check-results.edn")
        (pr-str result))
  (doseq [row rows]
    (println (:theta row)
             "| lambda2" (format "%.4f" (double (:lambda-2 row)))
             "| t" (get-in row [:time-to-uniform :steps])
             "| reached?" (get-in row [:time-to-uniform :reached?])))
  (println "Spearman rho:" rho "| prediction rho <= -0.8 | passed?" (:passed? result)))
