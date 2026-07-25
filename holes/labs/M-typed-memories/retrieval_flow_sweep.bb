#!/usr/bin/env bb
;; E-retrieval-flows v0: operator sweep over the phase-4 typed memory graph.
;;
;; Rob's reading (2026-07-25): the state update is a coarse heat/wave
;; finite-difference update X_{t+1}(x) - X_t(x) = Δ_x(X_t(x)); evolving the
;; rules means updating WHICH operator — a Sturm–Liouville operator with
;; location-dependent coefficients — and the question is which operators
;; produce NON-TRIVIAL STRUCTURE over time. Here: Δ_θ = Σ_r θ_r Δ_r over the
;; typed graph (θ_r = relation conductances), heat and wave forms, swept.

(require '[clojure.edn :as edn]
         '[clojure.string :as str])

(def corpus (edn/read-string (slurp "/home/joe/code/futon3c/holes/labs/M-typed-memories/phase4-wm-corpus.edn")))
(def edges (:control-edges corpus))

;; Node set: missions, patterns, memories appearing in typed edges.
(def nodes
  (vec (distinct (mapcat (fn [e] (concat [(str (:mission-id e))
                                          (str (:control-pattern-id e))]
                                         (map str (:memory-ids e))))
                         edges))))
(def n (count nodes))
(def idx (zipmap nodes (range)))

;; Per-relation symmetric adjacency: each typed edge links memory<->pattern
;; and pattern<->mission (the two hops the projection traverses).
(def relations (vec (distinct (map :relation edges))))
(defn adj [relation]
  (let [a (to-array-2d (repeat n (repeat n 0.0)))]
    (doseq [e edges :when (= relation (:relation e))
            :let [m (idx (str (:mission-id e)))
                  p (idx (str (:control-pattern-id e)))]]
      (doseq [mem (map (comp idx str) (:memory-ids e))]
        (aset a mem p 1.0) (aset a p mem 1.0))
      (aset a p m 1.0) (aset a m p 1.0))
    a))
(def adjs (into {} (map (fn [r] [r (adj r)]) relations)))

(defn laplacian-apply
  "y = Δ_θ x where Δ_θ = Σ_r θ_r (A_r - D_r)."
  [theta x]
  (vec
   (for [i (range n)]
     (reduce
      (fn [acc r]
        (let [w (double (theta r 0.0))
              a ^objects (adjs r)]
          (if (zero? w) acc
              (+ acc (* w (reduce (fn [s j]
                                    (+ s (* (aget ^objects a i j)
                                            (- (nth x j) (nth x i)))))
                                  0.0 (range n)))))))
      0.0 relations))))

(defn entropy [x]
  (let [ax (map #(Math/abs (double %)) x)
        tot (reduce + ax)]
    (if (< tot 1e-12) 0.0
        (- (reduce + (map (fn [v] (let [p (/ v tot)]
                                    (if (< p 1e-12) 0.0 (* p (Math/log p)))))
                          ax))))))
(defn participation [x]
  (let [ax (map #(Math/abs (double %)) x)
        tot (reduce + ax)]
    (if (< tot 1e-12) 0.0
        (/ 1.0 (reduce + (map (fn [v] (let [p (/ v tot)] (* p p))) ax))))))

(defn run-dynamics [form theta eps steps seed-i]
  (let [x0 (vec (assoc (vec (repeat n 0.0)) seed-i 1.0))]
    (loop [t 0 x x0 x-prev x0 trace []]
      (if (= t steps)
        trace
        (let [dx (laplacian-apply theta x)
              x' (case form
                   :heat (mapv (fn [xi di] (+ xi (* eps di))) x dx)
                   :wave (mapv (fn [xi xpi di] (+ (* 2.0 xi) (- xpi) (* eps di)))
                               x x-prev dx))
              mass (reduce + (map #(Math/abs (double %)) x'))
              ;; renormalize so classification reads structure, not scale
              x'' (if (> mass 1e-9) (mapv #(/ % mass) x') x')]
          (recur (inc t) x'' x
                 (conj trace {:t t :entropy (entropy x'') :participation (participation x'')
                              :raw-mass mass})))))))

(defn classify [trace]
  (let [tail (take-last 10 trace)
        e-tail (map :entropy tail)
        p-tail (map :participation tail)
        e-mean (/ (reduce + e-tail) (count e-tail))
        e-var (/ (reduce + (map #(Math/pow (- % e-mean) 2) e-tail)) (count e-tail))
        p-mean (/ (reduce + p-tail) (count p-tail))
        max-e (Math/log n)
        mass-explodes? (some #(> (:raw-mass %) 1e6) trace)
        mass-dies? (some #(< (:raw-mass %) 1e-9) trace)]
    (cond
      mass-explodes? :unstable
      mass-dies? :extinguished
      (< p-mean 1.6) :collapsed          ;; all activation on ~one node
      (> e-mean (* 0.95 max-e)) :dissipated  ;; flat — heat death
      (> e-var 1e-4) :sustained-oscillating  ;; persistent structured motion
      :else :sustained-structured)))     ;; stable non-uniform profile

(def theta-grid
  {:uniform (zipmap relations (repeat 1.0))
   :support-only (assoc (zipmap relations (repeat 1.0)) :blocked-by-control 0.0)
   :challenge-heavy (assoc (zipmap relations (repeat 0.3)) :blocked-by-control 2.0)
   :repairs-heavy (assoc (zipmap relations (repeat 0.3)) :repairs-control 2.0)})

(def seed (idx "e-wm-compliance-support"))

(println "== retrieval-flow operator sweep ==")
(println "graph:" n "nodes," (count edges) "typed edges, relations" relations)
(println)
(def results
  (vec
   (for [form [:heat :wave]
         [tname theta] theta-grid
         eps [0.1 0.3 0.6]]
     (let [trace (run-dynamics form theta eps 60 seed)
           cls (classify trace)
           last-t (last trace)]
       (println (format "%-5s %-16s eps=%.1f -> %-22s (H=%.2f/%.2f P=%.1f/%d)"
                        (name form) (name tname) eps (name cls)
                        (:entropy last-t) (Math/log n) (:participation last-t) n))
       {:form form :theta tname :eps eps :class cls
        :final (select-keys last-t [:entropy :participation])}))))

(spit "/home/joe/code/futon3c/holes/labs/M-typed-memories/retrieval-flow-sweep-results.edn"
      (pr-str {:sweep/version 1 :sweep/nodes n :sweep/relations relations
               :sweep/seed "e-wm-compliance-support"
               :results results}))
(println)
(println "class distribution:" (frequencies (map :class results)))
