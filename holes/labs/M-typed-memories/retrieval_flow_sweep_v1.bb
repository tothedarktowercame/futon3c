#!/usr/bin/env bb
;; E-retrieval-flows v1: operator sweep over the COMBINED rules graph.
;; v0 finding: phase-4 graph too sparse (disjoint stars) — operator choice
;; degenerate. v1 adds the six draft rewrite rules with their natural hubs
;; (shared problem a95A04, shared tactics — notably the linarith triple),
;; giving cycles, so Δ_θ finally has room to differentiate.

(require '[clojure.edn :as edn]
         '[clojure.string :as str])

(def rules (:rules (edn/read-string (slurp "/home/joe/code/futon3c/holes/labs/M-typed-memories/zai1-rewrite-rules-draft.edn"))))

(def tactic-re #"\b(linarith|nlinarith|rw|simp|split_ifs|show|convert|exact|ring|gcongr|grep)\b")

(defn tactics-in [s] (distinct (map second (re-seq tactic-re (str s)))))

;; Typed edge list: [from to relation]
(def edges
  (vec
   (mapcat
    (fn [r]
      (let [rid (name (:rule/id r))
            trigger (str "pat:" (str/replace (or (get-in r [:rule/scope :context]) "ctx")
                                             #"[^a-zA-Z]+" "-"))
            problem (str "prob:" (get-in r [:rule/evidence :problem] "a95A04"))]
        (concat
         [[rid problem :mined-from]
          [rid trigger :instance-of]]
         (for [t (tactics-in (get-in r [:rule/before :form]))]
           [rid (str "tac:" t) :uses-tactic])
         (for [t (tactics-in (get-in r [:rule/after :form]))]
           [rid (str "tac:" t) :prescribes-tactic]))))
    rules)))

(def nodes (vec (distinct (mapcat (fn [[a b _]] [a b]) edges))))
(def n (count nodes))
(def idx (zipmap nodes (range)))
(def relations [:mined-from :instance-of :uses-tactic :prescribes-tactic])

(defn adj [relation]
  (let [a (to-array-2d (repeat n (repeat n 0.0)))]
    (doseq [[from to rel] edges :when (= rel relation)]
      (aset a (idx from) (idx to) 1.0)
      (aset a (idx to) (idx from) 1.0))
    a))
(def adjs (into {} (map (fn [r] [r (adj r)]) relations)))

(defn laplacian-apply [theta x]
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
  (let [ax (map #(Math/abs (double %)) x) tot (reduce + ax)]
    (if (< tot 1e-12) 0.0
        (- (reduce + (map (fn [v] (let [p (/ v tot)]
                                    (if (< p 1e-12) 0.0 (* p (Math/log p))))) ax))))))
(defn participation [x]
  (let [ax (map #(Math/abs (double %)) x) tot (reduce + ax)]
    (if (< tot 1e-12) 0.0
        (/ 1.0 (reduce + (map (fn [v] (let [p (/ v tot)] (* p p))) ax))))))

(defn run-dynamics [form theta eps steps seed-i]
  (let [x0 (vec (assoc (vec (repeat n 0.0)) seed-i 1.0))]
    (loop [t 0 x x0 x-prev x0 trace []]
      (if (= t steps) trace
        (let [dx (laplacian-apply theta x)
              x' (case form
                   :heat (mapv (fn [xi di] (+ xi (* eps di))) x dx)
                   :wave (mapv (fn [xi xpi di] (+ (* 2.0 xi) (- xpi) (* eps di)))
                               x x-prev dx))
              mass (reduce + (map #(Math/abs (double %)) x'))
              x'' (if (> mass 1e-9) (mapv #(/ % mass) x') x')]
          (recur (inc t) x'' x
                 (conj trace {:t t :entropy (entropy x'') :participation (participation x'')
                              :raw-mass mass})))))))

(defn classify [trace]
  (let [tail (take-last 10 trace)
        e-tail (map :entropy tail) p-tail (map :participation tail)
        e-mean (/ (reduce + e-tail) (count e-tail))
        e-var (/ (reduce + (map #(Math/pow (- % e-mean) 2) e-tail)) (count e-tail))
        p-mean (/ (reduce + p-tail) (count p-tail))
        max-e (Math/log n)]
    (cond
      (some #(> (:raw-mass %) 1e6) trace) :unstable
      (some #(< (:raw-mass %) 1e-9) trace) :extinguished
      (< p-mean 1.6) :collapsed
      (> e-mean (* 0.95 max-e)) :dissipated
      (> e-var 1e-4) :sustained-oscillating
      :else :sustained-structured)))

(def theta-grid
  {:uniform {:mined-from 1.0 :instance-of 1.0 :uses-tactic 1.0 :prescribes-tactic 1.0}
   :prescribe-heavy {:mined-from 0.3 :instance-of 0.3 :uses-tactic 0.3 :prescribes-tactic 2.0}
   :uses-heavy {:mined-from 0.3 :instance-of 0.3 :uses-tactic 2.0 :prescribes-tactic 0.3}
   :hub-off {:mined-from 0.0 :instance-of 1.0 :uses-tactic 1.0 :prescribes-tactic 1.0}})

(def seed (idx "rw-occurrence-fight-use-linarith"))

(println "== v1 combined-graph operator sweep ==")
(println "graph:" n "nodes," (count edges) "typed edges")
(println "hubs: prob:a95A04 degree" (count (filter #(= "prob:a95A04" (second %)) edges))
         "| tac:linarith degree" (count (filter #(= "tac:linarith" (second %)) edges)))
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

(spit "/home/joe/code/futon3c/holes/labs/M-typed-memories/retrieval-flow-sweep-v1-results.edn"
      (pr-str {:sweep/version 2 :sweep/nodes n :sweep/edges (count edges)
               :sweep/seed "rw-occurrence-fight-use-linarith"
               :results results}))
(println)
(println "class distribution:" (frequencies (map :class results)))
(println "participation spread across theta (heat, eps=0.3):"
         (into {} (for [[tname _] theta-grid]
                    [tname (:participation (:final (first (filter #(and (= :heat (:form %)) (= tname (:theta %)) (= 0.3 (:eps %))) results))))])))