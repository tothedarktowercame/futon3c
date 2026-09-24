;; Kernel prototype for PROOF-2a (claude-10, 2026-09-24). Prototype, not spec.
;; Run: bb holes/labs/M-futon-seams/proto/kernels.clj [theta] [horizon] FILE...
;;
;; Candidate transition kernels for a cascade whose patterns carry an
;; `above` structure (context -> pattern edges), run on the same file:
;;   :list    the current WM kernel: first enabled pattern in a precedence
;;            list. The list is a linear extension of `above`; incomparable
;;            patterns get an arbitrary order, so EVERY linear extension is
;;            run (capped) and the spread is reported.
;;   :coapp   (A) every pattern in the enabled frontier fires independently,
;;            each succeeding with theta.
;;   :inter   (B) exactly one frontier pattern fires, chosen uniformly.
;; enabled(p,s): needs ⊆ s, forbids ∩ s = ∅, produces ⊄ s  (CascadeTransition.guard)
;; frontier(s):  enabled patterns with no enabled ancestor under `above`.
;; On a chain, frontier = the first enabled pattern, so all three agree there.
(require '[clojure.edn :as edn] '[clojure.set :as set] '[clojure.string :as str])

(defn enabled? [{:keys [guard produces]} s]
  (and (set/subset? (:needs guard) s)
       (empty? (set/intersection (:forbids guard) s))
       (not (set/subset? produces s))))

(defn ancestors-map [ids edges]
  (let [parents (reduce (fn [m {:keys [context pattern]}] (update m pattern (fnil conj #{}) context)) {} edges)]
    (into {} (for [i ids]
               [i (loop [seen #{} frontier (get parents i #{})]
                    (if (empty? frontier) seen
                        (recur (into seen frontier)
                               (set/difference (reduce set/union #{} (map #(get parents % #{}) frontier)) seen))))]))))

(defn frontier [pats anc s]
  (let [en (set (filter #(enabled? (pats %) s) (keys pats)))]
    (set (remove #(seq (set/intersection (anc %) en)) en))))

(defn mix [dists]  ; [[weight dist]...] -> dist
  (reduce (fn [acc [w d]] (merge-with + acc (update-vals d #(* w %)))) {} dists))

(defn step-pattern [p s theta] {(set/union s (:produces p)) theta, s (- 1.0 theta)})

(defn subsets [xs] (reduce (fn [acc x] (concat acc (map #(conj % x) acc))) [#{}] xs))

(defn kernel [kind pats anc order theta s]
  (case kind
    :list (if-let [p (first (filter #(enabled? (pats %) s) order))]
            (step-pattern (pats p) s theta) {s 1.0})
    :coapp (let [f (frontier pats anc s)]
             (if (empty? f) {s 1.0}
                 (mix (for [S (subsets f)]
                        [(* (reduce * (map (constantly theta) S))
                            (reduce * (map (constantly (- 1.0 theta)) (set/difference f S))))
                         {(reduce set/union s (map #(:produces (pats %)) S)) 1.0}]))))
    :inter (let [f (vec (sort (frontier pats anc s)))]
             (if (empty? f) {s 1.0}
                 (mix (for [p f] [(/ 1.0 (count f)) (step-pattern (pats p) s theta)]))))))

(defn rollout [kind pats anc order theta s0 T]
  (loop [d {s0 1.0} t 0]
    (if (= t T) d
        (recur (mix (for [[s w] d] [w (kernel kind pats anc order theta s)])) (inc t)))))

(defn linear-extensions [ids anc cap]
  (let [out (atom [])]
    (letfn [(go [placed remaining]
              (when (< (count @out) cap)
                (if (empty? remaining) (swap! out conj placed)
                    (doseq [i (sort remaining)
                            :when (set/subset? (anc i) (set placed))]
                      (go (conj placed i) (disj remaining i))))))]
      (go [] (set ids)))
    @out))

(defn summary [d want]
  {:p-all-wants (reduce + (for [[s w] d :when (set/subset? want s)] w))
   :e-wants (reduce + (for [[s w] d] (* w (count (set/intersection want s)))))})

(defn conflicts [pats f]
  (for [a f b f :when (not= a b)
        :let [x (set/intersection (:produces (pats a)) (get-in pats [b :guard :forbids]))]
        :when (seq x)] [a b x]))

(defn r3 [x] (/ (Math/round (* 1000.0 x)) 1000.0))

(let [[theta T & files] *command-line-args*
      theta (Double/parseDouble (or theta "0.8"))
      T (Integer/parseInt (or T "6"))]
  (doseq [f files]
    (let [c (edn/read-string (slurp f))
          pats (:patterns c) ids (keys pats)
          anc (ancestors-map ids (:above c))
          s0 (:initial c) want (:want c)
          exts (linear-extensions ids anc 200)]
      (println "=====" f "| instance" (:instance c) "| theta" theta "horizon" T)
      (println "patterns" (count ids) "above-edges" (count (:above c)) "linear extensions" (count exts) (if (= 200 (count exts)) "(capped)" ""))
      (let [lists (map #(summary (rollout :list pats anc % theta s0 T) want) exts)]
        (println " :list   p(all wants) range" (r3 (apply min (map :p-all-wants lists))) "-" (r3 (apply max (map :p-all-wants lists)))
                 "| E[#wants] range" (r3 (apply min (map :e-wants lists))) "-" (r3 (apply max (map :e-wants lists)))))
      (doseq [k [:coapp :inter]]
        (let [sm (summary (rollout k pats anc nil theta s0 T) want)]
          (println " " k " p(all wants)" (r3 (:p-all-wants sm)) "| E[#wants]" (r3 (:e-wants sm)))))
      ;; where the definitions part ways: reachable states with a frontier of 2+
      (let [reach (keys (rollout :coapp pats anc nil 0.5 s0 T))
            wide (filter #(> (count (frontier pats anc %)) 1) reach)]
        (println " states with frontier >= 2:" (count wide) "of" (count reach) "reachable")
        (doseq [s (take 3 wide)]
          (println "   at" (sort s))
          (println "     frontier" (sort (frontier pats anc s)) "conflicts" (vec (conflicts pats (frontier pats anc s)))))))))
