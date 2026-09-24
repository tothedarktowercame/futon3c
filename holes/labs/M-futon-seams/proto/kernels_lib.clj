(require (quote [clojure.edn :as edn]) (quote [clojure.set :as set]) (quote [clojure.string :as str]))

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
