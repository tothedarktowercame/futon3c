;; Probe: read a hand-crafted WM cascade as glued open causal theories.
;; Run from futon3c:  clojure -M holes/labs/M-diagramprover/cascade_glue_probe.clj
;;
;; Input: futon2/resources/wm/cascade-three-relations-sample-v1.edn (six
;; peripheral patterns with :support, :overlap and :precedence). Each pattern
;; becomes one open theory owning its component variable X, with mechanism
;;   X := apply-X and every prerequisite of X under the chosen relation,
;; where apply-X (the pattern was applied) is exogenous. The grammar of
;; causal.scm is binary, so a conjunction of n terms is chained through
;; helper variables X__1 .. X__n-2, owned by the same theory.
(require '[clojure.edn :as edn]
         '[clojure.pprint :refer [pprint]]
         '[futon3c.diagramprover.causal.dag :as dag]
         '[futon3c.diagramprover.causal.dsep :as dsep]
         '[futon3c.diagramprover.causal.open-theory :as ot]
         '[futon3c.diagramprover.causal.scm :as scm])

(def cascade (edn/read-string (slurp "../futon2/resources/wm/cascade-three-relations-sample-v1.edn")))
(def components (mapv :id (:components cascade)))
(defn apply-var [x] (keyword (str "apply-" (name x))))

(defn prerequisites
  "Map component -> set of components it needs, under RELATION."
  [relation edges]
  (reduce (fn [m e]
            (case relation
              :precedence (update m (:after e) (fnil conj #{}) (:before e))
              :support (update m (:parent e) (fnil conj #{}) (:child e))))
          (zipmap components (repeat #{})) edges))

(defn chain-and
  "Mechanisms making X the conjunction of TERMS (keywords), binary grammar."
  [x terms]
  (let [terms (mapv name terms)]
    (if (= 1 (count terms))
      {(name x) (first terms)}
      (loop [acc (first terms) [t & more] (rest terms) i 1 mechs {}]
        (if (empty? more)
          (assoc mechs (name x) (str acc " and " t))
          (let [h (str (name x) "__" i)]
            (recur h more (inc i) (assoc mechs h (str acc " and " t)))))))))

(defn pattern-theory [x needs]
  (let [terms (into [(apply-var x)] (sort needs))]
    (ot/theory x (chain-and x terms)
               :inputs (map name terms)
               :interface (into [x] needs))))

(defn theories [prereqs]
  (mapv (fn [[x needs]] (pattern-theory x needs)) (sort prereqs)))

(defn glue-or-refuse [ts] (ot/glue ts))

(def all-applied
  (merge (zipmap (map apply-var components) (repeat true))
         (zipmap components (repeat true))))

(defn knockout
  "For each pattern P: had P not been applied, which components fail?"
  [d]
  (into (sorted-map)
        (for [p components]
          [p (vec (for [x components
                        :let [r (scm/counterfactual d {:evidence all-applied
                                                       :intervention {(apply-var p) false}
                                                       :outcome x})]
                        :when (false? (:answer r))]
                    x))])))

(defn component-ancestors [d x]
  (into (sorted-set) (filter (set components)) (dag/ancestors d x)))

(defn component-independencies [d]
  (let [cs (set components)]
    (->> (dsep/implied-independencies d {:max-conditioning 1})
         (filter (fn [ci] (and (cs (:x ci)) (cs (:y ci))))))))

(def precedence (prerequisites :precedence (:precedence cascade)))
(def support (prerequisites :support (:support cascade)))
(def both (merge-with into precedence support))

(let [p (glue-or-refuse (theories precedence))
      s (glue-or-refuse (theories support))
      b (glue-or-refuse (theories both))
      unwarranted (glue-or-refuse (theories (update precedence :envelope conj :heartbeat)))
      pd (:dag p) sd (:dag s)]
  (println "== glue")
  (pprint {:precedence (:glued? p) :support (:glued? s)
           :support+precedence (select-keys b [:glued? :reason :details])
           :unwarranted-heartbeat->envelope (:glued? unwarranted)})
  (println "== derivation (component ancestors)")
  (pprint {:precedence (into (sorted-map) (for [x components] [x (component-ancestors pd x)]))
           :support (into (sorted-map) (for [x components] [x (component-ancestors sd x)]))})
  (println "== declared meets vs common lower bounds in the support order")
  (pprint (for [{:keys [left right meet]} (:overlap cascade)
                :let [lb #(conj (component-ancestors sd %) %)
                      common (clojure.set/intersection (lb left) (lb right))
                      maximal (remove (fn [c] (some #(contains? (component-ancestors sd %) c) common)) common)]]
            ;; :strict = units BOTH depend on, excluding the pair themselves
            ;; (Alexander's overlap); :maximal = the order-theoretic meet.
            {:left left :right right :declared meet
             :strict (clojure.set/intersection (component-ancestors sd left)
                                               (component-ancestors sd right))
             :maximal (set maximal)}))
  (println "== knockout: had pattern P not been applied, which components fail")
  (pprint {:precedence (knockout pd) :support (knockout sd)})
  (println "== implied independencies among components (conditioning <= 1)")
  (let [ci (component-independencies pd)
        ciu (component-independencies (:dag unwarranted))]
    (pprint {:precedence (count ci) :with-unwarranted-edge (count ciu)
             :lost-by-unwarranted-edge (vec (remove (set ciu) ci))
             :sample (vec (take 6 ci))})))
