(ns futon3c.giraffe.pass
  "M-giraffe stage 1, packet 1: the Giraffe pass over a tracker-neutral work
  pool (M-giraffe.md §DERIVE checked items, §ARGUE \"Order of build\" stage 1;
  VERIFY pilots 1 and 2, 2026-10-27).

  Input (a plain map, never a tracker's shape):

    {:as-of \"...\"
     :items [{:id <id> :state-group (:backlog|:unstarted|:started|
                                        :completed|:cancelled)
              :source-order <int>}]
     :edges [{:from <id> :to <id-or-external-key>
              :kind (:item|:external)
              :class (:closure|:artifact|:alternative)
              :source (:recorded|:author-stated|:prose-reading)
              :quote? :file? :line? :met?}]}

  Edge semantics (§DERIVE, revised from VERIFY):
  - :closure  met when the target item's group is :completed;
  - :artifact met when the target is :started or :completed (\"started stands
    in for the artifact\" until an artifact check is wired; the edge record
    says so);
  - :alternative never enters readiness; it is reported per item as
    :alternative-available;
  - a target in :cancelled is unmet and flagged :blocker-cancelled (the edge
    should be re-pointed);
  - an :external edge is unmet with reason :needs/external unless the input
    marks it met (:met true).

  Output: readiness per item (computed, never written back into the input),
  per ready item its cone (items that transitively need it) and its one-step
  :unblocks, cycle flags (:needs-breakdown with the component), the
  :anomaly/done-with-open-needs anomaly, and an :order of open items — ready
  items first by unblocks count, then cone size, then :source-order;
  not-ready items after, by :source-order — with a typed reason per item.

  The evaluation reuses futon3c.diagramprover.causal.open-theory (ot/theory,
  ot/glue) and scm/evaluate exactly as the VERIFY pilots did; there is no new
  evaluator.  One need variable per edge (not per target), as pilot 2
  established: in reading B an artifact edge on target j is met when j has
  started while a closure edge on the same j is not, so a single j-landed
  variable cannot carry both.  An intervention on item k sets all of k's
  in-edge need variables true (k alone lands).

  Statuses: open items carry :ready | :not-ready | :undetermined.  Closed
  items are reported with their lifecycle state (:done, :cancelled) so the
  per-item record covers every item; they take no place in :order."
  (:require [futon3c.diagramprover.causal.open-theory :as ot]
            [futon3c.diagramprover.causal.scm :as scm]))

(def ^:private closed-groups #{:completed :cancelled})

(defn- edge-class [e] (or (:class e) :closure))

(defn- counting-edge?
  "An edge that enters readiness: an item or external need that is not an
  :alternative disjunct.  External edges count too (they are unmet needs with
  a typed reason); alternatives never do."
  [e]
  (not= :alternative (edge-class e)))

(defn- edge-met?
  "Is this counting edge met in the current state of the pool?"
  [items-by-id e]
  (if (= :external (:kind e))
    (boolean (:met e))
    (let [g (get-in items-by-id [(:to e) :state-group])]
      (case (edge-class e)
        :artifact (contains? #{:started :completed} g)
        :closure  (= :completed g)))))

(defn- scc
  "Tarjan strongly-connected components (as in both pilots)."
  [nodes succ]
  (let [index (atom {}) low (atom {}) on (atom #{}) stack (atom [])
        out (atom []) counter (atom 0)]
    (letfn [(strong [v]
              (swap! index assoc v @counter)
              (swap! low assoc v @counter)
              (swap! counter inc)
              (swap! stack conj v)
              (swap! on conj v)
              (doseq [w (succ v)]
                (when-not (contains? @index w)
                  (strong w)
                  (swap! low update v min (get @low w)))
                (when (contains? @on w)
                  (swap! low update v min (get @index w))))
              (when (= (get @low v) (get @index v))
                (loop [c []]
                  (let [w (peek @stack)]
                    (swap! stack pop)
                    (swap! on disj w)
                    (let [c (conj c w)]
                      (if (= w v)
                        (swap! out conj c)
                        (recur c)))))))]
      (doseq [v nodes :when (not (contains? @index v))]
        (strong v)))
    @out))

(defn- need-var
  "One variable per need edge: <from>-needs-<to>-<class>.  Private to the
  needing item's theory (see the namespace docstring for why it is not
  <to>-landed).  The class is part of the name so that a :closure and an
  :artifact edge on the same target stay two needs."
  [e]
  (keyword (str (name (:from e)) "-needs-"
                (if (= :external (:kind e))
                  (str "ext:" (name (:to e)))
                  (name (:to e)))
                "-" (name (edge-class e)))))

(defn- build-theory
  "Item k's open theory: k-ready is the conjunction of its counting need
  variables, chained through private intermediates because the equation
  grammar has two-input connectives only (as in the pilots and daxiang_live)."
  [k needs]
  (let [vars (map need-var needs)
        id (name k)
        lhs-ready (keyword (str id "-ready"))]
    (if (= 1 (count vars))
      (ot/theory (keyword id) {lhs-ready (name (first vars))} :interface vars)
      (let [mids (map #(keyword (str id "-n" %)) (range 1 (dec (count vars))))
            lhs-chain (concat mids [lhs-ready])
            prev-chain (concat [(first vars)] mids)
            nxt-chain (rest vars)
            eqs (into {}
                      (map (fn [lhs prev nxt]
                             [lhs (str (name prev) " and " (name nxt))])
                           lhs-chain prev-chain nxt-chain))]
        (ot/theory (keyword id) eqs :interface vars)))))

(defn- downstream-cone
  "Open items that transitively need k through the counting item edges."
  [item-ids counting-item-edges open-fn k]
  (let [down (fn down [seen todo]
               (if (empty? todo)
                 seen
                 (let [x (first todo)
                       nxt (distinct
                            (for [e counting-item-edges
                                  :when (and (= (:to e) x)
                                             (contains? item-ids (:from e))
                                             (not (contains? seen (:from e))))]
                              (:from e)))]
                   (recur (into seen nxt) (into (vec (rest todo)) nxt)))))]
    (sort (remove #{k} (filter open-fn (down #{} [k]))))))

(defn- evidence-of
  [es]
  (mapv #(select-keys % [:from :to :kind :class :source :quote :file :line]) es))

(defn- readiness-entry
  "One item's readiness record."
  [items-by-id cyclic component-of needs-of unmet-of alternatives-of ready-val
   id]
  (let [g (get-in items-by-id [id :state-group])
        needs (needs-of id)
        alts (alternatives-of id)]
    (cond
      (= :completed g) {:status :done :reason :lifecycle/completed}
      (= :cancelled g) {:status :cancelled :reason :lifecycle/cancelled}

      (cyclic id)
      {:status :undetermined :reason :needs/cyclic
       :evidence (component-of id)
       :flag :needs-breakdown :component (component-of id)}

      :else
      (let [unmet (unmet-of id)
            v (ready-val id)
            cancelled-blockers
            (vec (sort (set (map :to
                                 (filter #(and (= :item (:kind %))
                                               (= :cancelled
                                                  (get-in items-by-id
                                                          [(:to %) :state-group])))
                                         needs)))))
            unmet-ext (filter #(= :external (:kind %)) unmet)]
        (cond-> {:status (if v :ready :not-ready)}
          (seq cancelled-blockers)
          (assoc :flag :blocker-cancelled :blockers cancelled-blockers)
          (and v (empty? needs) (empty? alts))
          (assoc :reason :needs/none-declared)
          (and v (empty? needs) (seq alts))
          (assoc :reason :needs/alternative-only)
          (and v (seq needs)) (assoc :reason :needs/all-met)
          (and (not v) (seq unmet-ext))
          (assoc :reason :needs/external :evidence (evidence-of unmet-ext))
          (and (not v) (empty? unmet-ext) (seq cancelled-blockers))
          (assoc :reason :needs/blocker-cancelled
                 :evidence (evidence-of (filter #(contains?
                                                 (set cancelled-blockers)
                                                 (:to %))
                                               unmet)))
          (and (not v) (empty? unmet-ext) (empty? cancelled-blockers))
          (assoc :reason :needs/unmet :evidence (evidence-of unmet))
          (seq alts)
          (assoc :alternative-available
                 (mapv #(select-keys % [:to :source :quote :file :line]) alts)))))))

(defn pass
  "The Giraffe pass.  Pure: computes readiness, cones, unblocking, cycles,
  anomalies and an order from the input map alone; nothing is stored back."
  [{:keys [as-of items edges] :as _input}]
  (let [items-by-id (into {} (map (juxt :id identity)) items)
        item-ids (set (map :id items))
        open-fn (fn [id] (not (contains? closed-groups
                                         (get-in items-by-id [id :state-group]))))
        all-edges (->> edges
                       (map (fn [e] (-> e
                                        (update :class #(or % :closure))
                                        (update :kind #(or % :item)))))
                       ;; a repeated need is one need (one variable)
                       (reduce (fn [[seen acc] e]
                                 (let [k ((juxt :from :to :kind :class) e)]
                                   (if (seen k) [seen acc] [(conj seen k) (conj acc e)])))
                               [#{} []])
                       second)
        counting (filter counting-edge? all-edges)
        counting-item (filter #(and (= :item (:kind %))
                                    (contains? item-ids (:to %))) counting)
        needs-of (fn [id] (filter #(= (:from %) id) counting))
        ;; ---- cycle pass over the counting item edges (record and flag,
        ;; never refuse: the cycle-policy invariant).  Members are withheld
        ;; from the glue; the other items are still evaluated.
        cycles (->> (scc (sort item-ids)
                         (fn [k] (->> (filter #(= (:from %) k) counting-item)
                                      (map :to))))
                    (filter #(> (count %) 1))
                    (mapv #(vec (sort %))))
        cyclic (set (mapcat identity cycles))
        component-of (fn [id] (first (filter #(some #{id} %) cycles)))
        theory-items (->> counting (group-by :from) keys (remove cyclic) sort)
        theories (mapv #(build-theory % (needs-of %)) theory-items)
        glued (ot/glue theories)
        _ (when-not (:glued? glued)
            (throw (ex-info "giraffe pass: theories did not glue"
                            {:kind :giraffe/glue-refused
                             :reason (:reason glued)})))
        model (scm/validate (:dag glued))
        base-assignment (into {} (map (fn [e] [(need-var e)
                                               (edge-met? items-by-id e)])
                                      counting))
        base-world (scm/evaluate model base-assignment {})
        ready-var (fn [id] (keyword (str (name id) "-ready")))
        ready-val (fn [id] (get base-world (ready-var id) true))
        world-ext-true
        (scm/evaluate model (into base-assignment
                                  (for [e counting :when (= :external (:kind e))]
                                    [(need-var e) true]))
                      {})
        ext-ready-val (fn [id] (and (not (cyclic id))
                                    (get world-ext-true (ready-var id) (ready-val id))))
        alternatives-of (fn [id] (filter #(and (= (:from %) id)
                                               (= :alternative (:class %)))
                                         all-edges))
        unmet-of (fn [id] (remove #(edge-met? items-by-id %) (needs-of id)))
        readiness
        (into {}
              (for [id (sort item-ids)]
                [id (readiness-entry items-by-id cyclic component-of needs-of
                                      unmet-of alternatives-of ready-val id)]))
        cones
        (into {}
              (for [id (sort item-ids)]
                [id (vec (downstream-cone item-ids counting-item open-fn id))]))
        ;; one-step unblocking: k alone lands -> every in-edge need var of k
        ;; is set true; the items whose readiness flips false->true.
        unblocks
        (into {}
              (for [id (sort item-ids)]
                [id (let [k-vars (set (map need-var
                                           (filter #(= (:to %) id) counting)))
                          w (scm/evaluate model
                                          (into base-assignment
                                                (map (fn [v] [v true])) k-vars)
                                          {})
                          w-ready (fn [j] (get w (ready-var j) (ready-val j)))]
                      (vec (sort (for [j (sort item-ids)
                                       :when (open-fn j)
                                       :when (and (not (ready-val j)) (w-ready j))]
                                   j))))]))
        ready-ignoring-externals
        (vec (sort (for [id (sort item-ids)
                         :when (and (open-fn id) (ext-ready-val id))]
                     id)))
        anomalies
        (vec (for [id (sort item-ids)
                   :when (= :completed (get-in items-by-id [id :state-group]))
                   :let [unmet (unmet-of id)]
                   :when (seq unmet)]
               {:anomaly :anomaly/done-with-open-needs
                :item id
                :edges (evidence-of unmet)}))
        source-order (fn [id] (get-in items-by-id [id :source-order] 0))
        open-ids (sort (filter open-fn item-ids))
        order
        (vec (map-indexed
              (fn [i id]
                (let [r (get readiness id)
                      cone-n (count (get cones id))
                      unb-n (count (get unblocks id))]
                  {:rank (inc i) :id id
                   :reason (if (= :ready (:status r))
                             (if (pos? unb-n)
                               :order/ready-by-unblocks
                               (if (pos? cone-n)
                                 :order/ready-by-cone
                                 :order/ready-by-source-order))
                             :order/not-ready-by-source-order)
                   :status (:status r)
                   :unblocks unb-n :cone cone-n
                   :source-order (source-order id)}))
              (sort-by (fn [id]
                         [(if (= :ready (:status (get readiness id))) 0 1)
                          (- (count (get unblocks id)))
                          (- (count (get cones id)))
                          (source-order id)])
                       open-ids)))]
    {:as-of as-of
     :glue {:glued? true :theories (count theories)}
     :counts {:open (count open-ids)
              :ready (count (filter #(= :ready (:status %)) (vals readiness)))
              :ready-ignoring-externals (count ready-ignoring-externals)}
     :ready-set (vec (sort (filter #(= :ready (:status (get readiness %)))
                                   open-ids)))
     :ready-ignoring-externals ready-ignoring-externals
     :readiness readiness
     :cones cones
     :unblocks unblocks
     :cycles cycles
     :anomalies anomalies
     :order order}))
