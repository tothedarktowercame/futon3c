(ns futon3c.diagramprover.skeleton.agency
  "Skeleton use case: Agency behaviour as resource-sensitive string diagrams.

  `futon3c.agency.logic` checks the registry at one instant. This namespace
  checks a run: an ordered window of Agency events becomes one string
  diagram, and the architectural invariants become typing laws over it
  (`futon3c.diagramprover.regime`):

  - I-1 (identity is singular): an agent's session is a :linear wire. It may
    not be copied (`:spawn`), and its vertices must be totally ordered, so a
    second concurrent session is a :concurrent-identity finding.
  - I-2 (transport routes, it does not create): transport generators
    (`:accept`, `:transport/create`) may not output an identity the signature
    does not let them create.
  - I-3 (peripherals are inhabited): `:hop` and `:hop-back` keep the same
    wire; any step that takes one agent in and another out is an
    :identity-not-preserved finding.
  - Bells are obligations. A bell is a :linear wire from the ring to its
    answer, so answering twice is :duplicated, answering a bell never
    received is :ill-typed, and an unanswered bell is an open output port.
    Two agents each holding an open bell to the other is a crossing.

  The diagram quotients out interleaving: two event orders that differ only
  in the order of independent steps give isomorphic diagrams (`same-run?`).
  And the queue-level steps (`:accept` then `:drain`) rewrite by DPO to the
  protocol-level step (`:deliver`), so an implementation trace can be
  checked against a protocol trace (`refines?`).

  Event shapes (one map per ledger entry, in ledger order):
    {:op :register   :agent a}           {:op :deregister :agent a}
    {:op :ring  :id j :from a :to b :type :query|:request}
    {:op :accept :bell j}                ; the turn queue takes the bell
    {:op :drain   :agent b :bell j}      ; b's queue hands it to b
    {:op :deliver :agent b :bell j}      ; protocol level: accept+drain
    {:op :answer  :agent b :ref j}       ; typed answer to a :query
    {:op :reply   :agent b :ref j}       ; reply to a :request
    {:op :hop     :agent a :peripheral p}  {:op :hop-back :agent a :peripheral p}
    {:op :publish :agent a :evidence e}  {:op :read :agent b :evidence e}
    {:op :spawn :agent a}  {:op :transport/create :agent a}   ; unlawful
  The mapping from the invoke-jobs ledger (`:bellback-of`, turn-queue
  accept!/drain!) onto these shapes is not written yet; see the mission's
  skeleton use-case section."
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.identify :as identify]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.matcher :as matcher]
            [futon3c.diagramprover.regime :as regime]
            [futon3c.diagramprover.rewrite :as rewrite]
            [futon3c.diagramprover.rule :as rule]))

;; ---------------------------------------------------------------------------
;; Signature and regime

(defn sort-of
  "[:agent a] and [:retired a] are parametrised by the agent; other sorts
  are plain keywords."
  [vtype]
  (if (vector? vtype) (first vtype) vtype))

(defn owner-of [vtype]
  (when (and (vector? vtype) (#{:agent :retired} (first vtype))) (second vtype)))

(def bell-types [:query :request])

(defn- bell-sort [stage t] (keyword (name stage) (name t)))

(def signature
  (merge
   {:register {:in [] :out [:agent] :identity :creates}
    :reregister {:in [:retired] :out [:agent]}
    :deregister {:in [:agent] :out [:retired]}
    :answer {:in [:agent :turn/query] :out [:agent]}
    :reply {:in [:agent :turn/request] :out [:agent]}
    :hop {:in [:agent] :out [:agent]}
    :hop-back {:in [:agent] :out [:agent]}
    :publish {:in [:agent] :out [:agent :evidence]}
    :read {:in [:agent :evidence] :out [:agent]}
    ;; Representable so that a ledger showing them can be ingested and
    ;; judged; the laws, not the ingest, reject them.
    :spawn {:in [:agent] :out [:agent :agent]}
    :transport/create {:in [] :out [:agent]}}
   (into {} (for [t bell-types
                  [value sig] {[:ring t] {:in [:agent] :out [:agent (bell-sort :bell t)]}
                               [:accept t] {:in [(bell-sort :bell t)] :out [(bell-sort :queued t)]}
                               [:drain t] {:in [:agent (bell-sort :queued t)]
                                           :out [:agent (bell-sort :turn t)]}
                               [:deliver t] {:in [:agent (bell-sort :bell t)]
                                             :out [:agent (bell-sort :turn t)]}}]
              [value sig]))))

(def regime
  "Everything is linear except evidence, which any number of agents may
  read, or none."
  {:evidence :cartesian})

(def laws {:signature signature :regime regime :sort-of sort-of :owner-of owner-of})

;; ---------------------------------------------------------------------------
;; Ingest: event window -> open string diagram

(defn- add-vertex [st vtype payload]
  (let [[g v] (graph/add-vertex (:g st) (merge {:vtype vtype} payload))]
    [(assoc st :g g) v]))

(defn- edge
  "Add a generator from SOURCES (vertex ids) to fresh vertices of TARGETS
  ([vtype payload] pairs). Returns [state target-ids]."
  [st value sources targets event]
  (let [[st ids] (reduce (fn [[st ids] [vt payload]]
                           (let [[st v] (add-vertex st vt payload)] [st (conj ids v)]))
                         [st []] targets)
        [g _] (graph/add-edge (:g st) sources ids {:value value :event event})]
    [(assoc st :g g) ids]))

(defn- head
  "The agent's current wire; a session already running before the window
  enters as an input port."
  [st a]
  (if-let [v (get-in st [:heads a])]
    [st v]
    (let [[st v] (add-vertex st [:agent a] {})]
      [(assoc-in st [:heads a] v) v])))

(defn- bell
  "The bell's current wire. A bell this window never saw ring enters as an
  input port of SORT, marked :dangling."
  [st j sort*]
  (if-let [v (get-in st [:bells j])]
    [st v]
    (let [[st v] (add-vertex st sort* {:ref j :dangling true})]
      [(assoc-in st [:bells j] v) v])))

(defn- bell-payload [st v]
  (select-keys (graph/vertex-data (:g st) v) [:ref :to :type]))

(defn- bell-type [st v t]
  (or (:type (graph/vertex-data (:g st) v)) t))

(defn- step [st {:keys [op agent] :as e}]
  (case op
    :register
    (let [retired (get-in st [:retired agent])
          [st [a]] (if retired
                     (edge st :reregister [retired] [[[:agent agent] {}]] e)
                     (edge st :register [] [[[:agent agent] {}]] e))]
      (-> st (assoc-in [:heads agent] a) (update :retired dissoc agent)))

    :deregister
    (let [[st a] (head st agent)
          [st [r]] (edge st :deregister [a] [[[:retired agent] {}]] e)]
      (-> st (update :heads dissoc agent) (assoc-in [:retired agent] r)))

    :ring
    (let [{:keys [id from to type]} e
          [st a] (head st from)
          [st [a' b]] (edge st [:ring type] [a]
                            [[[:agent from] {}]
                             [(bell-sort :bell type) {:ref id :to to :type type}]] e)]
      (-> st (assoc-in [:heads from] a') (assoc-in [:bells id] b)))

    :accept
    (let [j (:bell e)
          [st b] (bell st j :bell/request)
          t (bell-type st b :request)
          [st [q]] (edge st [:accept t] [b] [[(bell-sort :queued t) (bell-payload st b)]] e)]
      (assoc-in st [:bells j] q))

    (:drain :deliver)
    (let [j (:bell e)
          [st a] (head st agent)
          [st b] (bell st j (if (= op :drain) :queued/request :bell/request))
          t (bell-type st b :request)
          [st [a' turn]] (edge st [op t] [a b]
                               [[[:agent agent] {}] [(bell-sort :turn t) (bell-payload st b)]] e)]
      (-> st (assoc-in [:heads agent] a') (assoc-in [:bells j] turn)))

    (:answer :reply)
    (let [j (:ref e)
          [st a] (head st agent)
          [st turn] (bell st j (if (= op :answer) :turn/query :turn/request))
          [st [a']] (edge st op [a turn] [[[:agent agent] {}]] e)]
      (assoc-in st [:heads agent] a'))

    (:hop :hop-back)
    (let [[st a] (head st agent)
          [st [a']] (edge st op [a] [[[:agent agent] {:peripheral (:peripheral e)}]] e)]
      (assoc-in st [:heads agent] a'))

    :publish
    (let [[st a] (head st agent)
          [st [a' ev]] (edge st :publish [a] [[[:agent agent] {}] [:evidence {:ref (:evidence e)}]] e)]
      (-> st (assoc-in [:heads agent] a') (assoc-in [:evidence (:evidence e)] ev)))

    :read
    (let [[st a] (head st agent)
          [st ev] (if-let [v (get-in st [:evidence (:evidence e)])]
                    [st v]
                    (let [[st v] (add-vertex st :evidence {:ref (:evidence e) :dangling true})]
                      [(assoc-in st [:evidence (:evidence e)] v) v]))
          [st [a']] (edge st :read [a ev] [[[:agent agent] {}]] e)]
      (assoc-in st [:heads agent] a'))

    :spawn
    (let [[st a] (head st agent)
          [st [a' _clone]] (edge st :spawn [a] [[[:agent agent] {}] [[:agent agent] {}]] e)]
      (assoc-in st [:heads agent] a'))

    :transport/create
    (let [[st [a]] (edge st :transport/create [] [[[:agent agent] {}]] e)]
      (assoc-in st [:heads agent] a))

    (update st :unreadable conj e)))

(defn- port-key [g v]
  (let [d (graph/vertex-data g v)] [(pr-str (:vtype d)) (str (:ref d))]))

(defn- close-boundary
  "Inputs: every vertex nothing produced. Outputs: every vertex of a linear
  sort nothing consumed (cartesian leftovers are discarded, which the regime
  allows). Both in a canonical order, so that the boundary does not depend
  on the interleaving the ledger happened to record."
  [g]
  (let [vs (graph/vertices g)
        canon (fn [xs] (vec (sort-by #(port-key g %) xs)))
        linear? #(= :linear (get regime (sort-of (:vtype (graph/vertex-data g %))) :linear))]
    (-> g
        (graph/set-inputs (canon (filter #(empty? (graph/in-edges g %)) vs)))
        (graph/set-outputs (canon (filter #(and (linear? %) (empty? (graph/out-edges g %))) vs))))))

(defn ingest
  "Build the diagram of an event window. Returns {:diagram g :unreadable [events]}."
  [events]
  (let [st (reduce step {:g (graph/make-graph) :heads {} :bells {} :evidence {}
                         :retired {} :unreadable []}
                   events)]
    {:diagram (close-boundary (:g st)) :unreadable (:unreadable st)}))

;; ---------------------------------------------------------------------------
;; Agency-specific readings

(defn routing-findings
  "Steps that take a bell on the wire of an agent it was not addressed to."
  [g]
  (->> (sort (graph/edges g))
       (keep (fn [e]
               (let [{:keys [value event]} (graph/edge-data g e)
                     [a b] (graph/source g e)
                     owner (owner-of (:vtype (graph/vertex-data g a)))
                     to (:to (graph/vertex-data g b))]
                 (when (and b to (not= owner to)
                            (or (#{:answer :reply} value)
                                (and (vector? value) (#{:drain :deliver} (first value)))))
                   {:finding :misrouted :value value :event event :agent owner :addressed-to to}))))
       vec))

(defn open-bells
  "Unanswered bells: the linear bell wires left on the output boundary."
  [g]
  (->> (:outputs g)
       (map #(graph/vertex-data g %))
       (filter #(and (:ref %) (#{"bell" "queued" "turn"} (namespace (sort-of (:vtype %))))))
       (mapv #(select-keys % [:ref :to :type :vtype]))))

(defn crossings
  "Pairs of agents each holding an open bell to the other (E-crossed-bells).
  Needs the ringer, which the ring event records."
  [g]
  (let [ringer (into {} (for [e (graph/edges g)
                              :let [{:keys [value event]} (graph/edge-data g e)]
                              :when (and (vector? value) (= :ring (first value)))]
                          [(:id event) (:from event)]))
        owed (set (for [{:keys [ref to]} (open-bells g)] [(ringer ref) to]))]
    (vec (sort (for [[a b] owed :when (and a (pos? (compare b a)) (owed [b a]))] [a b])))))

(defn check
  "Every law and reading over one event window."
  [events]
  (let [{:keys [diagram unreadable]} (ingest events)]
    (assoc (regime/check diagram laws)
           :routing (routing-findings diagram)
           :unreadable unreadable
           :open-bells (open-bells diagram)
           :crossings (crossings diagram))))

(defn lawful? [report]
  (every? empty? (vals (select-keys report [:signature :linearity :identity
                                            :concurrency :routing :unreadable]))))

;; ---------------------------------------------------------------------------
;; Equivalence and refinement

(defn same-run?
  "Two event windows describe the same run up to interleaving of independent
  steps: their diagrams are isomorphic, boundary to boundary."
  [events-a events-b]
  (some? (matcher/find-iso (:diagram (ingest events-a)) (:diagram (ingest events-b)))))

(defn- open-graph [vertices edges inputs outputs]
  (let [[g ids] (reduce (fn [[g ids] [k vt]] (let [[g v] (graph/add-vertex g {:vtype vt})]
                                                [g (assoc ids k v)]))
                        [(graph/make-graph) {}] vertices)
        g (reduce (fn [g [value src tgt]]
                    (first (graph/add-edge g (mapv ids src) (mapv ids tgt) {:value value})))
                  g edges)]
    (-> g (graph/set-inputs (mapv ids inputs)) (graph/set-outputs (mapv ids outputs)))))

(defn delivery-rule
  "accept ; drain  =>  deliver, for bells of type T drained by agent A. The
  queue is a transport box: it touches the bell and never the agent wire,
  which is what lets the agent do other work between the two steps."
  [a t]
  (let [vs [[:a0 [:agent a]] [:b (bell-sort :bell t)] [:a1 [:agent a]] [:turn (bell-sort :turn t)]]]
    (rule/make-rule
     (open-graph (conj vs [:q (bell-sort :queued t)])
                 [[[:accept t] [:b] [:q]] [[:drain t] [:a0 :q] [:a1 :turn]]]
                 [:a0 :b] [:a1 :turn])
     (open-graph vs [[[:deliver t] [:a0 :b] [:a1 :turn]]] [:a0 :b] [:a1 :turn])
     {:name [:delivery a t]})))

(defn normalise
  "Apply RULES until none applies. Each rule here removes an edge, so this
  terminates. Returns {:diagram g :steps n}."
  [g rules]
  (loop [g g n 0]
    (if-let [{g' :graph} (some #(first (rewrite/rule-applications % g)) rules)]
      (recur g' (inc n))
      {:diagram g :steps n})))

(defn refines?
  "Does the queue-level IMPLEMENTATION window rewrite to the protocol-level
  SPEC window? Delivery rules are instantiated for every agent in either."
  [implementation spec]
  (let [agents (set (keep #(or (:agent %) (:from %)) (concat implementation spec)))
        rules (for [a (sort agents) t bell-types] (delivery-rule a t))
        {:keys [diagram steps]} (normalise (:diagram (ingest implementation)) rules)]
    {:refines? (some? (matcher/find-iso diagram (:diagram (ingest spec))))
     :rewrite-steps steps}))

;; ---------------------------------------------------------------------------
;; Causal tier: what must Agency record before an experiment can answer?

(defn- causal-graph [arrows latent]
  (let [nodes (set (mapcat (juxt :from :to) arrows))]
    (dag/validate
     {:variables (into (sorted-map)
                       (for [n nodes]
                         [n {:id n :kind (if (contains? latent n) :latent-unobserved :observed)}]))
      :arrows arrows})))

(defn typed-bells-receipt
  "Question: does turning on typed bells (FUTON3C_TYPED_BELLS) reduce
  crossed bells? Load on the mesh plausibly drives both when the flag is
  flipped and how many crossings happen, and typed bells act through the
  query threads they create. The receipt computes, per recording regime,
  whether the effect is identifiable from Agency's records — so the
  recording requirement is derived before the experiment, not after."
  []
  (let [base [{:from :load :to :typed-bells} {:from :load :to :crossings}
              {:from :typed-bells :to :query-threads} {:from :query-threads :to :crossings}]
        direct (conj base {:from :typed-bells :to :crossings})
        run (fn [label arrows latent]
              (let [r (identify/identify (causal-graph arrows latent) :typed-bells :crossings)]
                {:regime label :method (:method r)
                 :adjustment-sets (:adjustment-sets r) :mediators (:mediators r)}))]
    {:question "P(crossings | do(typed-bells))"
     :verdicts [(run :load-recorded base #{})
                (run :load-unrecorded base #{:load})
                (run :load-unrecorded+direct-effect direct #{:load})]}))
