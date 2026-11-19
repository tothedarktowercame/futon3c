(ns futon3c.arxana.holes
  "A pure-data implementation of the communicative-act model in Holes.lean.

  A hole is `{:hole name}`.  Forms are `[:atom relation a b]`,
  `[:conj p q]`, and `[:neg p]`; literal arguments are untagged values.
  Slots are `[:own v]`, `[:unknown]`, `[:offered party deadline]`, or
  `[:free]`.  Missing slots are free.  No macro or reader convention is
  involved, so syntax-quote cannot qualify a hole name.

  Lean's slot is a total function and `expand` maps every possible hole.  A
  finite map cannot enumerate an arbitrary name type, so this implementation
  expands the union of represented slot keys and holes occurring in the body.
  In particular it agrees with Lean and the Lisp companion for every hole
  that can affect the act."
  )

(defn hole
  "Construct a hole argument with NAME."
  [name]
  {:hole name})

(defn hole?
  "True exactly for the distinct tagged hole representation."
  [x]
  (and (map? x) (= #{:hole} (set (keys x)))))

(defn holes
  "Return body holes in occurrence order, retaining duplicates."
  [form]
  (if-not (vector? form)
    []
    (case (first form)
      :atom (into [] (comp (filter hole?) (map :hole)) (subvec form 2))
      :conj (into (holes (nth form 1)) (holes (nth form 2)))
      :neg (holes (nth form 1))
      [])))

(defn slot-of
  "Return ACT's slot for hole name X; an absent entry is free."
  [act x]
  (get-in act [:slots x] [:free]))

(defn free-holes
  "Return occurrences of holes left free in ACT."
  [act]
  (filterv #(= :free (first (slot-of act %))) (holes (:body act))))

(defn hygienic?
  "True when no body hole is free."
  [act]
  (empty? (free-holes act)))

(defn act
  "Construct an act and return any hygiene warning as data, never a side effect.

  The result is `{:act a :warning w}`.  W is nil for a hygienic act, otherwise
  `{:kind :unhygienic :free-holes [...]}`."
  ([body] (act body {}))
  ([body slots]
   (let [a {:body body :slots slots}
         free (vec (distinct (free-holes a)))]
     {:act a
      :warning (when (seq free)
                 {:kind :unhygienic :free-holes free})})))

(defn capture
  "Return `[hole value]` pairs captured from listener environment ENV."
  [act env]
  (into [] (keep #(when (contains? env %) [% (get env %)]))
        (free-holes act)))

(defn- receive-arg [act env arg]
  (if-not (hole? arg)
    arg
    (let [x (:hole arg)
          slot (slot-of act x)]
      (case (first slot)
        :own (second slot)
        :unknown [:something-i-cannot-name-yet]
        :offered [:open :chooser (second slot) :deadline (nth slot 2)]
        :free (if (contains? env x) [:captured (get env x)] arg)))))

(defn receive
  "Return ACT as heard in listener ENV."
  [act env]
  (letfn [(walk [form]
            (case (first form)
              :atom (into [:atom (second form)]
                          (map #(receive-arg act env %) (subvec form 2)))
              :conj [:conj (walk (nth form 1)) (walk (nth form 2))]
              :neg [:neg (walk (nth form 1))]))]
    (walk (:body act))))

(defn what-did-you-mean
  "Return the open questions, exactly Lean's `Act.whatDidYouMean`."
  [act]
  (free-holes act))

(defn- expand-slot [slot speaker-value]
  (if (= :free (first slot))
    (if (= ::absent speaker-value) [:unknown] [:own speaker-value])
    slot))

(defn expand
  "Expand free slots in speaker ENV; missing bindings become unknown."
  [act env]
  (let [names (into (set (keys (:slots act))) (holes (:body act)))]
    (assoc act :slots
           (into {} (map (fn [x]
                           [x (expand-slot (slot-of act x)
                                           (get env x ::absent))]))
                 names))))

(defn honest
  "The minimal honest expansion: every free body hole becomes unknown."
  [act]
  (expand act {}))

(defn- eval-arg [rho arg]
  (if (hole? arg) (get rho (:hole arg)) arg))

(defn- relation-holds? [model relation a b]
  (if-let [rel (:rel model)]
    (boolean (rel relation a b))
    (contains? (:facts model) [relation a b])))

(defn- eval-form [model rho form]
  (case (first form)
    :atom (relation-holds? model (second form)
                           (eval-arg rho (nth form 2))
                           (eval-arg rho (nth form 3)))
    :conj (and (eval-form model rho (nth form 1))
               (eval-form model rho (nth form 2)))
    :neg (not (eval-form model rho (nth form 1)))))

(defn witness
  "Find a finite-domain assignment proving ACT in MODEL, or nil.

  Owned holes are fixed; all other body holes are existential, exactly as in
  Lean's `Act.Holds`.  MODEL has `:domain` and either a `:facts` set of
  `[relation a b]` or a `:rel` predicate."
  [model act]
  (let [names (vec (distinct (holes (:body act))))
        fixed (into {} (keep (fn [x]
                               (let [slot (slot-of act x)]
                                 (when (= :own (first slot))
                                   [x (second slot)]))))
                    names)
        open (vec (remove #(contains? fixed %) names))]
    (letfn [(search [todo rho]
              (if-let [x (first todo)]
                (some #(search (subvec todo 1) (assoc rho x %))
                      (:domain model))
                (when (eval-form model rho (:body act)) rho)))]
      (search open fixed))))

(defn holds?
  "True when ACT has a witness in finite MODEL."
  [model act]
  (some? (witness model act)))

(defn- open-invite-slot? [slot]
  (and (= :offered (first slot))
       (not= :speaker (second slot))
       (nil? (nth slot 2))))

(defn- demand-slot? [slot]
  (and (= :offered (first slot))
       (not (open-invite-slot? slot))))

(defn sealed?
  "True when every occurring body hole is owned."
  [act]
  (every? #(= :own (first (slot-of act %))) (holes (:body act))))

(defn shrapnel?
  "True when ACT is hygienic and an owned body hole lands on YOU."
  [you act]
  (and (hygienic? act)
       (boolean (some #(= [:own you] (slot-of act %))
                      (holes (:body act))))))

(defn invitation?
  "True for a hygienic, non-shrapnel act with an open offered body hole."
  [you act]
  (and (hygienic? act)
       (not-any? #(= [:own you] (slot-of act %)) (holes (:body act)))
       (boolean (some #(open-invite-slot? (slot-of act %))
                      (holes (:body act))))))

(defn demand?
  "True when an occurring hole is offered by the speaker or has a deadline."
  [act]
  (boolean (some #(demand-slot? (slot-of act %)) (holes (:body act)))))

(defn classify
  "Return all applicable kinds, in the Lisp companion's order."
  ([act] (classify act :you))
  ([act you]
   (cond-> []
     (not (hygienic? act)) (conj :unhygienic)
     (sealed? act) (conj :sealed)
     (shrapnel? you act) (conj :shrapnel)
     (invitation? you act) (conj :invitation)
     (demand? act) (conj :demand))))
