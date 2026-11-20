(ns futon3c.arxana.together-structure
  "Pure workspace structure and serialised acts, including refused claims.")

(def empty-state "An empty workspace." {:nodes {} :edges {} :derived {} :parents {}})

(defn- valid-id? [x]
  (and (string? x) (boolean (re-matches #"[A-Za-z0-9._:-]{1,120}" x))))

(defn- shape? [op]
  (and (map? op) (every? keyword? (keys op))
       (case (:op op)
         :add-node (and (= (set (keys op)) #{:op :id :kind :attrs})
                        (valid-id? (:id op)) (string? (:kind op)) (map? (:attrs op)))
         :add-edge (and (= (set (keys op)) #{:op :id :type :ends})
                        (valid-id? (:id op)) (string? (:type op))
                        (vector? (:ends op)) (seq (:ends op)) (every? valid-id? (:ends op)))
         :supersede (and (= (set (keys op)) #{:op :id :replaces :kind :attrs})
                         (valid-id? (:id op)) (valid-id? (:replaces op))
                         (string? (:kind op)) (map? (:attrs op)))
         (:group :ungroup) (and (= (set (keys op)) #{:op :child :parent})
                                (valid-id? (:child op)) (valid-id? (:parent op)))
         :regroup (and (= (set (keys op)) #{:op :child :from :to})
                       (every? valid-id? ((juxt :child :from :to) op)))
         false)))

(defn- known? [state id]
  (or (contains? (:nodes state) id) (contains? (:edges state) id)))

(defn- cycle? [state child parent]
  ;; Equivalently, follow parents upward from the proposed parent to the child.
  (loop [todo [parent] seen #{}]
    (if-let [id (peek todo)]
      (cond (= id child) true
            (contains? seen id) (recur (pop todo) seen)
            :else (recur (into (pop todo) (get-in state [:parents id])) (conj seen id)))
      false)))

(defn check-op
  "Return nil or an error keyword without changing STATE.
  Check shape, then existence, then grouping constraints. Regroup checks its
  source pair, existing destination, and cycle in that order, in current STATE."
  [state op]
  (if-not (shape? op) :invalid-op
    (let [{:keys [id child parent from to]} op
          grouped? #(contains? (get-in state [:parents child] #{}) %)]
      (case (:op op)
        :add-node (when (known? state id) :id-in-use)
        :add-edge (cond (known? state id) :id-in-use
                        (not-every? #(known? state %) (:ends op)) :unknown-id)
        :supersede (cond (known? state id) :id-in-use
                         (not (contains? (:nodes state) (:replaces op))) :unknown-id)
        :group (cond (not-every? #(known? state %) [child parent]) :unknown-id
                     (grouped? parent) :already-grouped
                     (cycle? state child parent) :cycle)
        :ungroup (cond (not-every? #(known? state %) [child parent]) :unknown-id
                       (not (grouped? parent)) :not-grouped)
        :regroup (cond (not-every? #(known? state %) [child from to]) :unknown-id
                       (not (grouped? from)) :not-grouped
                       (grouped? to) :already-grouped
                       (cycle? state child to) :cycle)))))

(defn apply-op
  "Return a new STATE with OP applied, or throw ex-info with :together/error."
  [state op]
  (if-let [error (check-op state op)]
    (throw (ex-info "Invalid Together structure operation" {:together/error error :op op}))
    (let [{:keys [id child parent from to]} op]
      (case (:op op)
        :add-node (assoc-in state [:nodes id] (select-keys op [:kind :attrs]))
        :add-edge (assoc-in state [:edges id] (select-keys op [:type :ends]))
        :supersede (-> state
                       (assoc-in [:nodes id] (select-keys op [:kind :attrs]))
                       (assoc-in [:derived id] (:replaces op)))
        :group (update-in state [:parents child] (fnil conj #{}) parent)
        :ungroup (update-in state [:parents child] disj parent)
        :regroup (update-in state [:parents child] #(conj (disj % from) to))))))

(defn submit
  "Append an attributed act, including refusals, without mutating HUB.
  Invalid identity returns an error without logging. AT is caller-supplied data."
  [hub op identity at]
  (if-not (and (string? identity) (re-matches #"[A-Za-z0-9_-]{1,60}" identity))
    {:error :invalid-identity}
    (let [error (check-op (:state hub) op)
          entry (cond-> {:rev (inc (count (:log hub))) :op op :author identity :at at}
                  error (assoc :refused error))]
      {:hub (assoc hub :state (if error (:state hub) (apply-op (:state hub) op))
                   :log (conj (:log hub) entry))
       :entry entry})))

(defn rebuild
  "Replay LOG in revision order, skipping refused acts, into a new state."
  [log]
  (reduce (fn [state entry]
            (if (:refused entry) state (apply-op state (:op entry))))
          empty-state (sort-by :rev log)))
