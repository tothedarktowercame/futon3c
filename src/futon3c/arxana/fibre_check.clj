(ns futon3c.arxana.fibre-check
  "Check for the design choice \"every view opens its evidence fibre\"
  (ARGUE-revised §3.6): a view's every displayed record can be opened to where
  it came from. Concretely, per view kind:
  - catalogue view: every choice has an id and at least one pinned source
    (repo, revision, path), every check has an id, every latest run an id;
  - benefit filings: every reading (benefit, item, serving link, naming link,
    entity) carries its record with an id, an evidence vector, and evidence
    completeness :complete.
  The check judges a view already read; it reads nothing itself.")

(defn- violation [view-kind path reason] {:view view-kind :path path :reason reason})

(defn catalogue-fibres [view]
  (vec (concat
        (for [[i c] (map-indexed vector (:choices view))
              :when (or (nil? (:id c)) (empty? (:sources c))
                        (some #(not (and (:repo %) (:revision %) (:path %))) (:sources c)))]
          (violation :catalogue/view [:choices i] :choice-without-pinned-source))
        (for [[i c] (map-indexed vector (:choices view))
              [j k] (map-indexed vector (:checks c))
              :when (or (nil? (:id k)) (and (:latest-run k) (nil? (get-in k [:latest-run :id]))))]
          (violation :catalogue/view [:choices i :checks j] :check-or-run-without-id)))))

(defn- reading-ok? [r]
  (and (get-in r [:record :id]) (vector? (:evidence r)) (= :complete (:evidence-completeness r))))

(defn benefit-fibres [view]
  (let [readings (concat [[[:benefit] (:benefit view)]]
                         (for [[i it] (map-indexed vector (:items view))
                               r (concat [[[:items i :item] (:item it)]]
                                         (for [[j s] (map-indexed vector (:serves it))] [[:items i :serves j] s])
                                         (for [[j n] (map-indexed vector (:names it))
                                               r [[[:items i :names j :link] (:link n)] [[:items i :names j :entity] (:entity n)]]] r))]
                           r)
                         (for [[i s] (map-indexed vector (:substrate view))] [[:substrate i :serves] (:serves s)]))]
    (vec (for [[path r] readings :when (not (reading-ok? r))]
           (violation :cascade/benefit-filings path :reading-without-evidence)))))

(defn check
  "VIEWS is a vector of [kind view]. An inconclusive or missing view is an
  :error result, never a pass."
  [views]
  (if (or (empty? views) (some (fn [[_ v]] (not= :ok (:status v))) views))
    {:result :error :violations [{:reason :view-not-complete
                                  :views (mapv (fn [[k v]] [k (:status v) (:reason v)]) views)}]}
    (let [vs (vec (mapcat (fn [[k v]] (case k :catalogue/view (catalogue-fibres v)
                                            :cascade/benefit-filings (benefit-fibres v)))
                          views))]
      {:result (if (seq vs) :violation :pass) :violations vs})))
