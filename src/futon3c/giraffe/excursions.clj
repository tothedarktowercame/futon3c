(ns futon3c.giraffe.excursions
  "Combined mission/excursion census. Unread E dependency graphs remain unknown."
  (:require [futon2.aif.mission-registry :as registry]
            [futon2.aif.task-completion-surface :as completion]
            [futon2.aif.wm.construction-inputs :as sources]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.pass :as pass]
            [futon3c.giraffe.readings :as readings]))

(defn- census-input
  [{:keys [code-root] :as opts}]
  (let [code-root (or code-root registry/default-code-root)
        mission-input (missions/missions->input opts)
        loaded (registry/load-excursions code-root)
        entries (vec (sort-by :id (filter registry/live-excursion? (:excursions loaded))))
        declarations (sources/target-source-declarations
                      (mapv :id entries)
                      {:loaded-missions {:missions []} :loaded-excursions loaded
                       :loaded-tickets {:tickets []} :excursion-targets (mapv :id entries)})
        offset (count (get-in mission-input [:input :items]))
        rows (mapv (fn [i entry declaration]
                     (when (:source-absent declaration)
                       (throw (ex-info "Excursion source unresolved"
                                       {:kind :excursion/source-unavailable :target (:id entry)
                                        :declaration declaration})))
                     {:id (keyword (:id entry)) :state-group :unstarted :source-order (+ offset i)
                      :source-pin (:source entry) :source-declaration declaration
                      :completion (completion/describe entry)
                      :dependency-reading {:status :unknown :reason :dependency-coverage-not-reviewed
                                           :producer :dependency-reader-reviewer
                                           :source-pin (:source entry)}})
                   (range) entries declarations)]
    (-> mission-input
        (update-in [:input :items] into rows)
        (assoc-in [:input :graph-coverage]
                  {:status :incomplete :unknown-targets (mapv :id entries)
                   :producer :dependency-reader-reviewer})
        (update :excluded into
                (map #(assoc (select-keys % [:id :status-class :source]) :reason (:status-class %))
                     (remove registry/live-excursion? (:excursions loaded)))))))

(defn missions-and-excursions->input
  [{:keys [reading-batches reading-checker members-file repos] :as opts}]
  (let [adapter (census-input opts)]
    (if-not reading-batches
      adapter
      (let [reading (readings/consume {:checker reading-checker :members-file members-file
                                      :batches reading-batches :repos (or repos missions/default-repos)})
            coverage (:coverage reading)
            by-id (into {} (map (juxt :id identity)) (:members coverage))
            open-items (remove #(#{:completed :cancelled} (:state-group %))
                               (get-in adapter [:input :items]))
            _ (when-not (= (set (keys by-id)) (set (map (comp name :id) open-items)))
                (throw (ex-info "Reading population differs from adapter"
                                {:kind :reading/adapter-population-mismatch})))
            _ (doseq [item open-items]
                (when-not (= (:source-pin item) (:source (get by-id (name (:id item)))))
                  (throw (ex-info "Reading source differs from adapter"
                                  {:kind :reading/adapter-source-mismatch :id (:id item)}))))]
        (-> adapter
            (assoc :reading-evidence reading)
            (assoc-in [:input :edges] (:edges reading))
            (assoc-in [:input :graph-coverage] coverage)
            (assoc-in [:input :reading-evidence] (dissoc reading :checked-records))
            (update-in [:input :items]
                       (fn [items]
                         (mapv #(assoc % :dependency-reading
                                       (get by-id (name (:id %)))) items))))))))

(defn combined-pass
  "Compute population-relative terms from the complete checked outgoing graph.
  pass/downstream-cone inverts those edges. Partial coverage never implies zero."
  [input]
  (let [base (pass/pass input)
        items (into {} (map (juxt :id identity)) (:items input))
        coverage (:graph-coverage input)
        incomplete? (not= :complete (:status coverage))
        evidence (:reading-evidence input)
        ;; Missing/unresolved/out-of-scope prerequisites remain unknown for their
        ;; reading member. Non-prerequisite links do not enter readiness.
        unknowns (set (concat (map :reading-member (:rejected evidence))
                             (map :reading-member (filter #(#{"depends-on" "unblocks"} (:kind %))
                                                          (:unresolved evidence)))
                             (map :reading-member (filter #(= "depends-on" (:kind %))
                                                          (:out-of-scope evidence)))))
        scope {:scope :population/missions-excursions
               :population-sha (:population-sha coverage)
               :coverage (select-keys coverage [:status :member-count :counts :checker-sha])}
        rows (mapv (fn [row]
                     (let [item (get items (:id row))
                           unknown? (or incomplete? (contains? unknowns (name (:id row))))]
                       (cond-> (assoc row :dependency-reading (:dependency-reading item)
                                          :graph-terms (assoc scope :status (if unknown? :unknown :known)))
                         (:completion item) (assoc :completion (:completion item))
                         unknown? (assoc :status :undetermined :reason :needs/dependency-reading
                                         :cone nil :unblocks nil)))) (:order base))
        ready (mapv :id (filter #(= :ready (:status %)) rows))]
    (assoc base :order rows :ready-set ready :graph-coverage coverage
           :reading-evidence evidence
           :known-subgraph (select-keys base [:cones :unblocks])
           :cones (into {} (map (juxt :id :cone)) rows)
           :unblocks (into {} (map (juxt :id :unblocks)) rows)
           :ready-ignoring-externals (filterv (set ready) (:ready-ignoring-externals base))
           :readiness (reduce (fn [m row]
                               (assoc m (:id row) (select-keys row [:status :reason :dependency-reading :graph-terms])))
                             (:readiness base) rows)
           :counts (assoc (:counts base) :ready (count ready)
                          :ready-ignoring-externals (count (filter (set ready) (:ready-ignoring-externals base)))))))
