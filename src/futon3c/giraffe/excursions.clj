(ns futon3c.giraffe.excursions
  "Combined mission/excursion census. Unread E dependency graphs remain unknown."
  (:require [futon2.aif.mission-registry :as registry]
            [futon2.aif.task-completion-surface :as completion]
            [futon2.aif.wm.construction-inputs :as sources]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.pass :as pass]))

(defn missions-and-excursions->input
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

(defn combined-pass
  "Retain the known subgraph separately. Until incoming/outgoing coverage is
  reviewed, global cone/unblocks are unknown for every node, including M."
  [input]
  (let [base (pass/pass input)
        items (into {} (map (juxt :id identity)) (:items input))
        coverage (:graph-coverage input)
        uncertain? (= :incomplete (:status coverage))
        rows (mapv (fn [row]
                     (let [item (get items (:id row))
                           unread? (= :unknown (get-in item [:dependency-reading :status]))]
                       (cond-> row
                         (:completion item) (assoc :completion (:completion item))
                         unread? (assoc :status :undetermined :reason :needs/dependency-reading
                                        :dependency-reading (:dependency-reading item))
                         uncertain? (assoc :cone nil :unblocks nil
                                           :graph-terms {:status :unknown :reason :population-coverage-incomplete
                                                         :producer :dependency-reader-reviewer}))))
                   (:order base))
        ready (mapv :id (filter #(= :ready (:status %)) rows))]
    (cond-> (assoc base :order rows :ready-set ready
                       :ready-ignoring-externals (filterv (set ready) (:ready-ignoring-externals base))
                       :readiness (reduce (fn [m row] (assoc m (:id row) (select-keys row [:status :reason :dependency-reading :graph-terms])))
                                          (:readiness base) rows)
                       :counts (assoc (:counts base) :ready (count ready)
                                      :ready-ignoring-externals (count (filter (set ready) (:ready-ignoring-externals base)))))
      uncertain? (assoc :graph-coverage coverage
                        :glue {:glued? false :reason :population-coverage-incomplete}
                        :known-subgraph (select-keys base [:cones :unblocks])
                        :cones nil :unblocks nil))))
