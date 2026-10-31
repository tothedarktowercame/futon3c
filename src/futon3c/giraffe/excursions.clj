(ns futon3c.giraffe.excursions
  "Combined mission/excursion census. Unread E dependency graphs remain unknown."
  (:require [futon2.aif.mission-registry :as registry]
            [futon2.aif.task-completion-surface :as completion]
            [futon2.aif.wm.construction-inputs :as sources]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.pass :as pass]
            [futon3c.giraffe.provenance :as provenance]
            [futon3c.giraffe.readings :as readings]))

(defn- census-input
  [{:keys [code-root include-tickets] :as opts}]
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
                   (range) entries declarations)
        ;; Tickets: shared registry readers only (load-tickets, live-ticket?,
        ;; target-source-declarations, completion/describe). Enumerated only
        ;; under :include-tickets — default M/E census is byte-preserved.
        ticket-state (when include-tickets
                       (let [loaded-tickets {:tickets (:tickets (registry/load-tickets code-root))}
                             live-entries (vec (sort-by :id (filter registry/live-ticket?
                                                                    (:tickets loaded-tickets))))]
                         {:loaded loaded-tickets :entries live-entries
                          :declarations (sources/target-source-declarations
                                         (mapv :id live-entries)
                                         {:loaded-missions {:missions []} :loaded-excursions loaded
                                          :loaded-tickets loaded-tickets
                                          :ticket-targets (mapv :id live-entries)})
                          :excluded (mapv #(assoc (select-keys % [:id :status-class :source])
                                                  :reason (:status-class %))
                                          (remove registry/live-ticket? (:tickets loaded-tickets)))}))
        ticket-offset (+ offset (count entries))
        ticket-rows (mapv (fn [i entry declaration]
                            (when (:source-absent declaration)
                              (throw (ex-info "Ticket source unresolved"
                                              {:kind :ticket/source-unavailable :target (:id entry)
                                               :declaration declaration})))
                            {:id (keyword (:id entry)) :state-group :unstarted
                             :source-order (+ ticket-offset i)
                             :source-pin (:source entry) :source-declaration declaration
                             :declared-status (:status-class entry)
                             :completion (completion/describe entry)
                             :dependency-reading {:status :unknown
                                                  :reason :dependency-coverage-not-reviewed
                                                  :producer :dependency-reader-reviewer
                                                  :source-pin (:source entry)}})
                          (range) (:entries ticket-state) (:declarations ticket-state))]
    (-> mission-input
        (update-in [:input :items] into (concat rows ticket-rows))
        (assoc-in [:input :graph-coverage]
                  {:status :incomplete
                   :unknown-targets (concat (mapv :id entries) (mapv :id (:entries ticket-state)))
                   :producer :dependency-reader-reviewer})
        (update :excluded into
                (concat
                 (map #(assoc (select-keys % [:id :status-class :source]) :reason (:status-class %))
                      (remove registry/live-excursion? (:excursions loaded)))
                 (:excluded ticket-state))))))

(defn reading-drift
  "Classify drift between the adapter's open items and the checked reading
  corpus (claude-1, 2026-10-31).  The population check used to demand exact
  equality, so ONE new item blocked the whole field on every click -- and
  members drift daily.  Drift is partial instead: an open item with no
  current reading is :unread-new; one whose source sha changed is
  :reading-source-changed; a corpus member that is no longer open is
  ignored (:members-not-open, reported, never silently dropped); everything
  else is :read.  Nothing passes as read that was not: the source-pin
  equality is still demanded of every item that counts as read."
  [open-items coverage]
  (let [by-id (into {} (map (juxt :id identity)) (:members coverage))
        classified (group-by (fn [item]
                               (let [m (get by-id (name (:id item)))]
                                 (cond
                                   (nil? m) :unread-new
                                   (not= :read (:status m)) :unread-new
                                   (not= (:source-pin item) (:source m)) :reading-source-changed
                                   :else :read)))
                             open-items)
        open-names (set (map #(name (:id %)) open-items))]
    {:read (mapv :id (:read classified))
     :unread-new (mapv :id (:unread-new classified))
     :reading-source-changed (mapv :id (:reading-source-changed classified))
     :members-not-open (vec (sort (remove open-names (map :id (:members coverage)))))}))

(defn missions-and-excursions->input
  [{:keys [reading-batches reading-checker members-file repos reading-scope] :as opts}]
  (let [adapter (census-input opts)]
    (if-not reading-batches
      adapter
      (let [reading (readings/consume {:checker reading-checker :members-file members-file
                                       :batches reading-batches :repos (or repos missions/default-repos)
                                       :scope reading-scope})
            coverage (:coverage reading)
            by-id (into {} (map (juxt :id identity)) (:members coverage))
            open-items (remove #(#{:completed :cancelled} (:state-group %))
                               (get-in adapter [:input :items]))
            drift (reading-drift open-items coverage)
            read-ids (set (:read drift))
            excluded-ids (set (concat (:unread-new drift)
                                      (:reading-source-changed drift)))
            partial? (or (seq (:unread-new drift)) (seq (:reading-source-changed drift)))
            member-status (into {} (map (juxt :id :status)) (:members coverage))
            ;; Coverage says PARTIAL with exact counts when any open item
            ;; stayed out; a corpus member that is no longer open does not
            ;; count against completeness (it is ignored, but reported).
            coverage' (assoc coverage
                             :status (if partial? :partial :complete)
                             :open-item-count (count open-items)
                             :drift-counts {:read (count read-ids)
                                            :unread-new (count (:unread-new drift))
                                            :reading-source-changed
                                            (count (:reading-source-changed drift))
                                            :members-not-open
                                            (count (:members-not-open drift))}
                             :drift (select-keys drift [:unread-new
                                                        :reading-source-changed
                                                        :members-not-open]))]
        (-> adapter
            (assoc :reading-evidence reading)
            (assoc :reading-drift {:status (:status coverage')
                                   :open-item-count (:open-item-count coverage')
                                   :drift-counts (:drift-counts coverage')
                                   :drift (:drift coverage')})
            ;; Excluded, never silently dropped: every drifted open item is
            ;; a typed exclusion row (excluded.edn carries it into the run).
            (update :excluded into
                    (concat (map (fn [id] {:id id :reason :unread-new
                                           :member-status (get member-status id)})
                                 (:unread-new drift))
                            (map (fn [id] {:id id :reason :reading-source-changed})
                                 (:reading-source-changed drift))))
            (assoc-in [:input :edges] (:edges reading))
            (assoc-in [:input :graph-coverage] coverage')
            (assoc-in [:input :reading-evidence] (dissoc reading :checked-records))
            ;; Drifted items stay OUT of the graph: only read items remain,
            ;; each still pinned to the exact member record (source-pin
            ;; equality held for every one of them).
            (update-in [:input :items]
                       (fn [items]
                         (mapv (fn [item]
                                 (if (contains? read-ids (:id item))
                                   (assoc item :dependency-reading
                                          (get by-id (name (:id item))))
                                   item))
                               (remove #(contains? excluded-ids (:id %)) items)))))))))

(defn combined-pass
  "Compute population-relative terms from the complete checked outgoing graph.
  pass/downstream-cone inverts those edges. Partial coverage never implies zero."
  [input]
  (let [base (pass/pass input)
        ticket-provenance (provenance/project input)
        items (into {} (map (juxt :id identity)) (:items input))
        coverage (:graph-coverage input)
        evidence (:reading-evidence input)
        ;; Missing/unresolved/out-of-scope prerequisites remain unknown for their
        ;; reading member. Non-prerequisite links do not enter readiness.
        missing (group-by :reading-member
                          (concat
                           (map #(assoc % :absence :rejected-dependency-reading) (:rejected evidence))
                           (map #(assoc % :absence :unresolved-prerequisite)
                                (filter #(#{"depends-on" "unblocks"} (:kind %)) (:unresolved evidence)))
                           (map #(assoc % :absence :out-of-scope-prerequisite)
                                (filter #(= "depends-on" (:kind %)) (:out-of-scope evidence)))))
        unknowns (set (keys missing))
        scope {:scope (:scope coverage)
               :population-sha (:population-sha coverage)
               :coverage (select-keys coverage [:status :member-count :counts
                                                :checker-sha :open-item-count
                                                :drift-counts])}
        rows (mapv (fn [row]
                     (let [item (get items (:id row))
                           ;; Partial drift (claude-1, 2026-10-31) zeroes
                           ;; nothing wholesale: a row is unknown exactly
                           ;; when its own reading is not :read or one of
                           ;; its prerequisite readings is missing/rejected.
                           unread? (not= :read (get-in item [:dependency-reading :status]))
                           unknown? (or unread? (contains? unknowns (name (:id row))))]
                       (cond-> (assoc row :dependency-reading (:dependency-reading item)
                                          :graph-terms (cond-> (assoc scope :status (if unknown? :unknown :known))
                                                         unknown? (assoc :reason (if unread?
                                                                                  :dependency-reading-unread
                                                                                  :prerequisite-unresolved)
                                                                         :prerequisites (vec (get missing (name (:id row))))
                                                                         :producer :dependency-reader-reviewer)))
                         (:completion item) (assoc :completion (:completion item))
                         unknown? (assoc :status :undetermined :reason :needs/dependency-reading
                                         :cone nil :unblocks nil)))) (:order base))
        ready (mapv :id (filter #(= :ready (:status %)) rows))]
    (assoc base :order rows :ready-set ready :graph-coverage coverage
           :ticket-provenance ticket-provenance
           :reading-evidence evidence
           :known-subgraph (select-keys base [:cones :unblocks])
           :cones (into {} (map (fn [row] [(:id row) (when (some? (:cone row))
                                                   (get (:cones base) (:id row)))]) rows))
           :unblocks (into {} (map (fn [row] [(:id row) (when (some? (:unblocks row))
                                                      (get (:unblocks base) (:id row)))]) rows))
           :ready-ignoring-externals (filterv (set ready) (:ready-ignoring-externals base))
           :readiness (reduce (fn [m row]
                               (assoc m (:id row) (select-keys row [:status :reason :dependency-reading :graph-terms])))
                             (:readiness base) rows)
           :counts (assoc (:counts base) :ready (count ready)
                          :ready-ignoring-externals (count (filter (set ready) (:ready-ignoring-externals base)))))))
