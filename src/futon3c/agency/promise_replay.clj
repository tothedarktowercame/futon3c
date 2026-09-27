(ns futon3c.agency.promise-replay
  "Pure, compare-only replay. Never initializes, replaces, or writes a live store.
   Promise chains use sequence, never timestamps. Shared FIFO/index edits also
   require their recorded before-values: independently ordered promises may share
   a queue, so concatenating promise groups would corrupt it. Enabled commuting
   heads may interleave; conflicting heads are reported rather than guessed."
  (:require [clojure.set :as set]
            [futon3c.agency.promise-capture :as capture]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.promise-outcome :as outcome]))

(def empty-states
  {:parked {:records {} :index {} :coalesced {} :ready-inbox {} :leased {}}
   :followup {:queued {} :leased {} :terminal {} :dedupe {}}})

(defn promise-id [entry]
  (let [b (:evidence/body entry)]
    (or (:history/promise-id b) (:id b) (:followup-id b))))
(defn- sequence-of [entry] (get-in entry [:evidence/body :history/promise-sequence]))

(defn- decode [entry]
  (try
    (let [p (history/payload entry)]
      (when-not (and (map? (:record p)) (vector? (:changes p))
                    (= (:predecessor p) (get-in entry [:evidence/body :history/predecessor]))
                    (every? (fn [c]
                              (and (contains? empty-states (:store c)) (string? (:change-id c))
                                   (vector? (:edits c))
                                   (every? #(and (vector? (:path %))
                                                 (contains? #{:put :remove} (:op %))
                                                 (if (= :put (:op %)) (contains? % :value)
                                                     (and (seq (:path %)) (contains? % :before))))
                                           (:edits c)))) (:changes p)))
        (throw (ex-info "Invalid replay payload" {})))
      {:payload p})
    (catch Exception e {:issue {:promise-id (promise-id entry) :reason :invalid-replay-payload
                                :evidence-id (:evidence/id entry) :message (.getMessage e)}})))

(defn- apply-entry [state entry decoded]
  (reduce
   (fn [result change]
     (if (:issue result) (reduced result)
         (reduce
          (fn [r {:keys [path op before absent?] :as edit}]
            (let [store (:store change)
                  s (get-in r [:state store])
                  present? (or (empty? path) (contains? (get-in s (pop path)) (peek path)))
                  value (get-in s path)]
              (if (if absent? (not present?) (and present? (or (= before value)
                                                (and (empty? path) (nil? before)
                                                     (= s (get empty-states store))))))
                (update-in r [:state store] capture/apply-edits [edit])
                (reduced {:issue {:reason :missing-state-history :promise-id (promise-id entry)
                                  :sequence (sequence-of entry) :type (:evidence/type entry)
                                  :store store :path path :operation op}}))))
          result (:edits change))))
   {:state state} (:changes decoded)))

(defn- row-issues [entries]
  (vec
   (concat
    (history/check-chains entries)
    (for [[pid rows] (group-by promise-id entries)
          [n same] (group-by sequence-of rows)
          :when (and n (> (count same) 1))]
      {:promise-id pid :reason :duplicate-sequence :sequence n})
    (for [row entries :when (nil? (promise-id row))]
      {:promise-id nil :reason :missing-promise-id :evidence-id (:evidence/id row)})
    (keep (comp :issue decode) (filter #(= 3 (get-in % [:evidence/body :history/format])) entries)))))

(defn- exclude-dependent-batches [entries initial-issues]
  ;; A single captured CAS may edit a shared FIFO or several parks. Do not smuggle
  ;; an excluded promise back into reconstruction through another promise's row.
  (loop [issues initial-issues]
    (let [excluded (set (map :promise-id issues))
          additional (vec
                      (for [row entries :when (not (contains? excluded (promise-id row)))
                            :let [changes (:changes (:payload (decode row)))
                                  refs (set (filter string? (tree-seq coll? seq changes)))
                                  affected (set/intersection excluded refs)]
                            :when (seq affected)]
                        {:promise-id (promise-id row) :reason :depends-on-incomplete-history
                         :dependencies (vec (sort affected))}))]
      (if (empty? additional) issues (recur (into issues additional))))))

(defn rebuild
  "Return reconstructed snapshots and explicit completeness diagnostics. A promise
   with any invalid/legacy/gapped row is excluded entirely, including its good rows.
   Initial empty containers are the store schema, never a source of promise data."
  [entries]
  (let [entries (vec (remove #(contains? outcome/types (:evidence/type %)) entries))
        issues (exclude-dependent-batches entries (row-issues entries))
        excluded (set (map :promise-id issues))
        valid (remove #(contains? excluded (promise-id %)) entries)
        decoded (into {} (map (fn [r] [(:evidence/id r) (:payload (decode r))])) valid)
        groups (into (sorted-map) (for [[pid rows] (group-by promise-id valid)]
                                   [pid (vec (sort-by sequence-of rows))]))]
    (loop [pending groups state empty-states applied []]
      (if (empty? pending)
        {:states (merge-with merge empty-states state) :issues issues :replayed applied
         :excluded-promises excluded}
        (let [heads (mapv (comp first val) pending)
              trials (mapv (fn [row] [row (apply-entry state row (decoded (:evidence/id row)))]) heads)
              enabled (vec (sort-by (fn [[row _]] [(get-in row [:evidence/body :history/writer-id] "")
                                                          (get-in row [:evidence/body :history/sequence] Long/MAX_VALUE)
                                                          (promise-id row)])
                                     (filter (comp :state second) trials)))]
          (if (empty? enabled)
            {:states (merge-with merge empty-states state)
             :issues (into issues (map (comp :issue second) trials))
             :replayed applied :excluded-promises (into excluded (keys pending))}
            (let [[row result] (first enabled)
                  ;; Do not arbitrarily choose between conflicting enabled heads.
                  conflicts (for [[other other-result] (rest enabled)
                                  :let [ab (apply-entry (:state result) other (decoded (:evidence/id other)))
                                        ba (apply-entry (:state other-result) row (decoded (:evidence/id row)))]
                                  :when (and (or (:issue ab) (:issue ba) (not= (:state ab) (:state ba)))
                                             (not (and (some? (get-in row [:evidence/body :history/writer-id]))
                                                       (= (get-in row [:evidence/body :history/writer-id])
                                                          (get-in other [:evidence/body :history/writer-id]))
                                                       (< (get-in row [:evidence/body :history/sequence] Long/MAX_VALUE)
                                                          (get-in other [:evidence/body :history/sequence] Long/MAX_VALUE)))))]
                              (promise-id other))]
              (if (seq conflicts)
                {:states (merge-with merge empty-states state) :replayed applied
                 :excluded-promises (into excluded (keys pending))
                 :issues (conj issues {:reason :ambiguous-inter-promise-order
                                      :promise-id (promise-id row) :conflicts (vec conflicts)})}
                (recur (let [pid (promise-id row) remaining (subvec (get pending pid) 1)]
                         (if (seq remaining) (assoc pending pid remaining) (dissoc pending pid)))
                       (:state result) (conj applied (:evidence/id row)))))))))))

(defn- live-ids [{:keys [parked followup]}]
  (set (remove nil?
               (concat (keys (:records parked)) (keys (:leased parked)) (vals (:coalesced parked))
                       (map :park-id (mapcat val (:ready-inbox parked)))
                       (map :followup-id (mapcat val (:queued followup)))
                       (keys (:leased followup)) (keys (:terminal followup)) (vals (:dedupe followup))))))

(defn compare-state
  "Compare complete snapshots, not a selected-field projection. Diagnostics keep
   payload contents out of the report; callers can inspect rebuild's :states."
  [entries live]
  (let [entries (vec (remove #(contains? outcome/types (:evidence/type %)) entries))
        {:keys [states issues replayed excluded-promises]} (rebuild entries)
        known (set (map promise-id entries))
        no-history (sort (set/difference (live-ids live) known))
        differences (vec (for [[store snapshot] live
                               change (capture/coverage-issues store snapshot (get states store))]
                           (assoc change :store store)))
        issues (into issues (for [pid no-history]
                              {:promise-id pid :reason :no-history
                               :message "No captured history: predates P2a or an unrecorded transition"}))]
    {:equal? (and (empty? issues) (empty? differences))
     :record-count (count entries) :replayed-count (count replayed)
     :incomplete-promises (vec (sort-by str (into excluded-promises (map :promise-id issues))))
     :issues issues :differences differences}))
