(ns futon3c.file-change.journal
  "Pure bounded delivery-hint journal for file-change hygiene.

  The journal is not correctness authority. Consumers that fall behind the
  retention watermark must rebuild from their own authority before resuming.")

(defn empty-state [max-entries]
  {:pre [(and (integer? max-entries) (pos? max-entries))]}
  {:schema :file-change/journal-v2
   :max-entries max-entries
   :next-seq 1
   :watermark 0
   :journal []
   :latest {}
   :walk-levels {}
   :last-refusal nil
   :consumers {}})

(defn path-key [{:keys [repo worktree path]}]
  [repo worktree path])

(defn- valid-observation? [{:keys [repo worktree path content-id observed-at-ms]}]
  (and (string? repo) (not-empty repo)
       (string? worktree) (not-empty worktree)
       (string? path) (not-empty path)
       (string? content-id) (not-empty content-id)
       (integer? observed-at-ms) (not (neg? observed-at-ms))))

(defn- same-level? [old observation]
  (= (select-keys old [:content-id :operation])
     (select-keys observation [:content-id :operation])))

(defn observe
  "Record OBSERVATION unless the same path already has the same content level.
  Returns [state {:outcome ...}]. Repeated saves update neither sequence nor
  journal pressure. Retention is always bounded and never waits for a cursor."
  [state observation]
  (when-not (valid-observation? observation)
    (throw (ex-info "Invalid file-change observation" {:observation observation})))
  (let [k (path-key observation)
        prior (get-in state [:latest k])]
    (if (same-level? prior observation)
      [state {:outcome :duplicate :path-key k :seq (:seq prior)}]
      (let [seq-id (:next-seq state)
            entry (assoc observation :seq seq-id :path-key k)
            appended (conj (:journal state) entry)
            excess (max 0 (- (count appended) (:max-entries state)))
            dropped (take excess appended)
            kept (vec (drop excess appended))
            watermark (if (seq dropped) (:seq (last dropped)) (:watermark state))
            state' (-> state
                       (assoc :journal kept :watermark watermark)
                       (assoc-in [:latest k] entry)
                       (update :next-seq inc)
                       (update :consumers
                               (fn [cs]
                                 (into {}
                                       (map (fn [[id c]]
                                              [id (cond-> c
                                                    (< (:after c) watermark)
                                                    (assoc :rebuild-required? true))]))
                                       cs))))]
        [state' {:outcome :appended :path-key k :seq seq-id
                 :dropped (count dropped) :watermark watermark}]))))

(defn register-consumer
  "Register an owned consumer with a finite progress deadline. Registration
  begins at the current tail but fail-closed: it MUST complete a fresh authority
  rebuild before reading hints. REBUILD-EVIDENCE names that authority; it is a
  request/basis, not proof that the read already happened."
  [state consumer-id now-ms deadline-ms rebuild-evidence]
  {:pre [(keyword? consumer-id) (integer? now-ms)
         (integer? deadline-ms) (pos? deadline-ms)
         (map? rebuild-evidence)]}
  (let [tail (dec (:next-seq state))]
    (assoc-in state [:consumers consumer-id]
              {:after tail
               :registered-at-ms now-ms
               :last-progress-at-ms now-ms
               :deadline-ms deadline-ms
               :rebuild-evidence rebuild-evidence
               :rebuild-required? true})))

(defn read-next [state consumer-id limit]
  {:pre [(integer? limit) (pos? limit)]}
  (if-let [consumer (get-in state [:consumers consumer-id])]
    (if (:rebuild-required? consumer)
      {:status :rebuild-required
       :watermark (:watermark state)
       :authority (:authority (:rebuild-evidence consumer))}
      {:status :ok
       :entries (->> (:journal state)
                     (filter #(< (:after consumer) (:seq %)))
                     (take limit)
                     vec)
       :watermark (:watermark state)})
    {:status :consumer-not-registered}))

(defn acknowledge [state consumer-id through-seq now-ms]
  (let [consumer (get-in state [:consumers consumer-id])
        tail (dec (:next-seq state))]
    (when-not consumer
      (throw (ex-info "Consumer not registered" {:consumer-id consumer-id})))
    (when (or (:rebuild-required? consumer)
              (< through-seq (:after consumer))
              (< tail through-seq))
      (throw (ex-info "Invalid consumer acknowledgement"
                      {:consumer-id consumer-id :through-seq through-seq
                       :tail tail :consumer consumer})))
    (-> state
        (assoc-in [:consumers consumer-id :after] through-seq)
        (assoc-in [:consumers consumer-id :last-progress-at-ms] now-ms))))

(defn begin-rebuild
  "Pin the journal tail BEFORE a consumer begins its authority read. Changes
  appended during that read remain after the returned `:through-seq`."
  [state consumer-id]
  (let [consumer (get-in state [:consumers consumer-id])]
    (when-not consumer
      (throw (ex-info "Consumer not registered" {:consumer-id consumer-id})))
    {:status :rebuild-begun :consumer-id consumer-id
     :through-seq (dec (:next-seq state)) :watermark (:watermark state)
     :authority (:authority (:rebuild-evidence consumer))}))

(defn rebuild-consumer
  "Complete a rebuild through the sequence pinned by `begin-rebuild`.
  Returns [state receipt]; the receipt drives full-authority demand rather than
  relying on a level-jump entry that the rebuild cursor intentionally passes."
  [state consumer-id through-seq now-ms rebuild-evidence]
  {:pre [(map? rebuild-evidence)]}
  (when-not (get-in state [:consumers consumer-id])
    (throw (ex-info "Consumer not registered" {:consumer-id consumer-id})))
  (let [tail (dec (:next-seq state))]
    (when (or (< through-seq (:watermark state)) (> through-seq tail))
      (throw (ex-info "Rebuild basis outside retained journal bounds"
                      {:consumer-id consumer-id :through-seq through-seq
                       :watermark (:watermark state) :tail tail})))
    [(-> state
         (assoc-in [:consumers consumer-id :after] through-seq)
         (assoc-in [:consumers consumer-id :last-progress-at-ms] now-ms)
         (assoc-in [:consumers consumer-id :rebuild-evidence] rebuild-evidence)
         (assoc-in [:consumers consumer-id :rebuild-required?] false))
     {:outcome :consumer-rebuilt :consumer-id consumer-id
      :through-seq through-seq :basis :consumer-rebuild
      :authority (:authority rebuild-evidence)}]))

(defn expire-consumers
  "Drop consumers whose progress deadline has elapsed. The returned receipts
  are durable-output candidates for the later health/red-to-work slice."
  [state now-ms]
  (let [expired (->> (:consumers state)
                     (keep (fn [[id {:keys [last-progress-at-ms deadline-ms]}]]
                             (when (< (+ last-progress-at-ms deadline-ms) now-ms)
                               id)))
                     sort
                     vec)]
    [(update state :consumers #(apply dissoc % expired))
     (mapv (fn [id] {:consumer-id id :outcome :expired-and-dropped
                     :at-ms now-ms :recovery :rebuild-from-authority})
           expired)]))

(defn health [state now-ms]
  {:entries (count (:journal state))
   :max-entries (:max-entries state)
   :watermark (:watermark state)
   :last-refusal (:last-refusal state)
   :consumers
   (into {}
         (map (fn [[id c]]
                [id {:lag (- (dec (:next-seq state)) (:after c))
                     :age-ms (- now-ms (:last-progress-at-ms c))
                     :rebuild-required? (:rebuild-required? c)}]))
         (:consumers state))})

(defn record-refusal
  "Retain the latest shadow-observer refusal for operator-visible health."
  [state at-ms message]
  (assoc state :last-refusal {:at-ms at-ms :message message
                              :rearm :restart-or-repair-file-change-observer}))

(defn- require-all-rebuild [state]
  (update state :consumers
          (fn [consumers]
            (into {} (map (fn [[id consumer]]
                            [id (assoc consumer :rebuild-required? true)]))
                  consumers))))

(defn reconcile-snapshot
  "Reconcile a complete repository walk against the last walk level.

  The first snapshot seeds the level without emitting historical changes.
  Later calls append changed and deleted paths through `observe`. This makes
  watcher notifications acceleration hints within one watcher epoch. This walk
  is not durable authority: every newly registered consumer starts
  rebuild-required and must read Git/registry/Giraffe authority before using
  hints. FILES is {relative-path content-id}."
  [state {:keys [repo worktree files observed-at-ms]}]
  (when-not (and (string? repo) (not-empty repo)
                 (string? worktree) (not-empty worktree)
                 (map? files)
                 (every? string? (keys files))
                 (every? #(and (string? %) (not-empty %)) (vals files))
                 (integer? observed-at-ms) (not (neg? observed-at-ms)))
    (throw (ex-info "Invalid authority snapshot"
                    {:repo repo :worktree worktree :files files
                     :observed-at-ms observed-at-ms})))
  (let [scope [repo worktree]
        absent (Object.)
        prior (get-in state [:walk-levels scope] absent)]
    (if (identical? absent prior)
      [(assoc-in state [:walk-levels scope] files)
       {:outcome :seeded :repo repo :worktree worktree
        :files (count files) :appended 0}]
      (let [paths (sort (into (set (keys prior)) (keys files)))
            changes (keep (fn [path]
                            (let [before (get prior path absent)
                                  after (get files path absent)]
                              (cond
                                (identical? before after) nil
                                (= before after) nil
                                (identical? absent after)
                                {:repo repo :worktree worktree :path path
                                 :content-id before :operation :delete
                                 :observed-at-ms observed-at-ms}
                                :else
                                {:repo repo :worktree worktree :path path
                                 :content-id after :operation :write
                                 :observed-at-ms observed-at-ms})))
                          paths)
            burst? (> (count changes) (:max-entries state))
            changes-to-record
            (if burst?
              [{:repo repo :worktree worktree :path "."
                :content-id (str "walk-level-jump:" observed-at-ms ":" (count changes))
                :operation :level-jump :observed-at-ms observed-at-ms}]
              changes)
            [state results]
            (reduce (fn [[s rs] observation]
                      (let [[s' result] (observe s observation)]
                        [s' (conj rs result)]))
                    [state []]
                    changes-to-record)
            state (cond-> (assoc-in state [:walk-levels scope] files)
                    burst? require-all-rebuild)]
        [state {:outcome (if burst? :level-jump :reconciled)
                :repo repo :worktree worktree
                :changed (count (filter #(= :write (:operation %)) changes))
                :deleted (count (filter #(= :delete (:operation %)) changes))
                :total-changes (count changes)
                :appended (count (filter #(= :appended (:outcome %)) results))}]))))
