(ns futon3c.file-change.journal
  "Pure bounded delivery-hint journal for file-change hygiene.

  The journal is not correctness authority. Consumers that fall behind the
  retention watermark must rebuild from their own authority before resuming.")

(defn empty-state [max-entries]
  {:pre [(and (integer? max-entries) (pos? max-entries))]}
  {:schema :file-change/journal-v1
   :max-entries max-entries
   :next-seq 1
   :watermark 0
   :journal []
   :latest {}
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
  begins at the current tail: correctness comes from its initial authority
  read, represented by REBUILD-EVIDENCE."
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
               :rebuild-required? false})))

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

(defn rebuild-consumer
  "Resume a lagged consumer only after it supplies a fresh authority receipt."
  [state consumer-id now-ms rebuild-evidence]
  {:pre [(map? rebuild-evidence)]}
  (when-not (get-in state [:consumers consumer-id])
    (throw (ex-info "Consumer not registered" {:consumer-id consumer-id})))
  (-> state
      (assoc-in [:consumers consumer-id :after] (dec (:next-seq state)))
      (assoc-in [:consumers consumer-id :last-progress-at-ms] now-ms)
      (assoc-in [:consumers consumer-id :rebuild-evidence] rebuild-evidence)
      (assoc-in [:consumers consumer-id :rebuild-required?] false)))

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
   :consumers
   (into {}
         (map (fn [[id c]]
                [id {:lag (- (dec (:next-seq state)) (:after c))
                     :age-ms (- now-ms (:last-progress-at-ms c))
                     :rebuild-required? (:rebuild-required? c)}]))
         (:consumers state))})
