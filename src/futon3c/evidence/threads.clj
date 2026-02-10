(ns futon3c.evidence.threads
  "Thread projection over the evidence landscape.

   Threads are views: group EvidenceEntry by shared subject into a proof tree.
   Primary data remains in the evidence store (R8)."
  (:require [futon3c.evidence.store :as store])
  (:import [java.time Instant]))

(declare thread-status)

(def ^:private stalled-threshold-ms
  "Heuristic threshold for a thread to be considered stalled."
  (* 24 60 60 1000)) ; 24h

(defn- parse-instant
  [t]
  (cond
    (instance? Instant t) t
    (string? t) (Instant/parse t)
    :else (Instant/ofEpochMilli 0)))

(defn- entry-at [e] (parse-instant (:evidence/at e)))

(defn thread-status
  "Compute status of a thread projection.
   :closed = has a conclusion claim
   :forked = has forks
   :stalled = no activity in threshold window
   :open = default"
  [thread-projection]
  (let [entries (or (:thread/entries thread-projection) [])
        has-conclusion? (some #(= :conclusion (:evidence/claim-type %)) entries)
        has-fork? (some :evidence/fork-of entries)
        newest (last (sort-by entry-at entries))
        stalled? (when newest
                   (> (- (System/currentTimeMillis) (.toEpochMilli (entry-at newest)))
                      stalled-threshold-ms))]
    (cond
      has-conclusion? :closed
      has-fork? :forked
      stalled? :stalled
      :else :open)))

(defn project-thread
  "Group all entries for a subject into a thread projection (proof tree view).
   Returns ThreadProjection map or nil if subject has no goal entry."
  [store subject-ref]
  (let [entries (store/query* store {:query/subject subject-ref})
        entries (->> entries (sort-by entry-at) vec)
        goal (first (filter #(= :goal (:evidence/claim-type %)) entries))]
    (when goal
      (let [participants (->> entries (map :evidence/author) (remove nil?) set)
            thread-id (str "thread/" (:ref/type subject-ref) "/" (:ref/id subject-ref))
            projection {:thread/id thread-id
                        :thread/subject subject-ref
                        :thread/goal goal
                        :thread/entries entries
                        :thread/participants participants
                        :thread/entry-count (count entries)}]
        (assoc projection :thread/status (thread-status projection))))))

(defn- reply-descendants
  [entries root-id]
  (let [by-parent (group-by :evidence/in-reply-to entries)]
    (loop [q [root-id] acc #{}]
      (if-let [eid (first q)]
        (let [kids (map :evidence/id (get by-parent eid))]
          (recur (into (vec (rest q)) kids)
                 (into acc kids)))
        acc))))

(defn thread-forks
  "Return fork projections (alternative branches) for a thread projection."
  [thread-projection]
  (let [entries (:thread/entries thread-projection)
        subject (:thread/subject thread-projection)
        goal (:thread/goal thread-projection)
        forks (->> entries
                   (filter :evidence/fork-of)
                   (group-by :evidence/fork-of))
        by-id (into {} (map (juxt :evidence/id identity) entries))
        prefix-chain (fn [branch-id]
                       (loop [eid branch-id acc [] seen #{}]
                         (if (or (nil? eid) (contains? seen eid))
                           (vec (reverse acc))
                           (if-let [e (get by-id eid)]
                             (recur (:evidence/in-reply-to e) (conj acc e) (conj seen eid))
                             (vec (reverse acc))))))]
    (->> forks
         (map (fn [[branch-id fork-entries]]
                (let [prefix (prefix-chain branch-id)
                      fork-ids (into #{} (map :evidence/id fork-entries))
                      fork-desc (reduce (fn [acc eid]
                                          (into acc (reply-descendants entries eid)))
                                        #{}
                                        fork-ids)
                      wanted (into (set (map :evidence/id prefix))
                                   (concat fork-ids fork-desc))
                      branch-entries (->> entries (filter #(contains? wanted (:evidence/id %))) (sort-by entry-at) vec)
                      participants (->> branch-entries (map :evidence/author) (remove nil?) set)
                      tid (str (:thread/id thread-projection) "/fork/" branch-id)]
                  {:thread/id tid
                   :thread/subject subject
                   :thread/goal goal
                   :thread/entries branch-entries
                   :thread/participants participants
                   :thread/status :forked
                   :thread/entry-count (count branch-entries)})))
         vec)))

(defn thread-conjectures
  "Return conjectures and their lifecycle status (:open|:confirmed|:refuted)."
  [thread-projection]
  (let [entries (:thread/entries thread-projection)
        conjectures (filter (fn [e]
                              (or (= :conjecture (:evidence/claim-type e))
                                  (true? (:evidence/conjecture? e))))
                            entries)
        by-reply (group-by :evidence/in-reply-to entries)]
    (->> conjectures
         (map (fn [c]
                (let [cid (:evidence/id c)
                      replies (get by-reply cid)
                      confirmed? (some #(= :conclusion (:evidence/claim-type %)) replies)
                      refuted? (some #(= :correction (:evidence/type %)) replies)
                      status (cond
                               refuted? :refuted
                               confirmed? :confirmed
                               :else :open)]
                  {:conjecture c :status status})))
         vec)))

(defn thread-patterns
  "Aggregate pattern usage across a thread projection."
  [thread-projection]
  (let [entries (:thread/entries thread-projection)
        by-pid (->> entries
                    (keep :evidence/pattern-id)
                    frequencies)
        success? (fn [pid]
                   (boolean
                    (some (fn [e]
                            (and (= pid (:evidence/pattern-id e))
                                 (= :pattern-outcome (:evidence/type e))
                                 (let [b (:evidence/body e)]
                                   (and (map? b)
                                        (or (true? (:success? b))
                                            (true? (:success b))
                                            (true? (:ok b)))))))
                          entries)))]
    (->> by-pid
         (map (fn [[pid n]]
                {:pattern-id pid
                 :applied-count (int n)
                 :success? (success? pid)}))
         (sort-by :pattern-id)
         vec)))
