(ns futon3c.circus.chain-record
  "Read-only, public-safe projection of one recorded Circus decision chain."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.giraffe.run :as giraffe]
            [futon3c.giraffe.run-data :as run-data]
            [futon3c.xiang.ma-occurrence :as ma]
            [futon3c.xiang.turn-store :as turns]))

(def ^:private animal-specs
  [[:驴 [:pattern/proposal 1] :turn]
   [:馬 [:pattern/occurrence 1] :turn]
   [:狗 [:memory/recall 1] :turn]
   [:象 [:interpretation 1] :source]
   [:mentions [:workplan/mention 2] :evidence]
   [:猴 [:hou/focus 1] :evidence]])

(defn- sorted-value [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [k (sorted-value v)])) x)
    (set? x) (into (sorted-set) (map sorted-value) x)
    (sequential? x) (mapv sorted-value x)
    :else x))

(defn canonical-edn [x]
  (with-out-str (pp/pprint (sorted-value x))))

(defn- query! [cfg query]
  ;; A lagging index answers :stale-index; retry briefly before refusing
  ;; (same bound as operator_focus graph-query!, b4e2be3ef).
  (let [run #((or (:query-fn cfg) store/bounded-query!) (:graph-store cfg) query)
        answer (loop [tries 0]
                 (let [a (run)]
                   (if (and (= :stale-index (get-in a [:ok :status])) (< tries 3))
                     (do (Thread/sleep 50) (recur (inc tries)))
                     a)))
        value (:ok answer)]
    (when-not (and (= :ok (:status value)) (not (:truncated? value)))
      (throw (ex-info "Chain graph query refused" {:query query :answer answer})))
    value))

(defn- entity [cfg id]
  (:ok ((or (:get-entity-fn cfg) store/get-entity) (:graph-store cfg) id)))

(defn- reverse-ids [cfg type role endpoint-id]
  (-> (query! cfg {:query/type :role-reverse :edge-type type
                   :endpoint-role role :endpoint-id endpoint-id
                   :limit 200 :order :asc})
      :edge-ids vec))

(defn- present-or-absent [ids]
  (if (seq ids) {:status :present :clusion-ids (vec (sort (distinct ids)))}
      {:status :absent :reason :no-clusions}))

(defn- item-id [cfg clusion-id]
  (get-in (entity cfg clusion-id) [:asserted-content :item/id]))

(defn- row-item-clusion [row]
  (some (fn [{:keys [role target]}]
          (when (#{:item} role) (:clusion/id target)))
        (:endpoints row)))

(defn- priority-row [rank row]
  {:item-id (some-> (:target row) name)
   :status :ranked
   :rank rank
   :pass-rank (:pass-rank row)
   :priority-band (:priority-band row)
   :cutoff-status (:cutoff-status row)})

(defn- pinned-ids [manifest]
  (->> (get-in manifest [:focus-relation-input :evidence-pin :evidence-pin])
       (keep #(when (sequential? %) (first %))) set))

(def ^:private ma-parts [:I1 :I2 :G :B])

(defn- text-present? [x]
  (and (some? x) (not (str/blank? (str x)))))

(defn- complete-part? [part]
  (and (map? part)
       (if (text-present? (:element part))
         (and (text-present? (:turn part))
              (text-present? (:quote part))
              (text-present? (:who part)))
         (text-present? (:why part)))))

(defn- complete-pattern-row? [row]
  (and (map? row)
       (every? (fn [window]
                 (let [reading (get row window)]
                   (and (map? reading)
                        (every? #(complete-part? (get reading %)) ma-parts))))
               [:turn :chain])))

(defn ma-reading
  "Project the turn-local 馬 obligation from stored pattern verdicts. Reader
  failures are failed work, never typed absence."
  [turn]
  (let [turn (if (contains? turn :lu_proposal_ids)
               turn
               (assoc turn :lu_proposal_ids
                      (mapv :id (get-in turn [:lu_ranked :ranked]))))
        proposed (mapv :id (ma/patterns-of turn))
        verdict (:pattern_verdicts turn)
        rows (into {} (keep (fn [row]
                              (when-let [pattern (:pattern row)]
                                [(str pattern) row])))
                   (:patterns verdict))
        batches (get-in verdict [:model_outcome :batches])
        batch-status
        (into {}
              (mapcat (fn [batch]
                        (let [failure (if (contains? batch :failure_kind)
                                        (:failure_kind batch)
                                        "legacy-unknown")]
                          (map #(vector (str %) failure) (:patterns batch)))))
              batches)
        absence (cond
                  (not= "operator" (name (or (:origin turn) :unknown))) :non-operator-origin
                  (:reply_absence turn) :reply-absence
                  (not (text-present? (:reply_text turn))) :no-reply-text
                  (empty? proposed) :no-proposed-patterns
                  :else nil)
        pattern-rows
        (mapv (fn [pattern]
                (let [failure (get batch-status pattern "legacy-unknown")
                      row (get rows pattern)
                      completed? (and (nil? failure) (complete-pattern-row? row))]
                  {:pattern pattern
                   :status (if completed? :completed :not-completed)
                   :batch/failure-kind failure}))
              proposed)]
    {:proposed-pattern-ids proposed
     :patterns pattern-rows
     :batch-failure-kinds (mapv (fn [batch]
                                  (if (contains? batch :failure_kind)
                                    (:failure_kind batch)
                                    "legacy-unknown"))
                                (or batches []))
     :connection (:connection verdict)
     :legitimate-absence-reason absence
     :obligation-met? (or (some? absence)
                          (every? #(= :completed (:status %)) pattern-rows))}))

(defn item-absence
  "Typed reason a turn has no items, or nil. Legitimate only when the turn
  has no mention clusions AND no 猴 focus-relation edges (a 猴 header with
  :hou/relations [] is an empty focus reading, CL-62). Items that exist in
  evidence but fail to resolve are not absent; they fail the ranking theorem."
  [items mention-ids focus-link-ids]
  (when (and (empty? items) (empty? mention-ids) (empty? focus-link-ids))
    :no-item-evidence))

(defn- item-links [cfg relation-ids kind]
  (->> relation-ids
       (keep (fn [relation-id]
               (when-let [clusion-id (some-> (entity cfg relation-id)
                                              row-item-clusion)]
                 (when-let [id (item-id cfg clusion-id)]
                   {:item-id id :kind kind
                    :relation-id relation-id
                    :clusion-id clusion-id}))))
       distinct
       (sort-by (juxt :item-id :relation-id))
       vec))

(defn turn-gcc
  "Light, read-only projection of one turn's Circus graph. Unlike
  `chain-record`, this reads no Giraffe run and performs no verification.
  Optional query/read/entity seams are the same as `chain-record`."
  [{:keys [turn-id] :as opts}]
  (let [cfg (merge (mentions/default-config) opts)
        turn ((or (:read-turn-fn cfg) turns/read-record) (:turn-store cfg) turn-id)
        _ (when-not turn (throw (ex-info "Turn not found" {:reason :turn-not-found
                                                            :turn-id turn-id})))
        turn (assoc turn :id turn-id)
        turn-ref-id (or (:turn-reference-id cfg)
                        (get-in (mentions/field-evidence cfg turn :source_text)
                                [:reference :id]))
        _ (when-not turn-ref-id
            (throw (ex-info "Turn evidence reference unavailable"
                            {:reason :turn-reference-unavailable :turn-id turn-id})))
        ordinary (into {}
                       (for [[animal type role] animal-specs]
                         [animal (reverse-ids cfg type role turn-ref-id)]))
        header-ids (:猴 ordinary)
        focus-link-ids (vec (mapcat #(reverse-ids cfg [:hou/focus-relation 1]
                                                  :focus %) header-ids))
        gcc (-> ordinary
                (assoc :猴 (vec (concat header-ids focus-link-ids)))
                (update-vals present-or-absent))
        links (vec (concat (item-links cfg (:mentions ordinary) :mention)
                           (item-links cfg focus-link-ids :focus)))
        items (->> links (map :item-id) distinct sort vec)]
    {:turn {:id turn-id :evidence-ref turn-ref-id
            :created_at (:created_at turn) :origin (:origin turn)
            :session_id (:session_id turn)
            :mission_id (:mission_id turn)
            :prev_operator_turn_id (:prev_operator_turn_id turn)
            :next_operator_turn_id (:next_operator_turn_id turn)}
     :ma-connection (get-in turn [:pattern_verdicts :connection])
     ;; Route consumers derive the compact strand view from the same durable
     ;; turn read as the GCC.  Keeping these source fields here avoids a
     ;; second turn-store read on the request path.
     :pattern-verdicts (:pattern_verdicts turn)
     :lu-ranked (:lu_ranked turn)
     :still-open (get-in turn [:ports :still_open])
     :closed-this-turn (get-in turn [:ports :closed_this_turn])
     :acts (:acts turn)
     :gcc gcc
     :items items
     :item-links links
     :item-evidence {:mention-relation-ids (vec (:mentions ordinary))
                     :focus-relation-ids focus-link-ids}
     :item-absence (item-absence items (:mentions ordinary) focus-link-ids)}))

(defn chain-record
  "Collect one turn's recorded chain. Optional injected :query-fn,
  :get-entity-fn, :read-turn-fn and :turn-reference-id make proof evaluation
  and invented fixtures hermetic. No write port is present."
  [{:keys [turn-id run-dir] :as opts}]
  (let [base (mentions/default-config)
        cfg (merge base opts)
        turn ((or (:read-turn-fn cfg) turns/read-record) (:turn-store cfg) turn-id)
        _ (when-not turn (throw (ex-info "Turn not found" {:turn-id turn-id})))
        turn (assoc turn :id turn-id)
        turn-ref-id (or (:turn-reference-id cfg)
                        (get-in (mentions/field-evidence cfg turn :source_text)
                                [:reference :id]))
        _ (when-not turn-ref-id
            (throw (ex-info "Turn evidence reference unavailable" {:turn-id turn-id})))
        light (turn-gcc (assoc cfg :turn-id turn-id :turn-reference-id turn-ref-id))
        gcc (:gcc light)
        items (:items light)
        focus-link-ids (get-in light [:item-evidence :focus-relation-ids])
        mention-ids (get-in light [:item-evidence :mention-relation-ids])
        manifest (edn/read-string (slurp (io/file run-dir "manifest.edn")))
        priority-file (io/file run-dir "priority.edn")
        priority (if (.isFile priority-file) (edn/read-string (slurp priority-file)) [])
        ranked (map-indexed (fn [i row] (priority-row (inc i) row)) priority)
        all-ids (set (mapcat #(or (:clusion-ids %) []) (vals gcc)))
        focus-link (vec (sort (set/intersection all-ids (pinned-ids manifest))))
        body {:turn {:id turn-id :created_at (:created_at turn) :origin (:origin turn)
                     :reply_text_present? (contains? turn :reply_text)
                     :reply_absence (:reply_absence turn)}
              :ma-reading (ma-reading turn)
              :gcc gcc
              :items items
              :item-absence (item-absence items mention-ids focus-link-ids)
              :run {:run-id (:run-id manifest)
                    :producer-git-sha (:git-sha manifest)
                    :readings-generation (:readings-generation manifest)
                    :input-sha256 (run-data/logical-sha
                                   (run-data/read-run-file run-dir "input.edn"))
                    :focus/version (:focus/version manifest)
                    :verification ((or (:verify-run-fn cfg) giraffe/verify-run) run-dir)}
              :focus-link focus-link
              :priority {:items (let [by-item (into {} (map (juxt :item-id identity)) ranked)]
                                  (mapv #(get by-item % {:item-id % :status :not-ranked}) items))
                         :top-10 (vec (take 10 ranked))}}
        digest (giraffe/sha256 (canonical-edn body))]
    (assoc body :record/sha256 digest)))

(defn -main [& [turn-id run-dir]]
  (when-not (and turn-id run-dir)
    (throw (ex-info "usage: chain-record TURN-ID RUN-DIR" {})))
  (print (canonical-edn (chain-record {:turn-id turn-id :run-dir run-dir}))))
