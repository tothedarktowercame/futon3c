(ns futon3c.circus.operator-focus
  "Read-only cross-seat attention window. No publications."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.giraffe-needs :as giraffe-needs]
            [futon3c.arxana.item-inventory :as inventory]
            [futon3c.arxana.store :as store]
            [futon3c.circus.hou :as hou]
            [futon3c.circus.hou-store :as hou-store]
            [futon3c.giraffe.missions :as missions]
            [futon3c.xiang.turn-store :as turns])
  (:import [java.time Instant]))

(defn millis [t]
  (if (number? t) (long t) (.toEpochMilli (Instant/parse (str t)))))

(defn in-window? [until hours at]
  (< (- (millis until) (* hours 3600000)) (millis at) (inc (millis until))))

(defn read-successors
  "Current, exactly attested successor IDs for ITEM. A claim whose attested
  predecessor source SHA no longer matches the inventory is stale (including
  an un-superseded item) and is ignored."
  [cfg item]
  (let [predecessor (:id (inventory/item-record (:id item)))
        answer (store/bounded-query! (:graph-store cfg)
                                     {:query/type :role-reverse
                                      :edge-type [:supersedes 1]
                                      :endpoint-role :predecessor
                                      :endpoint-id predecessor
                                      :limit 200 :order :desc})
        q (:ok answer)
        _ (when (or (:error answer) (:truncated? q))
            (throw (ex-info "Supersession graph unreadable"
                            {:reason :supersession-unread :answer answer})))
        claims (mapv (fn [id]
                       (let [r (store/get-entity (:graph-store cfg) id)]
                         (or (:ok r)
                             (throw (ex-info "Supersession claim unreadable"
                                             {:reason :supersession-unread :answer r})))))
                     (:edge-ids q))]
    (->> claims
         (keep (fn [claim]
                 (let [answer (store/bounded-query! (:graph-store cfg)
                                                    {:query/type :attestations
                                                     :subject-id (:id claim)
                                                     :limit 50})
                       aq (:ok answer)
                       _ (when (or (:error answer) (:truncated? aq))
                           (throw (ex-info "Supersession attestations unreadable"
                                           {:reason :supersession-unread :answer answer})))
                       current? (some (fn [aid]
                                        (let [r (store/get-entity (:graph-store cfg) aid)
                                              a (or (:ok r)
                                                    (throw (ex-info "Supersession attestation unreadable"
                                                                    {:reason :supersession-unread
                                                                     :answer r})))]
                                          (and (= "futon3c/item-inventory" (:producer a))
                                               (= :supersession-census (get-in a [:method :name]))
                                               (= (get-in item [:source :sha256])
                                                  (get-in a [:content :file/sha256])))))
                                      (:attestation-ids aq))]
                   (when current?
                     (some (fn [{:keys [role target]}]
                             (when (= :successor role)
                               ((:item-id-by-clusion cfg) (:clusion/id target))))
                           (:endpoints claim))))))
         distinct vec)))

(defn with-gcc-successors
  "Attach GCC successor sets for ROOTS and the chains reachable from them."
  [cfg items roots]
  (let [by-id (into {} (map (juxt :id identity)) items)
        cfg (assoc cfg :items-by-id by-id
                   :item-id-by-clusion
                   (into {} (map (fn [id] [(:id (inventory/item-record id)) id]))
                         (keys by-id)))]
    (loop [todo (vec (distinct (filter by-id roots))) seen #{} successors {}]
      (if-let [id (first todo)]
        (if (seen id)
          (recur (subvec todo 1) seen successors)
          (let [ids (read-successors cfg (by-id id))]
            (recur (into (subvec todo 1) (filter by-id ids))
                   (conj seen id) (assoc successors id ids))))
        (mapv (fn [item] (assoc item :successors (get successors (:id item) []))) items)))))

(defn resolve-tag
  "Follow at most eight explicit SUPERSEDED successors. Named
  occurrences never call this resolver. Cycles/overlong chains keep the tag
  and emit a notice rather than inventing a successor."
  [by-id tag]
  (loop [id tag path []]
    (let [successors (:successors (by-id id))
          successor (first successors)]
      (cond
        (some #{id} path) {:item tag :notice {:item tag :reason :supersession-cycle :path (conj path id)}}
        (> (count successors) 1) {:item tag :notice {:item id :reason :supersession-ambiguous}}
        (nil? successor) {:item id :path (conj path id)}
        (>= (count path) 8) {:item tag :notice {:item tag :reason :supersession-limit :path (conj path id)}}
        :else (recur successor (conj path id))))))

(defn focus-window
  "Pure core. named/reply-named contain only unquoted occurrences. Unknown
  tags are reported, never invented as open items. Window is (start, until]."
  [{:keys [until window-hours items turns needs pins notices] :or {window-hours 6}}]
  (let [ts (filter #(in-window? until window-hours (:created-at %)) turns)
        by-id (into {} (map (juxt :id identity)) items)
        resolved (into {} (for [t ts :when (and (not (:pending-reason t))
                                                (empty? (:named t)) (seq (:mission-id t)))]
                            [(:id t) (resolve-tag by-id (:mission-id t))]))
        assignments (for [t ts id (distinct (if (seq (:named t)) (:named t)
                                               (when-let [id (:item (get resolved (:id t)))] [id])))]
                      [id t])
        grouped (group-by first assignments)
        summarize (fn [id]
                    (let [xs (map second (get grouped id))]
                      {:item id :turn-count (count xs) :seats (vec (sort (distinct (map :seat xs))))
                       :most-recent (when (seq xs) (:created-at (apply max-key #(millis (:created-at %)) xs)))}))
        focus (->> (keys grouped) (filter #(= :open (:state (by-id %))))
                   (map summarize)
                   (sort-by (juxt (comp - :turn-count) #(-> % :most-recent millis -) :item)) vec)
        closed (->> items
                    (filter #(and (= :closed (:state %)) (:closed-at %)
                                  (in-window? until window-hours (:closed-at %))))
                    (map #(merge (summarize (:id %)) (select-keys % [:closed-at :closing-commit])))
                    (sort-by :item) vec)
        no-mission (->> ts (filter #(and (not (:pending-reason %)) (empty? (:named %))
                                         (not (seq (:mission-id %)))))
                        (group-by :seat)
                        (map (fn [[seat xs]] {:seat seat :turn-count (count xs)})) (sort-by :seat) vec)
        relations (hou/focus-rule :mission-and-named
                                  {:operator-items (map :item focus) :reply-items (mapcat :reply-named ts)
                                   :needs needs :associated-radius 1})
        pin (vec (sort-by first (distinct (concat pins (mapcat :pin ts)))))
        header-ids (vec (distinct (mapcat :header-ids ts)))
        unknown (for [id (sort (remove by-id (keys grouped)))] {:item id :reason :unknown-item})
        inactive (for [id (sort (keys grouped))
                       :when (and (= :closed (:state (by-id id)))
                                  (not (some #(= id (:item %)) closed)))]
                   (merge (summarize id) {:reason :not-open :status-class (:status-class (by-id id))}))]
    {:report {:until (str until) :window-hours window-hours :turns (count ts)
              :turn-kinds {:headered (count (filter #(and (:header-id %) (seq (:named %))) ts))
                           :pending (count (filter :pending-reason ts))
                           :mission-tag (count (filter #(and (not (:pending-reason %))
                                                            (empty? (:named %)) (seq (:mission-id %))) ts))
                           :no-mission (count (filter #(and (not (:pending-reason %))
                                                           (empty? (:named %)) (not (seq (:mission-id %)))) ts))}
              :focus focus :just-closed closed :no-mission no-mission
              :associated (mapv first (filter #(= :associated (second %)) relations))
              :notices (vec (concat notices (keep :notice (vals resolved)) unknown inactive))}
     :focus-input {:relations (into {} relations) :evidence-pin pin :header-ids header-ids}}))

(def status-line #"(?i)^\s*(?:[-*]\s*)?(?:#+\s*)?(?:\*\*)?Status:?(?:\*\*)?\s*:?\s*(.+)$")

(defn closing-change
  "Compare the first status line (the registry's grammar) in full before/after
  versions. DONE/COMPLETE count as completed, as in the Giraffe adapter.
  Later edits of an already closed status sentence do not reset closure time."
  [before after]
  (let [group (fn [text] (some->> (str/split-lines text)
                                 (keep #(second (re-matches status-line %))) first
                                 missions/status->group))
        closed? #(contains? #{:completed :cancelled} (group %))]
    (and (closed? after) (not (closed? before)))))

(defn- git [root & args]
  (let [r (apply shell/sh "git" "-C" root args)]
    (when-not (zero? (:exit r)) (throw (ex-info "closure history unavailable" {:root root :args args})))
    (:out r)))

(defn- version-text [root revision path]
  (let [r (shell/sh "git" "-C" root "show" (str revision ":" path))]
    (cond (zero? (:exit r)) (:out r)
          (or (str/includes? (:err r) "does not exist in")
              (str/includes? (:err r) "exists on disk, but not in")
              (str/includes? (:err r) "invalid object name")) ""
          :else (throw (ex-info "git version unavailable" {:root root :revision revision :path path})))))

(defn with-closures
  "Git committer timestamps of status transitions within the requested window.
  First find changed paths per repository, then inspect only closed candidates.
  No mtime/date-in-prose inference. Uncommitted closures have no commit time."
  [items until hours]
  (let [start (str (Instant/ofEpochMilli (- (millis until) (* hours 3600000))))
        roots (group-by #(first (str/split (:path %) #"/holes/")) items)
        closures
        (into {}
              (mapcat
               (fn [[root rows]]
                 (let [changed (set (str/split-lines
                                     (git root "log" (str "--since=" start) (str "--until=" until)
                                          "--format=" "--name-only" "--" "holes")))]
                   (for [{:keys [id path state]} rows
                         :let [relative (subs path (inc (count root)))]
                         :when (and (= :closed state) (contains? changed relative))
                         :let [history (git root "log" (str "--since=" start) (str "--until=" until)
                                            "--format=%H %cI" "--" relative)
                               hit (some (fn [line]
                                           (let [[sha at] (str/split line #" ")]
                                             (when (closing-change (version-text root (str sha "^") relative)
                                                                   (version-text root sha relative))
                                               {:closed-at at :closing-commit sha})))
                                         (remove str/blank? (str/split-lines history)))]]
                     [id hit]))) roots))]
    (mapv #(merge % (get closures (:id %))) items)))

(defn read-turn-focus
  "Read the stored Hou focus projection used by mission-tag fallback. Public so
  QA can explain the same decision without recreating header semantics."
  [cfg {:keys [id record]}]
  (let [record (assoc record :id id)
        result (hou-store/headers-for-turn cfg record)
        header (last (:headers result))
        base {:id id :seat (or (:agent_id record) "unknown-seat") :created-at (:created_at record)
              :mission-id (:mission_id record)
              :pin [[(str "turn/" id) (digest/canonical-digest record)]]}]
    (if header
      {:turn (assoc base
                    :header-id (:id header)
                    :header-ids (mapv :id (:headers result))
                    :named (->> (get-in header [:asserted-content :hou/relations])
                                (keep (fn [[item relation]] (when (= :focus relation) item))) distinct vec)
                    :reply-named []
                    :pin (into (:pin base) (map (fn [h] [(:id h) (digest/canonical-digest h)])
                                                (:headers result))))}
      {:turn (assoc base :named [] :reply-named [] :pending-reason (:pending result))
       :notice {:turn id :reason :pending :detail (:pending result)}})))

(defn operator-focus
  ([opts] (operator-focus (hou-store/default-config) opts))
  ([cfg {:keys [until window-hours] :or {window-hours 6} :as opts}]
   (let [raw-items (with-closures (inventory/current-items cfg) until window-hours)
         records (filter #(and (= "operator" (get-in % [:record :origin]))
                               (in-window? until window-hours (get-in % [:record :created_at])))
                         (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE))
         items (with-gcc-successors cfg raw-items
                                    (keep #(get-in % [:record :mission_id]) records))
         ;; This read-only report recognizes the current census, including IDs
         ;; not yet published as clusions. It writes no dangling references.
         cfg (assoc cfg :item-ids (mapv :id items))
         read (mapv #(read-turn-focus cfg %) records)
         nd (try {:data ((or (:read-needs cfg) giraffe-needs/read-needs-gcc) cfg)}
                 (catch Exception e {:notice {:reason :needs-unavailable :detail (:reason (ex-data e))}}))
         data (:data nd)
         pins (concat (map (fn [{:keys [id record]}] [(str "turn/" id) (digest/canonical-digest (assoc record :id id))]) records)
                      (map #(vector (str "item/" (:id %)) (digest/canonical-digest %)) items)
                      (map #(vector (:id %) (digest/canonical-digest %))
                           (concat (:basis-records data) (map :record (:readings data)))))]
     (focus-window (merge opts {:items items :turns (keep :turn read)
                               :needs (mapcat :needs (:readings data)) :pins pins
                               :notices (concat (keep :notice read) (when (:notice nd) [(:notice nd)]))})))))

(defn -main [& [until]]
  (try (prn (operator-focus {:until (or until (str (Instant/now))) :window-hours 6}))
       (finally (shutdown-agents))))
