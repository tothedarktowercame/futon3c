(ns futon3c.circus.operator-focus
  "Read-only cross-seat attention window. No publications."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.item-inventory :as inventory]
            [futon3c.arxana.turn-mention-writer :as writer]
            [futon3c.circus.hou :as hou]
            [futon3c.circus.hou-store :as hou-store]
            [futon3c.giraffe.missions :as missions]
            [futon3c.xiang.turn-store :as turns])
  (:import [java.time Instant]))

(defn millis [t]
  (if (number? t) (long t) (.toEpochMilli (Instant/parse (str t)))))

(defn in-window? [until hours at]
  (< (- (millis until) (* hours 3600000)) (millis at) (inc (millis until))))

(defn with-successors
  "Read explicit successor declarations from SUPERSEDED items only. Ambiguous
  declarations refuse resolution instead of arbitrarily choosing a successor."
  [items]
  (mapv (fn [item]
          (if (re-find #"(?i)^\s*SUPERSEDED\b" (or (:status-line item) ""))
            (let [ids (distinct (map second (re-seq #"(?i)superseded by\s+`?([MET]-[A-Za-z0-9_-]+)"
                                                   (slurp (:path item)))))]
              (cond-> item
                (= 1 (count ids)) (assoc :successor (first ids))
                (> (count ids) 1) (assoc :successor-ambiguous? true)))
            item)) items))

(defn resolve-tag
  "Follow at most eight explicit SUPERSEDED successors. Named
  occurrences never call this resolver. Cycles/overlong chains keep the tag
  and emit a notice rather than inventing a successor."
  [by-id tag]
  (loop [id tag path []]
    (let [status (:status-line (by-id id))
          successor (when (and status (re-find #"(?i)^\s*SUPERSEDED\b" status))
                      (or (:successor (by-id id))
                          (second (re-find #"(?i)superseded by\s+`?([MET]-[A-Za-z0-9_-]+)" status))))]
      (cond
        (some #{id} path) {:item tag :notice {:item tag :reason :supersession-cycle :path (conj path id)}}
        (:successor-ambiguous? (by-id id)) {:item tag :notice {:item id :reason :supersession-ambiguous}}
        (nil? successor) {:item id}
        (>= (count path) 8) {:item tag :notice {:item tag :reason :supersession-limit :path (conj path id)}}
        :else (recur successor (conj path id))))))

(defn focus-window
  "Pure core. named/reply-named contain only unquoted occurrences. Unknown
  tags are reported, never invented as open items. Window is (start, until]."
  [{:keys [until window-hours items turns needs pins notices] :or {window-hours 6}}]
  (let [ts (filter #(in-window? until window-hours (:created-at %)) turns)
        by-id (into {} (map (juxt :id identity)) items)
        resolved (into {} (for [t ts :when (and (empty? (:named t)) (seq (:mission-id t)))]
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
        no-mission (->> ts (filter #(and (empty? (:named %)) (not (seq (:mission-id %)))))
                        (group-by :seat)
                        (map (fn [[seat xs]] {:seat seat :turn-count (count xs)})) (sort-by :seat) vec)
        relations (hou/focus-rule :mission-and-named
                                  {:operator-items (map :item focus) :reply-items (mapcat :reply-named ts)
                                   :needs needs :associated-radius 1})
        pin (vec (sort-by first (distinct (concat pins (mapcat :pin ts)))))
        unknown (for [id (sort (remove by-id (keys grouped)))] {:item id :reason :unknown-item})
        inactive (for [id (sort (keys grouped))
                       :when (and (= :closed (:state (by-id id)))
                                  (not (some #(= id (:item %)) closed)))]
                   (merge (summarize id) {:reason :not-open :status-class (:status-class (by-id id))}))]
    {:report {:until (str until) :window-hours window-hours :turns (count ts)
              :focus focus :just-closed closed :no-mission no-mission
              :associated (mapv first (filter #(= :associated (second %)) relations))
              :notices (vec (concat notices (keep :notice (vals resolved)) unknown inactive))}
     :focus-input {:relations (into {} relations) :evidence-pin pin}}))

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

(defn- read-turn [cfg by-clusion {:keys [id record]}]
  (let [record (assoc record :id id)
        source (writer/plan-field cfg record :source_text)
        reply (writer/plan-field cfg record :reply_text)
        item-ids (fn [p] (vec (distinct (keep #(when (= :mentioned (get-in % [:asserted-content :mention/role]))
                                                               (by-clusion (get-in % [:endpoints 0 :target :clusion/id])))
                                                             (:mentions p)))))]
    (if-not (:reference source)
      {:notice {:turn id :reason :evidence-unavailable}}
      {:turn {:id id :seat (or (:agent_id record) "unknown-seat") :created-at (:created_at record)
              :named (item-ids source) :reply-named (item-ids reply) :mission-id (:mission_id record)
              :pin (vec (concat [[(str "turn/" id) (digest/canonical-digest record)]
                                 [(str "evidence/" (:evidence_id record) "/body/text")
                                  (get-in source [:reference :revision/digest])]]
                                (when (:reference reply)
                                  [[(str "turn/" id "/reply_text") (get-in reply [:reference :revision/digest])]])))}})))

(defn operator-focus
  ([opts] (operator-focus (hou-store/default-config) opts))
  ([cfg {:keys [until window-hours] :or {window-hours 6} :as opts}]
   (let [items (with-closures (with-successors (inventory/current-items cfg)) until window-hours)
         ;; This read-only report recognizes the current census, including IDs
         ;; not yet published as clusions. It writes no dangling references.
         cfg (assoc cfg :item-ids (mapv :id items))
         by-clusion (into {} (map #(vector (:id (inventory/item-record (:id %))) (:id %))) items)
         records (filter #(and (= "operator" (get-in % [:record :origin]))
                               (in-window? until window-hours (get-in % [:record :created_at])))
                         (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE))
         read (mapv #(read-turn cfg by-clusion %) records)
         nd (try {:data ((or (:read-needs cfg) hou-store/read-needs) cfg)}
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
