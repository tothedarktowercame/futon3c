(ns futon3c.circus.hou
  "Deterministic focus, with separate association radius and measured Reach bound.
  Snapshot records are turn-field references, exact mention clusions and checked
  needs reading records. Item-to-reading-to-item costs two Reach hops; mentions
  share a turnText node only when their evidence ref IDs agree (four hops).
  Monkey.lean:76-110 supplies guard order and permits a successful empty
  relations vector when evidence and location are valid. Failure never means
  successful empty focus. No model call, store access or time source here."
  (:require [clojure.set :as set]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.turn-mentions :as mentions]))

(defn item-record [id]
  (records/make-clusion {:type-revision [:workplan/item 1] :endpoints []
                        :asserted-content {:item/id id}}))

(defn- connect [g a b]
  (-> g (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))

(defn distances [g start]
  (loop [frontier #{start} seen {start 0} depth 0]
    (if (empty? frontier) seen
        (let [nexts (set/difference (set (mapcat #(get g % #{}) frontier)) (set (keys seen)))]
          (recur nexts (into seen (map #(vector % (inc depth)) nexts)) (inc depth))))))

(defn focus-rule
  "Only rule selection lives here. :named-items is the default. The optional
  :mission-and-named includes mission_id and makes reply names associated.
  Neighbours follow already-normalized needs in BOTH directions, radius 1
  by default; associated excludes primary focus. Inventory validation is later."
  [rule {:keys [operator-items reply-items mission-id needs associated-radius]
         :or {associated-radius 1}}]
  (when-not (#{:named-items :mission-and-named} rule)
    (throw (ex-info "unknown focus rule" {:rule rule})))
  (let [focus (cond-> (set operator-items)
                (and (= rule :mission-and-named) (seq mission-id)) (conj mission-id))
        graph (reduce (fn [g [a b]] (connect g a b)) {} needs)
        near (set (mapcat (fn [x] (keep (fn [[y d]] (when (<= d associated-radius) y))
                                        (distances graph x))) focus))
        associated (set/difference
                    (into near (when (= rule :mission-and-named) reply-items)) focus)]
    (vec (sort-by first (concat (map #(vector % :focus) focus)
                               (map #(vector % :associated) associated))))))

(defn- mentioned-item [by-clusion m]
  (get by-clusion (get-in m [:endpoints 0 :target :clusion/id])))

(defn- evidence-valid? [{:keys [references mention-records readings basis-records stored text-by-ref source-ref]}]
  (let [rs (concat references mention-records (map :record readings) basis-records)
        ids (map :id rs)]
    (and (= (count ids) (count (distinct ids)))
         (some #(= source-ref (:id %)) references)
         (every? #(and (string? (:id %)) (= % (get stored (:id %)))) rs)
         (every? #(= (:revision/digest %)
                     (some-> (get text-by-ref (:id %)) (.getBytes "UTF-8") digest/sha256-hex)) references)
         (every? #(and (contains? (set (map :id references)) (get-in % [:endpoints 1 :target :ref/id]))
                       (mentions/mention-valid?
                        (get text-by-ref (get-in % [:endpoints 1 :target :ref/id])) %)) mention-records))))

(defn relation-links [header]
  (mapv (fn [[id relation]]
          (records/make-clusion
           {:type-revision [:hou/focus-relation 1]
            :endpoints [{:role :focus :kind :clusion :target {:clusion/id (:id header)}}
                        {:role :item :kind :clusion :target {:clusion/id (:id (item-record id))}}]
            :asserted-content {:relation relation}}))
        (get-in header [:asserted-content :hou/relations])))

(defn hou-run
  "Input contains the exact audited snapshot and independently read :stored
  records. :readings contains {:record {:id ... ...} :needs [[a b] ...]}.
  No record is silently dropped from the pin. A missing field reference (the
  mention writer does not emit refs for fields with no mentions) is explicitly
  invalidEvidence, not a made-up successful empty focus. Valid empty inputs
  yield a written header with empty relations, as Monkey.monkeyRun allows."
  [{:keys [settled? reply-landed? items references mention-records readings source-ref
           turn-id at rule associated-radius sources basis-records] :or {rule :named-items associated-radius 1}
    :as input}]
  (cond
    (not settled?) {:failed :turnNotSettled}
    (not reply-landed?) {:failed :replyNotLanded}
    (not (evidence-valid? input)) {:failed :invalidEvidence}
    :else
    (let [by-clusion (into {} (map (fn [id] [(:id (item-record id)) id]) items))
          item-of #(mentioned-item by-clusion %)
          operator-items (map item-of (filter #(= source-ref (get-in % [:endpoints 1 :target :ref/id])) mention-records))
          reply-items (map item-of (remove #(= source-ref (get-in % [:endpoints 1 :target :ref/id])) mention-records))
          needs (mapcat :needs readings)
          relations (focus-rule rule {:operator-items operator-items :reply-items reply-items
                                     :mission-id (:mission-id input) :needs needs
                                     :associated-radius associated-radius})
          touched (set (map item-of mention-records))
          primary (set (map first (filter #(= :focus (second %)) relations)))
          measured (set/union touched primary)
          graph (reduce (fn [g m]
                          (-> g
                              (connect [:item (item-of m)] [:record (:id m)])
                              (connect [:record (:id m)] [:record (get-in m [:endpoints 1 :target :ref/id])])))
                        {} mention-records)
          graph (reduce (fn [g {:keys [record needs incident-items]}]
                          (reduce #(connect %1 [:record (:id record)] [:item %2]) g
                                  (filter (set items) (distinct (or incident-items (mapcat identity needs)))))) graph readings)
          ds (map #(distances graph [:item %]) measured)
          located? (and (nat-int? associated-radius) (not (contains? touched nil))
                        (every? (set items) (map first relations))
                        (or (empty? primary) (seq touched))
                        (every? (fn [d] (every? #(contains? d [:item %]) measured)) ds)
                        (every? (fn [{:keys [record]}] (seq (get graph [:record (:id record)]))) readings))
          bound (reduce max 0 (for [d ds x measured] (get d [:item x] 0)))]
      (if-not located? {:failed :focusNotLocated}
        (let [read-records (concat references mention-records (map :record readings) basis-records)
              pin (vec (sort-by first (map #(vector (:id %) (digest/canonical-digest %)) read-records)))
              ref (first (filter #(= source-ref (:id %)) references))
              header (records/make-clusion
                      {:type-revision [:hou/focus 1]
                       :endpoints [{:role :evidence :kind :external-ref
                                    :target {:ref/id source-ref :revision/digest (:revision/digest ref)}}]
                       :asserted-content {:hou/turn turn-id :hou/at at :hou/rule rule
                                          :hou/relations relations :hou/associated-radius associated-radius
                                          :hou/hop-bound bound :hou/evidence-pin pin :hou/sources (vec sources)}})
              links (relation-links header)]
          (if (> (+ 2 (count links)) 500)
            {:failed :windowTooLargeAfterSplit}
            {:written header :links links}))))))

(defn header-links-agree?
  "The caller supplies the COMPLETE reverse-link query, never a truncated page."
  [header links]
  (try
    (let [rs (get-in header [:asserted-content :hou/relations])
          expected (mapv (fn [[id rel]]
                           [{:focus (:id header) :item (:id (item-record id))} rel]) rs)
          actual (mapv (fn [l]
                         [{:focus (get-in l [:endpoints 0 :target :clusion/id])
                           :item (get-in l [:endpoints 1 :target :clusion/id])}
                          (get-in l [:asserted-content :relation])]) links)]
      (and (= [:hou/focus 1] (:type-revision header))
           (= (:id header) (records/clusion-id header))
           (= (count rs) (count (distinct (map first rs))))
           (every? #(#{:focus :associated :useful-elsewhere} (second %)) rs)
           (every? #(and (= [:hou/focus-relation 1] (:type-revision %))
                         (= (:id %) (records/clusion-id %))) links)
           (= (frequencies expected) (frequencies actual))))
    (catch Exception _ false)))
