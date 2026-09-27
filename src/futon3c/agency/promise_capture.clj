(ns futon3c.agency.promise-capture
  "Lossless map edits from authoritative snapshots. Vectors (including FIFO order),
   sets, arbitrary payloads, nils, and empty map entries are preserved verbatim.
   :just-released is excluded because parked-on/snapshot and persist! exclude it:
   it is only an in-memory handoff between completion's two swaps."
  (:require [clojure.set :as set]))

(def snapshot-keys
  {:parked #{:records :index :coalesced :ready-inbox :leased}
   :followup #{:queued :leased :terminal :dedupe}})
(def exclusions {:parked {:just-released "Transient handoff, excluded by snapshot and persist!"}})
(def ^:dynamic *capture* nil)

(defn clean [store state]
  (when state (apply dissoc state (keys (get exclusions store)))))

(defn edits
  "Exact reversible edits; absence is distinct from nil. No application fields
   are selected away. Every nested field is carried by an edit or unchanged."
  ([before after] (edits [] before after))
  ([path before after]
   (cond
     (= before after) []
     (and (map? before) (map? after))
     (vec (mapcat
           (fn [k]
             (let [p (conj path k)]
               (cond
                 (not (contains? after k)) [{:path p :op :remove :before (get before k)}]
                 (not (contains? before k)) [{:path p :op :put :value (get after k) :absent? true}]
                 :else (edits p (get before k) (get after k)))))
           (sort-by pr-str (set/union (set (keys before)) (set (keys after))))))
     :else [{:path path :op :put :before before :value after}])))

(defn apply-edits [state changes]
  (reduce (fn [s {:keys [path op value]}]
            (if (= op :remove)
              (if (= 1 (count path)) (dissoc s (peek path))
                  (update-in s (pop path) dissoc (peek path)))
              (if (empty? path) value (assoc-in s path value)))) state changes))

(defn coverage-issues
  "An added snapshot root requires explicit review. Nested fields are covered by
   exact edits; compare replay to snapshot to catch any omitted nested value."
  [store snapshot replayed]
  (vec (concat
        (for [k (remove (snapshot-keys store) (keys snapshot))]
          {:reason :uncovered-snapshot-key :path [k]})
        (for [edit (edits replayed snapshot)]
          {:reason :snapshot-not-carried :path (:path edit)}))))

(defn changed! [store before after]
  (let [changes (edits (clean store before) (clean store after))]
    (when (and (seq changes) (= store (:store *capture*)))
      (swap! (:changes *capture*) conj
             {:store store :change-id (str (random-uuid)) :edits changes}))))

(defn swap-state! [store state f]
  (let [[old new] (swap-vals! state f)]
    (changed! store old new)
    new))

(defn swap-vals-state! [store state f]
  (let [[old new :as pair] (swap-vals! state f)]
    (changed! store old new)
    pair))

(defn reset-state! [store state value]
  (let [[old new] (reset-vals! state value)]
    (changed! store old new)
    new))

(defn drain! []
  (if *capture* (first (reset-vals! (:changes *capture*) [])) []))
