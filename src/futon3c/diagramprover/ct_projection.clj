(ns futon3c.diagramprover.ct-projection
  "Project a diagramprover wiring map (boxes with declared :reads/:writes)
  into futon5's ct/mission diagram EDN (futon5 src/futon5/ct/mission.clj at
  aeeee96, docstring :22-31), so that module's validators can be asked of it,
  and ask invariant I4 (exogeneity) of the map directly.

  Pure: takes the map as data, returns data. Does not depend on futon5 (not
  on futon3c's :test classpath); the caller supplies the validators.

  Projection (re-derived from the map on every call):
  - every box of :box/kind :component is a component; a :not-built box keeps
    {:status :not-built} and its :intended-site; :timescale from its site file
    (`timescale-of`), absent when the box has no site;
  - every :box/kind :test box is an OUTPUT port (the registered test is the
    consumer; :spec-ref is its file);
  - a component that reads no declared field takes its input from outside
    the map: an INPUT port :world/<box> with one edge into the box;
  - a field read and written by no box is an INPUT port :in/<field>; a field
    written and read by no box is an OUTPUT port :out/<field>;
  - each (writer, field, reader) is an edge {:from writer :to reader :field f}.
    Fields carry no types, so edges carry no :type;
  - a field in :preference-fields is an INPUT port :pref/<field> with
    :constraint true and :timescale :glacial (the owner's text changes
    slowest); its writers' edges go INTO the port and its readers' edges
    come out of it, so I3 sees any box that writes a preference.")

(def default-preference-fields
  "The C side of the flight's map: the want spans the read step places in the
  mission text, and the text's sha. :next-step and :eligible are support
  (Clause T: feasibility is support, not a term of G), not preferences."
  #{:want-span :text-sha256})

(def timescale-rules
  "Site file -> ct/mission timescale. :fast = inside one click (the tick);
  :medium = the flight around clicks; :slow = the outer loop that picks the
  next target. Matched on the path's file name."
  [[#"war_machine\.clj$|/efe\.clj$|/policy\.clj$|/observation_rates\.clj$|/construction\.clj$" :fast]
   [#"flight_runner\.clj$|flight_driver\.clj$|grain_gate\.clj$|enactment_habit\.clj$|served_by_reading\.clj$|extract-outcomes\.clj$|want_interpretation\.clj$|clock_store\.clj$" :medium]
   [#"target_field\.clj$|outer_cascade\.clj$|wm_scheduled_run\.clj$" :slow]])

(defn timescale-of [box]
  (when-let [file (:file (or (:site box) (:intended-site box)))]
    (some (fn [[re ts]] (when (re-find re file) ts)) timescale-rules)))

(defn- field-index [boxes k]
  (reduce (fn [m b] (reduce #(update %1 %2 (fnil conj []) (:box/id b)) m (get b k)))
          {} boxes))

(defn project
  "MAP (wiring-map EDN) -> ct/mission diagram EDN."
  ([m] (project m {}))
  ([{:keys [spec/id repos boxes]} {:keys [preference-fields map-sha]
                                   :or {preference-fields default-preference-fields}}]
   (let [writers (field-index boxes :writes)
         readers (field-index boxes :reads)
         kind (into {} (map (juxt :box/id :box/kind)) boxes)
         node (fn [box-id] (if (= :test (kind box-id)) (keyword "test" (name box-id)) box-id))
         fields (sort-by str (distinct (concat (keys writers) (keys readers))))
         pref? (set preference-fields)
         fnode (fn [f] (if (pref? f) (keyword "pref" (name f)) nil))
         sources (for [b boxes :when (and (= :component (:box/kind b)) (empty? (:reads b)))]
                   (:box/id b))
         inputs (concat
                 (for [f fields :when (pref? f)]
                   {:id (fnode f) :name (name f) :field f :constraint true :timescale :glacial
                    :source "preference: the owner's stated wants"})
                 (for [f fields :when (and (not (pref? f)) (empty? (writers f)))]
                   {:id (keyword "in" (name f)) :name (name f) :field f
                    :source "read by the map, written by no box"})
                 (for [b sources]
                   {:id (keyword "world" (name b)) :name (name b)
                    :source "outside the map: the box reads no declared field"}))
         outputs (concat
                  (for [b boxes :when (= :test (:box/kind b))]
                    {:id (node (:box/id b)) :name (name (:box/id b))
                     :consumer "registered test" :spec-ref (:file (:site b))})
                  (for [f fields :when (and (not (pref? f)) (empty? (readers f)))]
                    {:id (keyword "out" (name f)) :name (name f) :field f
                     :consumer "written by the map, read by no box"}))
         components (for [b boxes :when (= :component (:box/kind b))]
                      (cond-> {:id (:box/id b) :name (name (:box/id b))
                               :inputs (vec (:reads b)) :outputs (vec (:writes b))}
                        (:row b) (assoc :row (:row b))
                        (:site b) (assoc :site (:site b))
                        (:status b) (assoc :status (:status b) :intended-site (:intended-site b))
                        (timescale-of b) (assoc :timescale (timescale-of b))))
         edges (concat
                (for [f fields :when (not (pref? f)), w (writers f), r (readers f)]
                  {:from (node w) :to (node r) :field f})
                (for [f fields :when (pref? f), w (writers f)]
                  {:from (node w) :to (fnode f) :field f})
                (for [f fields :when (pref? f), r (readers f)]
                  {:from (fnode f) :to (node r) :field f})
                (for [f fields :when (and (not (pref? f)) (empty? (writers f))), r (readers f)]
                  {:from (keyword "in" (name f)) :to (node r) :field f})
                (for [f fields :when (and (not (pref? f)) (empty? (readers f))), w (writers f)]
                  {:from (node w) :to (keyword "out" (name f)) :field f})
                (for [b sources]
                  {:from (keyword "world" (name b)) :to b :field :world}))]
     {:mission/id id
      :mission/state :projected
      :projection/source {:map-spec id :map-sha map-sha :repos repos
                          :preference-fields (vec (sort-by str preference-fields))}
      :ports {:input (vec inputs) :output (vec outputs)}
      :components (vec components)
      :edges (vec (sort-by (juxt (comp str :field) (comp str :from) (comp str :to)) edges))})))

;; ---------------------------------------------------------------------------
;; I4 asked of the map: paths from the machine's own boxes to preference
;; fields, or to the facts the outer cascade scores targets on, that pass
;; through no observation box.

(def default-observation-boxes
  "Boxes whose output is a reading of the world rather than the machine's own
  state: the read step (served-by reading of the mission text, read-fn), the
  rate measurement, the publication observation, the grain gate (reads the
  built code), and the W_c checker (checks attempts against their evidence)."
  #{:r2-served-by-reading :r2-flight-read :r6-sourced-rates
    :r10-observe-publication :r5-grain-gate :wc-checker})

(defn- upstream-graph
  "box -> #{[upstream-box via]}: the writers of each field the box reads, and
  the predecessor of each positional trace hop into it."
  [boxes positional-hops]
  (let [writers (field-index boxes :writes)]
    (reduce (fn [g b]
              (reduce (fn [g f] (reduce #(update %1 (:box/id b) (fnil conj #{}) [%2 f])
                                        g (writers f)))
                      g (:reads b)))
            (reduce (fn [g [from to]] (update g to (fnil conj #{}) [from :positional]))
                    {} positional-hops)
            boxes)))

(defn- positional-hops
  "Consecutive box pairs of each :traces entry whose second hop is :positional."
  [traces]
  (vec (for [{bs :boxes} traces
             [a b] (partition 2 1 bs)
             :when (and (map? b) (= :positional (:hop b)))]
         [(if (map? a) (:box a) a) (:box b)])))

(defn bypass-paths
  "For each TARGET field, every simple path [source … writer-of-field] of
  length ≥ 2 that passes through no observation box, found by walking
  upstream from the field's writers. :via names the edge between each
  consecutive pair (a field, or :positional). An observation box ends the
  walk (a path through it is not a bypass); an observation box that writes
  the field itself yields no path. Positional trace hops are edges only with
  {:positional? true}."
  [{:keys [boxes traces]} target-fields
   {:keys [observation-boxes positional?]
    :or {observation-boxes default-observation-boxes}}]
  (let [g (upstream-graph boxes (when positional? (positional-hops traces)))
        writers (field-index boxes :writes)
        paths (volatile! [])]
    (letfn [(walk [box path via seen]
              (when-not (observation-boxes box)
                (let [path (cons box path)]
                  (vswap! paths conj {:path (vec path) :via (vec via)})
                  (doseq [[up f] (sort-by str (get g box))
                          :when (not (seen up))]
                    (walk up path (cons f via) (conj seen up))))))]
      (->> (for [f target-fields
                 w (writers f)
                 :let [_ (vreset! paths [])
                       _ (walk w () () #{w})]
                 p @paths
                 :when (< 1 (count (:path p)))]
             (assoc p :to f))
           (sort-by (juxt (comp str :to) (comp str :path)))
           vec))))

(defn i4-report
  "I4 asked of the map: preference fields and the outer cascade's scored facts,
  each with its writers, whether a writer is an observation box, and the
  bypass paths into it (`bypass-paths`, field edges only and with positional
  trace hops). SCORED-BY names the box whose :reads are the scored facts."
  [m {:keys [preference-fields scored-by observation-boxes]
      :or {preference-fields default-preference-fields
           scored-by :r1-outer-cascade
           observation-boxes default-observation-boxes}}]
  (let [writers (field-index (:boxes m) :writes)
        scored (vec (:reads (first (filter #(= scored-by (:box/id %)) (:boxes m)))))
        describe (fn [fs] (vec (for [f (sort-by str fs)]
                                 {:field f :writers (vec (sort-by str (writers f)))
                                  :observed-by (vec (sort-by str (filter observation-boxes (writers f))))})))
        opts {:observation-boxes observation-boxes}]
    {:preference-fields (describe preference-fields)
     :scored-facts {:read-by scored-by :fields (describe scored)}
     :observation-boxes (vec (sort-by str observation-boxes))
     :bypass {:to-preferences (bypass-paths m preference-fields opts)
              :to-preferences-positional (bypass-paths m preference-fields (assoc opts :positional? true))
              :to-scored-facts (bypass-paths m scored opts)
              :to-scored-facts-positional (bypass-paths m scored (assoc opts :positional? true))}}))
