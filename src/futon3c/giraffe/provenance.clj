(ns futon3c.giraffe.provenance
  "Ticket lineage and current-relevance projection.  These edges explain why
  work exists; they never enter readiness or work-cluster connectivity."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.readings :as readings]))

(def ^:private closed-groups #{:completed :cancelled})
(def ^:private work-id-pattern #"[MET]-[A-Za-z0-9](?:[A-Za-z0-9._-]*[A-Za-z0-9_-])?")

(defn- ticket? [id] (str/starts-with? (name id) "T-"))

(defn source-parent-declaration
  "Extract a source-pinned `Parent:` declaration from ticket ITEM.  Source
  drift is an invariant failure, not an absent lineage edge."
  [item]
  (when (ticket? (:id item))
    (let [{:keys [path sha256]} (:source-pin item)
          file (when path (io/file path))]
      (when-not (and file (.isFile file))
        (throw (ex-info "Ticket source unavailable for provenance"
                        {:kind :provenance/source-unavailable :id (:id item)
                         :path path})))
      (let [text (slurp file)
            actual (readings/sha256 text)]
        (when-not (= sha256 actual)
          (throw (ex-info "Ticket source changed after population pin"
                          {:kind :provenance/source-pin-mismatch :id (:id item)
                           :path path :expected sha256 :actual actual})))
        (some (fn [[i line]]
                (when (str/starts-with? (str/trim line) "Parent:")
                  (when-let [target (re-find work-id-pattern line)]
                    {:from (name (:id item))
                     :to (str/replace target #"[.]md$" "")
                     :kind "part-of"
                     :basis "declared"
                     :source "ticket-source-parent"
                     :evidence {:path path :line (inc i) :quote (str/trim line)
                                :sha256 sha256}})))
              (map-indexed vector (str/split-lines text)))))))

(defn- lineage-link? [link]
  (and (contains? #{"part-of" "relates-to" "motivated-by"} (:kind link))
       (str/starts-with? (str (:from link)) "T-")))

(defn- normalize-link [link source]
  (-> (select-keys link [:from :to :kind :basis :evidence :note])
      (update :basis #(if (keyword? %) (name %) (str %)))
      (assoc :source source)))

(defn- merge-links [links]
  (->> links
       (group-by (juxt :from :to :kind))
       (map (fn [[[from to kind] attestations]]
              {:from from :to to :kind kind
               :attestations (vec (sort-by (juxt :source #(get-in % [:evidence :path])
                                                 #(get-in % [:evidence :line]))
                                           attestations))}))
       (sort-by (juxt :from :to :kind))
       vec))

(defn project
  "Project ticket lineage and relevance from a complete checked INPUT.

  Optional :relevance-witnesses is keyed by ticket id (keyword or string).
  A witness must carry :status :current|:obsolete|:superseded and
  :evidence-id. Without one, relevance is review-required even when the
  origin is still open."
  [{:keys [items reading-evidence relevance-witnesses]}]
  (let [by-id (into {} (map (juxt (comp name :id) identity)) items)
        checked (map #(normalize-link % "checked-reading")
                     (filter lineage-link? (:links reading-evidence)))
        declared (keep #(some-> (source-parent-declaration %)
                                (normalize-link "source-declaration"))
                       items)
        links (merge-links (concat checked declared))
        by-ticket (group-by :from links)
        tickets (filter (comp ticket? :id) items)
        rows
        (mapv
         (fn [ticket]
           (let [id (name (:id ticket))
                 lineage (vec (get by-ticket id []))
                 origins (filter #(contains? #{"part-of" "motivated-by"} (:kind %)) lineage)
                 origin-states (into (sorted-map)
                                     (for [{:keys [to]} origins]
                                       [to (or (some-> (get by-id to) :state-group name)
                                               "outside-population")]))
                 witness (or (get relevance-witnesses (:id ticket))
                             (get relevance-witnesses id))
                 witness-status (some-> (:status witness) name keyword)
                 valid-witness? (and (contains? #{:current :obsolete :superseded}
                                                witness-status)
                                     (not (str/blank? (str (:evidence-id witness)))))
                 closed-origin? (some #(contains? closed-groups
                                                   (:state-group (get by-id (:to %))))
                                      origins)
                 relevance (if valid-witness?
                             {:status witness-status :witness witness}
                             {:status :review-required
                              :reason (if closed-origin?
                                        :closed-origin-open-ticket
                                        :no-current-relevance-witness)})]
             {:id id
              :state (name (:state-group ticket))
              :lineage lineage
              :origin-states origin-states
              :relevance relevance}))
         (sort-by :id tickets))]
    {:summary {:tickets (count rows)
               :with-lineage (count (filter (comp seq :lineage) rows))
               :with-origin (count (filter (comp seq :origin-states) rows))
               :relevance-current (count (filter #(= :current (get-in % [:relevance :status])) rows))
               :relevance-review-required
               (count (filter #(= :review-required (get-in % [:relevance :status])) rows))}
     :tickets rows}))
