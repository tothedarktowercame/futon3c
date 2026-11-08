(ns futon3c.arxana.formal-f3-projection
  "F3d store-backed projections. All source records enter through read!."
  (:require [clojure.edn :as edn]))

(def rules
  {:formal-f3/correspondence
   {:rule/id :formal-f3/correspondence :rule/version 2
    :drops [:source-prose-layout :lean-source-ranges :presentation-layout]}
   :formal-f3/manifestations
   {:rule/id :formal-f3/manifestations :rule/version 2
    :drops [:artifact-bytes :correspondence-adjacency :presentation-layout]}})

(defn- ok! [r] (or (:ok r) (throw (ex-info "public F3 read failed" {:response r}))))
(defn- record [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- query [read! body] (ok! (read! :graph/query (assoc body :query/limit 200))))
(defn- related [read! id]
  (let [a (query read! {:query/type :attestations :subject/id id})
        s (query read! {:query/type :assessments :subject/id id})
        r (query read! {:query/type :role-reverse :edge/type [:resolution-observation 1]
                        :endpoint/role :subject :endpoint/id id :time/order :asc})]
    {:attestations (mapv #(record read! %) (:result/attestation-ids a))
     :assessments (:result/assessments s)
     :resolutions (mapv #(record read! %) (:result/edge-ids r))
     :truncated (vec (keep (fn [[k x]] (when (= :truncated (:result/status x)) k))
                           [[:attestations a] [:assessments s] [:resolutions r]]))}))

;; The SYNTHETIC boundary is read from the stored records, never from the
;; caller's manifest: a manifest that forgot the label must not unlabel it.
(defn- synthetic-claim? [claim]
  (= :SYNTHETIC-negative (get-in claim [:asserted-content :fixture/label])))

(defn- synthetic-media? [media-type]
  (boolean (some->> media-type (re-find #"fixture=SYNTHETIC-negative"))))
(defn- restore [x]
  (cond (symbol? x) (keyword (namespace x) (name x))
        (map? x) (into {} (map (fn [[k v]] [(restore k) (restore v)]) x))
        (vector? x) (mapv restore x) (sequential? x) (mapv restore x) :else x))
(defn- manifest [read! rid]
  (restore (edn/read-string (:revision/text
                             (ok! (read! :revision/get {:artifact/revision-id rid}))))))

(defn correspondence [read! m]
  (assoc (:formal-f3/correspondence rules)
         :result/status :complete
         :projection/fragments
         (mapv (fn [{class :class :as entry}]
                 (let [id (:record/id entry) step-id (:step/id entry)
                       claim (record read! id) rel (related read! id)
                       ids (into [id] (map :id (concat (:attestations rel)
                                                      (:assessments rel)
                                                      (:resolutions rel))))
                       synthetic? (synthetic-claim? claim)
                       divergence? (= :methodological-divergence
                                      (get-in claim [:asserted-content :relation/kind]))]
                   {:fragment/id (str "f3:" id) :record/ids ids :correspondence/id id
                    :step/id step-id
                    :class (if synthetic? :synthetic class)
                    :resolution (vec (distinct (map #(get-in % [:asserted-content :resolution/state])
                                                    (:resolutions rel))))
                    :synthetic/label (when synthetic? :SYNTHETIC-negative)
                    :relation/kind (get-in claim [:asserted-content :relation/kind])
                    :target (get-in claim [:endpoints 1 :target])
                    :evidence (mapv #(select-keys % [:id :producer :basis :standing :coverage :method])
                                    (:attestations rel))
                    :assessments (mapv #(select-keys % [:id :actor :verdict :standing :policy
                                                        :kernel/attestation-id :fixture/label])
                                       (:assessments rel))
                    :divergence/contradicted? (boolean
                                               (and divergence?
                                                    (some #(= :contradicted (:standing %))
                                                          (:assessments rel))))
                    :related/truncated (:truncated rel)}))
               (:correspondences m))
         :coverage/fragments
         ;; Declarations with no informal counterpart (F3c formalization-coverage).
         (mapv (fn [id]
                 (let [c (record read! id)]
                   {:fragment/id (str "f3-coverage:" id) :record/ids [id]
                    :declaration/name (get-in c [:endpoints 0 :target :declaration/name])
                    :coverage/state (get-in c [:asserted-content :coverage/state])}))
               (:coverage m))))

(defn manifestations [read! m]
  (assoc (:formal-f3/manifestations rules)
         :result/status :complete
         :projection/fragments
         (mapv (fn [{role :role producer :producer :as entry}]
                 (let [id (:artifact/id entry) revision-id (:artifact/revision-id entry)
                       ids (:record/ids entry)
                       media (:media-type (ok! (read! :revision/get
                                                      {:artifact/revision-id revision-id})))
                       label (when (synthetic-media? media) :SYNTHETIC-negative)]
                 (doseq [rid ids] (record read! rid))
                 {:fragment/id (str "manifestation:" id) :record/ids ids
                  :artifact/id id :artifact/revision-id revision-id :role role
                  :producer producer :media-type media :synthetic/label label}))
               (:manifestations m))))

(defn project [read! op {:keys [manifest/revision-id]}]
  (let [m (manifest read! revision-id)]
    (case op
      :formal-f3/correspondence (correspondence read! m)
      :formal-f3/manifestations (manifestations read! m))))
