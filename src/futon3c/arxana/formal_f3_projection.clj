(ns futon3c.arxana.formal-f3-projection
  "F3d store-backed projections. All source records enter through read!."
  (:require [clojure.edn :as edn]))

(def rules
  {:formal-f3/correspondence
   {:rule/id :formal-f3/correspondence :rule/version 1
    :drops [:source-prose-layout :lean-source-ranges :presentation-layout]}
   :formal-f3/manifestations
   {:rule/id :formal-f3/manifestations :rule/version 1
    :drops [:artifact-bytes :correspondence-adjacency :presentation-layout]}})

(defn- ok! [r] (or (:ok r) (throw (ex-info "public F3 read failed" {:response r}))))
(defn- record [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- query [read! body] (ok! (read! :graph/query (assoc body :query/limit 200))))
(defn- related [read! id]
  (let [a (query read! {:query/type :attestations :subject/id id})
        s (query read! {:query/type :assessments :subject/id id})]
    {:attestations (mapv #(record read! %) (:result/attestation-ids a))
     :assessments (:result/assessments s)
     :truncated (vec (keep (fn [[k x]] (when (= :truncated (:result/status x)) k))
                           [[:attestations a] [:assessments s]]))}))
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
                                                      (:assessments rel))))
                       synthetic? (= class :synthetic)
                       divergence? (= :methodological-divergence
                                      (get-in claim [:asserted-content :relation/kind]))]
                   {:fragment/id (str "f3:" id) :record/ids ids :correspondence/id id
                    :step/id step-id :class class
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
               (:correspondences m))))

(defn manifestations [read! m]
  (assoc (:formal-f3/manifestations rules)
         :result/status :complete
         :projection/fragments
         (mapv (fn [{role :role producer :producer :as entry}]
                 (let [id (:artifact/id entry) revision-id (:artifact/revision-id entry)
                       ids (:record/ids entry) label (:synthetic/label entry)]
                 (doseq [rid ids] (record read! rid))
                 {:fragment/id (str "manifestation:" id) :record/ids ids
                  :artifact/id id :artifact/revision-id revision-id :role role
                  :producer producer :synthetic/label label}))
               (:manifestations m))))

(defn project [read! op {:keys [manifest/revision-id]}]
  (let [m (manifest read! revision-id)]
    (case op
      :formal-f3/correspondence (correspondence read! m)
      :formal-f3/manifestations (manifestations read! m))))
