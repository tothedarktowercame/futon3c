(ns futon3c.arxana.formal-f1-projection
  "F1c registered projections. Every source read is made through the supplied
  public operation function; this namespace has no store dependency."
  (:require [clojure.edn :as edn]))

(def rules
  {:formal-f1/linear
   {:rule/id :formal-f1/linear :rule/version 1
    :drops [:graph-adjacency :resolver-diagnostics :presentation-layout]}
   :formal-f1/outline
   {:rule/id :formal-f1/outline :rule/version 1
    :drops [:source-prose-layout :executable-action :presentation-layout]}
   :formal-f1/gcc
   {:rule/id :formal-f1/gcc :rule/version 1
    :drops [:non-action-prose :execution-authority :admission :presentation-layout]}})

(defn- codepoint-substring [^String s start end]
  (.substring s (.offsetByCodePoints s 0 start) (.offsetByCodePoints s 0 end)))

(defn- ok! [response]
  (or (:ok response)
      (throw (ex-info "public Arxana read failed" {:response response}))))

(defn- get-record [read! id]
  (:record (ok! (read! :entity/get {:entity/id id}))))

(defn- graph [read! body]
  (ok! (read! :graph/query body)))

(defn- reverse-ids [read! type role id]
  (:result/edge-ids
   (graph read! {:query/type :role-reverse :edge/type type
                 :endpoint/role role :endpoint/id id
                 :time/order :asc :query/limit 200})))

(defn- attestations [read! id]
  (mapv #(get-record read! %)
        (:result/attestation-ids
         (graph read! {:query/type :attestations :subject/id id :query/limit 200}))))

(defn- assessments [read! id]
  (:result/assessments
   (graph read! {:query/type :assessments :subject/id id :query/limit 200})))

(defn- related [read! id]
  (let [resolution-ids (reverse-ids read! [:resolution-observation 1] :subject id)
        predecessor-ids (reverse-ids read! [:supersedes 1] :predecessor id)
        successor-ids (reverse-ids read! [:supersedes 1] :successor id)]
    {:attestations (attestations read! id)
     :assessments (assessments read! id)
     :resolutions (mapv #(get-record read! %) resolution-ids)
     :supersedes (mapv #(get-record read! %)
                       (distinct (concat predecessor-ids successor-ids)))}))

(defn- standing [related]
  {:resolution (mapv #(get-in % [:asserted-content :resolution/state])
                     (:resolutions related))
   :assessment (mapv #(select-keys % [:id :verdict :actor :policy :reason])
                     (:assessments related))
   :evidence-qualification
   (mapv #(select-keys % [:id :producer :basis :standing :coverage :method])
         (:attestations related))})

(defn manifest [read! revision-id]
  (let [value (ok! (read! :revision/get {:artifact/revision-id revision-id}))]
    ;; The v1 canonical serializer writes keywords as equally named EDN
    ;; symbols. Restore their data-model form at this public projection seam.
    (letfn [(restore [x]
              (cond
                (symbol? x) (keyword (namespace x) (name x))
                (map? x) (into {} (map (fn [[k v]] [(restore k) (restore v)]) x))
                (vector? x) (mapv restore x)
                (sequential? x) (mapv restore x)
                :else x))]
      (restore (edn/read-string (:revision/text value))))))

(defn linear
  [read! manifest]
  (let [rule (:formal-f1/linear rules)
        source-text (:revision/text
                     (ok! (read! :revision/get
                                 {:artifact/revision-id
                                  (:artifact/revision-id manifest)})))]
    (assoc rule
           :projection/fragments
           (mapv (fn [turn]
                   (let [interp-ids (mapv :clusion/id (:interpretations turn))
                         mark-ids (mapv :mark-occurrence/id (:marks turn))]
                     ;; Resolve every emitted id through the public port.
                     (doseq [id (concat interp-ids mark-ids)] (get-record read! id))
                     {:fragment/id (str "linear:" (:turn/id turn))
                      :record/ids (vec (concat mark-ids interp-ids))
                      :turn/id (:turn/id turn)
                      :source/revision-id (:artifact/revision-id manifest)
                      :range (:range turn)
                      :text (codepoint-substring source-text
                                                 (first (:range turn))
                                                 (second (:range turn)))
                      :mark-occurrence/ids mark-ids
                      :interpretation/ids interp-ids}))
                 (:turns manifest)))))

(defn outline
  [read! manifest]
  (let [rule (:formal-f1/outline rules)]
    (assoc rule
           :projection/fragments
           (mapv
            (fn [[turn entry]]
              (let [id (:clusion/id entry)
                    claim (get-record read! id)
                    rel (related read! id)
                    related-ids (mapv :id (concat (:attestations rel)
                                                  (:assessments rel)
                                                  (:resolutions rel)
                                                  (:supersedes rel)))]
                {:fragment/id (str "outline:" id)
                 :record/ids (into [id] related-ids)
                 :turn/id (:turn/id turn)
                 :interpretation/id id
                 :intent (get-in claim [:endpoints 1 :target :intent/id])
                 :target (get-in claim [:endpoints 2 :target])
                 :standing (standing rel)
                 :relations (select-keys rel [:resolutions :supersedes])}))
            (for [turn (:turns manifest) entry (:interpretations turn)] [turn entry])))))

(defn gcc
  [read! manifest limit]
  (let [rule (:formal-f1/gcc rules)
        all (mapv
             (fn [[turn entry]]
               (let [id (:clusion/id entry) claim (get-record read! id)
                     rel (related read! id)]
                 {:fragment/id (str "gcc:" id)
                  :record/ids (into [id] (map :id (concat (:attestations rel)
                                                          (:assessments rel)
                                                          (:resolutions rel)
                                                          (:supersedes rel))))
                  :turn/id (:turn/id turn)
                  :interpretation/id id
                  :intent (get-in claim [:endpoints 1 :target :intent/id])
                  :target (get-in claim [:endpoints 2 :target])
                  :standing (standing rel)
                  :candidate/non-executable? true}))
             (for [turn (:turns manifest) entry (:interpretations turn)] [turn entry]))
        n (count all) limit (long (min 200 (max 1 limit)))]
    (assoc rule
           :result/status (if (> n limit) :truncated :complete)
           :result/truncation (when (> n limit) :limit)
           :result/total n
           :projection/fragments (vec (take limit all)))))

(defn project [read! operation {:keys [manifest/revision-id query/limit]}]
  (let [m (manifest read! revision-id)]
    (case operation
      :formal-f1/linear (linear read! m)
      :formal-f1/outline (outline read! m)
      :formal-f1/gcc (gcc read! m (or limit 50)))))
