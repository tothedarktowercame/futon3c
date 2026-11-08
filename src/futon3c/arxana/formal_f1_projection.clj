(ns futon3c.arxana.formal-f1-projection
  "F1c registered projections. Every source read is made through the supplied
  public operation function; this namespace has no store dependency."
  (:require [clojure.edn :as edn]))

(def rules
  {:formal-f1/linear
   ;; Version 2 (claude-13, 2026-11-08): each fragment lists its readings
   ;; with their range inside the fragment text, intent and verdicts, so a
   ;; client can mark the reading where it sits instead of the whole turn.
   {:rule/id :formal-f1/linear :rule/version 2
    :drops [:graph-adjacency :resolver-diagnostics :presentation-layout]}
   :formal-f1/outline
   {:rule/id :formal-f1/outline :rule/version 1
    :drops [:source-prose-layout :executable-action :presentation-layout]}
   :formal-f1/gcc
   {:rule/id :formal-f1/gcc :rule/version 2
    ;; DERIVE-F1 §2 item 1: the GCC view shows the action-bearing fragments.
    ;; intent-vocabulary/v1 has no action field, so the rule declares which
    ;; intents direct or redirect work (claude-13, F1c-1 review, 2026-11-08).
    ;; Readings with other intents (propose, approve, qualify, ...) are dropped.
    ;; A vector: canonical Arxana payloads refuse sets.
    :action-intents [:ask-action :constrain :continue :defer :delegate
                     :prioritize :redirect :withdraw]
    :drops [:non-action-interpretations :non-action-prose :execution-authority
            :admission :presentation-layout]}})

(defn- codepoint-substring [^String s start end]
  (.substring s (.offsetByCodePoints s 0 start) (.offsetByCodePoints s 0 end)))

(defn- ok! [response]
  (or (:ok response)
      (throw (ex-info "public Arxana read failed" {:response response}))))

(defn- get-record [read! id]
  (:record (ok! (read! :entity/get {:entity/id id}))))

(defn- graph [read! body]
  (ok! (read! :graph/query body)))

(defn- related [read! id]
  ;; Each bounded read can come back :truncated; a view that drops records
  ;; must say so (Lean `Read`: truncation is visible), so keep the statuses.
  (let [q (fn [body] (graph read! (assoc body :query/limit 200)))
        reverse-q (fn [type role]
                    (q {:query/type :role-reverse :edge/type type :endpoint/role role
                        :endpoint/id id :time/order :asc}))
        att (q {:query/type :attestations :subject/id id})
        ass (q {:query/type :assessments :subject/id id})
        res (reverse-q [:resolution-observation 1] :subject)
        pre (reverse-q [:supersedes 1] :predecessor)
        suc (reverse-q [:supersedes 1] :successor)
        truncated (vec (keep (fn [[k r]] (when (= :truncated (:result/status r)) k))
                             [[:attestations att] [:assessments ass] [:resolutions res]
                              [:supersedes pre] [:supersedes suc]]))]
    {:attestations (mapv #(get-record read! %) (:result/attestation-ids att))
     :assessments (:result/assessments ass)
     :resolutions (mapv #(get-record read! %) (:result/edge-ids res))
     :supersedes (mapv #(get-record read! %)
                       (distinct (concat (:result/edge-ids pre) (:result/edge-ids suc))))
     :truncated (vec (distinct truncated))}))

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
                     {:readings
                      (mapv (fn [{:keys [clusion/id range]}]
                              (let [record (get-record read! id)
                                    [turn-start] (:range turn)]
                                {:interpretation/id id
                                 :range [(- (first range) turn-start)
                                         (- (second range) turn-start)]
                                 :intent (some #(when (= :intent (:role %))
                                                  (get-in % [:target :intent/id]))
                                               (:endpoints record))
                                 :verdicts (mapv :verdict
                                                 (:result/assessments
                                                  (graph read! {:query/type :assessments
                                                                :subject/id id
                                                                :query/limit 200})))}))
                            (:interpretations turn))
                      :fragment/id (str "linear:" (:turn/id turn))
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
                 :relations (select-keys rel [:resolutions :supersedes])
                 :related/truncated (:truncated rel)}))
            (for [turn (:turns manifest) entry (:interpretations turn)] [turn entry])))))

(defn gcc
  [read! manifest limit]
  (let [rule (:formal-f1/gcc rules)
        claims (for [turn (:turns manifest) entry (:interpretations turn)]
                 [turn entry (get-record read! (:clusion/id entry))])
        actions (filter (fn [[_ _ claim]]
                          (some #{(get-in claim [:endpoints 1 :target :intent/id])}
                                (:action-intents rule)))
                        claims)
        all (mapv
             (fn [[turn entry claim]]
               (let [id (:clusion/id entry)
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
                  :related/truncated (:truncated rel)
                  :candidate/non-executable? true}))
             actions)
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
(def rules-gcc (:formal-f1/gcc rules))
