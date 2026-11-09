(ns futon3c.arxana.codedocs-view
  "Deterministic, bounded code↔docs projection over public Arxana reads."
  (:require [clojure.string :as str]))

(def query-limit 100)

(defn- endpoint-id [record role]
  (let [target (:target (first (filter #(= role (:role %)) (:endpoints record))))]
    (or (:entity/id target) (:clusion/id target))))

(defn- first-sentence [doc]
  (when (seq doc)
    (let [without-markers (-> doc
                              (str/replace #"^/--\s*" "")
                              (str/replace #"\s*-/\s*$" ""))
          flat (-> without-markers
                   (str/replace #"[\s\n]+" " ") str/trim
                   (str/replace "`" ""))
          matcher (re-matcher #"\.(?:\s|$)" flat)]
      (if (.find matcher) (subs flat 0 (.end matcher)) flat))))

(defn- substring-codepoints [^String text [start end]]
  (.substring text (.offsetByCodePoints text 0 start)
              (.offsetByCodePoints text 0 end)))

(defn- read-record [read! id]
  (get-in (read! :entity/get {:entity/id id}) [:ok :record]))

(defn- revision-text [read! revision-id]
  (let [response (read! :revision/get {:artifact/revision-id revision-id})]
    (if-let [error (:error response)] {:error error}
      {:text (get-in response [:ok :revision/text])})))

(defn- query! [read! truncations body]
  (let [result (:ok (read! :graph/query (assoc body :query/limit query-limit)))]
    (when (= :truncated (:result/status result))
      (swap! truncations conj {:query (dissoc body :query/limit)
                               :reason (:result/truncation result)
                               :total (:result/total result)}))
    result))

(defn- attestations [read! truncations subject-id]
  (let [result (query! read! truncations
                       {:query/type :attestations :subject/id subject-id})]
    (->> (:result/attestation-ids result)
         (map #(read-record read! %))
         (remove nil?) (sort-by :id) vec)))

(defn- attestation-by-method [read! truncations subject-id method-id]
  (first (filter #(= method-id (get-in % [:method :id]))
                 (attestations read! truncations subject-id))))

(defn- correspondences [read! truncations role scope-id relation]
  (let [result (query! read! truncations
                       {:query/type :role-reverse :edge/type [:correspondence 1]
                        :endpoint/role role :endpoint/id scope-id
                        :time/order :asc})]
    (->> (:result/edge-ids result)
         (map #(read-record read! %))
         (filter #(= relation (get-in % [:asserted-content :relation/kind])))
         (sort-by :id) vec)))

(defn- doc-entry [read! truncations correspondence]
  (let [scope-id (endpoint-id correspondence :target)
        scope (read-record read! scope-id)
        range-att (attestation-by-method read! truncations scope-id
                                         "codedocs/def-doc-bullet")
        link-att (attestation-by-method read! truncations (:id correspondence)
                                        "codedocs/source-path+symbol")
        revision-id (get-in scope [:asserted-content :artifact/revision-id])
        text (:text (revision-text read! revision-id))
        range (get-in range-att [:content :codepoint-range])
        ids (vec (sort (keep identity [scope-id (:id range-att)
                                       (:id correspondence) (:id link-att)])))]
    {:scope/id scope-id
     :entry/id (get-in scope [:asserted-content :entry/id])
     :text (when (and text range) (substring-codepoints text range))
     :correspondence/id (:id correspondence)
     :attestation/id (:id link-att)
     :range/attestation-id (:id range-att)
     :record/ids ids}))

(defn- lean-entry [read! truncations correspondence]
  (let [scope-id (endpoint-id correspondence :source)
        scope (read-record read! scope-id)
        range-att (attestation-by-method read! truncations scope-id
                                         "codedocs/lean-declaration")
        link-att (attestation-by-method read! truncations (:id correspondence)
                                        "codedocs/lean-backtick-citation")
        revision-id (get-in scope [:asserted-content :artifact/revision-id])
        text (:text (revision-text read! revision-id))
        doc-range (get-in range-att [:content :doc-comment-range])
        doc (when (and text doc-range) (substring-codepoints text doc-range))
        ids (vec (sort (keep identity [scope-id (:id range-att)
                                       (:id correspondence) (:id link-att)])))]
    {:scope/id scope-id
     :declaration/fqn (get-in scope [:asserted-content :declaration/fqn])
     :doc/first-sentence (first-sentence doc)
     :citation/codepoint-range (get-in link-att [:content :token/codepoint-range])
     :correspondence/id (:id correspondence)
     :attestation/id (:id link-att)
     :range/attestation-id (:id range-att)
     :standing {:basis (:basis link-att) :standing (:standing link-att)}
     :record/ids ids}))

(defn- definition-entry [read! truncations membership scope]
  (let [scope-id (:id scope)
        range-att (attestation-by-method read! truncations scope-id
                                         "codedocs/clojure-definition")
        docs (mapv #(doc-entry read! truncations %)
                   (correspondences read! truncations :source scope-id :code-documents))
        lean (->> (correspondences read! truncations :target scope-id :cites-code)
                  (map #(lean-entry read! truncations %))
                  (sort-by (juxt :declaration/fqn :correspondence/id)) vec)
        ids (->> (concat [(:id membership) scope-id (:id range-att)]
                         (mapcat :record/ids docs) (mapcat :record/ids lean))
                 (remove nil?) distinct sort vec)]
    {:scope/id scope-id :symbol (get-in scope [:asserted-content :symbol/name])
     :form (get-in scope [:asserted-content :definition/form])
     :codepoint/range (get-in range-att [:content :codepoint-range])
     :docs docs :lean lean :record/ids ids}))

(defn view [read! body]
  (let [revision-id (:artifact/revision-id body)
        revision (revision-text read! revision-id)]
    (if-let [error (:error revision)]
      error
      (let [truncations (atom [])
            memberships-result
            (query! read! truncations
                    {:query/type :role-reverse :edge/type [:scope-in-revision 1]
                     :endpoint/role :revision :endpoint/id revision-id :time/order :asc})
            rows (->> (:result/edge-ids memberships-result)
                      (map #(read-record read! %))
                      (filter #(= :code-definition
                                  (get-in % [:asserted-content :scope/kind])))
                      (map (fn [membership]
                             [membership (read-record read! (endpoint-id membership :scope))]))
                      (map (fn [[membership scope]]
                             [membership scope
                              (attestation-by-method read! truncations (:id scope)
                                                     "codedocs/clojure-definition")]))
                      (sort-by #(get-in % [2 :content :codepoint-range 0])))
            definitions (mapv (fn [[membership scope _]]
                                (definition-entry read! truncations membership scope)) rows)
            linked? (boolean (some #(or (seq (:docs %)) (seq (:lean %))) definitions))
            record-ids (->> definitions (mapcat :record/ids) distinct sort vec)]
        (cond-> {:result/status (if (seq @truncations) :truncated :complete)
                 :artifact/revision-id revision-id
                 :links/status (if linked? :materialized :not-materialized-for-revision)
                 :links/at-revision (when linked? revision-id)
                 :definitions definitions :record/ids record-ids}
          (seq @truncations) (assoc :result/truncation :limit
                                    :result/truncated-queries @truncations))))))
