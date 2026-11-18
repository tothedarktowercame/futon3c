(ns futon3c.arxana.draft-edition-catalogue
  (:require [futon3c.arxana.design-catalogue :as c]
            [futon3c.arxana.draft-edition :as edition]))
(defn probe [snapshot]
  (let [good (edition/edition-view snapshot)
        bad (edition/edition-view (assoc snapshot :truncated? true))
        violations (cond-> []
                     (not= :ok (:status good)) (conj :complete-input-refused)
                     (not= :inconclusive (:status bad)) (conj :truncation-hidden))]
    {:result (if (empty? violations) :pass :violation) :violations violations}))
(defn records [{:keys [revision implementation-digest]}]
  (let [choice (c/choice {:choice/key :backend/draft-edition :kind :constraint :lifecycle :proposed
                          :statement "The in-memory Draft 2 edition resolves pinned external spans, distinguishes lexical matches, proposals and human reviews, and refuses incomplete inputs."
                          :sources [{:repo "futon3c" :revision revision :path "src/futon3c/arxana/draft_edition.clj" :section "build, resolve-span, edition-view"}]
                          :applies-to [:draft/edition] :rationale "Annotations do not copy authority or turn proposals into acceptance."
                          :alternatives []})
        check (c/check-record {:check/key :draft/edition :entry-point "futon3c.arxana.draft-edition-catalogue/probe"
                               :runtime :clojure :implementation-digest implementation-digest
                               :input-contract {:snapshot :complete-in-memory-record-set}
                               :output-contract {:result [:pass :violation]}
                               :fixtures ["test/futon3c/arxana/draft_edition_test.clj"]})]
    [choice check (c/link :design/checked-by choice check)]))
