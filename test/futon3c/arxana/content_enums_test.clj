(ns futon3c.arxana.content-enums-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.type-registry :as types]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.write :as write]
            [futon3c.diagramprover.sentence-records :as sentence]))

(defn record-with [revision scope]
  (records/make-clusion
   {:type-revision [:sentence/manifestation revision]
    :endpoints [{:role :role :kind :clusion :target {:clusion/id "invented-role"}}]
    :asserted-content {:renderer-commit "invented-render" :renderer-commit-time "2026-01-01T00:00:00.000Z"
                       :corpus-sha256 "invented-corpus" :wordings [] :count 0 :scope scope}}))

(defn execute [r]
  (let [saved (atom [])
        memory (reify store/ArxanaGraphStore
                 (get-entity [_ _] {:ok nil})
                 (conditional-write! [_ command]
                   (swap! saved conj (get-in command [:body :record]))
                   {:ok {:created? true}}))
        result (write/execute memory {:principal/id "invented-test" :message/id "invented-write"}
                              :clusion/assert r)]
    {:result result :saved @saved}))

(deftest write-authority-enforces-declared-enums
  (let [{:keys [result saved]} (execute (record-with 3 :sentence))]
    (is (= :arxana.clusion/invalid-content-enum (get-in result [:error :error/code])))
    (is (= {:type-revision [:sentence/manifestation 3]
            :field :scope :value :sentence :allowed #{:role :declaration}}
           (get-in result [:error :error/detail])))
    (is (empty? saved)))
  (doseq [scope [:role :declaration]]
    (is (= 1 (count (:saved (execute (record-with 3 scope))))))))
(deftest legacy-gap-is-explicit
  (is (= 1 (count (:saved (execute (record-with 2 :sentence))))))
  (is (= 1 (count (:saved (execute (records/make-clusion
                                  {:type-revision [:workplan/item 1]
                                   :endpoints [] :asserted-content {:item/id "M-invented"}}))))))
  ;; Mutation: stripping the declaration's enum permits the precise bad case.
  (let [lookup types/declaration]
    (with-redefs [types/declaration #(dissoc (lookup %) :content/enums)]
      (is (= 1 (count (:saved (execute (record-with 3 :sentence)))))))))
(deftest new-builder-and-declaration-shape
  (let [r (sentence/scoped-manifestation-v3 {:id "invented-role"} "invented-render"
                                          "2026-01-01T00:00:00Z" "invented-corpus" [] :declaration)]
    (is (= [:sentence/manifestation 3] (:type-revision r)))
    (is (= 1 (count (:saved (execute r))))))
  (is (= #{:role :declaration} (get-in (types/declaration [:sentence/manifestation 3]) [:content/enums :scope])))
  (doseq [bad [{:scope [:role]} {:unknown #{:role}} nil]]
    (is (= :arxana.type/invalid-content-enums
           (try (types/validate-declaration (assoc (types/declaration [:sentence/manifestation 3]) :content/enums bad))
                nil (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))
