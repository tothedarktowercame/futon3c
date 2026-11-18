(ns futon3c.arxana.design-catalogue-facts
  "Facts for the design-catalogue checks, produced by bounded store queries.
  The query decides the fact set; the evaluators (core.logic, Reazon, ramo)
  only judge it. Joe 2026-11-18: the query layer is the final authority."
  (:require [futon3c.arxana.queries :as q]))

(def function-patterns
  "Rule patterns that denote an opaque (Python function) rule. Term-schema rules
  (e.g. beta, [:app [:lam ?B] ?A]) are not function rules and the
  one-rule-per-function ruling does not apply to them."
  [:whole-sequence :opaque-function])

(defn current-function-rules
  "Function rules not superseded at the read time: the ruling is that, at a
  valid time, one unsuperseded rule record identifies one function. A
  superseded version (raw Q56 under Q83) legitimately shares its qualname."
  [function-rules supersessions]
  (let [predecessors (set (for [s supersessions e (:endpoints s)
                                :when (= :predecessor (:role e))]
                            (get-in e [:target :clusion/id])))]
    (vec (sort-by :id (remove #(predecessors (:id %)) (distinct function-rules))))))

(defn read-function-rules
  "Query the store at AT. Returns {:status :ok :rules [...]} or an inconclusive
  result if any query is truncated or refused, never a partial fact set."
  [store at]
  (let [by-pattern (mapv #(q/records-by-field store {:type-revision [:rule/content 1] :field :rule/pattern
                                                     :value % :valid-time at :limit 200})
                         function-patterns)
        sups (q/records-by-type store {:type-revision [:supersedes 1] :valid-time at :limit 200})
        results (conj by-pattern sups)]
    (cond
      (some :error results) {:status :error :results results}
      (some #(not= :ok (:status %)) results) {:status :inconclusive :reason :query-status}
      (some :truncated? results) {:status :inconclusive :reason :truncated}
      :else {:status :ok
             :rules (current-function-rules (mapcat :records by-pattern) (:records sups))
             :queries (mapv #(select-keys % [:limit :truncated? :watermark :valid-time]) results)})))
