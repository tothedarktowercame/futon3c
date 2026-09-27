(ns futon3c.diagramprover.wm-get-in-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wiring :as w]))

(defn usage [text field record & [aliases]]
  (#'w/vertex-usage text [field record] {field #{record}}
                   {:aliases (or aliases {:outer #{"x"}})
                    :scoped #{:outer :inner :deep} :returns #{}}))

(deftest literal-paths-and-declared-nested-scopes
  (is (= 1 (:reads (usage "(get-in x [:inner :deep :value])" :inner :outer))))
  (is (= 1 (:reads (usage "(get-in x [:inner :deep :value])" :deep :inner))))
  (is (= 1 (:reads (usage "(get-in x [:inner :deep :value])" :value :deep))))
  (is (= 1 (:reads (usage "(get x :inner)" :inner :outer))))
  (is (= 1 (:reads (usage "(get-in x [:inner :value] :fallback)" :inner :outer))))
  (is (= 0 (:reads (usage "(get-in unknown [:inner :value] x)" :inner :outer)))
      "A default which names the alias is not the receiver"))

(deftest paths-that-cannot-be-attributed
  (doseq [form ["(get-in x ks)" "(get-in wrong [:value])"
                "(get-in x [k :value])" "(get-in x [:undeclared :value])"]]
    (is (= 0 (:reads (usage form :value :deep))) form))
  (is (= 0 (:reads (usage "(get-in x [:value])" :value :inner))))
  (is (= 0 (:reads (usage "'(get-in x [:inner])" :inner :outer)))))

(deftest keyword-threading-needs-every-intermediate-scope
  (doseq [form ["(-> x :inner :deep :value)" "(some-> x (:inner) (:deep) (:value))"]]
    (is (= 1 (:reads (usage form :inner :outer))))
    (is (= 1 (:reads (usage form :value :deep)))))
  (is (= 0 (:reads (usage "(-> x :inner unknown :value)" :value :deep))))
  (is (= 0 (:reads (usage "(-> wrong :inner :deep :value)" :value :deep)))))

(deftest real-envelope-and-initialization-conformance
  (let [root (.getCanonicalPath (io/file ".."))
        boxes [{:box/id :envelope :site {:file "futon2/src/futon2/aif/temporal_update.clj" :var "envelope"}
                :record-aliases {:enactment ["record"]}
                :reads [[:temporal-receipt {:record :enactment}]]}
               {:box/id :initialization
                :site {:file "futon2/src/futon2/aif/token_belief_predecessor.clj" :var "initialization-input-receipt"}
                :record-aliases {:token-belief-stage ["stage"]}
                :reads [[:initialization {:record :token-belief-stage}]]}
               {:box/id :apply-observations
                :site {:file "futon2/src/futon2/aif/token_initialization_policy.clj" :var "apply-observations"}
                :record-aliases {:token-belief-stage ["stage"]}
                :reads [[:initialization {:record :token-belief-stage}]]}
               {:box/id :inspect
                :site {:file "futon2/src/futon2/aif/token_belief_predecessor.clj" :var "inspect-trace"}
                :record-aliases {:options ["opts"]}
                :reads [[:flight {:record :options}] [:temporal-previous {:record :flight}]]}]
        report (w/conformance root {:boxes boxes} {:heuristic? true})]
    (doseq [b boxes entry (:reads b)]
      (let [vertex (w/vertex-key entry)]
        (is (not-any? #(and (= (:box/id b) (:box/id %)) (= vertex (:field %))) report)
            (pr-str {:box (:box/id b) :field vertex :report report}))))))
