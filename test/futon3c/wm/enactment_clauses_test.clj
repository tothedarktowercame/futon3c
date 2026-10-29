(ns futon3c.wm.enactment-clauses-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.enactment-clauses :as producer]
            [futon2.aif.wm.authored.req-f0ba279a :as authored]
            [futon2.aif.wm.cascade-enactment-compiler :as compiler]))

(load-file "test/resources/wm/e43/prior_producer.clj")
(def context {:cascade-digest "af6294ee687ca69fac2cb0ddd7e75c92cccf38e716a5082f3a3333d41e218f91"
              :phase :compile})
(def required-fields [:operation :capability :completion-check])
(defn compile-product [clauses]
  (let [structural (:structural (edn/read-string (slurp "test/resources/wm/e43/cascade.edn")))
        result (compiler/compile-executable structural
                 {:attribution {:author :wm-author :reviewer :wm-reviewer
                                :authority :wm/reviewed-resume}
                  :application-order [] :clauses clauses})]
    {:status (if (or (:refusal result) (seq (get-in result [:executable-inputs :holes])))
               :cascade-enactment-open :cascade-ready)
     :result result}))

(deftest retained-cascade-compiles-through-producer
  (let [prior ((ns-resolve 'futon3c.wm.e43-prior-producer 'enact-clauses) context)
        current (producer/enact-clauses context)]
    (is (= 9 (count current)))
    (is (= (set (keys prior)) (set (keys current)) (set (keys authored/clauses))))
    (is (every? #(every? (fn [k] (some? (get % k))) required-fields)
                (vals authored/clauses)))
    (is (= :cascade-enactment-open (:status (compile-product prior))))
    (doseq [[id old] prior]
      (is (= required-fields (filterv #(nil? (get old %)) required-fields)))
      (is (= old (select-keys (get current id) (keys old))))
      (is (= (get authored/clauses id) (select-keys (get current id) (keys (get authored/clauses id))))))
    (is (= :cascade-ready (:status (compile-product current))))))

(deftest context-refusals-unchanged
  (doseq [[ctx schema] [[{} :wm/patch-context-missing-keys]
                       [(assoc context :cascade-digest "wrong") :wm/patch-cascade-mismatch]
                       [(assoc context :phase :execute) :wm/patch-phase-mismatch]]]
    (is (= schema (try (producer/enact-clauses ctx) nil
                      (catch clojure.lang.ExceptionInfo e (:schema (ex-data e))))))))
