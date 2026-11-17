(ns futon3c.diagramprover.publish-findings-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.publish-findings :as p]
            [futon3c.arxana.store-http :as http]))
(deftest offline-plan-validates-without-http
  (with-redefs [http/append-batch! (fn [& _] (throw (ex-info "Unexpected HTTP write" {})))
                http/get-entity-as-of (fn [& _] (throw (ex-info "Unexpected HTTP read" {})))]
    (let [plan (p/build-batch {:checker-sha "fixture-sha" :event-time "2026-11-17T23:20:00.000Z"})]
      (is (= 13 (count (:rows plan))))
      (is (= {:validated 13} (p/validate-batch! plan)))
      (is (= [:failing :settled :failing]
             (mapv #(get-in % [:record :asserted-content :evaluation])
                   (filter #(= [:conflict/finding 1] (get-in % [:record :type-revision])) (:rows plan)))))
      (is (= plan (p/build-batch {:checker-sha "fixture-sha" :event-time "2026-11-17T23:20:00.000Z"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (p/validate-batch! (assoc-in plan [:rows 0 :record :id] "wrong-digest")))))))
