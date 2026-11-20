(ns futon3c.arxana.incubator-catalogue-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.design-catalogue :as design]
            [futon3c.arxana.design-catalogue-test :as design-test]
            [futon3c.arxana.incubator-catalogue :as catalogue]
            [futon3c.arxana.together-catalogue :as together-catalogue]))

(def at "2026-11-20T00:00:00Z")
(def inputs {:pins [{:repo "futon4a" :revision "fixture"}]
             :at at :implementation-digest "incubator-catalogue-test-digest"})

(deftest ^:slow every-incubator-probe-passes
  (doseq [{:keys [feature tests]} catalogue/features :when (seq tests)]
    (testing (name feature)
      (is (= {:result :pass :violations []} (catalogue/probe feature))))))

(deftest process-failures-never-pass
  (let [feature :layers-on-a-main-line]
    (with-redefs [together-catalogue/run-process (fn [& _] {:exit 1 :out "Ran 2 tests, 2 results as expected"})]
      (is (= :violation (:result (catalogue/probe feature)))))
    (with-redefs [together-catalogue/run-process (fn [& _] {:exit 0 :out "Ran 0 tests, 0 results as expected"})]
      (is (= :violation (:result (catalogue/probe feature)))))
    (with-redefs [together-catalogue/run-process
                  (fn [& _] {:exit 0 :out (str "MISSING:incubator-proposals-sit-above-joes-pass\n"
                                               "Ran 1 test, 1 result as expected")})]
      (let [result (catalogue/probe feature)]
        (is (= :violation (:result result)))
        (is (some #(= ["incubator-proposals-sit-above-joes-pass"] (:tests %))
                  (:violations result)))))
    (with-redefs [together-catalogue/run-process (fn [& _] (throw (ex-info "boom" {})))]
      (is (= :error (:result (catalogue/probe feature)))))))

(deftest built-records-validate-and-report-debt
  (let [{:keys [choices checks links runs records]} (catalogue/build inputs)
        by-key (into {} (map #(vector (get-in % [:asserted-content :choice/key]) %) choices))]
    (is (= [22 20 20 20] (mapv count [choices checks links runs])))
    (is (= {:accepted 21 :proposed 1}
           (frequencies (map #(get-in % [:asserted-content :lifecycle]) choices))))
    (is (every? :ok (design-test/validate records)))
    (doseq [{:keys [feature tests]} catalogue/features]
      (let [choice (by-key (keyword "incubator" (name feature)))]
        (is (= (if (seq tests)
                 {:status :enforced}
                 {:status :unknown :reason :no-check})
               (design/enforcement-status records (:id choice))))))))
