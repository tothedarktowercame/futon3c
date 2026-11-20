(ns futon3c.arxana.together-catalogue-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.design-catalogue :as design]
            [futon3c.arxana.design-catalogue-test :as design-test]
            [futon3c.arxana.together :as together]
            [futon3c.arxana.together-catalogue :as catalogue]
            [futon3c.arxana.together-structure :as structure]))

(def at "2026-11-20T00:00:00Z")
(def inputs {:pins [{:fixture "resources/arxana/together/conformance.json"}]
             :at at :implementation-digest "together-catalogue-test-digest"})

(defn- temp-catalogue-dirs []
  (set (keep #(when (.startsWith (.getName ^java.io.File %) "together-catalogue-")
                (.getCanonicalPath ^java.io.File %))
             (.listFiles (io/file (System/getProperty "java.io.tmpdir"))))))

(deftest every-executable-probe-passes-and-cleans-up
  (let [before (temp-catalogue-dirs)]
    (doseq [feature (take 12 catalogue/features)]
      (testing (name (:feature feature))
        (is (= {:result :pass :violations []} (catalogue/probe (:feature feature))))))
    (is (= before (temp-catalogue-dirs)))))

(deftest planted-bad-cases-cannot-pass
  (with-redefs [together/compare-ids (constantly 0)]
    (is (not= :pass (:result (catalogue/probe :convergence)))))
  (with-redefs [together/site-of? (constantly true)]
    (is (not= :pass (:result (catalogue/probe :authorship-checked)))))
  (with-redefs [together/check-op (constantly nil)]
    (is (not= :pass (:result (catalogue/probe :repeated-insert-acknowledged)))))
  (with-redefs [structure/check-op (constantly nil)]
    (is (not= :pass (:result (catalogue/probe :regroup-cycle-refused))))))

(deftest client-probes-do-not-pass-on-a-failing-or-empty-run
  ;; a client test run that fails, or that runs nothing, is not a pass
  (doseq [output [{:exit 1 :out "Ran 3 tests, 2 results as expected, 1 unexpected"}
                  {:exit 0 :out "Ran 0 tests, 0 results as expected"}
                  {:exit 0 :out ""}]]
    (with-redefs [catalogue/run-process (fn [_ _] output)]
      (is (= :violation (:result (catalogue/probe :emacs-client-conforms))))))
  (doseq [output [{:exit 1 :out "# pass 3\n# fail 1\n"}
                  {:exit 0 :out "# pass 0\n# fail 0\n"}
                  {:exit 0 :out ""}]]
    (with-redefs [catalogue/run-process (fn [_ _] output)]
      (is (= :violation (:result (catalogue/probe :browser-client-conforms))))))
  (with-redefs [catalogue/run-process (fn [_ _] (throw (java.io.IOException. "no such program")))]
    (is (= :error (:result (catalogue/probe :emacs-client-conforms))))))

(deftest records-counts-validation-and-debt
  (let [{:keys [choices checks links runs records]} (catalogue/build inputs)
        by-key (into {} (map #(vector (get-in % [:asserted-content :choice/key]) %) choices))]
    (is (= [13 12 12 12] (mapv count [choices checks links runs])))
    (is (every? :ok (design-test/validate records)))
    (doseq [n (range 1 13)]
      (let [feature (:feature (nth catalogue/features (dec n)))
            choice (by-key (keyword "together" (name feature)))]
        (is (= {:status :enforced} (design/enforcement-status records (:id choice))))))
    (let [choice (by-key :together/peer-version-stays-possible)]
      (is (= {:status :unknown :reason :no-check}
             (design/enforcement-status records (:id choice)))))))
