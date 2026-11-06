(ns futon3c.arxana.correspond-fixture-test
  "Fixture-gate tests for INSTANTIATE increment 5 against the committed futon5
  gate 17dc53a: negative fixture rejection, positive three-case verification,
  and the repository built from the fixture."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.correspond-fixture :as fx]))

(deftest negative-fixture-rejected-test
  (let [neg (fx/load-negative-scopes)
        report (fx/verify-scope-revisions neg)]
    (is (false? (:ok? report)))
    (testing "the contradiction-bearing scope table fails with revision-mismatch (D4 negative tripwire)"
      (is (some #(= :fixture/revision-mismatch (:error %)) (:errors report))))))

(deftest positive-fixture-coherent-test
  (let [scopes (fx/load-scopes)
        rev (fx/verify-scope-revisions scopes)]
    (testing "recorded source-shas equal the exact live bytes"
      (is (:ok? rev) (pr-str (:errors rev))))
    (testing "three normative cases hold"
      (let [cases (fx/verify-cases scopes (fx/fixture-path "draft8.html"))]
        (is (:ok? cases) (pr-str (:errors cases)))
        (is (contains? (:found cases) :paragraph-point))
        (is (contains? (:found cases) :section-range))
        (is (contains? (:found cases) :math-transformation))))))

(deftest committed-gate-equality-test
  (testing "fixture files on disk equal the blobs committed at 17dc53a"
    (is (true? (fx/verify-committed! [(str fx/fixture-dir "/manifest.json")
                                      (str fx/fixture-dir "/scopes.json")
                                      (str fx/fixture-dir "/scopes-negative-v1.json")])))))

(deftest fixture-repository-test
  (let [{:keys [repo manifestations assertions]} (fx/build-fixture-repository)
        {:keys [intro-source intro-html section-html math-source math-html]} manifestations
        {:keys [paragraph-point section-range math-transformation]} assertions]
    (testing "paragraph case: partial point anchor, quote evidence, no DOM id, no invented end"
      (is (= :partial (:verification paragraph-point)))
      (is (= :codepoint-point (get-in paragraph-point [:source :locator :kind])))
      (is (nil? (get-in paragraph-point [:source :locator :end])))
      (is (some #(= :quote (:type %)) (:evidence paragraph-point)))
      (is (not= :dom-id (get-in paragraph-point [:target :locator :kind]))))
    (testing "section case: verified source-map + quote; DOM id is locator evidence"
      (is (= :verified (:verification section-range)))
      (is (= [:source-map :quote] (:method section-range)))
      (is (= {:kind :dom-id :value "S2"} (get-in section-range [:target :locator]))))
    (testing "math case: directional transformation, reverse unsupported"
      (is (= [:transformation] (:method math-transformation)))
      (is (not (corr/reversible? math-transformation)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (corr/reverse-assertion math-transformation)))
      (is (some #(= "P_{\\sigma}" (:alttext %)) (:evidence math-transformation))))
    (testing "authored and generated manifestations never alias"
      (is (= :authored (:manifestation/role intro-source)))
      (is (= :generated (:manifestation/role intro-html)))
      (is (not= (:manifestation/id math-source) (:manifestation/id math-html)))
      (is (= 6 (count (set (map :manifestation/id (vals manifestations)))))))
    (testing "every assertion endpoint revision occurs in the fixture manifest"
      (let [revs (set (map :revision-id (:artifacts (fx/load-manifest))))]
        (doseq [a (vals assertions)]
          (is (revs (get-in a [:source :artifact/revision-id])))
          (is (revs (get-in a [:target :artifact/revision-id]))))))
    (testing "queries over the fixture repository"
      (is (= [(:id paragraph-point)]
             (map :id (corr/query repo {:verification :partial}))))
      (is (= [(:id math-transformation)]
             (map :id (corr/query repo {:methods #{:transformation}}))))
      (is (= 2 (count (corr/query repo {:role :generated
                                        :verification-floor :verified}))))
      (is (= 3 (count (corr/query repo {:artifact/revision-id
                                        (get-in section-range [:target :artifact/revision-id])
                                        :direction :target-to-source}))))
      (is (= [(:id section-range)]
             (map :id (corr/query repo {:manifestation/id (:manifestation/id section-html)
                                        :direction :target-to-source})))))
    (testing "all-manifestations per subject"
      (is (= 2 (count (corr/all-manifestations repo "draft8/introduction"))))
      (is (= 2 (count (corr/all-manifestations repo "draft8/intro-math")))))))
