(ns futon3c.arxana.resolve-correspond-integration-test
  "Cross-increment acceptance: one authority revision is resolved and then
  used to retrieve its fixture-backed manifestation correspondence."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.correspond-fixture :as fixture]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.resolve.evaluator :as evaluator]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store]))

(deftest resolve-then-correspond-on-one-revision-test
  (let [{:keys [repo manifestations assertions]} (fixture/build-fixture-repository)
        source-manifestation (:section-source manifestations)
        expected-assertion (:section-range assertions)
        source-path (io/file (:src-dir (fixture/load-scopes)) "draft8.tex")
        bytes (java.nio.file.Files/readAllBytes (.toPath source-path))
        provider (artifact/in-memory-provider)
        {:keys [ok]} (artifact/ingest! provider "draft8.tex" bytes "text/x-tex")
        revision (:artifact/revision-id ok)
        graph-store (store/mock-store)
        strategy {:arxana/semantics :arxana.resolution/v1
                  :arxana/strategy-id "fixture/section-the-object"
                  :arxana/scope {:artifact "draft8.tex"}
                  :arxana/cascade-mode :first-unique
                  :arxana/finders
                  [{:arxana/type :arxana.finder/symbol-as-term
                    :arxana/on [:ambiguous :missing]
                    :arxana/config {:patterns ["<term>"]
                                    :language :latex
                                    :normalization :literal
                                    :term "\\section{The Object}"}}]}
        strategy-revision (service/register-strategy! graph-store strategy)
        resolved (evaluator/evaluate
                  {:artifact-provider provider
                   :strategy-source
                   (fn [id]
                     (get-in (:ok (store/get-entity graph-store id))
                             [:props :strategy]))}
                  {:request/id "fixture-section-resolution"
                   :artifact/revision-id revision
                   :strategy/revision-id strategy-revision})
        neighbours (corr/query
                    repo {:manifestation/id (:manifestation/id source-manifestation)
                          :artifact/revision-id revision
                          :direction :source-to-target})
        semantic-digest (env/semantic-digest
                         env/scope-resolve-projection-v1
                         (assoc resolved :operation :scope/resolve))]
    (testing "fixture and ArtifactProvider name identical canonical bytes"
      (is (= revision
             (get-in expected-assertion [:source :artifact/revision-id]))))
    (testing "Resolve finds the named section uniquely without choosing among ties"
      (is (= :resolved (:result/status resolved)))
      (is (= 1 (count (:result/candidates resolved))))
      (is (= :verified-hash
             (get-in resolved [:result/candidates 0 :verification])))
      (is (string? semantic-digest)))
    (testing "the same revision retrieves its committed HTML correspondence"
      (is (= [(:id expected-assertion)] (mapv :id neighbours)))
      (is (= {:kind :dom-id :value "S2"}
             (get-in (first neighbours) [:target :locator])))
      (is (= revision
             (get-in (first neighbours) [:source :artifact/revision-id]))))))
