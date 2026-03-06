(ns futon3c.enrichment.query-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.enrichment.query :as eq]))

(deftest path->namespace-test
  (testing "standard Clojure source paths"
    (is (= "futon3c.peripheral.mission-control-backend"
           (eq/path->namespace "src/futon3c/peripheral/mission_control_backend.clj")))
    (is (= "futon3c.transport.http"
           (eq/path->namespace "src/futon3c/transport/http.clj")))
    (is (= "futon3c.core"
           (eq/path->namespace "src/futon3c/core.clj"))))

  (testing "paths with leading repo dir"
    (is (= "futon3c.agency.registry"
           (eq/path->namespace "/home/joe/code/futon3c/src/futon3c/agency/registry.clj"))))

  (testing "cljc extension"
    (is (= "futon3c.shared.util"
           (eq/path->namespace "src/futon3c/shared/util.cljc"))))

  (testing "nil and blank"
    (is (nil? (eq/path->namespace nil)))
    (is (nil? (eq/path->namespace "")))))

(deftest classify-hyperedge-test
  (testing "classification by hx/type"
    (is (= :missions (#'eq/classify-hyperedge {:hx/type :project/mission-file})))
    (is (= :patterns (#'eq/classify-hyperedge {:hx/type :project/pattern})))
    (is (= :evidence (#'eq/classify-hyperedge {:hx/type :evidence/namespace-binding})))
    (is (= :tensions (#'eq/classify-hyperedge {:hx/type :tension/orphan-namespace})))
    (is (= :deps (#'eq/classify-hyperedge {:hx/type :dep/classpath})))
    (is (= :churn (#'eq/classify-hyperedge {:hx/type :code/file-churn})))
    (is (= :requires (#'eq/classify-hyperedge {:hx/type :code/requires})))
    (is (= :invariants (#'eq/classify-hyperedge {:hx/type :invariant/undocumented-entry-point})))))

(deftest extract-var-name-test
  (is (= "foo" (#'eq/extract-var-name "var:futon3c.core/foo")))
  (is (= "bar-baz" (#'eq/extract-var-name "var:futon3c.util/bar-baz")))
  (is (nil? (#'eq/extract-var-name "ns:futon3c.core"))))
