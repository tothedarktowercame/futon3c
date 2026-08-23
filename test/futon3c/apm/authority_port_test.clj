(ns futon3c.apm.authority-port-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.authority-port :as sut])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-authority []
  (let [base (Files/createTempDirectory "authority-port"
                                        (make-array FileAttribute 0))]
    (into {}
          (map (fn [kind]
                 (let [path (.resolve base (name kind))]
                   (Files/createDirectories path (make-array FileAttribute 0))
                   [kind (str path)])))
          [:control-root :campaign-root :qualification-root :workspace])))

(deftest every-path-kind-has-one-declared-root
  (let [authority (temp-authority)]
    (doseq [[path-kind root-kind] sut/path-roots]
      (let [resolved (sut/resolve-path authority root-kind path-kind "x")]
        (is (:ok resolved) (str path-kind))
        (is (.startsWith (Path/of (:path resolved) (make-array String 0))
                         (Path/of (get authority root-kind)
                                  (make-array String 0))))))))

(deftest missing-role-card-refuses-dispatch
  (let [result (sut/require-dispatch-paths
                (temp-authority)
                [[:role-card "holes/role-cards/missing.md"]])]
    (is (false? (:ok result)))
    (is (= :authority-path-missing (:error/code result)))
    (is (= :role-card (:path-kind result)))))
