(ns futon3c.arxana.join-registry-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.join-registry :as joins]
            [futon3c.arxana.type-registry :as types]))

(deftest committed-snapshot-matches-registry
  ;; Cross-repo worktree review may select its candidate snapshot explicitly.
  ;; Normally this resource is provided by the futon1b local dependency.
  (let [source (or (System/getProperty "futon1b.join-snapshot")
                   (io/resource joins/snapshot-resource))]
    (is (some? source) "futon1b must supply the committed registry snapshot")
    (when source
      (let [snapshot (edn/read-string (slurp source))]
        (is (= (joins/join-shapes) snapshot))
        (with-redefs [types/registry-v1
                      (assoc types/registry-v1 [:invented/new-link 1]
                             {:record/kind :clusion :roles [{:role :a} {:role :b}]})]
          (is (not= (joins/join-shapes) snapshot)
              "adding a registered link without refreshing the snapshot is detected"))))))
