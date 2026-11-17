(ns futon3c.circus.source-inventory-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def inventory-path "resources/circus/source-inventory.edn")

(deftest source-inventory-is-complete-and-machine-readable
  (let [inventory (edn/read-string (slurp (io/file inventory-path)))
        required #{:source/id :store :schema/version :pin/method
                   :retention :rebuild/role :check}]
    (is (= 1 (:inventory/version inventory)))
    (is (seq (:sources inventory)))
    (doseq [source (:sources inventory)]
      (testing (str (:source/id source))
        (is (every? #(contains? source %) required))
        (is (string? (:check source)))))))
