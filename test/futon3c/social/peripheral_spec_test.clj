(ns futon3c.social.peripheral-spec-test
  "Tests that resources/peripherals.edn validates against PeripheralSpec shapes.

   The peripheral definitions are the contract for what each capability
   envelope permits. This test ensures the EDN is well-formed."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.social.shapes :as shapes]))

(defn- load-peripherals []
  (-> (io/resource "peripherals.edn")
      slurp
      edn/read-string
      :peripherals))

(deftest peripherals-edn-loads
  (testing "peripherals.edn is loadable and has six peripherals"
    (let [peripherals (load-peripherals)]
      (is (= 6 (count peripherals)))
      (is (= #{:explore :edit :test :deploy :reflect :proof} (set (keys peripherals)))))))

(deftest all-peripherals-validate-against-shape
  (testing "every peripheral in peripherals.edn conforms to PeripheralSpec"
    (doseq [[id spec] (load-peripherals)]
      (testing (str "peripheral: " (name id))
        (is (shapes/valid? shapes/PeripheralSpec spec)
            (str (name id) " failed validation: "
                 (:error (shapes/validate shapes/PeripheralSpec spec))))))))

(deftest peripheral-ids-match-keys
  (testing "each peripheral's :peripheral/id matches its map key"
    (doseq [[id spec] (load-peripherals)]
      (is (= id (:peripheral/id spec))
          (str "key " id " doesn't match :peripheral/id " (:peripheral/id spec))))))

(deftest every-peripheral-has-session-id-context
  (testing "all peripherals carry session-id for hop continuity"
    (doseq [[id spec] (load-peripherals)]
      (is (= :inherit (get-in spec [:peripheral/context :session-id]))
          (str (name id) " missing session-id context")))))

(deftest explore-is-read-only
  (testing "explore peripheral has no write tools"
    (let [explore (get (load-peripherals) :explore)
          write-tools #{:edit :write :bash-git :bash-deploy}]
      (is (empty? (clojure.set/intersection (:peripheral/tools explore) write-tools))))))

(deftest deploy-cannot-edit
  (testing "deploy peripheral cannot edit files"
    (let [deploy (get (load-peripherals) :deploy)
          edit-tools #{:edit :write}]
      (is (empty? (clojure.set/intersection (:peripheral/tools deploy) edit-tools))))))

(deftest reflect-is-read-only
  (testing "reflect peripheral is read-only"
    (let [reflect (get (load-peripherals) :reflect)
          write-tools #{:edit :write :bash :bash-git :bash-deploy}]
      (is (empty? (clojure.set/intersection (:peripheral/tools reflect) write-tools))))))
