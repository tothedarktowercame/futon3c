(ns futon3c.peripheral.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.registry :as reg]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]))

;; =============================================================================
;; Registry — factory dispatch
;; =============================================================================

(deftest make-peripheral-creates-correct-types
  (testing "make-peripheral dispatches to the right factory for each ID"
    (doseq [pid reg/peripheral-ids]
      (let [p (reg/make-peripheral pid)]
        (is (= pid (get-in p [:spec :peripheral/id]))
            (str "wrong spec for " pid))))))

(deftest make-peripheral-with-custom-backend
  (testing "make-peripheral passes backend to factory"
    (let [backend (tools/make-mock-backend {:read {:content "hello"}})
          p (reg/make-peripheral :explore backend)
          start (runner/start p {:session-id "s-reg-1"})
          step (runner/step p (:state start) {:tool :read :args ["a.clj"]})]
      (is (:ok start))
      (is (:ok step))
      (is (= {:content "hello"} (:result step))))))

(deftest make-peripheral-unknown-id-throws
  (testing "make-peripheral throws for unknown peripheral-id"
    (is (thrown? clojure.lang.ExceptionInfo (reg/make-peripheral :nonexistent)))))

(deftest peripheral-ids-covers-all-six
  (testing "peripheral-ids contains all known IDs"
    (is (= #{:explore :edit :test :deploy :reflect :proof} reg/peripheral-ids))))

(deftest load-peripherals-returns-correct-format
  (testing "load-peripherals loads from classpath and returns expected format"
    (let [p (reg/load-peripherals)]
      (is (map? (:peripherals p)))
      (is (= #{:explore :edit :test :deploy :reflect :proof}
             (set (keys (:peripherals p))))))))

;; =============================================================================
;; Chain orchestration
;; =============================================================================

(deftest run-chain-single-peripheral
  (testing "run-chain with one step runs a single peripheral"
    (let [backend (tools/make-mock-backend {:glob {:found ["src/a.clj"]}})
          peripherals (reg/load-peripherals)
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-chain-1"}
                   [{:peripheral-id :explore
                     :actions [{:tool :glob :args ["**/*.clj"]}]
                     :stop-reason "found target"
                     :exit-condition :found-target}])]
      (is (true? (:ok result)))
      (is (= 1 (count (:fruits result))))
      (is (= 3 (count (:evidence result))))
      (is (= [:goal :step :conclusion]
             (mapv :evidence/claim-type (:evidence result)))))))

(deftest run-chain-two-peripherals-with-hop
  (testing "run-chain validates hop and transfers context between peripherals"
    (let [backend (tools/make-mock-backend {:glob {:found ["src/a.clj"]}
                                            :edit {:ok true}})
          peripherals (reg/load-peripherals)
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-chain-2"}
                   [{:peripheral-id :explore
                     :actions [{:tool :glob :args ["**/*.clj"]}]
                     :stop-reason "found target"
                     :exit-condition :found-target}
                    {:peripheral-id :edit
                     :actions [{:tool :edit :args ["src/a.clj"]}]
                     :stop-reason "ready to test"
                     :exit-condition :tests-pass}])]
      (is (true? (:ok result)))
      (is (= 2 (count (:fruits result))))
      ;; 3 evidence from explore + 3 from edit = 6
      (is (= 6 (count (:evidence result)))))))

(deftest run-chain-rejects-invalid-hop
  (testing "run-chain returns error for invalid hop transition"
    (let [backend (tools/make-mock-backend {:glob {:found ["src/a.clj"]}})
          peripherals (reg/load-peripherals)
          ;; explore → deploy is invalid (deploy entry requires :from-test or :tests-passed)
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-chain-3"}
                   [{:peripheral-id :explore
                     :actions [{:tool :glob :args ["**/*.clj"]}]
                     :stop-reason "found target"
                     :exit-condition :found-target}
                    {:peripheral-id :deploy
                     :actions []
                     :stop-reason "ship it"
                     :exit-condition :deployed}])]
      (is (false? (:ok result)))
      (is (shapes/valid? shapes/SocialError (:error result)))
      ;; First peripheral's evidence was still collected
      (is (= 3 (count (:evidence result)))))))

(deftest run-chain-with-evidence-store
  (testing "run-chain injects evidence-store into peripheral contexts"
    (let [evidence-store (atom {:entries {} :order []})
          backend (tools/make-mock-backend {:read {:content "hello"}})
          peripherals (reg/load-peripherals)
          result (reg/run-chain
                   {:backend backend :peripherals peripherals
                    :evidence-store evidence-store}
                   {:session-id "s-chain-4"}
                   [{:peripheral-id :explore
                     :actions [{:tool :read :args ["src/a.clj"]}]
                     :stop-reason "found target"
                     :exit-condition :found-target}])]
      (is (true? (:ok result)))
      ;; Evidence was appended to the store by the peripheral
      (is (pos? (count (:entries @evidence-store)))))))

(deftest run-chain-step-error-propagates
  (testing "run-chain propagates errors from peripheral step failures"
    (let [backend (tools/make-mock-backend)
          peripherals (reg/load-peripherals)
          ;; :edit is not in explore's tool set → SocialError
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-chain-5"}
                   [{:peripheral-id :explore
                     :actions [{:tool :edit :args ["src/a.clj"]}]
                     :stop-reason "done"
                     :exit-condition :found-target}])]
      (is (false? (:ok result)))
      (is (shapes/valid? shapes/SocialError (:error result))))))
