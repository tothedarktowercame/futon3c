(ns futon3c.wm.run4-effective-environment-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.run4-effective-environment :as sut]))

(def declaration
  {:required-environment
   {"FUTON_WM_FPI_DARK" "1"
    "FUTON_WM_BETA_DARK" "1"
    "FUTON_WM_TRACE_POLICY_DETAILS" "1"}
   :hierarchy {:model :single-level :scope :RUN4}})

(defn reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest successful-attestation-is-record-ready-and-bounded
  (let [a (sut/attest declaration {:env-read (constantly "1")
                                   :var-read (constantly true)})]
    (is (= :wm/run4-effective-environment-attestation-v1 (:schema a)))
    (is (= (:hierarchy declaration) (:hierarchy a)))
    (is (= 3 (count (:flags a))))
    (is (every? #(and (= "1" (:required %)) (= "1" (:observed %))
                      (true? (:effective %))) (:flags a)))
    (is (= :not-attested-by-this-component (get-in a [:recording :status])))))

(deftest mismatches-and-unloaded-consumers-refuse
  (testing "environment enabled but loaded value false"
    (is (= :required-observed-effective-mismatch
           (reason #(sut/attest declaration {:env-read (constantly "1")
                                             :var-read (constantly false)})))))
  (testing "loaded value true but current environment disabled"
    (is (= :required-observed-effective-mismatch
           (reason #(sut/attest declaration {:env-read (constantly "0")
                                             :var-read (constantly true)})))))
  (testing "namespace or var unavailable"
    (is (= :effective-consumer-unavailable
           (reason #(sut/attest declaration {:env-read (constantly "1")
                                             :var-read (constantly nil)}))))))

(deftest malformed-requirements-and-hierarchy-refuse
  (doseq [d [(assoc declaration :required-environment {})
             (assoc-in declaration [:required-environment "FUTON_WM_FPI_DARK"] "0")
             (assoc declaration :hierarchy {:model :hierarchical :scope :RUN4})]]
    (is (= :malformed-declaration
           (reason #(sut/attest d {:env-read (constantly "1")
                                   :var-read (constantly true)}))))))

(deftest absent-opt-in-remains-unattested
  (is (= :malformed-declaration
         (reason #(sut/attest {} {:env-read (constantly nil)
                                  :var-read (constantly false)})))))
