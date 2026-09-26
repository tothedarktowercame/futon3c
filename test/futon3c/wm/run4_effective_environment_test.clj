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
             (assoc declaration :required-environment "1")
             (assoc declaration :required-environment [true])
             (assoc declaration :required-environment false)
             (assoc-in declaration [:required-environment "FUTON_WM_FPI_DARK"] "0")
             (assoc declaration :hierarchy {:model :hierarchical :scope :RUN4})]]
    (is (= :malformed-declaration
           (reason #(sut/attest d {:env-read (constantly "1")
                                   :var-read (constantly true)}))))))

(deftest absent-opt-in-remains-unattested
  (is (= :malformed-declaration
         (reason #(sut/attest {} {:env-read (constantly nil)
                                  :var-read (constantly false)})))))

(deftest historical-reader-validates-complete-pinned-attestation
  (let [sha (apply str (repeat 64 "a"))
        pin {:sha256 sha}
        base (assoc (sut/attest declaration {:env-read (constantly "1")
                                             :var-read (constantly true)})
                    :provenance
                    {:task-pin-sha256 sha
                     :config-pin {:path "config.edn" :sha256 sha}
                     :serving-declaration
                     (assoc declaration :recording-requirement
                            {:contract :wm/realized-recording-v1
                             :environment {"FUTON_WM_RECORDING_CONTRACT" "1"}})})]
    (is (= base (sut/validate-recorded! base pin)))
    (doseq [bad [nil false
                 (assoc-in base [:provenance :serving-declaration :required-environment "FUTON_WM_FPI_DARK"] "0")
                 (assoc base :flags [])
                 (update base :flags pop)
                 (update base :flags conj (first (:flags base)))
                 (assoc-in base [:flags 0 :flag] "FOREIGN")
                 (assoc-in base [:flags 0 :effective] false)
                 (assoc-in base [:flags 0 :observed] "0")
                 (assoc-in base [:provenance :task-pin-sha256]
                           (apply str (repeat 64 "b")))]]
      (is (= :invalid-recorded-attestation
             (reason #(sut/validate-recorded! bad pin)))))))

(deftest production-reader-does-not-load-missing-consumers
  (let [n 'run4-review.absent-consumer]
    (is (nil? (find-ns n)))
    (is (nil? (sut/production-var-read [n '*flag*])))
    (is (nil? (find-ns n)))))
