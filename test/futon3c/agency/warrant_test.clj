(ns futon3c.agency.warrant-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [futon3c.agency.warrant :as warrant]))

(def valid-entry-id
  "test-registry-0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def valid-entry-id-2
  "test-registry-fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210")

(def valid-warrant
  {:entry-id valid-entry-id :namespace "futon3c.agency.warrant-test"
   :lane "routine" :base-sha "14ace87"})

(def valid-warrant-2
  {:entry-id valid-entry-id-2 :namespace "futon3c.flight.pretty-print-test"
   :lane "pre-push" :base-sha "14ace8725bcf8dcd623e00d2b6a3a5b286b2e22"})

(deftest normalize-valid
  (testing "a non-empty valid vector is warranted with normalized lanes"
    (let [r (warrant/normalize-warrants [valid-warrant valid-warrant-2])]
      (is (= :warranted (:handoff/warrant-status r)))
      (is (= 2 (count (:warrants r))))
      (is (= {:entry-id valid-entry-id
              :namespace "futon3c.agency.warrant-test"
              :lane :routine
              :base-sha "14ace87"}
             (first (:warrants r))))
      (is (= :pre-push (-> r :warrants second :lane))))))

(deftest normalize-absent-and-empty
  (testing "absent and empty are :unwarranted, not refusals"
    (is (= {:handoff/warrant-status :unwarranted}
           (warrant/normalize-warrants nil)))
    (is (= {:handoff/warrant-status :unwarranted}
           (warrant/normalize-warrants [])))))

(deftest normalize-malformed-shape
  (testing "a non-vector warrants value refuses"
    (let [r (warrant/normalize-warrants {"entry-id" valid-entry-id})]
      (is (= :warrant-invalid (:handoff/refusal r)))
      (is (= :warrants (:field r))))))

(deftest normalize-malformed-fields
  (testing "each malformed field refuses with that field named"
    (are [w field] (let [r (warrant/normalize-warrants [w])]
                     (and (= :warrant-invalid (:handoff/refusal r))
                          (= field (:field r))))
      ;; entry-id: wrong shape, wrong prefix, uppercase hex
      (assoc valid-warrant :entry-id "test-registry-short") :entry-id
      (assoc valid-warrant :entry-id "notregistry-" ) :entry-id
      (assoc valid-warrant :entry-id (str/upper-case valid-entry-id)) :entry-id
      ;; namespace: blank or non-string
      (assoc valid-warrant :namespace "  ") :namespace
      (assoc valid-warrant :namespace 7) :namespace
      ;; lane: not one of the three
      (assoc valid-warrant :lane "sometimes") :lane
      (assoc valid-warrant :lane nil) :lane
      ;; base-sha: too short, non-hex, non-string
      (assoc valid-warrant :base-sha "14ace") :base-sha
      (assoc valid-warrant :base-sha "14ace8z") :base-sha
      (assoc valid-warrant :base-sha 14) :base-sha)))

(deftest normalize-never-drops
  (testing "a malformed second element refuses rather than dropping it"
    (let [r (warrant/normalize-warrants [valid-warrant
                                          (assoc valid-warrant-2 :lane "ad-hoc")])]
      (is (= :warrant-invalid (:handoff/refusal r)))
      (is (= :lane (:field r)))
      (is (= "ad-hoc" (:value r))))))

(deftest render-lines
  (testing "warranted rendering lists each warrant"
    (let [lines (warrant/render-warrant-lines
                 (warrant/normalize-warrants [valid-warrant]))]
      (is (str/starts-with? lines "Warrants: :warranted (1)\n"))
      (is (str/includes? lines valid-entry-id))
      (is (str/includes? lines "ns=futon3c.agency.warrant-test"))
      (is (str/includes? lines "lane=routine"))
      (is (str/includes? lines "base=14ace87"))))
  (testing "unwarranted rendering states the full-rerun consequence"
    (let [lines (warrant/render-warrant-lines
                 (warrant/normalize-warrants nil))]
      (is (str/starts-with? lines "Warrants: :unwarranted"))
      (is (str/includes? lines "full rerun")))))

(defn- lane-of [& args] (:lane (apply warrant/reviewer-lane args)))
(defn- reason-of [& args] (:reason (apply warrant/reviewer-lane args)))
(def ok-check {:warrant? true})
(def refused-check {:warrant? false :reason :stale-manifest})

(deftest reviewer-lane-reasons
  (let [w (warrant/normalize-warrants [valid-warrant])]
    (testing "no warrant => full rerun"
      (is (= :full-rerun (lane-of {:handoff/warrant-status :unwarranted} ok-check false)))
      (is (= :no-warrant (reason-of {:handoff/warrant-status :unwarranted} ok-check false)))
      (is (= :full-rerun (lane-of nil ok-check false))))
    (testing "check not {:warrant? true} => full rerun"
      (is (= :full-rerun (lane-of w refused-check false)))
      (is (= :warrant-check-failed (reason-of w refused-check true))))
    (testing "mandatory lanes => full rerun even with a passing check"
      (let [pw (warrant/normalize-warrants [valid-warrant-2])]
        (is (= :full-rerun (lane-of pw ok-check false)))
        (is (= :mandatory-lane (reason-of pw ok-check false)))))
    (testing "tests changed or undeclared => full rerun (first-run rule)"
      (is (= :full-rerun (lane-of w ok-check true)))
      (is (= :tests-changed (reason-of w ok-check true)))
      (is (= :full-rerun (lane-of w ok-check nil)))
      (is (= :tests-changed-undeclared (reason-of w ok-check nil))))
    (testing "valid warrant, passing check, declared unchanged => spot check"
      (is (= :spot-check (lane-of w ok-check false)))
      (is (= :warrant-valid-routine (reason-of w ok-check false))))))
