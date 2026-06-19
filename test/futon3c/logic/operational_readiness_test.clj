(ns futon3c.logic.operational-readiness-test
  "Tests for the per-family fresh-fire diagnostic. Mission:
   M-bounded-in-flight-state INSTANTIATE D-06."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.operational-readiness :as op-rd]
            [futon3c.logic.probe :as probe]))

(defn- with-isolated-registry
  "Run BODY-FN with a fresh empty registry, restoring original on exit."
  [body-fn]
  (let [original @probe/family-check-fns]
    (try
      (reset! probe/family-check-fns {})
      (body-fn)
      (finally
        (reset! probe/family-check-fns original)))))

(deftest snapshot-empty-when-no-registrations
  (with-isolated-registry
    (fn []
      (is (= [] (op-rd/snapshot))))))

(deftest snapshot-fires-each-registered-check
  (with-isolated-registry
    (fn []
      (swap! probe/family-check-fns assoc
             :test/ok-fam     (fn [_] {:outcome :ok :detail {:n 1}})
             :test/violate    (fn [_] {:outcome :violation :detail {:n 2}})
             :test/inactive   (fn [_] {:outcome :inactive :detail {:n 3}})
             :test/throws     (fn [_] (throw (ex-info "boom" {}))))
      (let [snap (op-rd/snapshot)]
        (is (= 4 (count snap)))
        (let [by-id (into {} (map (juxt :family-id :outcome) snap))]
          (is (= :ok        (get by-id :test/ok-fam)))
          (is (= :violation (get by-id :test/violate)))
          (is (= :inactive  (get by-id :test/inactive)))
          (is (= :error     (get by-id :test/throws))
              "exception in check-fn renders :error, not propagation"))))))

(deftest snapshot-surfaces-graduated-tier
  (with-isolated-registry
    (fn []
      (swap! probe/family-check-fns assoc
             :test/graduated
             (fn [_] {:outcome :violation
                      :detail {:max-tier :high :max-pressure 2.5}}))
      (let [r (first (op-rd/snapshot))]
        (is (= :high (:tier r)) "graduated-drive families surface :tier")))))

(deftest ready-returns-true-only-for-ok
  (with-isolated-registry
    (fn []
      (swap! probe/family-check-fns assoc
             :test/green (fn [_] {:outcome :ok :detail {}})
             :test/red   (fn [_] {:outcome :violation :detail {}}))
      (is (true?  (op-rd/ready? :test/green)))
      (is (false? (op-rd/ready? :test/red)))
      (is (nil?   (op-rd/ready? :test/missing-from-registry))))))

(deftest enabled-but-not-firing-cleanly-lists-violators
  (with-isolated-registry
    (fn []
      (swap! probe/family-check-fns assoc
             :test/clean  (fn [_] {:outcome :ok :detail {}})
             :test/dirty1 (fn [_] {:outcome :violation :detail {}})
             :test/dirty2 (fn [_] {:outcome :inactive :detail {}}))
      (let [violators (set (op-rd/enabled-but-not-firing-cleanly))]
        (is (= #{:test/dirty1 :test/dirty2} violators))))))
