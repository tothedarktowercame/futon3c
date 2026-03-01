(ns futon3c.portfolio.heartbeat-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.portfolio.heartbeat :as hb]))

;; =============================================================================
;; Effort band tests
;; =============================================================================

(deftest effort-distance-same-band
  (is (= 0 (hb/effort-distance :hard :hard)))
  (is (= 0 (hb/effort-distance :trivial :trivial))))

(deftest effort-distance-harder-than-expected
  (is (pos? (hb/effort-distance :easy :hard)))
  (is (= 2 (hb/effort-distance :easy :hard))))

(deftest effort-distance-easier-than-expected
  (is (neg? (hb/effort-distance :hard :easy)))
  (is (= -2 (hb/effort-distance :hard :easy))))

;; =============================================================================
;; Action error computation tests
;; =============================================================================

(deftest perfect-match-no-errors
  (let [bids [{:action :work-on :mission "M-foo" :effort :hard}]
        clears [{:action :work-on :mission "M-foo" :effort :hard :outcome :complete}]
        result (hb/compute-action-errors bids clears)]
    (testing "no mismatches"
      (is (empty? (:action-mismatches result)))
      (is (empty? (:effort-mismatches result)))
      (is (empty? (:outcome-mismatches result)))
      (is (empty? (:unplanned result))))
    (testing "summary counts"
      (is (= 1 (get-in result [:summary :planned])))
      (is (= 1 (get-in result [:summary :taken])))
      (is (= 0 (get-in result [:summary :not-taken])))
      (is (= 0 (get-in result [:summary :unplanned]))))))

(deftest action-not-taken
  (let [bids [{:action :work-on :mission "M-foo" :effort :hard}
              {:action :review :mission nil :effort :trivial}]
        clears [{:action :work-on :mission "M-foo" :effort :hard :outcome :complete}]
        result (hb/compute-action-errors bids clears)]
    (is (= 1 (count (:action-mismatches result))))
    (is (= :not-taken (:status (first (:action-mismatches result)))))))

(deftest effort-mismatch
  (let [bids [{:action :work-on :mission "M-foo" :effort :easy}]
        clears [{:action :work-on :mission "M-foo" :effort :hard :outcome :complete}]
        result (hb/compute-action-errors bids clears)]
    (is (= 1 (count (:effort-mismatches result))))
    (is (= 2 (:distance (first (:effort-mismatches result)))))))

(deftest outcome-mismatch
  (let [bids [{:action :work-on :mission "M-foo" :effort :hard}]
        clears [{:action :work-on :mission "M-foo" :effort :hard :outcome :partial}]
        result (hb/compute-action-errors bids clears)]
    (is (= 1 (count (:outcome-mismatches result))))
    (is (= :partial (:outcome (first (:outcome-mismatches result)))))))

(deftest unplanned-work
  (let [bids [{:action :work-on :mission "M-foo" :effort :hard}]
        clears [{:action :work-on :mission "M-foo" :effort :hard :outcome :complete}
                {:action :work-on :mission "M-bar" :effort :medium :outcome :complete}]
        result (hb/compute-action-errors bids clears)]
    (is (= 1 (count (:unplanned result))))
    (is (= "M-bar" (get-in result [:unplanned 0 :clear :mission])))))

(deftest multiple-errors-compound
  (let [bids [{:action :work-on :mission "M-foo" :effort :easy}
              {:action :review :mission nil :effort :trivial}]
        clears [{:action :work-on :mission "M-foo" :effort :hard :outcome :partial}
                {:action :work-on :mission "M-baz" :effort :medium :outcome :complete}]
        result (hb/compute-action-errors bids clears)]
    (testing "review not taken"
      (is (= 1 (count (:action-mismatches result)))))
    (testing "M-foo effort mismatch"
      (is (= 1 (count (:effort-mismatches result)))))
    (testing "M-foo outcome partial"
      (is (= 1 (count (:outcome-mismatches result)))))
    (testing "M-baz unplanned"
      (is (= 1 (count (:unplanned result)))))
    (testing "summary"
      (is (= 2 (get-in result [:summary :planned])))
      (is (= 1 (get-in result [:summary :taken])))
      (is (= 1 (get-in result [:summary :not-taken])))
      (is (= 1 (get-in result [:summary :unplanned]))))))

;; =============================================================================
;; Mode error tests
;; =============================================================================

(deftest mode-match-no-error
  (is (nil? (hb/compute-mode-error :BUILD :BUILD))))

(deftest mode-mismatch
  (let [err (hb/compute-mode-error :BUILD :CONSOLIDATE)]
    (is (some? err))
    (is (= :BUILD (:predicted err)))
    (is (= :CONSOLIDATE (:observed err)))))

(deftest mode-nil-no-error
  (is (nil? (hb/compute-mode-error nil :BUILD)))
  (is (nil? (hb/compute-mode-error nil nil))))
