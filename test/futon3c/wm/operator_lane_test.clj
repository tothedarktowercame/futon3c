(ns futon3c.wm.operator-lane-test
  "Verifies the lane classifier against the independently-authored invariant
   model. The classifier (this author) and the invariants (codex-1) are separate
   artifacts, so feeding classify-item's output back through query-violations is a
   genuine author≠checker gate, not a self-confirmation."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.operator-lane :as ol]
            [futon3c.logic.wm-operator-lane-invariants :as inv]))

(def ^:private bool-keys
  [:in-joes-model? :futon-important? :risk-mode? :acknowledged?
   :operator-dependent? :framing-blocked? :predicted-important?])

(defn- all-bool-combos
  "Every assignment of the boolean attributes × both modes (2^7 × 2 = 256)."
  []
  (for [bits (range (bit-shift-left 1 (count bool-keys)))
        mode [:autonomous :supervised]]
    (assoc (into {} (map-indexed (fn [i k] [k (bit-test bits i)]) bool-keys))
           :mode mode)))

(defn- classified-trace
  "Tag each item with a unique :id and the lane classify-item assigns it."
  [items]
  (map-indexed (fn [i item]
                 (assoc item :id (keyword (str "item-" i))
                        :lane (ol/classify-item item)))
               items))

(deftest classifier-satisfies-all-invariants
  (testing "every classified attribute-combination yields zero invariant violations"
    (let [trace (classified-trace (all-bool-combos))
          v     (inv/query-violations (inv/build-db trace))]
      (is (not (inv/violations? v))
          (str "invariant violations: " (into {} (filter (comp seq val) v)))))))

(deftest classifier-reproduces-witness-lanes
  (testing "classify-item reproduces the invariant model's witness lane per item"
    (doseq [item inv/witness-trace]
      (is (= (:lane item) (ol/classify-item item))
          (str "lane mismatch for " (:id item))))))

(deftest descriptive-only-importance
  (testing "predicted-important? never changes the lane (INV-5)"
    (doseq [item (all-bool-combos)]
      (is (= (ol/classify-item (assoc item :predicted-important? false))
             (ol/classify-item (assoc item :predicted-important? true)))))))

(deftest nag-requires-full-conjunction
  (testing "dropping any nag condition demotes from :nag (INV-3 + INV-4)"
    (let [base {:in-joes-model? true :futon-important? true
                :risk-mode? true :acknowledged? true}]
      (is (= :nag (ol/classify-item base)))
      (doseq [k [:in-joes-model? :futon-important? :risk-mode? :acknowledged?]]
        (is (not= :nag (ol/classify-item (assoc base k false)))
            (str "still nag without " k))))))

(deftest silent-only-when-dischargeable
  (testing ":silent implies not operator-dependent and not framing-blocked (INV-6)"
    (doseq [item (all-bool-combos)
            :when (= :silent (ol/classify-item item))]
      (is (not (:operator-dependent? item)))
      (is (not (:framing-blocked? item))))))
