(ns futon3c.wm.operator-bulletin-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [futon3c.wm.operator-bulletin :as bull]))

;; Item fixtures spanning the three lanes (attributes drive classify-item).
(def ^:private nag-item
  {:id :crux :title "Cold-conversion crux"
   :in-joes-model? true :futon-important? true :risk-mode? true :acknowledged? true})
(def ^:private brief-novel
  {:id :new-mission :title "A surfaced mission"
   :in-joes-model? true :futon-important? true :risk-mode? true :acknowledged? false}) ; novel → brief
(def ^:private brief-blocked
  {:id :blocked :title "Framing-blocked" :framing-blocked? true :operator-dependent? true})
(def ^:private silent-item
  {:id :auto :title "Autonomous fix"})

(def ^:private items [nag-item brief-novel brief-blocked silent-item])

(deftest groups-items-by-lane
  (let [b (bull/build-bulletin items :date "2026-06-05")]
    (is (= [:crux] (mapv :id (:nag b))))
    (is (= [:new-mission :blocked] (mapv :id (:brief b))))
    (is (= 1 (:silent-count b)))
    (is (= 4 (:total b)))))

(deftest silent-items-never-listed
  (testing "silent items are counted but never appear in nag/brief"
    (let [b (bull/build-bulletin items)
          listed (set (map :id (concat (:nag b) (:brief b))))]
      (is (not (contains? listed :auto)))
      (is (= 1 (:silent-count b))))))

(deftest novelty-flows-down-visible-in-bulletin
  (testing "an unacknowledged item with full nag attrs lands in :brief, not :nag"
    (let [b (bull/build-bulletin [brief-novel])]
      (is (empty? (:nag b)))
      (is (= [:new-mission] (mapv :id (:brief b)))))))

(deftest acknowledgement-is-surfaced-minus-prior
  (let [b (bull/build-bulletin items)]
    (is (= #{:crux :new-mission :blocked} (bull/surfaced-ids b)))
    (testing "newly-acknowledged excludes already-known ids"
      (is (= #{:new-mission :blocked}
             (bull/newly-acknowledged b #{:crux})))
      (is (= #{} (bull/newly-acknowledged b #{:crux :new-mission :blocked}))))))

(deftest render-shows-sections-and-silent-count
  (let [out (bull/render-bulletin (bull/build-bulletin items :date "2026-06-05"))]
    (is (str/includes? out "Awaiting Joe"))
    (is (str/includes? out "## Nag"))
    (is (str/includes? out "## Brief"))
    (is (str/includes? out "Cold-conversion crux"))
    (is (str/includes? out "1 of 4 items discharged silently"))
    (testing "silent item's title is not rendered"
      (is (not (str/includes? out "Autonomous fix"))))))

(deftest render-shows-pattern-warrant-line
  (let [item (assoc nag-item
                    :pattern-warrant
                    {:pattern-id :war-machine/advanceability
                     :gap "articulate the next hole / agree the gap, or it's not ready"})
        out (bull/render-bulletin (bull/build-bulletin [item]))]
    (is (str/includes?
         out
         "Sorry Joe, because of war-machine/advanceability: articulate the next hole"))))

(deftest empty-input-renders-cleanly
  (let [b (bull/build-bulletin [])]
    (is (= 0 (:total b)))
    (is (str/includes? (bull/render-bulletin b) "_(none)_"))))
