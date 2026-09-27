(ns futon3c.diagramprover.wm-wire-r3-flight-ask-r3-prompt-library-root-test
  "Real calls with IO isolated; no live record carries both ends.
  See support/live-records-read for the pinned record survey."
  (:require [clojure.string :as str]
            [futon3c.diagramprover.wm-wire-ask-library-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-small-support :as support]))
(defn check [] (support/observe :prompt (fn [v _] v)))
(def wire {:second-layer {:test `prompt-records-root-without-reading-patterns :kind :record
                          :product [:text] :intervention :before-reader}
           :wire [:r3-flight-ask :r3-prompt :library-root] :kind :witnessed-hermetically
           :test `the-writer-reaches-the-reader :check check
           :live-records-read support/live-records-read})
(deftest the-writer-reaches-the-reader
  (is (w/received? (check))))
(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :prompt (fn [_ _] {:status :absent :reason :not-carried}))))))
(deftest different-carrier-before-reader-fails
  (let [o (support/observe :prompt (fn [_ other] other))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest live-records-do-not-carry-both-ends
  (support/assert-live-records))

(deftest prompt-records-root-without-reading-patterns
  ;; want_interpretation.clj:400-409 interpolates the path, no library IO.
  (products/with-libraries
    (fn [a b]
      (let [[x y] (products/prompts a b)]
        (is (= (:written x) (:written y) (:carrier x)))
        (is (= (:carrier x) (assoc (:carrier y) :library-root a)))
        (is (str/includes? (:text x) a))
        (is (str/includes? (:text y) b))
        (is (not= (:text x) (:text y)))
        (is (= (:text x) (str/replace (:text y) b a)))
        (is (not-any? #(str/includes? (str (:text x) (:text y)) %)
                      ["Pattern Alpha" "Pattern Beta" "a.flexiarg" "b.flexiarg"]))
        (prn :library-excerpts (mapv #(re-find #"captured library under .*? and append conformant" (:text %)) [x y]))))))
