(ns futon3c.arxana.benefit-names-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn] [clojure.java.io :as io]
            [futon3c.arxana.benefit-names :as names]
            [futon3c.arxana.records :as records]))
(deftest explicit-rows-and-unknown-codes
  (let [ben (java.io.File/createTempFile "invented-ben" ".py")
        table (java.io.File/createTempFile "invented-language" ".md")]
    (try
      (spit ben "BEN={'L':'L Learning','harm':'Harm avoided'}\n")
      (spit table "# Invented fixture\n| L1 | example/pattern | learn a move | ignored item |\n| H1 | lost records | 2 |\n")
      (let [parsed (names/parse-sources (.getPath ben) [(.getPath table)])
            benefits [{:id "invented-benefit" :asserted-content {:benefit/line "L1"}}
                      {:id "invented-unknown" :asserted-content {:benefit/line "Z9"}}]
            proposal (names/manifest benefits parsed) a (first (:attestations proposal))]
        (is (= "learn a move" (get-in parsed ["L1" :gloss])))
        (is (= 2 (get-in parsed ["L1" :source :line])))
        (is (= (names/sha256 (.getPath table)) (get-in parsed ["L1" :source :sha256])))
        (is (= "harm" (get-in parsed ["H1" :trunk])))
        (is (= ["Z9"] (:unnamed proposal)))
        (is (= 1 (:named proposal)))
        (is (= :parser (:basis a)))
        (is (= a (records/make-attestation a)))
        (is (= proposal (names/manifest benefits parsed))))
      (finally (.delete ben) (.delete table)))))
(deftest saved-proposal-is-explicitly-not-live
  (let [p (edn/read-string (slurp (io/resource "arxana/benefit-names-proposed-attestations.edn")))
        labels (edn/read-string (slurp (io/resource "arxana/benefit-names.edn")))]
    (is (:dry-run p))
    (is (= (count labels) (:named p) (count (:attestations p))))
    (is (every? #(= % (records/make-attestation %)) (:attestations p)))
    (is (every? #(= :unverified (:standing %)) (:attestations p)))
    (is (= :source-file (:status (names/label (first (keys labels))))))
    (is (nil? (names/label "invented-unknown-code")))))
