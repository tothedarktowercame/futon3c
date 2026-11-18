(ns futon3c.circus.chain-certificate-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.circus.chain-certificate :as certificate]))

(def animals [:驴 :馬 :狗 :象 :mentions :猴])

(defn- fixture []
  {:turn {:id "turn-fixture"}
   :run {:run-id "run-fixture" :verification {:ok? true}
         :focus/version {:schema [:hou/focus 1] :digest "digest"}}
   :gcc (into {} (map-indexed
                  (fn [i animal]
                    [animal {:status :present :clusion-ids [(str "gcc-" i)]}])
                  animals))
   :items ["M-one"]
   :ma-reading {:proposed-pattern-ids ["p-one"]
                :patterns [{:pattern "p-one" :status :completed}]
                :legitimate-absence-reason nil
                :commit-state :post-1ae694834}
   :focus-link ["gcc-0"]
   :priority {:items [{:item-id "M-one" :status :ranked
                       :cutoff-status :within-cutoff}]}
   :record/sha256 "fixture-record-sha"})

(defn- check-result [tag record pins]
  (let [file (io/file (System/getProperty "java.io.tmpdir")
                      (str "chain-certificate-" tag "-" (System/nanoTime) ".lean"))]
    (spit file (certificate/render record pins))
    (try (certificate/lean-check file)
         (finally (io/delete-file file true)))))

(defn- check [tag record pins]
  (:exit (check-result tag record pins)))

(deftest generated-chain-theorems-pass-and-negative-controls-fail-in-lean
  (let [good (fixture)]
    (testing "complete fixture passes"
      (is (zero? (check "good" good ["gcc-0"]))))
    (testing "unranked item fails the fixed ranking theorem"
      (is (not (zero? (check "unranked"
                             (assoc-in good [:priority :items 0 :status] :not-ranked)
                             ["gcc-0"])))))
    (testing "a ranked item outside the cutoff fails the ranking theorem"
      (is (not (zero? (check "outside-cutoff"
                             (assoc-in good [:priority :items 0 :cutoff-status] :outside-cutoff)
                             ["gcc-0"])))))
    (testing "focus id outside the supplied pins fails the fixed link theorem"
      (is (not (zero? (check "unpinned" good ["some-other-id"])))))
    (testing "a turn that reached no focus pin cannot pass vacuously"
      (is (not (zero? (check "no-focus" (assoc good :focus-link []) ["gcc-0"])))))
    (testing "a turn with no items cannot pass vacuously"
      (is (not (zero? (check "no-items" (assoc good :items []) ["gcc-0"])))))
    (testing "failed run verification fails the fixed run theorem"
      (is (not (zero? (check "unverified"
                             (assoc-in good [:run :verification :ok?] false)
                             ["gcc-0"])))))
    (testing "an unread proposed pattern fails the Ma theorem in Lean"
      (let [result (check-result "ma-unread"
                                 (assoc-in good [:ma-reading :patterns 0 :status]
                                           :not-completed)
                                 ["gcc-0"])]
        (is (not (zero? (:exit result))))
        (is (re-find #"ma_read_every_proposed_pattern.*depends on axioms: \[sorryAx\]"
                     (:out result)))
        (is (not (re-find #"turn_items_ranked_within_cutoff.*depends on axioms: \[sorryAx\]"
                          (:out result))))))))
