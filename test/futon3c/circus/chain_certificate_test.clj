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
   :focus-link ["gcc-0"]
   :priority {:items [{:item-id "M-one" :status :ranked
                       :cutoff-status :within-cutoff}]}
   :record/sha256 "fixture-record-sha"})

(defn- check [tag record pins]
  (let [file (io/file (System/getProperty "java.io.tmpdir")
                      (str "chain-certificate-" tag "-" (System/nanoTime) ".lean"))]
    (spit file (certificate/render record pins))
    (:exit (certificate/lean-check file))))

(deftest generated-chain-theorems-pass-and-negative-controls-fail-in-lean
  (let [good (fixture)]
    (testing "complete fixture passes"
      (is (zero? (check "good" good ["gcc-0"]))))
    (testing "unranked item fails the fixed ranking theorem"
      (is (not (zero? (check "unranked"
                             (assoc-in good [:priority :items 0 :status] :not-ranked)
                             ["gcc-0"])))))
    (testing "focus id outside the supplied pins fails the fixed link theorem"
      (is (not (zero? (check "unpinned" good ["some-other-id"])))))
    (testing "failed run verification fails the fixed run theorem"
      (is (not (zero? (check "unverified"
                             (assoc-in good [:run :verification :ok?] false)
                             ["gcc-0"])))))))
