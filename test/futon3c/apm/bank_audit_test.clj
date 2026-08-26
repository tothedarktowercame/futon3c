(ns futon3c.apm.bank-audit-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.bank-audit :as bank-audit]))

(defn- write-terminal!
  [campaign-dir frame receipt]
  (let [terminal-dir (io/file campaign-dir frame "terminal")]
    (.mkdirs terminal-dir)
    (spit (io/file terminal-dir "frame-terminal.edn") (pr-str receipt))))

(defn- receipt
  [frame problem-id outcome head]
  {:frame/id frame
   :problem/id problem-id
   :problem/outcome outcome
   :workspace/terminal-heads {:solver head}})

(defn- with-temp-dir
  [f]
  (let [path (java.nio.file.Files/createTempDirectory
              "bank-audit-test-"
              (make-array java.nio.file.attribute.FileAttribute 0))
        dir (.toFile path)]
    (try
      (f dir)
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file true))))))

(deftest classifies-solved-terminal-receipts
  (with-temp-dir
    (fn [campaign-dir]
      (write-terminal! campaign-dir "campaign-f35"
                       (receipt "f35" "a95J04" :solved "different-head"))
      (write-terminal! campaign-dir "campaign-f36"
                       (receipt "f36" "a95J05" :solved "banked-head"))
      (write-terminal! campaign-dir "campaign-f37"
                       (receipt "f37" "a95J06" :partial "partial-head"))
      (write-terminal! campaign-dir "campaign-f38"
                       (receipt "f38" "a95J07" :solved "missing-head"))
      (let [content {"different-head" "new proof"
                     "banked-head" "same proof"
                     "master" {"a95J04" "old proof"
                               "a95J05" "same proof"
                               "a95J07" "old proof"}}
            read-at-rev (fn [rev path]
                          (let [problem-id (second (re-find #"problems/([^/]+)/" path))]
                            (if (= rev "master")
                              (get-in content [rev problem-id])
                              (get content rev))))
            results (bank-audit/unbanked-solved
                     {:campaign-dir (.getPath campaign-dir)
                      :read-at-rev read-at-rev})]
        (is (= [{:frame "f35" :problem-id "a95J04"
                 :head "different-head" :status :unbanked}
                {:frame "f36" :problem-id "a95J05"
                 :head "banked-head" :status :banked}
                {:frame "f38" :problem-id "a95J07"
                 :head "missing-head" :status :head-unresolvable}]
               results))
        ;; Regression: banking copies content into a new commit, so reachability
        ;; cannot distinguish this banked proof from an unbanked solver head.
        (is (= :banked (:status (second results))))
        (is (not-any? #(= "f37" (:frame %)) results))))))

(deftest empty-campaign-has-no-unbanked-solves
  (with-temp-dir
    (fn [campaign-dir]
      (is (= [] (bank-audit/unbanked-solved
                 {:campaign-dir (.getPath campaign-dir)
                  :read-at-rev (constantly nil)}))))))
