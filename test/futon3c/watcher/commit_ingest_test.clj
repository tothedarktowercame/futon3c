(ns futon3c.watcher.commit-ingest-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.commit-ingest :as sut]))

(deftest ingest-new-commits-records-head-cursor
  (testing "live ingestion anchors the in-memory cursor at HEAD"
    (let [recorded (atom nil)]
      (with-redefs [sut/last-indexed-commit-sha (fn [_] "old-side-tip")
                    sut/list-commits (fn [_ since]
                                       (is (= "old-side-tip" since))
                                       [{:sha "older-mainline"}
                                        {:sha "latest-non-merge"}])
                    sut/current-head-sha (fn [_] "merge-head")
                    sut/ingest-commits-batch! (fn [_]
                                                {:n-ingested 2
                                                 :latest-sha "latest-non-merge"
                                                 :n-failed 0
                                                 :n-blocks 0
                                                 :n-mana-credited 0})
                    sut/record-last-ingested! (fn [repo-label sha]
                                                (reset! recorded [repo-label sha]))]
        (is (= {:n-ingested 2
                :latest-sha "latest-non-merge"
                :n-failed 0
                :n-blocks 0
                :n-mana-credited 0}
               (sut/ingest-new-commits! {:repo-root "/tmp/repo"
                                         :repo-label "demo"
                                         :file->vars (constantly nil)})))
        (is (= ["demo" "merge-head"] @recorded))))))

(deftest ingest-new-commits-falls-back-to-latest-commit-when-head-missing
  (testing "HEAD lookup failure still preserves the previous latest-sha behaviour"
    (let [recorded (atom nil)]
      (with-redefs [sut/last-indexed-commit-sha (constantly "old")
                    sut/list-commits (constantly [{:sha "only-new"}])
                    sut/current-head-sha (constantly nil)
                    sut/ingest-commits-batch! (constantly {:n-ingested 1
                                                           :latest-sha "only-new"
                                                           :n-failed 0
                                                           :n-blocks 0
                                                           :n-mana-credited 0})
                    sut/record-last-ingested! (fn [repo-label sha]
                                                (reset! recorded [repo-label sha]))]
        (sut/ingest-new-commits! {:repo-root "/tmp/repo"
                                  :repo-label "demo"
                                  :file->vars (constantly nil)})
        (is (= ["demo" "only-new"] @recorded))))))
