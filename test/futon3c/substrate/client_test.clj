(ns futon3c.substrate.client-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.substrate.client :as sut]))

(deftest endpoint-reads-respect-authoritative-limit-and-fail-on-saturation
  (let [seen (atom nil)]
    (with-redefs-fn {#'sut/get-edn!
                     (fn [url _timeout]
                       (reset! seen url)
                       {:hyperedges [{:hx/id "one"}]})}
      #(do
         (is (= [{:hx/id "one"}] (sut/hyperedges-by-end "memory-1")))
         (is (re-find #"[?&]limit=1000(?:&|$)" @seen)))))
  (testing "a full endpoint window cannot be mistaken for complete evidence"
    (with-redefs-fn {#'sut/get-edn!
                     (fn [_url _timeout]
                       {:hyperedges (vec (repeat 2 {:hx/id "edge"}))})}
      #(let [error (try
                     (sut/hyperedges-by-end "memory-1" {:limit 2})
                     nil
                     (catch clojure.lang.ExceptionInfo t t))]
         (is (= :hyperedge-end-window-saturated
                (:error/code (ex-data error))))))))
