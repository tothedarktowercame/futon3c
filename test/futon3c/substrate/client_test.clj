(ns futon3c.substrate.client-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.substrate.client :as sut]))

(deftest hyperedge-read-follows-server-cursor
  (let [urls (atom [])
        get-edn-var (ns-resolve 'futon3c.substrate.client 'get-edn!)]
    (with-redefs-fn
      {#'sut/configured-url (constantly "http://substrate.test")
       get-edn-var
       (fn [url _timeout-ms]
         (swap! urls conj url)
         (if (str/includes? url "after=edge-1")
           {:hyperedges [{:hx/id "edge-2"}]}
           {:hyperedges [{:hx/id "edge-1"}]
            :next-cursor "edge-1"}))}
      #(let [rows (sut/hyperedges-by-type :test/edge {:limit 10})]
         (is (= ["edge-1" "edge-2"] (mapv :hx/id rows)))
         (is (= 2 (count @urls)))
         (is (every? (fn [url] (re-find #"limit=(?:10|9)" url)) @urls))
         (is (false? (sut/partial-result? rows)))))))

(deftest hyperedge-budget-exhaustion-is-marked-partial
  (let [get-edn-var (ns-resolve 'futon3c.substrate.client 'get-edn!)]
    (with-redefs-fn
      {#'sut/configured-url (constantly "http://substrate.test")
       get-edn-var
       (fn [_url _timeout-ms]
         {:hyperedges [{:hx/id "edge-1"}]
          :next-cursor "edge-1"})}
      #(let [rows (sut/hyperedges-by-type
                   :test/edge {:limit 10 :request-budget 1})]
         (is (= ["edge-1"] (mapv :hx/id rows)))
         (is (sut/partial-result? rows))
         (is (= "edge-1" (:next-cursor (meta rows))))
         (is (= 1 (:request-budget (meta rows))))))))
