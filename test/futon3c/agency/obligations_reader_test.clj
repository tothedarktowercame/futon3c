(ns futon3c.agency.obligations-reader-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.obligations-reader :as reader]))

(def t "2026-09-28T12:00:00Z")

(deftest agreement-query-uses-agent-endpoint
  (let [paths (atom [])]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (swap! paths conj path)
                (cond
                  (str/includes? path "/evidence?") {:entries [] :count 0}
                  :else {:hyperedges [] :count 0}))]
      (reader/read-inputs "http://store" "agent-a" t))
    (is (some #(str/includes? % "type=agreement%2Frecord") @paths))
    (is (some #(str/includes? % "end=agent%3Aagent-a") @paths))
    (is (every? #(and (str/includes? % "system-as-of=")
                      (str/includes? % "valid-as-of=")) @paths))))

(deftest full-page-refuses-instead-of-projecting-partial-input
  (binding [reader/*request!*
            (fn [_ _ path _]
              (if (str/includes? path "promise-history")
                {:entries (vec (repeat reader/page-limit {:evidence/id "x"}))
                 :count reader/page-limit}
                {:entries [] :count 0}))]
    (try
      (reader/read-inputs "http://store" "a" t)
      (is false "expected truncation refusal")
      (catch clojure.lang.ExceptionInfo e
        (is (= :truncated-input (:reason (ex-data e))))
        (is (= :promise-history (:source (ex-data e))))))))

(deftest unreadable-agreement-is-kept-as-incomplete
  (let [bad {:hx/id "act:bad" :hx/type :agreement/record :hx/props {}}]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (cond
                  (str/includes? path "/evidence?") {:entries [] :count 0}
                  (str/includes? path "agreement%2Frecord") {:hyperedges [bad] :count 1}
                  :else {:hyperedges [] :count 0}))]
      (let [result (reader/read-inputs "http://store" "a" t)]
        (is (empty? (:agreements result)))
        (is (= :unreadable-agreement
               (get-in result [:reader-incomplete 0 :reason])))))))
