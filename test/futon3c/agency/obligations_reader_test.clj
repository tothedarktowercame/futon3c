(ns futon3c.agency.obligations-reader-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.obligations-reader :as reader]))

(def t "2026-09-28T12:00:00Z")
(defn empty-response [path]
  (if (str/includes? path "/evidence?") {:entries [] :count 0}
      {:hyperedges [] :count 0}))

(deftest three-pages-join-in-order-and-preserve-as-of-basis
  (let [paths (atom []) page (atom 0)]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (swap! paths conj path)
                (if (str/includes? path "promise-history")
                  (case (swap! page inc)
                    1 {:entries [{:evidence/id "a"}]
                       :next-cursor {:at "2026-09-28T11:00:00Z" :id "a"}}
                    2 {:entries [{:evidence/id "b"}]
                       :next-cursor {:at "2026-09-28T10:00:00Z" :id "b"}}
                    3 {:entries [{:evidence/id "c"}]})
                  (empty-response path)))]
      (let [result (reader/read-inputs "http://store" "agent-a" t :as-of)
            history-paths (filter #(str/includes? % "promise-history") @paths)]
        (is (= ["a" "b" "c"] (mapv :evidence/id (:promise-history result))))
        (is (= 3 (get-in result [:basis :pages :promise-history])))
        (is (= 3 (get-in result [:basis :rows :promise-history])))
        (is (every? #(and (str/includes? % "system-as-of=")
                          (str/includes? % "valid-as-of=")) history-paths))
        (is (str/includes? (second history-paths) "cursor-at="))
        (is (str/includes? (second history-paths) "cursor-id="))))))

(deftest current-mode-omits-store-as-of-parameters
  (let [paths (atom [])]
    (binding [reader/*request!* (fn [_ _ path _]
                                 (swap! paths conj path) (empty-response path))]
      (let [result (reader/read-inputs "http://store" "agent-a" t :current)]
        (is (= :current (get-in result [:basis :mode])))
        (is (= t (get-in result [:basis :t])))))
    (is (every? #(not (re-find #"(?:system|valid)-as-of=" %)) @paths))
    (is (some #(str/includes? % "end=agent%3Aagent-a") @paths))))

(deftest page-cap-refuses-and-names-source
  (let [n (atom 0)]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "promise-history")
                  (let [i (swap! n inc)]
                    {:entries [{:evidence/id (str i)}]
                     :next-cursor {:at (format "2026-09-28T11:00:%02dZ" i)
                                   :id (str i)}})
                  (empty-response path)))]
      (try
        (reader/read-inputs "http://store" "a" t :current)
        (is false "expected page cap refusal")
        (catch clojure.lang.ExceptionInfo e
          (is (= :truncated-input (:reason (ex-data e))))
          (is (= :promise-history (:source (ex-data e))))
          (is (= reader/max-pages (:pages (ex-data e)))))))))

(deftest timeout-is-typed-with-source
  (binding [reader/*request!*
            (fn [& _] (throw (ex-info "query deadline" {:status 504})))]
    (try
      (reader/read-inputs "http://store" "a" t :as-of)
      (is false "expected timeout")
      (catch clojure.lang.ExceptionInfo e
        (is (= :store-timeout (:reason (ex-data e))))
        (is (= :promise-history (:source (ex-data e))))))))

(deftest fulfilment-check-survives-the-outcome-reader
  (let [check {:evidence/id "promise-check:x"
               :evidence/type :promise/fulfilment-check
               :evidence/body {:promise-id "x" :verdict :unfulfilled}}]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "promise-outcome")
                  {:entries [check] :count 1}
                  (empty-response path)))]
      (is (= [check]
             (:promise-outcomes (reader/read-inputs "http://store" "a" t :as-of)))))))

(deftest unreadable-agreement-is-kept-as-incomplete
  (let [bad {:hx/id "act:bad" :hx/type :agreement/record :hx/props {}}]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "agreement%2Frecord")
                  {:hyperedges [bad] :count 1}
                  (empty-response path)))]
      (let [result (reader/read-inputs "http://store" "a" t :as-of)]
        (is (empty? (:agreements result)))
        (is (= :unreadable-agreement
               (get-in result [:reader-incomplete 0 :reason])))))))
