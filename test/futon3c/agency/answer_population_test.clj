(ns futon3c.agency.answer-population-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.answer-population :as population]))

(def t "2026-09-28T12:00:00Z")

(defn declaration [mode]
  {:question {:agent "agent-a" :at-or-cutoff t
              :kinds #{:promise :agreement} :mode mode}
   :sources
   [{:kind :evidence :filter {:tags ["promise-history"]}
     :rows-fetched 2 :rows-used 2 :pages 1 :page-limit 1000 :complete? true}
    {:kind :evidence
     :filter {:tags ["promise-outcome"]
              :types [:promise/fulfilled :promise/lapsed :promise/fulfilment-check]}
     :rows-fetched 2 :rows-used 1 :pages 1 :page-limit 1000 :complete? true}
    {:kind :hyperedge
     :filter {:type :agreement/record :end "agent:agent-a"}
     :rows-fetched 1 :rows-used 1 :pages 1 :page-limit 1000 :complete? true}
    {:kind :hyperedge :filter {:type :offer/record :ids ["act:offer"]}
     :rows-fetched 1 :rows-used 1 :pages 1 :page-limit 1000 :complete? true}]
   :excluded [{:reason :incomplete :rows 1 :scope :repository-wide}]
   :read (if (= :current mode)
           {:mode :current :system-as-of :unpinned :cutoff t
            :started-at t :finished-at t}
           {:mode :as-of :system-as-of t :valid-as-of t})})

(deftest declarations-match-their-own-questions
  (doseq [mode [:current :as-of]]
    (let [p (declaration mode)]
      (is (= {:status :same}
             (population/same-population? (:question p) p))))))

(deftest every-population-substitution-has-a-typed-reason
  (let [base (declaration :as-of)
        question (:question base)
        mutations
        [[:agent-mismatch #(assoc-in % [:question :agent] "agent-b")]
         [:time-mismatch #(assoc-in % [:question :at-or-cutoff]
                                     "2026-09-28T11:00:00Z")]
         [:kinds-mismatch #(assoc-in % [:question :kinds] #{:promise})]
         [:source-missing #(update % :sources pop)]
         [:filter-mismatch #(assoc-in % [:sources 2 :filter :end] "agent:agent-b")]
         [:incomplete-source #(assoc-in % [:sources 0 :complete?] false)]
         [:read-axis-mismatch #(assoc % :read
                                      {:mode :current :system-as-of :unpinned
                                       :cutoff t :started-at t :finished-at t})]]]
    (doseq [[reason mutate] mutations]
      (testing (name reason)
        (is (some #{reason}
                  (:reasons (population/same-population? question (mutate base)))))))))

(deftest declaration-is-closed
  (try
    (population/validate! (assoc (declaration :current) :answer []))
    (is false "expected an unexpected-key refusal")
    (catch clojure.lang.ExceptionInfo e
      (is (= :unexpected-key (:reason (ex-data e)))))))
