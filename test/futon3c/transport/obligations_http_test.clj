(ns futon3c.transport.obligations-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.obligations-reader :as reader]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(defn body [response] (json/parse-string (:body response) true))

(defn population [agent at mode]
  {:question {:agent agent :at-or-cutoff at
              :kinds #{:promise :agreement} :mode mode}
   :sources [{:kind :evidence :filter {:tags ["promise-history"]}
              :rows-fetched 0 :rows-used 0 :pages 1 :page-limit 1000 :complete? true}
             {:kind :evidence
              :filter {:tags ["promise-outcome"]
                       :types [:promise/fulfilled :promise/lapsed
                               :promise/fulfilment-check]}
              :rows-fetched 0 :rows-used 0 :pages 1 :page-limit 1000 :complete? true}
             {:kind :hyperedge
              :filter {:type :agreement/record :end (str "agent:" agent)}
              :rows-fetched 0 :rows-used 0 :pages 1 :page-limit 1000 :complete? true}
             {:kind :hyperedge :filter {:type :offer/record :ids []}
              :rows-fetched 0 :rows-used 0 :pages 0 :page-limit 1000 :complete? true}]
   :excluded [{:reason :incomplete :rows 0 :scope :repository-wide}]
   :read (if (= mode :as-of)
           {:mode :as-of :system-as-of at :valid-as-of at}
           {:mode :current :system-as-of :unpinned :cutoff at
            :started-at at :finished-at at})})

(deftest obligations-route-projects-and-hides-closed-detail
  (with-redefs [reader/read-inputs
                (fn [_ agent at mode]
                  (is (= "agent-a" agent))
                  (is (= "2026-09-28T12:00:00Z" at))
                  (is (= :as-of mode))
                  {:promise-history [] :promise-outcomes [] :agreements [] :offers []
                   :basis {:mode mode :t at
                           :pages {:promise-history 1 :promise-outcomes 1
                                   :agreements 1 :offers 0}
                           :rows {:promise-history 0 :promise-outcomes 0
                                  :agreements 0 :offers 0}
                           :population (population agent at mode)}})]
    (let [response ((handler) {:request-method :get :uri "/api/alpha/obligations"
                               :query-string "agent=agent-a&at=2026-09-28T12%3A00%3A00Z"})
          result (body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok result)))
      (is (= "as-of" (get-in result [:basis :mode])))
      (is (= 0 (:ignored-count result)))
      (is (not (contains? result :ignored))))))

(deftest obligations-route-refuses-a-substituted-population-without-answer-rows
  (with-redefs [reader/read-inputs
                (fn [_ _ at _]
                  {:promise-history [] :promise-outcomes [] :agreements [] :offers []
                   :basis {:mode :current :t at :pages {} :rows {}
                           :population (population "agent-b" at :current)}})]
    (let [response ((handler) {:request-method :get :uri "/api/alpha/obligations"
                               :query-string "agent=agent-a&at=2026-09-28T12%3A00%3A00Z"})
          result (body response)]
      (is (= 500 (:status response)))
      (is (= "population-mismatch" (:reason result)))
      (is (every? (set (:reasons result))
                  ["agent-mismatch" "read-axis-mismatch"]))
      (is (not (contains? result :owes)))
      (is (not (contains? result :owed))))))

(deftest obligations-route-refusals
  (is (= 400 (:status ((handler) {:request-method :get
                                  :uri "/api/alpha/obligations"}))))
  (is (= 400 (:status ((handler) {:request-method :get
                                  :uri "/api/alpha/obligations"
                                  :query-string "agent=a&at=bad"}))))
  (with-redefs [reader/read-inputs
                (fn [& _] (throw (ex-info "full" {:reason :truncated-input
                                                   :source :promise-history})))]
    (let [response ((handler) {:request-method :get :uri "/api/alpha/obligations"
                               :query-string "agent=a"})]
      (is (= 409 (:status response)))
      (is (= "promise-history" (:source (body response))))))
  (with-redefs [reader/read-inputs
                (fn [& _] (throw (ex-info "timeout" {:reason :store-timeout
                                                      :source :promise-outcomes})))]
    (let [response ((handler) {:request-method :get :uri "/api/alpha/obligations"
                               :query-string "agent=a"})]
      (is (= 504 (:status response)))
      (is (= "store-timeout" (:reason (body response)))))))
