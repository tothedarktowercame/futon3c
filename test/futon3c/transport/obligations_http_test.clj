(ns futon3c.transport.obligations-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.obligations-reader :as reader]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(defn body [response] (json/parse-string (:body response) true))

(deftest obligations-route-projects-and-hides-closed-detail
  (with-redefs [reader/read-inputs
                (fn [_ agent at]
                  (is (= "agent-a" agent))
                  (is (= "2026-09-28T12:00:00Z" at))
                  {:promise-history [] :promise-outcomes [] :agreements [] :offers []
                   :source-counts {:promise-history 0 :promise-outcomes 0
                                   :agreements 0 :offers 0}})]
    (let [response ((handler) {:request-method :get :uri "/api/alpha/obligations"
                               :query-string "agent=agent-a&at=2026-09-28T12%3A00%3A00Z"})
          result (body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok result)))
      (is (= 0 (:ignored-count result)))
      (is (not (contains? result :ignored))))))

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
      (is (= "promise-history" (:source (body response)))))))
