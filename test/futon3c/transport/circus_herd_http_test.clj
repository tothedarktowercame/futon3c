(ns futon3c.transport.circus-herd-http-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [futon3c.circus.routes :as routes]
            [futon3c.transport.http]))

(deftest herd-get-is-wired-to-the-circus-adapter
  (let [seen (atom nil)
        handle (ns-resolve 'futon3c.transport.http 'extra-routes)]
    (with-redefs [routes/herd-response
                  (fn [cfg params]
                    (reset! seen [cfg params])
                    {:status 200 :body {:lanes [] :bells [] :cursor "invented"}})]
      (let [response (handle {:request-method :get :uri "/api/alpha/circus/herd"
                              :query-string "agents=agent-a%2Cagent-b&since=2026-01-01T00%3A00%3A00Z"}
                             {:circus-routes {:fixture true}})]
        (is (= 200 (:status response)))
        (is (= "invented" (get (json/parse-string (:body response)) "cursor")))
        (is (= [{:fixture true} {"agents" "agent-a,agent-b" "since" "2026-01-01T00:00:00Z"}]
               @seen))))))
