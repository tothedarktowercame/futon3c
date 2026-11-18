(ns futon3c.transport.circus-mission-session-patterns-http-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [futon3c.circus.routes :as routes]
            [futon3c.transport.http]))

(deftest mission-session-patterns-get-is-wired
  (let [seen (atom nil)
        handle (ns-resolve 'futon3c.transport.http 'extra-routes)]
    (with-redefs [routes/mission-session-patterns-response
                  (fn [cfg params]
                    (reset! seen [cfg params])
                    {:status 200 :body {:mission "M-demo" :patterns [] :unindexed true :turns_read 0}})]
      (let [response (handle {:request-method :get :uri "/api/alpha/circus/mission-session-patterns"
                              :query-string "mission=M-demo&n=2"}
                             {:circus-routes {:fixture true}})]
        (is (= 200 (:status response)))
        (is (= true (get (json/parse-string (:body response)) "unindexed")))
        (is (= [{:fixture true} {"mission" "M-demo" "n" "2"}] @seen))))))
