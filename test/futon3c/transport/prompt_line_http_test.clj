(ns futon3c.transport.prompt-line-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(deftest prompt-line-routes-require-an-exact-seat
  (let [h (handler)]
    (is (= 400 (:status (h {:request-method :get :uri "/api/alpha/prompt-line"}))))
    (is (= 400 (:status (h {:request-method :get :uri "/api/alpha/prompt-line/last"
                            :query-string "agent=a"}))))))

(deftest prompt-line-routes-render-and-read-last
  (let [h (handler)
        rendered {:prompt "$~x/y> " :segments [] :omitted []
                  :rendered-at "2026-09-27T20:00:00Z"}]
    (with-redefs [prompt-line/render! (fn [ctx]
                                       (is (= ["a" "s"]
                                              [(:agent-id ctx) (:session-id ctx)]))
                                       rendered)
                  prompt-line/last-render (fn [a s]
                                            (when (= ["a" "s"] [a s]) rendered))]
      (doseq [uri ["/api/alpha/prompt-line" "/api/alpha/prompt-line/last"]]
        (let [response (h {:request-method :get :uri uri
                           :query-string "agent=a&session=s"})]
          (is (= 200 (:status response)))
          (is (= "$~x/y> " (:prompt (json/parse-string (:body response) true)))))))))
