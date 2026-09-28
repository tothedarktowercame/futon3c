(ns futon3c.transport.prompt-line-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.agency.turn-notice :as turn-notice]
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

(deftest turn-notice-route-validates-and-does-not-consume-on-prompt-get
  (turn-notice/reset-state!)
  (let [h (handler)
        post (fn [body]
               (h {:request-method :post :uri "/api/alpha/turn-notice"
                   :body (java.io.ByteArrayInputStream.
                          (.getBytes (json/generate-string body) "UTF-8"))}))]
    (is (= 403 (:status (post {:caller "claude-17" :agent "a" :session "s"
                               :notice-id "n1" :kind "unresolved"}))))
    (is (= 400 (:status (post {:caller "xiang" :agent "a" :session "s"
                               :notice-id "n1" :kind "wrong"}))))
    (is (= 400 (:status (post {:caller "xiang" :agent "a" :session "s"
                               :notice-id "n1" :kind "effect"}))))
    (is (= 200 (:status (post {:caller "xiang" :agent "a" :session "s"
                               :notice-id "n1" :kind "effect"
                               :effect-id "act:e1"}))))
    (is (= 200 (:status
                (h {:request-method :get :uri "/api/alpha/prompt-line"
                    :query-string "agent=a&session=s"}))))
    (is (= "withdraw inferred: effect act:e1 (undo to reverse)"
           (:notice/text (turn-notice/take! "a" "s"))))))
