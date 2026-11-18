(ns futon3c.xiang.whistle-ask-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.xiang.whistle-ask :as wa])
  (:import [com.sun.net.httpserver HttpServer HttpHandler]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]))

(defn- with-http-response [status body delay-ms f]
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)]
    (.createContext
     server "/"
     (reify HttpHandler
       (handle [_ exchange]
         (try
           (when (pos? delay-ms) (Thread/sleep delay-ms))
           (let [bytes (.getBytes (str body) StandardCharsets/UTF_8)]
             (.sendResponseHeaders exchange status (count bytes))
             (with-open [out (.getResponseBody exchange)] (.write out bytes)))
           (catch Throwable _)
           (finally (.close exchange))))))
    (.start server)
    (try (f (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/"))
         (finally (.stop server 0)))))

(deftest ask-types-http-parse-timeout-quota-and-success
  (let [call (fn [status body]
               (with-http-response
                 status body 0
                 (fn [url]
                   (binding [wa/*whistle-url* url]
                     (wa/ask {:seat "invented" :prompt "invented"
                              :timeout-ms 100 :caller "test"})))))]
    (is (= "model/quota" (:failure_kind (call 429 "{\"ok\":false,\"error\":\"busy\"}"))))
    (is (= "model/http" (:failure_kind (call 500 "{\"ok\":false,\"error\":\"broken\"}"))))
    (is (= "model/parse" (:failure_kind (call 200 "not-json"))))
    (is (= "answer" (call 200 "{\"ok\":true,\"response\":\"answer\"}")))
    (is (= "model/timeout" (:failure_kind (call 504 "{\"ok\":false,\"error\":\"timeout\"}"))))
    (is (= "model/quota" (:failure_kind
                            (call 502 "{\"ok\":false,\"error\":\"invented rate limit hit\"}"))))
    (is (= "model/budget" (:failure_kind
                            (call 502 "{\"ok\":false,\"error\":\"too large\",\"failure-kind\":\"model/budget\",\"error-data\":{\"bound-usd\":0.09,\"max-usd\":0.08}}"))))
    (let [out (call 502 "{\"ok\":false,\"error\":\"empty\",\"failure-kind\":\"model/empty-response\",\"error-data\":{\"stop_reason\":\"max_tokens\",\"content_block_types\":[\"thinking\"],\"usage\":{\"output_tokens\":0}}}")]
      (is (= "model/empty-response" (:failure_kind out)))
      (is (= "max_tokens" (get-in out [:diagnostics :stop_reason])))))
  (let [out (with-http-response
              200 "{\"ok\":true,\"response\":\"late\"}" 100
              (fn [url]
                (binding [wa/*whistle-url* url wa/*whistle-read-slack-ms* 0]
                  (wa/ask {:seat "invented" :prompt "invented"
                           :timeout-ms 1 :caller "test"}))))]
    (is (= "model/timeout" (:failure_kind out)))))

(deftest animal-wrappers-pass-the-typed-result-and-their-caller
  (let [seen (atom [])
        failure {:failure_kind "model/quota" :http_status 429}]
    (with-redefs [wa/ask (fn [req] (swap! seen conj (:caller req)) failure)]
      (is (= failure ((requiring-resolve 'futon3c.xiang.lu-rerank/whistle-ask)
                      {:seat "invented" :prompt "invented"})))
      (is (= failure ((requiring-resolve 'futon3c.xiang.ma-occurrence/whistle-ask)
                      {:seat "invented" :prompt "invented"}))))
    (is (= ["lu-rerank" "ma-occurrence"] @seen))))
