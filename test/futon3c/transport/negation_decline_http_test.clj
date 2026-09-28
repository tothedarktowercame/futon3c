(ns futon3c.transport.negation-decline-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http])
  (:import [java.net URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.util UUID]))

(def interpretation-id "interpretation-negation:test")
(defn derived-id [prefix value]
  (str prefix (UUID/nameUUIDFromBytes (.getBytes value StandardCharsets/UTF_8))))
(def routing-id (derived-id "interpretation-negation-routing:" interpretation-id))
(def interpretation
  {:evidence/id interpretation-id :evidence/type :interpretation/negation
   :evidence/session-id "sid" :evidence/body {:target "act:d"}})
(def routing
  {:evidence/id routing-id :evidence/type :interpretation/negation-routing
   :evidence/body {:status :routed :orchestrator "claude-17"
                   :target "act:d" :source-job "invoke:j"}})

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn request [body]
  {:request-method :post :uri "/api/alpha/interpretation/negation/decline"
   :body (json/generate-string body)})
(defn response-body [response]
  (assoc (json/parse-string (:body response) true) :status (:status response)))

(defn fake-store [withdrawn?]
  (let [docs (atom {interpretation-id interpretation routing-id routing})
        writes (atom 0)]
    {:docs docs :writes writes
     :request!
     (fn [_ method path body]
       (cond
         (and (= method "GET") (str/includes? path "type=act%2Fwithdrawal"))
         {:hyperedges (if withdrawn?
                        [{:hx/id "act:w" :hx/type :act/withdrawal
                          :hx/props {:target "act:d" :status :effective}}] [])}
         (= method "GET")
         (let [id (URLDecoder/decode (last (str/split path #"/")) "UTF-8")]
           (or (get @docs id) (throw (ex-info "missing" {:status 404}))))
         (= method "POST")
         (do (swap! writes inc) (swap! docs assoc (:evidence/id body) body)
             {:ok true :evidence/id (:evidence/id body)})))}))

(deftest orchestrator-declines-idempotently
  (let [{:keys [request! writes]} (fake-store false)]
    (with-redefs [store/request! request!]
      (let [body {:caller "claude-17" :interpretation-id interpretation-id
                  :reason "The implementation choice is still required."}
            first (response-body ((handler) (request body)))
            replay (response-body ((handler) (request body)))
            conflict (response-body ((handler) (request (assoc body :reason "different"))))]
        (is (= 201 (:status first)))
        (is (= "claude-17" (get-in first [:entry :evidence/author])))
        (is (nil? (get-in first [:entry :act/stamp])))
        (is (= 200 (:status replay)))
        (is (true? (:existing? replay)))
        (is (= 409 (:status conflict)))
        (is (= 1 @writes))))))

(deftest decline-refusals-write-nothing
  (doseq [[body withdrawn? status reason]
          [[{:caller "other" :interpretation-id interpretation-id :reason "no"}
            false 403 "not-orchestrator"]
           [{:caller "claude-17" :interpretation-id interpretation-id :reason "no"}
            true 409 "already-withdrawn"]
           [{:caller "claude-17" :interpretation-id interpretation-id :reason " "}
            false 400 "blank-reason"]]]
    (let [{:keys [request! writes]} (fake-store withdrawn?)]
      (with-redefs [store/request! request!]
        (let [result (response-body ((handler) (request body)))]
          (is (= status (:status result)))
          (is (= reason (:reason result)))
          (is (zero? @writes)))))))
