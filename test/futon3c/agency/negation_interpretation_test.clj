(ns futon3c.agency.negation-interpretation-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.negation-interpretation :as negation]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def disclosures [{:id "act:a" :chosen "drop it"}
                  {:id "act:b" :chosen "keep it"}])

(deftest resolves-only-by-id-or-standing-cardinality
  (is (= :explicit-id
         (:resolution (negation/resolve-negation-target
                       {:fragment-id "f" :fragment-text "x" :target "act:a"}
                       disclosures))))
  (is (= :target-not-in-source-jobs
         (:resolution (negation/resolve-negation-target
                       {:fragment-id "f" :fragment-text "x" :target "act:z"}
                       disclosures))))
  (is (= {:fragment-id "f" :fragment-text "x" :target "act:a"
          :resolution :single-standing}
         (negation/resolve-negation-target
          {:fragment-id "f" :fragment-text "x"} [(first disclosures)])))
  (is (= :target-unresolved
         (:resolution (negation/resolve-negation-target
                       {:fragment-id "f" :fragment-text "x"} []))))
  (is (= ["act:a" "act:b"]
         (:candidates (negation/resolve-negation-target
                       {:fragment-id "f" :fragment-text "x"} disclosures)))))

(deftest matching-disclosure-text-never-selects
  (let [result (negation/resolve-negation-target
                {:fragment-id "f" :fragment-text "drop it"} disclosures)]
    (is (= :target-ambiguous (:resolution result)))
    (is (= ["act:a" "act:b"] (:candidates result)))))

(def operator
  {:evidence/id "turn:joe" :evidence/session-id "sid"
   :evidence/origin {:kind "operator" :actor "joe"}
   :evidence/body {:event "chat-turn" :role "user" :text "drop it"}})

(defn- request [body]
  {:request-method :post :uri "/api/alpha/interpretation/negation"
   :body (io/input-stream (.getBytes (json/generate-string body) "UTF-8"))})

(defn- handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(def base-body
  {:caller "xiang" :operator-evidence-id "turn:joe"
   :fragment-id "fragment-1" :fragment-text "drop it" :analysis-version 3})

(defn- parse-response [response]
  (assoc (json/parse-string (:body response) true) :status (:status response)))

(deftest route-refuses-non-operator-without-writing
  (let [writes (atom 0)]
    (with-redefs [store/request!
                  (fn [_ method path _]
                    (if (= method "POST")
                      (do (swap! writes inc) {:ok true})
                      (if (str/ends-with? path "turn%3Ajoe")
                        (assoc-in operator [:evidence/origin :kind] "harness")
                        (throw (ex-info "missing" {:status 404})))))
                  http/read-operator-turn-source-jobs
                  (fn [_] (throw (ex-info "must not resolve" {})))]
      (let [response ((handler) (request base-body))]
        (is (= 403 (:status response)))
        (is (zero? @writes))))))

(deftest route-writes-replays-and-conflicts
  (let [stored (atom nil)
        writes (atom 0)
        fake-store
        (fn [_ method path body]
          (cond
            (and (= method "GET") (str/ends-with? path "turn%3Ajoe")) operator
            (and (= method "GET") @stored) @stored
            (= method "GET") (throw (ex-info "missing" {:status 404}))
            (= method "POST") (do (swap! writes inc) (reset! stored body)
                                   {:ok true :evidence/id (:evidence/id body)})))]
    (with-redefs [store/request! fake-store
                  http/read-operator-turn-source-jobs
                  (fn [_] {:source-jobs ["job:1"] :basis :park-resume
                           :disclosures [{:id "act:a" :source-job "job:1"}]})]
      (let [first-result (parse-response ((handler) (request base-body)))
            replay (parse-response ((handler) (request base-body)))
            conflict (parse-response
                      ((handler) (request (assoc base-body :fragment-text "changed"))))]
        (is (= 201 (:status first-result)))
        (is (= "single-standing"
               (get-in first-result [:entry :evidence/body :resolution])))
        (is (= "act:a" (get-in first-result [:entry :evidence/body :target])))
        (is (nil? (get-in first-result [:entry :evidence/body :act/stamp])))
        (is (nil? (get-in first-result [:entry :act/stamp])))
        (is (= 200 (:status replay)))
        (is (true? (:existing? replay)))
        (is (= 409 (:status conflict)))
        (is (= "idempotency-conflict" (:reason conflict)))
        (is (= 1 @writes))))))

(deftest route-validates-required-fields
  (let [response ((handler) (request (dissoc base-body :fragment-id)))]
    (is (= 400 (:status response)))
    (is (= "missing-field" (:reason (json/parse-string (:body response) true))))))
