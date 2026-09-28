(ns futon3c.transport.pattern-card-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.pattern-card-provider :as provider]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(defn request [uri body]
  {:request-method :post :uri uri
   :body (json/generate-string body)})

(defn response-body [response]
  (json/parse-string (:body response) true))

(defn fake-store []
  (let [docs (atom {})
        keys (atom {})
        next-id (atom 0)]
    {:docs docs
     :request!
     (fn [_ method path body]
       (cond
         (= method "POST")
         (let [key (:hx/idempotency-key body)]
           (when (contains? @keys key)
             (throw (ex-info "conflict" {:status 409})))
           (let [id (str "act:test-" (swap! next-id inc))
                 doc (-> body
                         (dissoc :hx/mint-id :hx/idempotency-key :hx/valid-time)
                         (assoc :hx/id id))]
             (swap! docs assoc id doc)
             (swap! keys assoc key id)
             {:ok true :hx/id id :minted? true}))

         (str/starts-with? path "/api/alpha/hyperedge/")
         (let [id (java.net.URLDecoder/decode
                   (subs path (count "/api/alpha/hyperedge/")) "UTF-8")]
           (or (get @docs id) (throw (ex-info "missing" {:status 404}))))

         (str/includes? path "type=pattern-card%2Fselection")
         {:hyperedges (vec (filter #(= :pattern-card/selection (:hx/type %))
                                   (vals @docs)))}

         (str/includes? path "type=act%2Fwithdrawal")
         {:hyperedges (vec (filter #(= :act/withdrawal (:hx/type %))
                                   (vals @docs)))}

         :else (throw (ex-info "unexpected fake store request" {:path path}))))}))

(use-fixtures :each (fn [f] (provider/reset-cache!) (f)))

(deftest select-and-withdraw-update-the-next-render
  (let [{:keys [request!]} (fake-store)
        h (handler)
        select-body {:caller "agent-a" :agent "agent-a" :session "session-a"
                     :pattern-id "card/chosen" :at "2026-09-28T13:00:00Z"
                     :idempotency-key "route-select"}]
    (provider/observe-results! "agent-a" "session-a"
                               [{:id "retrieved/fallback" :score 0.8 :rank 1}]
                               "2026-09-28T12:59:59Z" "e:retrieval" :persisted)
    (with-redefs [store/request! request!
                  provider/refresh-async! (fn [& _])
                  provider/refresh-cards-async! (fn [& _])]
      (let [selected (h (request "/api/alpha/pattern-card/select" select-body))
            selected-body (response-body selected)
            target (get-in selected-body [:record :id])]
        (is (= 200 (:status selected)))
        (is (= "~card/chosen"
               (:segment/value
                (provider/provider {:agent-id "agent-a" :session-id "session-a"
                                    :render-at "2026-09-28T13:00:01Z"}))))
        (let [withdrawn
              (h (request "/api/alpha/pattern-card/withdraw"
                          {:caller "agent-a" :target target :status "effective"
                           :basis "self" :at "2026-09-28T13:00:02Z"
                           :idempotency-key "route-withdraw"}))]
          (is (= 200 (:status withdrawn)))
          (is (= "~retrieved/fallback"
                 (:segment/value
                  (provider/provider {:agent-id "agent-a" :session-id "session-a"
                                      :render-at "2026-09-28T13:00:03Z"})))))))))

(deftest non-author-withdrawal-is-forbidden
  (let [{:keys [request!]} (fake-store)
        h (handler)]
    (with-redefs [store/request! request!]
      (let [selected (response-body
                      (h (request "/api/alpha/pattern-card/select"
                                  {:caller "agent-a" :agent "agent-a" :session "s"
                                   :pattern-id "card/a" :idempotency-key "select-a"})))
            target (get-in selected [:record :id])
            response (h (request "/api/alpha/pattern-card/withdraw"
                                 {:caller "agent-b" :target target :status "effective"
                                  :basis "self" :idempotency-key "withdraw-b"}))]
        (is (= 403 (:status response)))
        (is (= "not-author" (:reason (response-body response))))))))

(deftest malformed-and-conflicting-requests-are-typed
  (let [h (handler)]
    (is (= 400 (:status (h (request "/api/alpha/pattern-card/select"
                                    {:caller "agent-a" :agent "agent-a"})))))
    (with-redefs [store/request! (fn [& _] (throw (ex-info "conflict" {:status 409})))]
      (let [response (h (request "/api/alpha/pattern-card/select"
                                 {:caller "agent-a" :agent "agent-a" :session "s"
                                  :pattern-id "card/a" :idempotency-key "same"}))]
        (is (= 409 (:status response)))
        (is (= "idempotency-conflict" (:reason (response-body response))))))))

(deftest selection-on-another-agents-seat-is-forbidden
  (let [{:keys [request!]} (fake-store)
        h (handler)]
    (with-redefs [store/request! request!]
      (let [response (h (request "/api/alpha/pattern-card/select"
                                 {:caller "agent-b" :agent "agent-a" :session "s"
                                  :pattern-id "card/a" :idempotency-key "select-b"}))]
        (is (= 403 (:status response)))
        (is (= "not-seat-owner" (:reason (response-body response)))))
      (is (= 200 (:status (h (request "/api/alpha/pattern-card/select"
                                      {:caller "joe" :agent "agent-a" :session "s"
                                       :pattern-id "card/a" :idempotency-key "select-joe"}))))))))
