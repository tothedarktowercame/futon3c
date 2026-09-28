(ns futon3c.transport.pattern-card-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.pattern-card-provider :as provider]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http])
  (:import [java.time Instant]))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(defn request [uri body]
  {:request-method :post :uri uri
   :body (json/generate-string body)})

(defn response-body [response]
  (json/parse-string (:body response) true))

(defn own-grant []
  {:hx/id http/own-acts-grant-id :hx/type :grant/record
   :hx/props
   {:grant/grantor "joe" :grant/grantee "*" :grant/basis :explicit
    :grant/scope {:description "own acts"
                  :act-kinds [:pattern-card/selection :act/withdrawal]
                  :own-acts-only true}
    :grant/interval {:from "2026-09-28T10:00:00Z"}
    :grant/source {:id "e:joe" :author "joe" :at "2026-09-28T10:00:00Z"
                   :quote "own acts"}}})

(defn fake-store []
  (let [docs (atom {http/own-acts-grant-id (own-grant)})
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
        start (Instant/now)
        selected-at (str start)
        withdrawn-at (str (.plusSeconds start 2))
        select-body {:caller "agent-a" :agent "agent-a" :session "session-a"
                     :pattern-id "card/chosen" :at selected-at
                     :idempotency-key "route-select"}]
    (provider/observe-results! "agent-a" "session-a"
                               [{:id "retrieved/fallback" :score 0.8 :rank 1}]
                               selected-at "e:retrieval" :persisted)
    (with-redefs [store/request! request!
                  provider/refresh-async! (fn [& _])
                  provider/refresh-cards-async! (fn [& _])]
      (let [selected (h (request "/api/alpha/pattern-card/select" select-body))
            selected-body (response-body selected)
            target (get-in selected-body [:record :id])]
        (is (= 200 (:status selected)))
        (is (= "agent-a" (get-in selected-body [:record :act/stamp :signer])))
        (is (= http/own-acts-grant-id
               (get-in selected-body [:record :act/stamp :authority :grant])))
        (is (= "~card/chosen"
               (:segment/value
                (provider/provider {:agent-id "agent-a" :session-id "session-a"
                                    :render-at (get-in selected-body
                                                       [:receipt :system-as-of])}))))
        (let [withdrawn
              (h (request "/api/alpha/pattern-card/withdraw"
                           {:caller "agent-a" :target target :status "effective"
                           :basis "self" :at withdrawn-at
                           :idempotency-key "route-withdraw"}))]
          (is (= 200 (:status withdrawn)))
          (is (= "~retrieved/fallback"
                 (:segment/value
                  (provider/provider {:agent-id "agent-a" :session-id "session-a"
                                      :render-at (get-in (response-body withdrawn)
                                                         [:receipt :system-as-of])})))))))))

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
  (let [{:keys [request! docs]} (fake-store)
        h (handler)]
    (with-redefs [store/request! request!]
      (let [response (h (request "/api/alpha/pattern-card/select"
                                 {:caller "agent-b" :agent "agent-a" :session "s"
                                  :pattern-id "card/a" :idempotency-key "select-b"}))]
        (is (= 403 (:status response)))
        (is (= "not-seat-owner" (:reason (response-body response)))))
      ;; Joe's operator authority is self-authenticating and performs no grant
      ;; read; remove the standing grant to prove the route does not need it.
      (swap! docs dissoc http/own-acts-grant-id)
      (is (= 200 (:status (h (request "/api/alpha/pattern-card/select"
                                      {:caller "joe" :agent "agent-a" :session "s"
                                       :pattern-id "card/a" :idempotency-key "select-joe"}))))))))

(deftest missing-grant-refuses-non-joe-write
  (let [{:keys [request! docs]} (fake-store)
        h (handler)]
    (swap! docs dissoc http/own-acts-grant-id)
    (with-redefs [store/request! request!]
      (let [response (h (request "/api/alpha/pattern-card/select"
                                 {:caller "agent-a" :agent "agent-a" :session "s"
                                  :pattern-id "card/a" :idempotency-key "no-grant"}))]
        (is (= 403 (:status response)))
        (is (= "no-grant" (:reason (response-body response))))))))

(deftest legacy-selection-author-is-the-withdrawal-target-signer
  (let [{:keys [request! docs]} (fake-store)
        h (handler)
        legacy-id "act:legacy"
        legacy {:hx/id legacy-id :hx/type :pattern-card/selection
                :hx/endpoints ["agent:agent-a" "session:s" "pattern:card/a"]
                :hx/props {:author "agent-a" :agent "agent-a" :session "s"
                           :pattern-id "card/a" :at "2026-09-28T12:00:00Z"
                           :act/harness {:kind :none :basis :producer-context
                                         :source-ref "legacy"}
                           :pattern-card/schema 1}}]
    (swap! docs assoc legacy-id legacy)
    (with-redefs [store/request! request!]
      (let [response (h (request "/api/alpha/pattern-card/withdraw"
                                 {:caller "agent-a" :target legacy-id
                                  :status "effective" :basis "self"
                                  :at "2026-09-28T13:00:00Z"
                                  :idempotency-key "legacy-withdraw"}))]
        (is (= 200 (:status response)))
        (is (= "agent-a" (get-in (response-body response)
                                  [:record :act/stamp :signer])))))))
