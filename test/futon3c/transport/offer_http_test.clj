(ns futon3c.transport.offer-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.offer-provider :as offer-provider]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(defn request [body]
  {:request-method :post :uri "/api/alpha/offer"
   :body (json/generate-string body)})

(defn body [response]
  (json/parse-string (:body response) true))

(defn grant [id grantee]
  {:hx/id id :hx/type :grant/record
   :hx/props {:grant/grantor "joe" :grant/grantee grantee :grant/basis :explicit
              :grant/scope {:description "own offers" :act-kinds [:offer/record]
                            :own-acts-only true}
              :grant/interval {:from "2026-09-28T10:00:00Z"}
              :grant/source {:id "e:joe" :author "joe"
                             :at "2026-09-28T10:00:00Z" :quote "offers"}}})

(defn fake-store
  ([] (fake-store true))
  ([with-grant?]
   (fake-store with-grant? (fn [id] (grant id "*"))))
  ([with-grant? make-grant]
   (let [grant-id "act:offer-grant"
         docs (atom (cond-> {} with-grant? (assoc grant-id (make-grant grant-id))))
         keys (atom {})
         calls (atom [])
         next-id (atom 0)]
     {:calls calls
      :request!
      (fn [_ method path value]
        (swap! calls conj [method path value])
        (cond
          (= method "POST")
          (let [key (:hx/idempotency-key value)]
            (if-let [id (get @keys key)]
              {:ok true :hx/id id :no-op? true}
              (let [id (str "act:offer-" (swap! next-id inc))
                    doc (-> value
                            (dissoc :hx/mint-id :hx/idempotency-key :hx/valid-time)
                            (assoc :hx/id id))]
                (swap! docs assoc id doc)
                (swap! keys assoc key id)
                {:ok true :hx/id id :minted? true})))

          (str/starts-with? path "/api/alpha/hyperedge/")
          (let [id (java.net.URLDecoder/decode
                    (subs path (count "/api/alpha/hyperedge/")) "UTF-8")]
            (or (get @docs id) (throw (ex-info "missing" {:status 404}))))

          (str/includes? path "type=grant%2Frecord")
          {:hyperedges (vec (filter #(= :grant/record (:hx/type %)) (vals @docs)))}

          (str/includes? path "type=offer%2Frecord&end=session%3A")
          {:hyperedges (vec (filter #(= :offer/record (:hx/type %)) (vals @docs)))}

          :else (throw (ex-info "unexpected fake store request" {:path path}))))})))

(def base-body
  {:caller "agent-a" :session "session-a"
   :options [{:id "1" :label "one" :scope {:description "one packet"}}
             {:id "2" :label "two" :scope {:description "two packets"}}]
   :at "2026-09-28T14:00:00Z" :idempotency-key "offer-route-1"})

(use-fixtures :each
  (fn [f]
    (offer-provider/reset-cache!)
    (prompt-line/reset-registry!)
    (offer-provider/register!)
    (f)))

(deftest missing-grant-refuses-before-post
  (let [{:keys [request! calls]} (fake-store false)]
    (with-redefs [store/request! request!]
      (let [response ((handler) (request base-body))]
        (is (= 403 (:status response)))
        (is (= "no-grant" (:reason (body response))))
        (is (empty? (filter #(= "POST" (first %)) @calls)))))))

(deftest verified-write-immediately-renders-cache-only-segment
  (let [{:keys [request!]} (fake-store)]
    (with-redefs [store/request! request!]
      (let [response ((handler) (request base-body))
            response-body (body response)
            id (get-in response-body [:record :id])
            render (prompt-line/render
                    {:agent-id "agent-a" :session-id "session-a"
                     :render-at (get-in response-body [:receipt :system-as-of])}
                    [{:segment/id :offer
                      :provider "futon3c.agency.offer-provider/provider"
                      :fn (fn [ctx]
                            (with-redefs [store/request!
                                          (fn [& _] (throw (ex-info "HTTP on render" {})))]
                              (offer-provider/provider ctx)))
                      :budget-ms 25}])]
        (is (= 200 (:status response)))
        (is (= (str "offer " id " (2 options)")
               (get-in render [:segments 0 :segment/header])))
        (is (= "$!> " (:prompt render)))
        (is (empty? (:omitted render)))))))

(deftest another-seat-and-duplicate-options-are-typed
  (let [{:keys [request! calls]} (fake-store)]
    (with-redefs [store/request! request!]
      (let [foreign ((handler) (request (assoc base-body :agent "agent-b")))
            duplicate ((handler)
                       (request (assoc base-body :options
                                       [{:id "1" :scope {:description "a"}}
                                        {:id "1" :scope {:description "b"}}])))]
        (is (= 403 (:status foreign)))
        (is (= "not-seat-owner" (:reason (body foreign))))
        (is (= 400 (:status duplicate)))
        (is (= "duplicate-option-ids" (:reason (body duplicate))))
        (is (empty? (filter #(= "POST" (first %)) @calls)))))))

(deftest idempotent-replay-returns-the-same-act
  (let [{:keys [request!]} (fake-store)]
    (with-redefs [store/request! request!]
      (let [first-response ((handler) (request base-body))
            second-response ((handler) (request base-body))]
        (is (= 200 (:status first-response)))
        (is (= 200 (:status second-response)))
        (is (= (get-in (body first-response) [:record :id])
               (get-in (body second-response) [:record :id])))
        (is (true? (get-in (body second-response) [:receipt :no-op?])))))))

(deftest grants-that-do-not-cover-own-offers-are-not-found
  ;; The live "*" own-acts grant lists other act kinds; a grant without
  ;; :own-acts-only, or one naming another agent, must not cover agent-a.
  ;; For "*" without :own-acts-only, grant validation (wildcard-needs-own-acts)
  ;; refuses it even when the route's lookup filter is removed.
  (doseq [make-grant [(fn [id] (assoc-in (grant id "*") [:hx/props :grant/scope :act-kinds]
                                         [:pattern-card/selection :act/withdrawal]))
                      (fn [id] (update-in (grant id "*") [:hx/props :grant/scope]
                                          dissoc :own-acts-only))
                      (fn [id] (grant id "agent-b"))]]
    (let [{:keys [request! calls]} (fake-store true make-grant)]
      (with-redefs [store/request! request!]
        (let [response ((handler) (request base-body))]
          (is (= 403 (:status response)))
          (is (= "no-grant" (:reason (body response))))
          (is (empty? (filter #(= "POST" (first %)) @calls))))))))

(deftest expired-offer-leaves-the-prompt
  (let [{:keys [request!]} (fake-store)]
    (with-redefs [store/request! request!]
      (let [response ((handler) (request (assoc base-body :until "2026-09-28T15:00:00Z")))
            seg (fn [at] (offer-provider/provider
                          {:agent-id "agent-a" :session-id "session-a" :render-at at}))]
        (is (= 200 (:status response)))
        (is (some? (seg "2026-09-28T14:59:59Z")))
        (is (nil? (seg "2026-09-28T15:00:00Z")))))))
