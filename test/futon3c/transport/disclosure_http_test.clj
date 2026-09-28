(ns futon3c.transport.disclosure-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.coordination-ledger :as coordination]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def job-id "invoke-1790631987472-26463-cee3cb87")
(def prompt "Build the route and quote this exact request span.")
(def edge {:evidence-id "edge-evidence-1" :edge-id job-id :kind :invoke
           :from "claude-17" :to "codex-5"})
(def base-body
  {:caller "codex-5" :source-job job-id
   :unspecified "The CLI caller source was not stated."
   :chosen "Read caller from AGENCY_AGENT_ID."
   :affects {:kind "file" :id "src/futon3c/agency/disclosure_record_cli.clj"}
   :quote "quote this exact request span"})

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn request [body]
  {:request-method :post :uri "/api/alpha/disclosure"
   :body (json/generate-string body)})
(defn body [response] (json/parse-string (:body response) true))

(defn fake-store []
  (let [docs (atom {}) posts (atom []) n (atom 0)]
    {:docs docs :posts posts
     :request!
     (fn [_ method path value]
       (cond
         (= "POST" method)
         (let [id (str "act:disclosure-" (swap! n inc))
               doc (-> value (dissoc :hx/mint-id :hx/idempotency-key)
                       (assoc :hx/id id))]
           (swap! posts conj value) (swap! docs assoc id doc)
           {:ok true :hx/id id :minted? true})
         (and (= "GET" method) (str/includes? path "end=job%3A"))
         {:hyperedges (vec (vals @docs))}
         :else (throw (ex-info "unexpected store request" {:path path}))))}))

(defmacro with-source [edges & forms]
  `(with-redefs [http/invoke-job-request-commission
                 (fn [_#] {:commission {:prompt prompt}})
                 coordination/recent-mesh-edges (fn [& _#] ~edges)]
     ~@forms))

(deftest two-disclosures-are-distinct-and-readable-by-job-endpoint
  (let [{:keys [request! docs posts]} (fake-store)]
    (with-redefs [store/request! request!]
      (with-source [edge]
        (let [a ((handler) (request base-body))
              b ((handler) (request (assoc base-body :chosen "Use the seat environment.")))
              aid (get-in (body a) [:record :id])
              bid (get-in (body b) [:record :id])]
          (is (= [200 200] [(:status a) (:status b)])
              (pr-str [(body a) (body b)]))
          (is (not= aid bid))
          (is (= #{aid bid} (set (keys @docs))))
          (is (= 2 (count @posts)))
          (is (every? #(some #{(str "job:" job-id)} (:hx/endpoints %))
                      (vals @docs))))))))

(deftest refusals-perform-zero-writes
  (let [{:keys [request! posts]} (fake-store)]
    (with-redefs [store/request! request!]
      (testing "unknown job"
        (with-redefs [http/invoke-job-request-commission
                      (fn [_] (throw (ex-info "missing" {:refusal :invoke-job-missing})))]
          (is (= 404 (:status ((handler) (request base-body)))))))
      (doseq [[edges request-body status reason]
              [[[] base-body 409 "orchestrator-unknown"]
               [[edge (assoc edge :evidence-id "edge-evidence-2")]
                base-body 409 "orchestrator-ambiguous"]
               [[edge] (assoc base-body :caller "other") 403 "not-the-assignee"]
               [[edge] (assoc base-body :caller "claude-17") 403 "not-the-assignee"]
               [[edge] (assoc base-body :quote "absent") 400 "span-not-in-request"]
               [[edge] (assoc base-body :at "2020-01-01T00:00:00Z")
                400 "caller-supplied-at"]]]
        (with-source edges
          (let [response ((handler) (request request-body))]
            (is (= status (:status response)))
            (is (= reason (:reason (body response)))))))
      (testing "prompt unavailable"
        (with-redefs [http/invoke-job-request-commission (fn [_] {:commission {}})]
          (let [response ((handler) (request base-body))]
            (is (= 409 (:status response)))
            (is (= "request-text-unavailable" (:reason (body response)))))))
      (is (empty? @posts)))))
