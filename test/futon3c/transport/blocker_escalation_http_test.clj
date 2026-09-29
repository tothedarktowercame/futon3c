(ns futon3c.transport.blocker-escalation-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.coordination-ledger :as coordination]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def job "invoke-blocker-job")
(def edge {:evidence-id "edge:blocker" :edge-id job :kind :invoke
           :from "claude-17" :to "codex-5"})
(def psr {:evidence/id "psr:1" :evidence/type :pattern-selection
          :evidence/author "codex-5" :pattern-id :musn/plan-before-tool
          :evidence/body {:selected "musn/plan-before-tool"}})
(def pur {:evidence/id "pur:1" :evidence/type :pattern-outcome
          :evidence/author "codex-5" :pattern-id :musn/plan-before-tool
          :evidence/body {:outcome "blocked"}})
(def request-body {:caller "codex-5" :source-job job
                   :blocker "The dependency is still unavailable."
                   :psr-id "psr:1" :pur-id "pur:1"})

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn request [body]
  {:request-method :post :uri "/api/alpha/escalation/blocker"
   :body (json/generate-string body)})
(defn response-body [response] (json/parse-string (:body response) true))

(defn fake-store [entries]
  (let [docs (atom {}) posts (atom []) keys-seen (atom {})]
    {:docs docs :posts posts
     :request!
     (fn [_ method path value]
       (cond
         (and (= "GET" method) (str/includes? path "/api/alpha/evidence/"))
         (let [id (java.net.URLDecoder/decode (last (str/split path #"/")) "UTF-8")]
           (or (get entries id) (throw (ex-info "not found" {:status 404}))))

         (= "POST" method)
         (let [key (:hx/idempotency-key value)]
           (if (contains? @keys-seen key)
             (throw (ex-info "conflict" {:status 409}))
             (let [id "act:blocker-1"
                   doc (-> value (dissoc :hx/mint-id :hx/idempotency-key)
                           (assoc :hx/id id))]
               (swap! keys-seen assoc key value)
               (swap! posts conj value) (swap! docs assoc id doc)
               {:ok true :hx/id id :minted? true})))

         (and (= "GET" method) (str/includes? path "type=escalation%2Fblocker"))
         {:hyperedges (vec (vals @docs))}
         :else (throw (ex-info "unexpected store request" {:method method :path path}))))}))

(defmacro with-edge [edges & body]
  `(with-redefs [coordination/recent-mesh-edges (fn [& _#] ~edges)] ~@body))

(deftest happy-path-writes-one-act-and-routes-one-bell
  (let [{:keys [request! posts]} (fake-store {"psr:1" psr "pur:1" pur})
        routed (atom [])]
    (with-redefs [store/request! request!
                  http/route-blocker-escalation!
                  (fn [_ record outcome]
                    (swap! routed conj [record outcome])
                    {:status :routed :job-id "invoke:routed"})]
      (with-edge [edge]
        (let [response ((handler) (request request-body))
              body (response-body response)
              record (:record body)]
          (is (= 200 (:status response)) (pr-str body))
          (is (= 1 (count @posts)))
          (is (= 1 (count @routed)))
          (is (= "codex-5" (:author record)))
          (is (= "claude-17" (:orchestrator record)))
          (is (= "musn/plan-before-tool" (:pattern-id record)))
          (is (= "blocked" (second (first @routed)))))))))

(deftest pattern-proof-refusals-write-and-bell-nothing
  (doseq [[label entries body edges status]
          [["missing PUR" {"psr:1" psr} request-body [edge] 422]
           ["other PUR author" {"psr:1" psr "pur:1" (assoc pur :evidence/author "other")}
            request-body [edge] 422]
           ["pattern mismatch" {"psr:1" psr "pur:1" (assoc pur :pattern-id :other)}
            request-body [edge] 422]
           ["success" {"psr:1" psr "pur:1" (assoc-in pur [:evidence/body :outcome] "success")}
            request-body [edge] 422]
           ["not assignee" {"psr:1" psr "pur:1" pur}
            (assoc request-body :caller "other") [edge] 403]
           ["missing edge" {"psr:1" psr "pur:1" pur} request-body [] 409]
           ["duplicate edge" {"psr:1" psr "pur:1" pur} request-body
            [edge (assoc edge :evidence-id "edge:duplicate")] 409]]]
    (testing label
      (let [{:keys [request! posts]} (fake-store entries) bells (atom 0)]
        (with-redefs [store/request! request!
                      http/route-blocker-escalation! (fn [& _] (swap! bells inc))]
          (with-edge edges
            (is (= status (:status ((handler) (request body)))))
            (is (empty? @posts))
            (is (zero? @bells))))))))

(deftest joe-orchestrator-does-not-bell
  (let [{:keys [request!]} (fake-store {"psr:1" psr "pur:1" pur})]
    (with-redefs [store/request! request!]
      (with-edge [(assoc edge :from "joe")]
        (let [body (response-body ((handler) (request request-body)))]
          (is (= "joe-orchestrator-pending" (get-in body [:routing :status]))))))))

(deftest replay-uses-real-ledger-dedupe
  (let [record {:id "act:blocker-1" :author "codex-5" :orchestrator "claude-17"
                :source-job job :blocker "blocked" :pattern-id "musn/x"}
        job-id (#'http/blocker-routing-job-id "act:blocker-1")
        created (atom 0)]
    (with-redefs-fn {#'http/ensure-invoke-jobs-ledger!
                     (fn [] {:jobs {job-id {:state "done"}}})
                     #'http/read-commission-archive (constantly nil)
                     #'http/create-invoke-job! (fn [_] (swap! created inc))}
      (fn []
        (let [result (http/route-blocker-escalation! {} record "blocked")]
          (is (true? (:existing? result)))
          (is (zero? @created)))))))

(deftest exact-replay-is-existing-and-changed-replay-conflicts
  (let [{:keys [request! posts]} (fake-store {"psr:1" psr "pur:1" pur})]
    (with-redefs [store/request! request!
                  http/route-blocker-escalation! (fn [_ _ _] {:status :routed})]
      (with-edge [edge]
        (let [first-response ((handler) (request request-body))
              same-response ((handler) (request request-body))
              changed-response ((handler) (request (assoc request-body
                                                           :blocker "Different block")))]
          (is (= [200 200 409] (mapv :status
                                     [first-response same-response changed-response])))
          (is (true? (get-in (response-body same-response) [:receipt :existing?])))
          (is (= "idempotency-conflict"
                 (:reason (response-body changed-response))))
          (is (= 1 (count @posts))))))))

(deftest store-refusal-is-service-unavailable
  (with-edge [edge]
    (with-redefs [store/request!
                  (fn [_ _ _ _]
                    (throw (ex-info "busy" {:status 503})))
                  http/route-blocker-escalation! (fn [& _] (throw (Error. "must not route")))]
      (let [response ((handler) (request request-body))]
        (is (= 503 (:status response)))
        (is (= "store-unavailable" (:reason (response-body response))))))))
