(ns futon3c.transport.disclosure-withdraw-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.act-harness :as harness]
            [futon3c.agency.act-stamp :as stamp]
            [futon3c.agency.disclosure-record :as disclosure]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.coordination-ledger :as coordination]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def job-id "invoke-1790632281043-26472-505890df")
(def edge {:evidence-id "edge:source" :edge-id job-id :kind :invoke
           :from "claude-17" :to "codex-5"})
(defn disclosure-record [id chosen]
  {:id id :kind :disclosure/choice :schema 1 :author "codex-5"
   :at "2026-09-28T20:00:00Z" :source-job job-id
   :unspecified "unspecified" :chosen chosen
   :affects {:kind :file :id "src/x.clj"}
   :inside-request {:basis :source-span :quote "packet"
                    :text-sha256 (apply str (repeat 64 "a"))}
   :act/stamp (stamp/stamp "codex-5" "codex-5"
                            {:dispatch-edge "edge:source"} :declared)
   :act/harness (harness/plain "test")})
(def disclosure-a (disclosure-record "act:choice-a" "choice a"))
(def disclosure-b (disclosure-record "act:choice-b" "choice b"))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn request [body]
  {:request-method :post :uri "/api/alpha/disclosure/withdraw"
   :body (json/generate-string body)})
(defn body [response] (json/parse-string (:body response) true))

(defn fake-store []
  (let [initial {(:id disclosure-a) (disclosure/->hyperedge disclosure-a)
                 (:id disclosure-b) (disclosure/->hyperedge disclosure-b)}
        docs (atom initial) posts (atom []) n (atom 0)]
    {:docs docs :posts posts
     :request!
     (fn [_ method path value]
       (cond
         (= "POST" method)
         (let [id (str "act:withdrawal-" (swap! n inc))
               doc (-> value (dissoc :hx/mint-id :hx/idempotency-key)
                       (assoc :hx/id id))]
           (swap! posts conj value) (swap! docs assoc id doc)
           {:ok true :hx/id id :minted? true})
         (str/starts-with? path "/api/alpha/hyperedge/")
         (let [id (java.net.URLDecoder/decode
                   (subs path (count "/api/alpha/hyperedge/")) "UTF-8")]
           (or (get @docs id) (throw (ex-info "missing" {:status 404}))))
         (str/includes? path "type=act%2Fwithdrawal")
         {:hyperedges (filterv #(= :act/withdrawal (:hx/type %)) (vals @docs))}
         :else (throw (ex-info "unexpected request" {:method method :path path}))))}))

(defmacro with-edge [& forms]
  `(with-redefs [coordination/recent-mesh-edges (fn [& _#] [edge])]
     ~@forms))

(deftest orchestrator-withdraws-one-choice-and-routes-once
  (let [{:keys [request! posts docs]} (fake-store)
        routed (atom {})]
    (with-redefs [store/request! request!
                  http/route-disclosure-withdrawal!
                  (fn [_# disclosure# withdrawal#]
                    (let [id# (str "invoke-route-" (:id withdrawal#))]
                      (swap! routed #(if (contains? % id#) %
                                        (assoc % id# {:to (:author disclosure#)
                                                      :in-reply-to (:source-job disclosure#)})))
                      {:job-id id#}))]
      (with-edge
        (let [payload {:caller "claude-17" :target "act:choice-a"
                       :reason "The choice no longer matches the packet."}
              first-response ((handler) (request payload))
              replay ((handler) (request payload))
              withdrawal (get-in (body first-response) [:record])]
          (is (= 200 (:status first-response)))
          (is (= 200 (:status replay)))
          (is (= "act:choice-a" (:target withdrawal)))
          (is (= 1 (count @posts)))
          (is (= 1 (count @routed)))
          (is (= {:to "codex-5" :in-reply-to job-id}
                 (first (vals @routed))))
          (is (contains? @docs "act:choice-b"))
          (is (= 409 (:status ((handler)
                              (request (assoc payload :reason "different")))))))))))

(deftest only-edge-from-may-withdraw
  (let [{:keys [request! posts]} (fake-store)]
    (with-redefs [store/request! request!]
      (with-edge
        (doseq [caller ["codex-5" "third-agent"]]
          (let [response ((handler) (request {:caller caller :target "act:choice-a"
                                              :reason "attempt"}))]
            (is (= 403 (:status response)))
            (is (= "not-the-orchestrator" (:reason (body response)))))))
      (is (empty? @posts)))))

(deftest missing-target-and-edge-ambiguity-are-typed-before-write
  (let [{:keys [request! posts]} (fake-store)
        payload {:caller "claude-17" :target "act:choice-a" :reason "withdraw"}]
    (with-redefs [store/request! request!]
      (let [missing ((handler) (request (assoc payload :target "act:absent")))]
        (is (= 404 (:status missing)))
        (is (= "unknown-disclosure" (:reason (body missing)))))
      (doseq [[edges status reason]
              [[[] 409 "orchestrator-unknown"]
               [[edge (assoc edge :evidence-id "edge:second")]
                409 "orchestrator-ambiguous"]]]
        (with-redefs [coordination/recent-mesh-edges (fn [& _] edges)]
          (let [response ((handler) (request payload))]
            (is (= status (:status response)))
            (is (= reason (:reason (body response)))))))
      (is (empty? @posts)))))

(deftest validator-refuses-a-forged-dispatch-signer
  (let [withdrawal {:id "act:w" :kind :act/withdrawal :author "codex-5"
                    :target "act:choice-a" :status :effective
                    :basis {:kind :dispatch-edge} :reason "attempt"
                    :at "2026-09-28T20:01:00Z"
                    :act/stamp (stamp/stamp "codex-5" "codex-5"
                                             {:dispatch-edge "edge:source"} :declared)
                    :act/harness (harness/plain "test")}
        raw-edge {:evidence/id "edge:source"
                  :evidence/body {:edge/id job-id :edge/kind :invoke
                                  :edge/from "claude-17" :edge/to "codex-5"}}]
    (is (= :not-the-orchestrator
           (try (disclosure/validate-withdrawal-against-source!
                 withdrawal disclosure-a [raw-edge]) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(deftest bell-failure-does-not-roll-back-withdrawal
  (let [{:keys [request! posts]} (fake-store)]
    (with-redefs [store/request! request!
                  http/route-disclosure-withdrawal!
                  (fn [& _] (throw (ex-info "bell unavailable" {})))]
      (with-edge
        (let [response ((handler) (request {:caller "claude-17"
                                            :target "act:choice-a"
                                            :reason "withdraw it"}))]
          (is (= 200 (:status response)))
          (is (false? (:routed (body response))))
          (is (= "bell unavailable" (:routing-error (body response))))
          (is (= 1 (count @posts))))))))
