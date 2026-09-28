(ns futon3c.agency.negation-interpretation-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.act-stamp :as act-stamp]
            [futon3c.agency.disclosure-record :as disclosure-record]
            [futon3c.agency.disclosure-record-cli :as disclosure-cli]
            [futon3c.agency.negation-interpretation :as negation]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.coordination-ledger :as coordination-ledger]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def disclosures [{:id "act:a" :chosen "drop it"}
                  {:id "act:b" :chosen "keep it"}])

(def source-job "invoke-source")
(def disclosure-a
  {:id "act:a" :kind :disclosure/choice :schema 1 :author "codex-5"
   :at "2026-09-28T20:00:00Z" :source-job source-job
   :unspecified "choice absent" :chosen "drop it"
   :affects {:kind :file :id "src/x.clj"}
   :inside-request {:basis :source-span :quote "packet"
                    :text-sha256 (apply str (repeat 64 "a"))}
   :act/stamp (act-stamp/stamp "codex-5" "codex-5"
                               {:dispatch-edge "edge:source"} :declared)
   :act/harness (act-harness/plain "test")})
(def source-edge {:evidence-id "edge:source" :edge-id source-job :kind :invoke
                  :from "claude-17" :to "codex-5"})

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
  (let [stored (atom {})
        writes (atom 0)
        routed (atom [])
        fake-store
        (fn [_ method path body]
          (cond
            (and (= method "GET") (str/ends-with? path "turn%3Ajoe")) operator
            (= method "GET")
            (let [id (java.net.URLDecoder/decode
                      (last (str/split path #"/")) "UTF-8")]
              (or (get @stored id) (throw (ex-info "missing" {:status 404}))))
            (= method "POST") (do (swap! writes inc)
                                   (swap! stored assoc (:evidence/id body) body)
                                   {:ok true :evidence/id (:evidence/id body)})))]
    (with-redefs [store/request! fake-store
                  disclosure-record/hyperedge->record
                  (fn [_] disclosure-a)
                  disclosure-cli/read-act!
                  (fn [_ _] (disclosure-record/->hyperedge disclosure-a))
                  coordination-ledger/recent-mesh-edges (fn [& _] [source-edge])
                  http/route-negation-reading!
                  (fn [_ _ _ orchestrator]
                    (swap! routed conj orchestrator)
                    {:status :routed :job-id "invoke:routing"
                     :orchestrator orchestrator})
                  http/read-operator-turn-source-jobs
                  (fn [_] {:source-jobs ["job:1"] :basis :park-resume
                           :disclosures [{:id "act:a" :source-job "job:1"}]})]
      (let [first-result (parse-response ((handler) (request base-body)))
            replay (parse-response ((handler) (request base-body)))
            conflict (parse-response
                      ((handler) (request (assoc base-body :fragment-text "changed"))))]
        (is (= 201 (:status first-result)) (pr-str first-result))
        (is (= "single-standing"
               (get-in first-result [:entry :evidence/body :resolution])))
        (is (= "act:a" (get-in first-result [:entry :evidence/body :target])))
        (is (nil? (get-in first-result [:entry :evidence/body :act/stamp])))
        (is (nil? (get-in first-result [:entry :act/stamp])))
        (is (= 200 (:status replay)))
        (is (true? (:existing? replay)))
        (is (= 409 (:status conflict)))
        (is (= "idempotency-conflict" (:reason conflict)))
        (is (= ["claude-17"] @routed))
        (is (= "routed" (get-in first-result [:routing :status])))
        (is (= 2 @writes))))))

(deftest route-validates-required-fields
  (let [response ((handler) (request (dissoc base-body :fragment-id)))]
    (is (= 400 (:status response)))
    (is (= "missing-field" (:reason (json/parse-string (:body response) true))))))

(deftest route-stores-unresolved-when-no-agent-turn-precedes
  ;; DERIVE-2 item 16.1: such a turn is stored as :target-unresolved.
  (let [stored (atom nil)
        fake-store
        (fn [_ method path body]
          (cond
            (and (= method "GET") (str/ends-with? path "turn%3Ajoe")) operator
            (and (= method "GET") @stored) @stored
            (= method "GET") (throw (ex-info "missing" {:status 404}))
            (= method "POST") (do (reset! stored body)
                                  {:ok true :evidence/id (:evidence/id body)})))]
    (with-redefs [store/request! fake-store
                  http/read-operator-turn-source-jobs
                  (fn [_] (throw (ex-info "no chain" {:reason :turn-chain-not-found})))]
      (let [result (parse-response ((handler) (request base-body)))]
        (is (= 201 (:status result)) (pr-str result))
        (is (= "target-unresolved"
               (get-in result [:entry :evidence/body :resolution])))
        (is (= "no-agent-turn" (get-in result [:entry :evidence/body :basis])))))))

(defn- routing-case [edges visible]
  (let [stored (atom {})
        bells (atom 0)
        fake-store
        (fn [_ method path body]
          (cond
            (and (= method "GET") (str/ends-with? path "turn%3Ajoe")) operator
            (= method "GET")
            (let [id (java.net.URLDecoder/decode
                      (last (str/split path #"/")) "UTF-8")]
              (or (get @stored id) (throw (ex-info "missing" {:status 404}))))
            (= method "POST")
            (do (swap! stored assoc (:evidence/id body) body)
                {:ok true :evidence/id (:evidence/id body)})))]
    (with-redefs [store/request! fake-store
                  disclosure-cli/read-act!
                  (fn [_ _] (disclosure-record/->hyperedge disclosure-a))
                  coordination-ledger/recent-mesh-edges (fn [& _] edges)
                  http/route-negation-reading!
                  (fn [_ _ _ orchestrator]
                    (if (= "joe" orchestrator)
                      {:status :joe-orchestrator-pending}
                      (do (swap! bells inc)
                          {:status :routed :job-id "invoke:routing"})))
                  http/read-operator-turn-source-jobs
                  (fn [_] {:source-jobs [source-job] :basis :park-resume
                           :disclosures visible})]
      {:response (parse-response ((handler) (request base-body)))
       :bells @bells :stored @stored})))

(deftest routing-is-typed-for-joe-missing-and-ambiguous-orchestrators
  (let [joe (routing-case [(assoc source-edge :from "joe")] [disclosure-a])
        missing (routing-case [] [disclosure-a])
        ambiguous (routing-case [source-edge (assoc source-edge
                                                     :evidence-id "edge:second")]
                                [disclosure-a])]
    (is (= "joe-orchestrator-pending"
           (get-in joe [:response :routing :status])))
    (is (zero? (:bells joe)))
    (is (= "orchestrator-unknown"
           (get-in missing [:response :routing :status])))
    (is (zero? (:bells missing)))
    (is (= "orchestrator-ambiguous"
           (get-in ambiguous [:response :routing :status])))
    (is (zero? (:bells ambiguous)))))

(deftest ambiguous-negation-target-does-not-route
  (let [result (routing-case [source-edge] disclosures)]
    (is (= "target-ambiguous"
           (get-in result [:response :entry :evidence/body :resolution])))
    (is (nil? (get-in result [:response :routing])))
    (is (zero? (:bells result)))))
