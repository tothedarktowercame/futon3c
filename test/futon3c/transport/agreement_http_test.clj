(ns futon3c.transport.agreement-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.offer-provider :as offer-provider]
            [futon3c.agency.offer-record :as offer-record]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.agency.registry :as registry]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http])
  (:import [java.time Instant]))

(def harness {:kind :none :basis :producer-context :source-ref "test:p11-4a"})
(def stamp {:executor "agent-a" :signer "agent-a"
            :authority {:grant "act:offer-grant"} :executor-basis :declared})
(defn offer [id options]
  {:id id :kind :offer/record :author "agent-a" :addressee "joe"
   :seat {:agent "agent-a" :session "session-a"}
   :at (str (.minusSeconds (Instant/now) 60))
   :until (str (.plusSeconds (Instant/now) 3600))
   :options options :act/stamp stamp :act/harness harness})
(def options [{:option/id "1" :option/label "one"
               :option/scope {:description "one"}}
              {:option/id "2" :option/label "two"
               :option/scope {:description "two"}}])
(defn evidence [id author session text agent]
  {:evidence/id id :evidence/author author :evidence/session-id session
   :evidence/origin {:kind :operator}
   :evidence/body {:event "chat-turn" :role "user" :text text
                   :turn-id (str agent "-turn-1")}})

(defn fake-store [offers evidence-map & withdrawals]
  (let [docs (atom (into {} (map (fn [o] [(:id o) (offer-record/record->hyperedge o)]) offers)))
        keys (atom {}) calls (atom []) n (atom 0)]
    (doseq [w withdrawals]
      (swap! docs assoc (:hx/id w) w))
    {:calls calls :docs docs
     :request!
     (fn [_ method path value]
       (swap! calls conj [method path value])
       (cond
         (= method "POST")
         (let [key (:hx/idempotency-key value)]
           (if-let [id (get @keys key)]
             {:ok true :hx/id id :no-op? true}
             (let [id (str "act:agreement-" (swap! n inc))
                   doc (-> value (dissoc :hx/mint-id :hx/idempotency-key :hx/valid-time)
                           (assoc :hx/id id))]
               (swap! docs assoc id doc) (swap! keys assoc key id)
               {:ok true :hx/id id})))
         (str/starts-with? path "/api/alpha/evidence/")
         (or (get evidence-map (java.net.URLDecoder/decode
                                (subs path (count "/api/alpha/evidence/")) "UTF-8"))
             (throw (ex-info "missing" {:status 404})))
         (str/includes? path "type=offer%2Frecord")
         {:hyperedges (filterv #(= :offer/record (:hx/type %)) (vals @docs))}
         (str/includes? path "type=act%2Fwithdrawal")
         {:hyperedges (filterv #(= :act/withdrawal (:hx/type %)) (vals @docs))}
         (str/includes? path "type=agreement%2Frecord")
         {:hyperedges (filterv #(= :agreement/record (:hx/type %)) (vals @docs))}
         :else (throw (ex-info "unexpected" {:path path}))))}))

(defn handler [] (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn req [text evidence-id]
  {:request-method :post :uri "/api/alpha/agreement"
   :body (json/generate-string {:agent "agent-a" :session "session-a"
                                :text text :evidence-id evidence-id})})
(defn response-body [r] (json/parse-string (:body r) true))
(use-fixtures :each (fn [f] (offer-provider/reset-cache!) (prompt-line/reset-registry!)
                      (offer-provider/register!)
                      (with-redefs [registry/get-agent
                                    (fn [agent] {:agent/id agent
                                                 :agent/session-id "session-a"})]
                        (f))))

(deftest evidence-must-be-joes-exact-operator-turn
  (let [o (offer "act:offer-a" [(first options)])
        cases [(evidence "e:agent" "agent-a" "session-a" "yes" "agent-a")
               (evidence "e:text" "joe" "session-a" "no" "agent-a")
               (evidence "e:session" "joe" "session-b" "yes" "agent-a")
               ;; the live shape of a park resume: author joe, origin harness
               (assoc (evidence "e:resume" "joe" "session-a" "yes" "agent-a")
                      :evidence/origin {:kind "harness" :actor "parked-resume"})
               (dissoc (evidence "e:no-origin" "joe" "session-a" "yes" "agent-a")
                       :evidence/origin)]]
    (doseq [entry cases]
      (let [{:keys [request! calls]} (fake-store [o] {(:evidence/id entry) entry})]
        (with-redefs [store/request! request!]
          (let [r ((handler) (req "yes" (:evidence/id entry)))]
            (is (= 403 (:status r)))
            (is (= "evidence-not-operator-turn" (:reason (response-body r))))
            (is (empty? (filter #(= "POST" (first %)) @calls)))))))))

(deftest ambiguity-and-withdrawal-write-nothing
  (let [o1 (offer "act:offer-a" options)
        o2 (offer "act:offer-b" [(first options)])
        e (evidence "e:yes" "joe" "session-a" "yes" "agent-a")]
    (let [{:keys [request! calls]} (fake-store [o1 o2] {"e:yes" e})]
      (with-redefs [store/request! request!]
        (let [r ((handler) (req "yes" "e:yes"))]
          (is (= 409 (:status r))) (is (= "ambiguous" (:reason (response-body r))))
          (is (empty? (filter #(= "POST" (first %)) @calls))))))
    (let [w {:hx/id "act:w" :hx/type :act/withdrawal
             :hx/props {:author "agent-a" :target "act:offer-a" :status :effective
                        :basis {:kind :self} :at (str (Instant/now))}}
          named (evidence "e:named" "joe" "session-a" "yes act:offer-a" "agent-a")
          {:keys [request! calls]} (fake-store [o1] {"e:named" named} w)]
      (with-redefs [store/request! request!]
        (let [r ((handler) (req "yes act:offer-a" "e:named"))]
          (is (= 409 (:status r))) (is (= "unknown-offer" (:reason (response-body r))))
          (is (empty? (filter #(= "POST" (first %)) @calls))))))))

(deftest accept-clears-prompt-and-replay-is-idempotent
  (let [o (offer "act:offer-a" options)
        e1 (evidence "e:yes" "joe" "session-a" "yes 2" "agent-a")
        e2 (evidence "e:second" "joe" "session-a" "yes act:offer-a 2" "agent-a")
        {:keys [request! calls]} (fake-store [o] {"e:yes" e1 "e:second" e2})]
    (offer-provider/publish! {:record o :receipt {:system-as-of (str (Instant/now))}})
    (with-redefs [store/request! request!]
      (let [first-r ((handler) (req "yes 2" "e:yes"))
            first-body (response-body first-r)
            replay ((handler) (req "yes 2" "e:yes"))
            second-r ((handler) (req "yes act:offer-a 2" "e:second"))]
        (is (= 200 (:status first-r)))
        (is (= {:description "two"} (get-in first-body [:record :agreement/scope])))
        (is (nil? (offer-provider/provider {:agent-id "agent-a" :session-id "session-a"})))
        (is (= (get-in first-body [:record :id]) (get-in (response-body replay) [:record :id])))
        (is (true? (get-in (response-body replay) [:receipt :no-op?])))
        (is (= 409 (:status second-r)))
        (is (= "unknown-offer" (:reason (response-body second-r))))
        (is (= 1 (count (filter #(= "POST" (first %)) @calls))))))))
