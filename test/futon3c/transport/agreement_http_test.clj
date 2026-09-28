(ns futon3c.transport.agreement-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.offer-provider :as offer-provider]
            [futon3c.agency.offer-record :as offer-record]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.agency.grant-record :as grant-record]
            [futon3c.agency.registry :as registry]
            [futon3c.agency.rule-record :as store]
            [futon3c.agency.turn-notice :as turn-notice]
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
        written-evidence (atom {}) keys (atom {}) calls (atom []) n (atom 0)
        fail-grant? (atom false) fail-evidence? (atom false)]
    (doseq [w withdrawals]
      (swap! docs assoc (:hx/id w) w))
    {:calls calls :docs docs :written-evidence written-evidence
     :fail-grant? fail-grant? :fail-evidence? fail-evidence?
     :request!
     (fn [_ method path value]
       (swap! calls conj [method path value])
       (cond
         (and (= method "POST") (= path "/api/alpha/evidence"))
         (do
           (when @fail-evidence?
             (throw (ex-info "evidence store failed" {:status 503})))
           (let [id (:evidence/id value)]
             (if (contains? @written-evidence id)
               (throw (ex-info "duplicate" {:status 409}))
               (do (swap! written-evidence assoc id value)
                   {:ok true :evidence/id id :entry value}))))
         (= method "POST")
         (let [key (:hx/idempotency-key value)
               grant? (= :grant/record (:hx/type value))]
           (when (and grant? @fail-grant?)
             (throw (ex-info "grant store failed" {:status 503})))
           (if-let [id (get @keys key)]
             {:ok true :hx/id id :no-op? true}
             (let [id (str (if grant? "act:grant-" "act:agreement-") (swap! n inc))
                   doc (-> value (dissoc :hx/mint-id :hx/idempotency-key :hx/valid-time)
                           (assoc :hx/id id))]
               (swap! docs assoc id doc) (swap! keys assoc key id)
               {:ok true :hx/id id})))
         (str/starts-with? path "/api/alpha/evidence/")
         (let [id (java.net.URLDecoder/decode
                   (subs path (count "/api/alpha/evidence/")) "UTF-8")]
           (or (get evidence-map id) (get @written-evidence id)
               (throw (ex-info "missing" {:status 404}))))
         (str/starts-with? path "/api/alpha/hyperedge/")
         (or (get @docs (java.net.URLDecoder/decode
                         (subs path (count "/api/alpha/hyperedge/")) "UTF-8"))
             (throw (ex-info "missing" {:status 404})))
         (str/includes? path "type=offer%2Frecord")
         {:hyperedges (filterv #(= :offer/record (:hx/type %)) (vals @docs))}
         (str/includes? path "type=act%2Fwithdrawal")
         {:hyperedges (filterv #(= :act/withdrawal (:hx/type %)) (vals @docs))}
         (str/includes? path "type=agreement%2Frecord")
         {:hyperedges (filterv #(= :agreement/record (:hx/type %)) (vals @docs))}
         (str/includes? path "type=grant%2Frecord")
         {:hyperedges (filterv #(= :grant/record (:hx/type %)) (vals @docs))}
         :else (throw (ex-info "unexpected" {:path path}))))}))

(defn handler [] (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn req [text evidence-id]
  {:request-method :post :uri "/api/alpha/agreement"
   :body (json/generate-string {:agent "agent-a" :session "session-a"
                                :text text :evidence-id evidence-id})})
(defn response-body [r] (json/parse-string (:body r) true))
(defn header
  ([] (header "agent-a" "session-a"))
  ([agent session]
   (#'http/wrap-surface-header "body" "emacs-repl" "joe" agent nil session)))
(use-fixtures :each (fn [f] (offer-provider/reset-cache!) (prompt-line/reset-registry!)
                      (turn-notice/reset-state!)
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
            (is (not (str/includes? (header) "agreement ")))
            (is (empty? (filter #(= "POST" (first %)) @calls)))))))))

(deftest ambiguity-and-withdrawal-record-durable-refusals
  (let [o1 (offer "act:offer-a" options)
        o2 (offer "act:offer-b" [(first options)])
        e (evidence "e:yes" "joe" "session-a" "yes" "agent-a")]
    (let [{:keys [request! written-evidence]} (fake-store [o1 o2] {"e:yes" e})]
      (with-redefs [store/request! request!]
        (let [r ((handler) (req "yes" "e:yes"))]
          (is (= 409 (:status r))) (is (= "ambiguous" (:reason (response-body r))))
          (is (not (str/includes? (header "other" "session-a") "agreement ambiguous:")))
          (is (str/includes?
               (header)
               "agreement ambiguous: ask Joe one short question naming which (act:offer-a 1, act:offer-a 2, act:offer-b 1)"))
          (is (true? (:recorded (response-body r))))
          (is (= 1 (count @written-evidence)))
          (let [entry (first (vals @written-evidence))]
            (is (= :agreement/ambiguous (:evidence/type entry)))
            (is (= "futon3c/agreement-route" (:evidence/author entry)))
            (is (= "e:yes" (:evidence/in-reply-to entry)))
            (is (= 3 (count (get-in entry [:evidence/body :candidates]))))
            (is (= :harness (get-in entry [:evidence/origin :kind])))))))
    (let [w {:hx/id "act:w" :hx/type :act/withdrawal
             :hx/props {:author "agent-a" :target "act:offer-a" :status :effective
                        :basis {:kind :self} :at (str (Instant/now))}}
          named (evidence "e:named" "joe" "session-a" "yes act:offer-a" "agent-a")
          {:keys [request! written-evidence]} (fake-store [o1] {"e:named" named} w)]
      (with-redefs [store/request! request!]
        (let [r ((handler) (req "yes act:offer-a" "e:named"))]
          (is (= 409 (:status r))) (is (= "unknown-offer" (:reason (response-body r))))
          (is (str/includes? (header) "agreement refused: unknown-offer"))
          (is (true? (:recorded (response-body r))))
          (let [entry (first (vals @written-evidence))]
            (is (= :agreement/refused (:evidence/type entry)))
            (is (= :unknown-offer (get-in entry [:evidence/body :reason])))
            (is (= "act:offer-a" (get-in entry [:evidence/body :offer-id]))))
          ;; Replaying the same acceptance verifies the deterministic record.
          (let [replay ((handler) (req "yes act:offer-a" "e:named"))]
            (is (= 409 (:status replay)))
            (is (true? (:recorded (response-body replay))))
            (is (= (:record-id (response-body r)) (:record-id (response-body replay))))
            (is (= 1 (count @written-evidence)))))))))

(deftest refusal-evidence-failure-does-not-change-the-409
  (let [o (offer "act:offer-a" [(first options)])
        e (evidence "e:failure-refusal" "joe" "session-a" "yes act:missing" "agent-a")
        {:keys [request! fail-evidence? written-evidence]}
        (fake-store [o] {"e:failure-refusal" e})]
    (reset! fail-evidence? true)
    (with-redefs [store/request! request!]
      (let [r ((handler) (req "yes act:missing" "e:failure-refusal"))
            b (response-body r)]
        (is (= 409 (:status r)))
        (is (= "unknown-offer" (:reason b)))
        (is (false? (:recorded b)))
        (is (nil? (:record-id b)))
        (is (empty? @written-evidence))))))

(deftest accept-clears-prompt-and-replay-is-idempotent
  (let [o (offer "act:offer-a" options)
        e1 (evidence "e:yes" "joe" "session-a" "yes 2" "agent-a")
        e2 (evidence "e:second" "joe" "session-a" "yes act:offer-a 2" "agent-a")
        {:keys [request! calls]} (fake-store [o] {"e:yes" e1 "e:second" e2})]
    (offer-provider/publish! {:record o :receipt {:system-as-of (str (Instant/now))}})
    (with-redefs [store/request! request!]
      (let [first-r ((handler) (req "yes 2" "e:yes"))
            first-body (response-body first-r)
            replay ((handler) (req "yes 2" "e:yes"))]
        (is (= 200 (:status first-r)))
        (is (= {:description "two"} (get-in first-body [:record :agreement/scope])))
        (is (nil? (offer-provider/provider {:agent-id "agent-a" :session-id "session-a"})))
        (is (= (get-in first-body [:record :id]) (get-in (response-body replay) [:record :id])))
        (is (true? (get-in (response-body replay) [:receipt :no-op?])))
        (is (str/includes?
             (header)
             (str "agreement " (get-in first-body [:record :id])
                  ": you offered act:offer-a, Joe accepted option 2")))
        (is (not (str/includes? (header) "agreement ")))
        (let [second-r ((handler) (req "yes act:offer-a 2" "e:second"))]
          (is (= 409 (:status second-r)))
          (is (= "unknown-offer" (:reason (response-body second-r)))))
        (is (= 2 (count (filter #(= "POST" (first %)) @calls))))))))

(deftest finite-grant-option-writes-one-idempotent-covering-grant
  (let [grant-until (str (.plusSeconds (Instant/now) 3600))
        scoped-option {:option/id "1" :option/label "withdraw own acts"
                       :option/scope {:description "withdraw own acts"
                                      :act-kinds [:act/withdrawal]
                                      :rule-ids ["act:rule-a"]
                                      :grant-until grant-until}}
        o (offer "act:offer-grant" [scoped-option])
        e (evidence "e:grant-yes" "joe" "session-a" "yes" "agent-a")
        {:keys [request! calls docs]} (fake-store [o] {"e:grant-yes" e})]
    (with-redefs [store/request! request!]
      (let [first-r ((handler) (req "yes" "e:grant-yes"))
            first-body (response-body first-r)
            agreement-id (get-in first-body [:record :id])
            grant-id (get-in first-body [:grant :id])
            grant-edge (get @docs grant-id)
            props (:hx/props grant-edge)
            at (get-in first-body [:record :agreement/at])]
        (is (= 200 (:status first-r)))
        (is (= grant-until (get-in first-body [:grant :until])))
        (is (= {:description "withdraw own acts"
                :act-kinds [:act/withdrawal]
                :rule-ids ["act:rule-a"] :own-acts-only true}
               (:grant/scope props)))
        (is (= {:from at :until grant-until} (:grant/interval props)))
        (is (= {:kind :agreement :offer "act:offer-grant"
                :agreement agreement-id}
               (:grant/source props)))
        (is (= :granted
               (:status (grant-record/grant-covers?
                         [grant-edge] "agent-a" :act/withdrawal at
                         {:target-signer "agent-a"}))))
        (is (= :not-own-act
               (:reason (grant-record/grant-covers?
                         [grant-edge] "agent-a" :act/withdrawal at
                         {:target-signer "agent-b"}))))
        ;; the written grant covers nothing beyond what Joe was shown
        (is (= :out-of-scope
               (:reason (grant-record/grant-covers?
                         [grant-edge] "agent-a" :pattern-card/selection at
                         {:target-signer "agent-a"}))))
        (is (= :no-candidate
               (:reason (grant-record/grant-covers?
                         [grant-edge] "agent-b" :act/withdrawal at
                         {:target-signer "agent-b"}))))
        (is (= :out-of-time
               (:reason (grant-record/grant-covers?
                         [grant-edge] "agent-a" :act/withdrawal grant-until
                         {:target-signer "agent-a"}))))
        (is (str/includes? (header)
                           (str "; grant " grant-id " until " grant-until)))
        (let [replay ((handler) (req "yes" "e:grant-yes"))]
          (is (= 200 (:status replay)))
          (is (= grant-id (get-in (response-body replay) [:grant :id]))))
        (is (= 2 (count (filter #(= "POST" (first %)) @calls))))))))

(deftest agreement-only-and-grant-failure-preserve-the-agreement
  (let [description (offer "act:offer-description" [(first options)])
        e1 (evidence "e:description" "joe" "session-a" "yes" "agent-a")
        store-1 (fake-store [description] {"e:description" e1})]
    (with-redefs [store/request! (:request! store-1)]
      (let [r ((handler) (req "yes" "e:description"))
            b (response-body r)]
        (is (= 200 (:status r)))
        (is (nil? (:grant b)))
        (is (= "agreement-only" (:grant-reason b)))
        (is (str/includes? (header) "; agreement only, no grant"))
        (is (= 1 (count (filter #(= "POST" (first %)) @(:calls store-1))))))))
  (turn-notice/reset-state!)
  (let [grant-until (str (.plusSeconds (Instant/now) 3600))
        o (offer "act:offer-failure"
                 [{:option/id "1" :option/label "finite"
                   :option/scope {:description "finite"
                                  :act-kinds [:act/withdrawal]
                                  :grant-until grant-until}}])
        e (evidence "e:failure" "joe" "session-a" "yes" "agent-a")
        {:keys [request! calls docs fail-grant?]} (fake-store [o] {"e:failure" e})]
    (reset! fail-grant? true)
    (with-redefs [store/request! request!]
      (let [r ((handler) (req "yes" "e:failure"))
            b (response-body r)]
        (is (= 200 (:status r)))
        (is (some? (get-in b [:record :id])))
        (is (nil? (:grant b)))
        (is (= "grant-write-failed" (:grant-reason b)))
        (is (some #(= :agreement/record (:hx/type %)) (vals @docs)))
        (is (str/includes? (header) "; grant write failed"))
        (is (= 2 (count (filter #(= "POST" (first %)) @calls))))))))

(deftest ambiguous-notice-renders-at-most-six-candidates
  (turn-notice/publish!
   {:agent "agent-a" :session "session-a" :notice-id "e:eight"
    :kind "agreement-ambiguous"
    :candidates (mapv (fn [n] {:offer-id (str "act:offer-" n)
                               :option-id (str n)})
                      (range 1 9))})
  (let [h (header)]
    (doseq [n (range 1 7)]
      (is (str/includes? h (str "act:offer-" n " " n))))
    (is (not (str/includes? h "act:offer-7 7")))
    (is (not (str/includes? h "act:offer-8 8")))))
