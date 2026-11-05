(ns futon3c.agency.operator-turn-source-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.operator-turn-source :as source]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def session "seat-session")
(def operator
  {:evidence/id "turn:joe" :evidence/session-id session
   :evidence/in-reply-to "turn:agent"
   :evidence/body {:event "chat-turn" :role "user" :text "continue"}})
(def parked-predecessor
  {:evidence/id "turn:agent" :evidence/session-id session
   :evidence/origin {:kind "harness" :actor "parked-resume"
                     :source-id "park:one"}
   :evidence/body {:event "chat-turn" :role "assistant" :text "summary"}})

(defn woken [jobs]
  {:evidence/id "history:woken" :evidence/type :promise/woken
   :evidence/body
   {:history/format 3
    :awaiting (vec (sort jobs))
    :history/payload-edn
    (pr-str {:record {:id "park:one" :agent "claude-17"
                      :awaiting (set jobs) :arrived {}}
             :changes [] :predecessor nil})}})

(defn woken-edge [jobs]
  {:hx/id "promise-log:woken" :hx/type :promise/history-event
   :hx/endpoints ["park:one" "claude-17"]
   :hx/props {:history/payload-edn (get-in (woken jobs) [:evidence/body :history/payload-edn])
              :history/promise-id "park:one" :history/promise-sequence 2
              :history/transition-type :promise/woken
              :history/event-time "2026-09-28T00:00:00Z"
              :history/evidence-id "promise-history:woken"}})

(deftest park-resume-resolves-one-or-many-awaiting-jobs
  (is (= {:source-jobs ["job:a"] :basis :park-resume :park-id "park:one"}
         (source/source-jobs-for-turn operator parked-predecessor
                                      [(woken ["job:a"])])))
  (is (= #{"job:a" "job:b"}
         (set (:source-jobs
               (source/source-jobs-for-turn operator parked-predecessor
                                            [(woken ["job:a" "job:b"])]))))))

(deftest park-resume-can-be-carried-by-harness-through-an-auxiliary-chain-row
  (let [previous (-> parked-predecessor
                     (assoc :evidence/origin {:kind "agent" :actor "claude-17"})
                     (assoc :evidence/harness
                            {:kind "none" :basis "producer-context"
                             :source-ref "park:one"}))
        operator (assoc operator :chain/previous-agent-id "turn:agent")]
    (is (= ["job:a"]
           (:source-jobs (source/source-jobs-for-turn
                          operator previous [(woken ["job:a"])]))))))

(deftest bellback-and-ordinary-predecessors
  (is (= {:source-jobs ["original-job"] :basis :bellback}
         (source/source-jobs-for-turn
          operator (assoc parked-predecessor
                          :evidence/origin {:kind "agent" :actor "claude-17"}
                          :bellback-of "original-job") [])))
  (is (= {:source-jobs [] :basis :none}
         (source/source-jobs-for-turn
          operator (assoc parked-predecessor
                          :evidence/origin {:kind "agent" :actor "claude-17"}) []))))

(deftest displayed-job-id-is-never-a-source
  (let [previous (-> parked-predecessor
                     (assoc :evidence/origin {:kind "agent" :actor "claude-17"})
                     (assoc-in [:evidence/body :text]
                               "Job invoke-visible-only finished; continue it"))]
    (is (= {:source-jobs [] :basis :none}
           (source/source-jobs-for-turn operator previous [])))))

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(deftest source-jobs-route-uses-only-get
  (let [calls (atom [])
        fake-request
        (fn [_ method path body]
          (swap! calls conj [method path body])
          (cond
            (str/ends-with? path "/turn%3Ajoe") operator
            (str/ends-with? path "/turn%3Aagent") parked-predecessor
            (str/includes? path "type=promise%2Fhistory-event")
            {:hyperedges [(woken-edge ["job:a"])]}
            :else (throw (ex-info "unexpected request" {:path path}))))]
    (with-redefs [store/request! fake-request
                  http/disclosure-audit-job (constantly nil)
                  http/standing-disclosures-for-job
                  (fn [job-id] [{:id "act:d" :source-job job-id :status :standing}])]
      (binding [http/*woken-history-request!* fake-request]
        (let [response ((handler) {:request-method :get
                                   :uri "/api/alpha/operator-turn/source-jobs"
                                   :query-string "evidence=turn%3Ajoe"})
              result (json/parse-string (:body response) true)]
          (is (= 200 (:status response)) (pr-str result))
          (is (= ["job:a"] (:source-jobs result)))
          (is (= "act:d" (get-in result [:disclosures 0 :id])))
          (is (some #(str/includes? (second %) "end=park%3Aone") @calls))
          (is (not-any? #(str/includes? (second %) "tags=promise-history") @calls))
          (is (every? #(= "GET" (first %)) @calls))
          (is (every? nil? (map #(nth % 2) @calls))))))))

(deftest source-jobs-route-refuses-full-cursorless-history-window
  (let [fake-request
        (fn [_ _ path _]
          (cond
            (str/ends-with? path "/turn%3Ajoe") operator
            (str/ends-with? path "/turn%3Aagent") parked-predecessor
            (str/includes? path "type=promise%2Fhistory-event")
            {:hyperedges (vec (repeat 1000 (woken-edge ["job:late"])))}
            :else (throw (ex-info "unexpected request" {:path path}))))]
    (with-redefs [store/request! fake-request
                  http/disclosure-audit-job (constantly nil)
                  http/standing-disclosures-for-job (constantly [])]
      (binding [http/*woken-history-request!* fake-request]
        (let [response ((handler) {:request-method :get
                                   :uri "/api/alpha/operator-turn/source-jobs"
                                   :query-string "evidence=turn%3Ajoe"})]
          (is (= 409 (:status response)))
          (is (str/includes? (:body response) "truncated-input")))))))

(deftest source-jobs-route-refuses-repeated-cursor
  (let [fake-request
        (fn [_ _ path _]
          (cond
            (str/ends-with? path "/turn%3Ajoe") operator
            (str/ends-with? path "/turn%3Aagent") parked-predecessor
            (str/includes? path "type=promise%2Fhistory-event")
            {:hyperedges [] :next-cursor {:at "2026-09-28T00:00:00Z" :id "same"}}
            :else (throw (ex-info "unexpected request" {:path path}))))]
    (with-redefs [store/request! fake-request
                  http/disclosure-audit-job (constantly nil)]
      (binding [http/*woken-history-request!* fake-request]
        (let [response ((handler) {:request-method :get
                                   :uri "/api/alpha/operator-turn/source-jobs"
                                   :query-string "evidence=turn%3Ajoe"})]
          (is (= 409 (:status response)))
          (is (str/includes? (:body response) "truncated-input")))))))
