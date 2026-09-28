(ns futon3c.transport.disclosure-audit-http-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.act-harness :as harness]
            [futon3c.agency.act-stamp :as stamp]
            [futon3c.agency.disclosure-audit :as audit]
            [futon3c.agency.disclosure-record :as disclosure]
            [futon3c.agency.pattern-card-record :as withdrawal]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def job-id "invoke-audit")
(def d {:id "act:d" :kind :disclosure/choice :schema 1 :author "codex-5"
        :at "2026-09-28T20:00:00Z" :source-job job-id
        :unspecified "x" :chosen "y" :affects {:kind :file :id "x.clj"}
        :inside-request {:basis :source-span :quote "x"
                         :text-sha256 (apply str (repeat 64 "a"))}
        :act/stamp (stamp/stamp "codex-5" "codex-5"
                                 {:dispatch-edge "edge:e"} :declared)
        :act/harness (harness/plain "test")})
(def w {:id "act:w" :kind :act/withdrawal :author "claude-17"
        :target "act:d" :status :effective :basis {:kind :dispatch-edge}
        :reason "withdraw" :at "2026-09-28T20:01:00Z"
        :act/stamp (stamp/stamp "claude-17" "claude-17"
                                 {:dispatch-edge "edge:e"} :declared)
        :act/harness (harness/plain "test")})

(defn handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn body [response] (json/parse-string (:body response) true))

(deftest audit-route-is-get-only-and-returns-basis
  (let [calls (atom [])
        route-id (audit/routing-job-id "act:w")
        fake-request
        (fn [_ method path _]
          (swap! calls conj [method path])
          (cond
            (str/includes? path "type=disclosure%2Fchoice")
            {:hyperedges [(disclosure/->hyperedge d)]}
            (str/includes? path "type=act%2Fwithdrawal")
            {:hyperedges [(withdrawal/record->hyperedge w)]}
            (str/includes? path "type=interpretation%2Fnegation-decline")
            {:entries []}
            (str/includes? path "type=interpretation%2Fnegation")
            {:entries []}
            (str/ends-with? path "act%3Agrant")
            {:hx/id "act:grant" :hx/type :grant/record}
            (str/ends-with? path "act%3Aghost")
            (throw (ex-info "missing" {:status 404}))
            :else (throw (ex-info "unexpected GET" {:path path}))))]
    (with-redefs [store/request! fake-request
                  http/disclosure-audit-job
                  (fn [_] {:job-id job-id
                           :result "Stored act:grant and missing act:ghost"})
                  http/disclosure-audit-routing-job
                  (fn [id] (when (= route-id id)
                             {:job-id id :agent-id "codex-5"
                              :bellback-of job-id}))]
      (let [response ((handler) {:request-method :get
                                 :uri "/api/alpha/disclosure/audit"
                                 :query-string "job=invoke-audit"})
            result (body response)]
        (is (= 200 (:status response)) (pr-str result))
        (is (= "withdrawn" (get-in result [:disclosures 0 :status])))
        (is (= [{:id "act:ghost" :reason "disclosure-unrecorded"}]
               (:findings result)))
        (is (= 1 (get-in result [:basis :rows :routing-jobs])))
        (is (true? (get-in result [:basis :negation-check :run?])))
        (is (every? #(= "GET" (first %)) @calls))))))

(defn- negation [id jobs target]
  {:evidence/id id :evidence/type :interpretation/negation
   :evidence/body {:source-jobs jobs :target target :intent "withdraw"
                   :resolution "single-standing"}})

(deftest audit-reports-stored-negation-without-effect
  ;; P12-5-3: a stored negation of a standing disclosure of this job, with no
  ;; withdrawal, is a finding; a negation bound to another job is not read.
  (let [pages (atom 0)
        fake-request
        (fn [_ _ path _]
          (cond
            (str/includes? path "type=disclosure%2Fchoice")
            {:hyperedges [(disclosure/->hyperedge d)]}
            (str/includes? path "type=act%2Fwithdrawal") {:hyperedges []}
            (str/includes? path "type=interpretation%2Fnegation-decline")
            {:entries []}
            (and (str/includes? path "type=interpretation%2Fnegation")
                 (str/includes? path "cursor-id=n1"))
            (do (swap! pages inc)
                {:entries [(negation "neg:mine" [job-id] "act:d")]})
            (str/includes? path "type=interpretation%2Fnegation")
            (do (swap! pages inc)
                {:entries [(negation "neg:other" ["invoke-other"] "act:d")]
                 :next-cursor {:at "2026-09-28T20:00:00Z" :id "n1"}})
            :else (throw (ex-info "unexpected GET" {:path path}))))]
    (with-redefs [store/request! fake-request
                  http/disclosure-audit-job
                  (fn [_] {:job-id job-id :result "report"})]
      (let [result (body ((handler) {:request-method :get
                                     :uri "/api/alpha/disclosure/audit"
                                     :query-string "job=invoke-audit"}))]
        (is (= 2 @pages))
        (is (= 1 (get-in result [:basis :rows :interpretations])))
        (is (= [{:reason "negation-without-effect" :disclosure-id "act:d"
                 :interpretation-id "neg:mine"}]
               (filter #(= "negation-without-effect" (:reason %))
                       (:findings result))))))))
